{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Prune where

import           Control.Concurrent                           (forkIO,
                                                               killThread)
import           Control.Concurrent.MVar
import           Control.Exception                            hiding (catch)
import           Control.Monad                                (when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class                       (liftIO)
import qualified Data.ByteString                              as B
import qualified Data.ByteString.Base16                       as B16
import qualified Data.ByteString.Char8                        as BC
import           Data.Default
import           Data.Maybe                                   (catMaybes)
import           Data.Monoid
import qualified Database.LevelDB                             as DB

import           Database
import           Prune.InsertHandler
import           Prune.Types

import           Blockchain.Data.RLP
import           Blockchain.Database.MerklePatricia.NodeData  (NodeData (..),
                                                               NodeRef (..))
import           Blockchain.Database.MerklePatricia.StateRoot (StateRoot (..))




prune :: (DB.MonadResource m, MonadCatch m)
      => String
      -> String
      -> Int
      -> m ()
prune originDir toDir bn  = do
  inDB <- DB.open originDir def
  outDB <- DB.open toDir def{DB.createIfMissing=True, DB.writeBufferSize=104857600}
  writeBucket <- liftIO $ newMVar ([] :: DB.WriteBatch, 0)
  insertHandlerDone <- liftIO $ newMVar False
  done <- liftIO $ newMVar False

  -- Accumulate writes to LevelDB and bulk insert in a separate thread.
  threadId <- liftIO . forkIO $ insertLoop
                                  insertHandlerDone
                                  done
                                  outDB
                                  writeBucket

  -- Pruning
  let blockNums = if bn < 15 then [0..bn]
                  else            [(bn-15)..bn]
  mStateRootsAndBlockHashes <- liftIO (sequence <$> mapM (stateRootAndHashFromBlockNumber inDB)
                                                 blockNums)
  case mStateRootsAndBlockHashes of
    Just stateRootsAndBlockHashes -> do
      let (_,targetBH) = last stateRootsAndBlockHashes
          stateRoots   = map fst stateRootsAndBlockHashes
          countSR = length stateRoots
      liftIO . putStrLn $ "Copying blocks and transactions"
      backupBlocksTransactionsMiscData inDB writeBucket targetBH
      liftIO . putStrLn $ "Done ..."
      liftIO $ do
        privSRs <- catMaybes <$> mapM (getPrivateStateRoot inDB) stateRoots
        putStrLn ("Copying from " <> show (countSR * 2)  <> "state roots")
        mapM_ (copyMPTFromStateRoot inDB writeBucket) stateRoots
        mapM_ (copyMPTFromStateRoot inDB writeBucket) privSRs
        _ <- liftIO $ swapMVar done True
        _ <- liftIO $ takeMVar insertHandlerDone
        putStrLn "Done ..."
        killThread threadId
    Nothing -> liftIO . putStrLn $ "Issue finding stateroots and hash of blocks"
  return ()

getPrivateStateRoot :: DB.DB
                     -> StateRoot
                     -> IO (Maybe StateRoot)
getPrivateStateRoot db (StateRoot sr) =  (StateRoot <$>) <$> getValByKey db ("P" <> sr)

stateRootAndHashFromBlockNumber :: DB.DB
                                -> Int
                                -> IO (Maybe (StateRoot, B.ByteString))
stateRootAndHashFromBlockNumber db bn = do
  let key = "h" <> B.pack (leftPad 8 0 [fromIntegral bn]) <> "n"
  mBlockHash <- getValByKey db key
  case mBlockHash of
    Nothing -> do
      liftIO . putStrLn $ "Block " <> show bn <> " not found."
      return Nothing
    Just bh -> do
      mSR <- stateRootFromBlockHash bh
      case mSR of
        Nothing -> return Nothing
        Just sr -> return . Just $ (sr,bh)
  where
    stateRootFromBlockHash bh = do
      let key = "h" <> B.pack (leftPad 8 0 [fromIntegral bn]) <> bh
      mblockBodyRLP <- getValByKey db key
      case mblockBodyRLP of
        Nothing           -> return Nothing
        Just blockBodyRLP ->
          let blockHeader = rlpDecode . rlpDeserialize $ blockBodyRLP :: BlockHeader
          in return . Just $ stateRoot blockHeader


backupBlocksTransactionsMiscData :: (DB.MonadResource m, MonadCatch m)
                                 => DB.DB
                                 -> MVar (DB.WriteBatch,Int)
                                 -> B.ByteString
                                 -> m ()
backupBlocksTransactionsMiscData inDB writeBucket bh = do
  iter <- DB.iterOpen inDB def
  liftIO $ do
    pushWriteBucket writeBucket "LastHeader" bh
    pushWriteBucket writeBucket "LastBlock" bh
    pushWriteBucket writeBucket "LastFast" bh
    ldbForEach iter $ \key val ->
      case getFirst . foldMap (\v -> First $ BC.stripPrefix v key) $ [ "secure-key-"
                                                                     , "ethereum-config-"
                                                                     , "BlockchainVersion"
                                                                     , "mipmap-log-bloom-"
                                                                     , "receipts-"
                                                                     ] of
        Just _  -> pushWriteBucket writeBucket key val
        Nothing ->
          case BC.length key of
            -- totalDifficulty: headerPrefix(1) + num (uint64 big endian)(8) + hash(32) + tdSuffix(1)
            42 -> pushWriteBucket writeBucket key val

            -- blockHeader: headerPrefix(1) + num (uint64 big endian)(8) + hash(32)
            -- blockBody: bodyPrefix(1) + num (uint64 big endian)(8) + hash(32)
            -- blockReciept: blockRecieptPrefix(1) + num (uint64 big endian)(8) + hash(32)
            41 -> pushWriteBucket writeBucket key val

            33 -> case BC.length val of
              -- blockNumber: blockHashPrefix(1) + hash(32) -> blockNumber(8)
              8 -> pushWriteBucket writeBucket key val

              _ -> case BC.unpack . B16.encode $ B.drop 32 key of
                -- transactionMetadata: hash(32) + txMetaSuffix(1)
                "01" -> do
                    let hash = BC.take 32 key
                    mTx <- getValByKey inDB hash
                    case mTx of
                      Just tx -> do
                        pushWriteBucket writeBucket key val
                        pushWriteBucket writeBucket hash tx
                      Nothing -> putStrLn $
                                  "Missing Transaction" <> (BC.unpack . B16.encode $ hash)


                -- privateRoot: privateRootPrefix(1) + hash(32)
                _ -> pushWriteBucket writeBucket key val

            -- blockHash: headerPrefix(1) + num (uint64 big endian)(8) + numSuffix(1)
            -- privateBlockBloom: bloomPrefix(2) + num (uint64 big endian)(8)
            10 -> when (key /= "LastHeader") (pushWriteBucket writeBucket key val)

            _ -> return ()

copyMPTFromStateRoot :: DB.DB
                     -> MVar (DB.WriteBatch,Int)
                     -> StateRoot
                     -> IO ()
copyMPTFromStateRoot inDB writeBucket = recCopyMPT
  where
    recCopyMPT (StateRoot sr) = do
      let key = sr
      mVal <- getValByKey inDB sr
      case mVal of
        Nothing -> return ()
        Just val -> do
          pushWriteBucket writeBucket key val
          mNodeData <- catch (Just <$> liftIO (evaluate
                                (rlpDecode $ rlpDeserialize val::NodeData)))
                             (\ (_ :: IOException) -> return Nothing)
          case mNodeData of
            Just nd -> nodeDataHandler nd
            Nothing -> return ()

    nodeDataHandler = \case
      ShortcutNodeData { nextVal= Left (PtrRef sr') } -> recCopyMPT sr'
      ShortcutNodeData { nextVal= Right (RLPString rlpData )} -> do
        mAddrState <- catch (Just <$> liftIO (evaluate
                              (rlpDecode $ rlpDeserialize rlpData::AddressState)))
                              (\ (_ :: SomeException) -> return Nothing)
        case mAddrState of
          Just as -> addressStateHandler as
          Nothing -> return ()
      FullNodeData {choices=nodeRefs} -> do
        _ <- mapM (\nr -> case nr of
                   PtrRef sr' -> recCopyMPT sr'
                   _          -> return () )
                 nodeRefs
        return ()
      _ -> return ()

    addressStateHandler as = do
      let codeHash = addressStateCodeHash as
      mCode <- getValByKey inDB codeHash
      case mCode of
        Just code -> do
          pushWriteBucket writeBucket codeHash code
          recCopyMPT (addressStateContractRoot as)
        Nothing   -> return ()

leftPad :: Int -> a -> [a] -> [a]
leftPad n x xs = replicate (max 0 (n - length xs)) x ++ xs

pushWriteBucket :: MVar (DB.WriteBatch,Int) -> B.ByteString -> B.ByteString -> IO ()
pushWriteBucket mvWriteBucket k v = do
  (writeBucket,bucketSize) <- takeMVar mvWriteBucket
  let kvOp = DB.Put k v
      c = bucketSize+1
      newBucket = c `seq` (kvOp:writeBucket,c)
  putMVar mvWriteBucket newBucket

