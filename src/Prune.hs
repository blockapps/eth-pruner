{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prune where

import           Control.Exception                            hiding (catch)
import           Control.Monad                                (when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class                       (liftIO)
import qualified Data.ByteString                              as B
import qualified Data.ByteString.Base16                       as B16
import qualified Data.ByteString.Char8                        as BC
import           Data.Default
import           Data.Monoid
import qualified Database.LevelDB                             as DB

import           Database
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
  outDB <- DB.open toDir def{DB.createIfMissing=True}
  let blockNums = if bn < 15 then [0..bn]
                  else            [(bn-15)..bn]
  listMStateRootsAndBlockHashes <- mapM (stateRootAndHashFromBlockNumber inDB)
                                        blockNums
  let mStateRootsAndBlockHashes = sequence listMStateRootsAndBlockHashes

  case mStateRootsAndBlockHashes of
    Just stateRootsAndBlockHashes -> do
      let (_,targetBH) = last stateRootsAndBlockHashes
          srs = map fst stateRootsAndBlockHashes
      backupBlocksTransactionsMiscData inDB outDB targetBH
      mapM_ (copyMPTFromStateRoot inDB outDB) srs
    Nothing -> liftIO . putStrLn $ "Issue finding stateroots and hash of blocks"
  return ()


stateRootAndHashFromBlockNumber :: (DB.MonadResource m, MonadCatch m)
                                => DB.DB
                                -> Int
                                -> m (Maybe (StateRoot, B.ByteString))
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
                                 -> DB.DB
                                 -> B.ByteString
                                 -> m ()
backupBlocksTransactionsMiscData inDB outDB bh = do
  insertToLvlDB outDB "LastHeader" bh
  insertToLvlDB outDB "LastBlock" bh
  insertToLvlDB outDB "LastFast" bh
  ldbForEach inDB $ \key val ->
    case getFirst . foldMap (\v -> First $ BC.stripPrefix v key) $ [ "secure-key-"
                                                                   , "ethereum-config-"
                                                                   , "BlockchainVersion"
                                                                   , "mipmap-log-bloom-"
                                                                   , "receipts-"
                                                                   ] of
      Just _  -> insertToLvlDB outDB key val
      Nothing ->
        case BC.length key of
          -- blockHeader: headerPrefix(1) + num (uint64 big endian)(8) + hash(32)
          -- blockBody: bodyPrefix(1) + num (uint64 big endian)(8) + hash(32)
          -- blockReciept: blockRecieptPrefix(1) + num (uint64 big endian)(8) + hash(32)
          41 -> insertToLvlDB outDB key val

           -- totalDifficulty: headerPrefix(1) + num (uint64 big endian)(8) + hash(32) + tdSuffix(1)
          42 -> insertToLvlDB outDB key val

          -- blockHash: headerPrefix(1) + num (uint64 big endian)(8) + numSuffix(1)
          10 -> when (key /= "LastHeader") (insertToLvlDB outDB key val)
          33 -> case BC.length val of
            -- blockNumber: blockHashPrefix(1) + hash(32)
            8 -> insertToLvlDB outDB key val

            -- transactionMetadata: hash(32) + txMetaSuffix(1)
            _ -> case BC.unpack . B16.encode $ B.drop 32 key of
              "01" -> do
                  let hash = BC.take 32 key
                  mTx <- getValByKey inDB hash
                  case mTx of
                    Just tx -> do
                      insertToLvlDB outDB key val
                      insertToLvlDB outDB hash tx
                    Nothing -> liftIO . putStrLn $
                                "Missing Transaction" <> (BC.unpack . B16.encode $ hash)
              _ -> insertToLvlDB outDB key val
          _ -> return ()

copyMPTFromStateRoot :: (DB.MonadResource m, MonadCatch m)
                     => DB.DB
                     -> DB.DB
                     -> StateRoot
                     -> m ()
copyMPTFromStateRoot inDB outDB = recCopyMPT
  where
    recCopyMPT (StateRoot sr) = do
      let key = sr
      mVal <- getValByKey inDB sr
      case mVal of
        Nothing -> return ()
        Just val -> do
          insertToLvlDB outDB key val
          mNodeData <- catch (return . Just $
                                (rlpDecode $ rlpDeserialize val::NodeData))
                             (\ (_ :: IOException) -> return Nothing)
          case mNodeData of
            Just nd -> handleNodeData nd
            Nothing -> return ()

    handleNodeData = \case
      ShortcutNodeData { nextVal= Left (PtrRef sr') } -> recCopyMPT sr'
      ShortcutNodeData { nextVal= Right (RLPString rlpData )} -> do
        mAddrState <- catch (return . Just $
                              (rlpDecode $ rlpDeserialize rlpData::AddressState))
                              (\ (_ :: IOException) -> return Nothing)
        case mAddrState of
          Just as -> handleAddressState as
          Nothing -> return ()
      FullNodeData {choices=nodeRefs} -> do
        _ <- mapM (\nr -> case nr of
                   PtrRef sr' -> recCopyMPT sr'
                   _          -> return () )
                 nodeRefs
        return ()
      _ -> return ()

    handleAddressState as = do
      let codeHash = addressStateCodeHash as
      mCode <- getValByKey inDB codeHash
      case mCode of
        Just code -> do
          insertToLvlDB outDB codeHash code
          recCopyMPT (addressStateContractRoot as)
        Nothing   -> return ()

leftPad :: Int -> a -> [a] -> [a]
leftPad n x xs = replicate (max 0 (n - length xs)) x ++ xs
