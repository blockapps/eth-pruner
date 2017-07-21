{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Prune where

import           Control.Monad.IO.Class                       (liftIO)
import           Control.Monad.Loops
import           Control.Monad.Trans.Resource
import qualified Data.ByteString                              as B
import qualified Data.ByteString.Base16                       as B16
import qualified Data.ByteString.Char8                        as BC
import           Data.Default
import           Data.Monoid                                  ((<>))
import           Data.Monoid
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Word
import qualified Database.LevelDB                             as DB

import           Blockchain.Data.RLP
import           Blockchain.Database.MerklePatricia.NodeData  (NodeData (..),
                                                               NodeRef (..))
import           Blockchain.Database.MerklePatricia.StateRoot (StateRoot (..))
import           Blockchain.Strato.Model.Address
import           Blockchain.Strato.Model.ExtendedWord

leftPad :: Int -> a -> [a] -> [a]
leftPad n x xs = replicate (max 0 (n - length xs)) x ++ xs

prune :: String -> String -> Int -> ResourceT IO ()
prune originDir toDir bn  = do
  inDB <- DB.open originDir def
  outDB <- DB.open toDir def{DB.createIfMissing=True}
  mSRAndBH <- stateRootAndHashFromBlockNumber inDB bn
  case mSRAndBH of
    Just (sr,bh) -> do
      backupBlocksTransactionsMiscData inDB outDB bh
      copyMPTFromStateRoot inDB outDB sr
    Nothing -> liftIO . putStrLn $ "Issue finding stateroot and hash of block"
  return ()


stateRootAndHashFromBlockNumber :: DB.DB -> Int -> ResourceT IO (Maybe (StateRoot, B.ByteString))
stateRootAndHashFromBlockNumber db bn= do
  let key = "h" <> B.pack (leftPad 8 0 [fromIntegral bn]) <> "n"
  mBlockHash <- getValByKey db key
  case mBlockHash of
    Nothing -> do
      liftIO . putStrLn $ "Block " <> (show bn) <> " not found."
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


backupBlocksTransactionsMiscData :: DB.DB
                                 -> DB.DB
                                 -> B.ByteString
                                 -> ResourceT IO ()
backupBlocksTransactionsMiscData inDB outDB bh = do
  insertToLvlDB outDB "LastHeader" bh
  insertToLvlDB outDB "LastBlock" bh
  ldbForEach inDB $ \key val ->
    case getFirst . foldMap (\v -> First $ BC.stripPrefix v key) $ [ "LastFast"
                                                                   , "secure-key-"
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
          41 -> do
            insertToLvlDB outDB key val

           -- totalDifficulty: headerPrefix(1) + num (uint64 big endian)(8) + hash(32) + tdSuffix(1)
          42 -> insertToLvlDB outDB key val

          -- blockHash: headerPrefix(1) + num (uint64 big endian)(8) + numSuffix(1)
          10 -> insertToLvlDB outDB key val

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
              _ -> do
                -- liftIO . putStrLn $ "Inserting len 33 key not matching patterns: " <> (BC.unpack . B16.encode $ key)
                insertToLvlDB outDB key val
          _ -> return ()

copyMPTFromStateRoot :: DB.DB -> DB.DB -> StateRoot -> ResourceT IO ()
copyMPTFromStateRoot inDB outDB stateroot = recCopyMPT stateroot
  where
    recCopyMPT (StateRoot sr) = do
      let key = sr
      mVal <- getValByKey inDB sr
      case mVal of
        Nothing -> return ()
        Just val -> do
          insertToLvlDB outDB key val
          case rlpDecode $ rlpDeserialize val::NodeData of
            ShortcutNodeData { nextVal= Left (PtrRef sr') } -> recCopyMPT sr'
            -- ShortcutNodeData { nextVal= Right (RLPString addrData } -> do
            --   let addressState =
            FullNodeData {choices=nodeRefs} -> do
              _ <- mapM (\nr -> case nr of
                         PtrRef sr' -> recCopyMPT sr'
                         _          -> return () )
                       nodeRefs
              return ()
            _ -> return ()

insertToLvlDB :: DB.DB -> B.ByteString -> B.ByteString -> ResourceT IO ()
insertToLvlDB db k v = DB.put db DB.defaultWriteOptions k v

getValByKey :: DB.DB -> B.ByteString -> ResourceT IO (Maybe B.ByteString)
getValByKey db k  = DB.get db DB.defaultReadOptions k

ldbForEach :: DB.DB -> (B.ByteString -> B.ByteString -> ResourceT IO ()) -> ResourceT IO ()
ldbForEach db f = do
    i <- DB.iterOpen db def
    DB.iterFirst i
    whileM_ (DB.iterValid i) $ do
      Just key <- DB.iterKey i
      Just val <- DB.iterValue i
      f key val
      DB.iterNext i
      return ()

type SHA = B.ByteString
data BlockHeader =
  BlockHeader {
    parentHash       :: SHA,
    ommersHash       :: SHA,
    beneficiary      :: Address,
    stateRoot        :: StateRoot,
    transactionsRoot :: StateRoot,
    receiptsRoot     :: StateRoot,
    logsBloom        :: B.ByteString,
    difficulty       :: Integer,
    number           :: Integer,
    gasLimit         :: Integer,
    gasUsed          :: Integer,
    timestamp        :: UTCTime,
    extraData        :: Integer,
    mixHash          :: SHA,
    nonce            :: Word64
    } deriving (Eq, Read, Show)

instance RLPSerializable BlockHeader where
  rlpEncode (BlockHeader ph oh b sr tr rr lb d number' gl gu ts ed mh nonce') =
    RLPArray [
      rlpEncode ph,
      rlpEncode oh,
      rlpEncode b,
      rlpEncode sr,
      rlpEncode tr,
      rlpEncode rr,
      rlpEncode lb,
      rlpEncode d,
      rlpEncode number',
      rlpEncode gl,
      rlpEncode gu,
      rlpEncode (round $ utcTimeToPOSIXSeconds ts::Integer),
      rlpEncode ed,
      rlpEncode mh,
      rlpEncode $ B.pack $ word64ToBytes nonce'
      ]
  rlpDecode (RLPArray [ph, oh, b, sr, tr, rr, lb, d, number', gl, gu, ts, ed, mh, nonce']) =
    BlockHeader {
      parentHash=rlpDecode ph,
      ommersHash=rlpDecode oh,
      beneficiary=rlpDecode b,
      stateRoot=rlpDecode sr,
      transactionsRoot=rlpDecode tr,
      receiptsRoot=rlpDecode rr,
      logsBloom=rlpDecode lb,
      difficulty=rlpDecode d,
      number=rlpDecode number',
      gasLimit=rlpDecode gl,
      gasUsed=rlpDecode gu,
      timestamp=posixSecondsToUTCTime $ fromInteger $ rlpDecode ts,
      extraData=rlpDecode ed,
      mixHash=rlpDecode mh,
      nonce=bytesToWord64 $ B.unpack $ rlpDecode nonce'
      }
  rlpDecode x = error $ "can not run rlpDecode on BlockHeader for value " ++ show x

data AddressState =
  AddressState { addressStateNonce        :: Integer
               , addressStateBalance      :: Integer
               , addressStateContractRoot :: StateRoot
               , addressStateCodeHash     :: SHA
               }
    deriving (Eq, Read, Show)

instance RLPSerializable AddressState where
  rlpEncode a | addressStateBalance a < 0 = error $ "Error in cal to rlpEncode for AddressState: AddressState has negative balance: " ++ show a
  rlpEncode a = RLPArray [
    rlpEncode $ toInteger $ addressStateNonce a,
    rlpEncode $ toInteger $ addressStateBalance a,
    rlpEncode $ addressStateContractRoot a,
    rlpEncode $ addressStateCodeHash a
                ]

  rlpDecode (RLPArray [n, b, cr, ch]) =
    AddressState {
      addressStateNonce=fromInteger $ rlpDecode n,
      addressStateBalance=fromInteger $ rlpDecode b,
      addressStateContractRoot=rlpDecode cr,
      addressStateCodeHash=rlpDecode ch
      }
  rlpDecode x = error $ "Missing case in rlpDecode for AddressState: " ++ show x
