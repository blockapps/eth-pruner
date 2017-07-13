{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Prune where
-- import           Blockchain.Format

import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import           Control.Monad.Loops
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as B
import qualified Data.ByteString.Base16       as B16
import qualified Data.ByteString.Char8        as BC
import           Data.Default
import           Data.Monoid
import qualified Database.LevelDB             as DB

instance Pretty B.ByteString where
  pretty = blue . text . BC.unpack . B16.encode


prune :: String -> String -> ResourceT IO ()
prune originDir toDir = do
  dbOrigin <- DB.open originDir def
  dbTo <- DB.open toDir def{DB.createIfMissing=True}
  backupBlocksTransactionsMiscData dbOrigin dbTo

backupBlocksTransactionsMiscData :: DB.DB
                                 -> DB.DB
                                 -> ResourceT IO ()
backupBlocksTransactionsMiscData dbOrigin dbTo = do
  ldbForEach dbOrigin $ \key val ->
    case getFirst . foldMap (\v -> First $ BC.stripPrefix v key) $ [ "LastHeader"
                                                                   , "LastBlock"
                                                                   , "LastFast"
                                                                   , "secure-key-"
                                                                   , "ethereum-config-"
                                                                   , "mipmap-log-bloom-"
                                                                   , "receipts-"
                                                                   ] of
      Just _ -> insertToLvlDB dbTo key val
      Nothing ->
        case BC.length key of
          -- blockHeader: headerPrefix(1) + num (uint64 big endian)(8) + hash(32)
          -- blockBody: bodyPrefix(1) + num (uint64 big endian)(8) + hash(32)
          -- blockReciept: blockRecieptPrefix(1) + num (uint64 big endian)(8) + hash(32)
          41 -> insertToLvlDB dbTo key val

          -- totalDifficulty: headerPrefix(1) + num (uint64 big endian)(8) + hash(32) + tdSuffix(1)
          42 -> insertToLvlDB dbTo key val

          -- blockHash: headerPrefix(1) + num (uint64 big endian)(8) + numSuffix(1)
          10  -> insertToLvlDB dbTo key val

          33 -> case BC.length val of
            -- blockNumber: blockHashPrefix(1) + hash(32)
            8 -> insertToLvlDB dbTo key val

            -- transactionMetadata: hash(32) + txMetaSuffix(1)
            _ -> do
              let hash = BC.take 32 key
              mTx <- getValByKey dbOrigin hash
              case mTx of
                Just tx -> do
                  insertToLvlDB dbTo key val
                  insertToLvlDB dbTo hash tx
                Nothing -> return ()
          _ -> return ()

copyMPTFromStateRoot :: DB.DB -> DB.DB -> B.Bytestring -> ResourceT IO ()
copyMPTFromStateRoot originDB toDB sr = do
  mVal <- getValByKey originDB sr
  case mVal of
    Nothing -> return ()
    Just val -> do
      case rlpDecode $ rlpDeserialize value::NodeData of
        EmptyNodeData -> return ()
        ShortcutNodeData ->



insertToLvlDB :: DB.DB -> B.ByteString -> B.ByteString -> ResourceT IO ()
insertToLvlDB db k v = DB.put db DB.defaultWriteOptions k v

getValByKey :: DB.DB -> B.ByteString -> ResourceT IO (Maybe B.ByteString)
sdjfldsajl;klj;fd
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

