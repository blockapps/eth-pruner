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
import qualified Database.LevelDB                             as DB

import           Blockchain.Data.RLP
import           Blockchain.Database.MerklePatricia.NodeData  (NodeData (..),
                                                               NodeRef (..))
import           Blockchain.Database.MerklePatricia.StateRoot (StateRoot (..))


prune :: String -> String -> StateRoot -> ResourceT IO ()
prune originDir toDir sr  = do
  inDB <- DB.open originDir def
  outDB <- DB.open toDir def{DB.createIfMissing=True}
  backupBlocksTransactionsMiscData inDB outDB
  copyMPTFromStateRoot inDB outDB sr


backupBlocksTransactionsMiscData :: DB.DB
                                 -> DB.DB
                                 -> ResourceT IO ()
backupBlocksTransactionsMiscData inDB outDB = do
  ldbForEach inDB $ \key val ->
    case getFirst . foldMap (\v -> First $ BC.stripPrefix v key) $ [ "LastHeader"
                                                                   , "LastBlock"
                                                                   , "LastFast"
                                                                   , "secure-key-"
                                                                   , "ethereum-config-"
                                                                   , "mipmap-log-bloom-"
                                                                   , "receipts-"
                                                                   ] of
      Just _ -> insertToLvlDB outDB key val
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
          10  -> insertToLvlDB outDB key val

          33 -> case BC.length val of
            -- blockNumber: blockHashPrefix(1) + hash(32)
            8 -> insertToLvlDB outDB key val

            -- transactionMetadata: hash(32) + txMetaSuffix(1)
            _ -> do
              let hash = BC.take 32 key
              mTx <- getValByKey inDB hash
              case mTx of
                Just tx -> do
                  insertToLvlDB outDB key val
                  insertToLvlDB outDB hash tx
                Nothing -> liftIO . putStrLn $
                            "Missing Transaction" <> (BC.unpack . B16.encode $ hash)
          _ -> return ()

copyMPTFromStateRoot :: DB.DB -> DB.DB -> StateRoot -> ResourceT IO ()
copyMPTFromStateRoot inDB outDB stateroot = recCopyMPTF stateroot
  where
    recCopyMPTF (StateRoot sr) = do
      let key = sr
      mVal <- getValByKey inDB sr
      case mVal of
        Nothing -> return ()
        Just val -> do
          insertToLvlDB outDB key val
          case rlpDecode $ rlpDeserialize val::NodeData of
            ShortcutNodeData { nextVal= Left (PtrRef sr') } -> recCopyMPTF sr'
            FullNodeData {choices=nodeRefs} -> do
              _ <- mapM (\nr -> case nr of
                         PtrRef sr' -> recCopyMPTF sr'
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
