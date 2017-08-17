{-# LANGUAGE OverloadedStrings #-}

module Prune.InsertHandler where

import           Control.Concurrent
import           Data.Monoid         ((<>))
-- import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Loops
import qualified Database.LevelDB    as DB

import           Database

insertLoop :: DB.DB -> MVar DB.WriteBatch -> IO ()
insertLoop db mvWriteBucket = do
  _ <- iterateUntilM snd loop (1,False)
  return ()

  where
    loop (timeout,_) =  do
      writeBucket <- readMVar mvWriteBucket
      let bucketSize = length writeBucket
      if bucketSize >= 50000
        then do
          void writeAndSwap
          return (timeout,False)
        else
          if timeout > 1000000
            then do
              void writeAndSwap
              return (timeout,True)
            else do
              threadDelay timeout
              writeBucket' <- readMVar mvWriteBucket
              if length writeBucket' == bucketSize
                then return (timeout*10, False)
                else return (timeout `div` 10 ,False)

    writeAndSwap = do
      writeBucket <- swapMVar mvWriteBucket []
      batchWriteToLvlDB db writeBucket
      putStrLn ("wrote to DB: " <> show (length writeBucket))

