{-# LANGUAGE OverloadedStrings #-}

module Prune.InsertHandler where

import           Control.Concurrent
import           Data.Monoid         ((<>))
-- import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Loops
import qualified Database.LevelDB    as DB

import           Database

insertLoop :: MVar Bool -> MVar Bool -> DB.DB -> MVar (DB.WriteBatch,Int) -> IO ()
insertLoop done mvParentDone db mvWriteBucket = do
  _ <- takeMVar done
  _ <- iterateUntilM snd loop (1024,False)
  return ()

  where
    loop (timeout,_) =  do
      (_, bucketSize) <- readMVar mvWriteBucket
      parentDone <- readMVar mvParentDone

      if bucketSize >= 100000
        then do
          void writeAndSwap
          return (timeout `div` 2,False)
        else
          if timeout > 40000000 || parentDone
            then do
              void writeAndSwap
              _ <- putMVar done True
              return (timeout,True)
            else do
              threadDelay timeout
              (_, bucketSize') <- readMVar mvWriteBucket
              if bucketSize' == bucketSize || bucketSize < 100000
                then return (timeout*2, False)
                else return (timeout, False)

    writeAndSwap = do
      (writeBucket,bucketSize) <- swapMVar mvWriteBucket ([],0)
      batchWriteToLvlDB db writeBucket
      putStrLn ("wrote to DB: " <> show bucketSize)

