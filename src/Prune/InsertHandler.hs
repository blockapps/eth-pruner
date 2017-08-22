{-# LANGUAGE OverloadedStrings #-}

module Prune.InsertHandler where

import           Control.Concurrent
import           Data.Monoid         ((<>))
-- import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Loops
import qualified Database.LevelDB    as DB
import           System.IO

import           Database

insertLoop :: MVar Bool -> MVar Bool -> DB.DB -> MVar (DB.WriteBatch,Int) -> IO ()
insertLoop done mvParentDone db mvWriteBucket = do
  hSetBuffering stdout LineBuffering
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
          if  parentDone
            then do
              void writeAndSwap
              _ <- putMVar done True
              return (timeout,True)
            else do
              putStrLn ( "timing out for: "
                      <> show timeout
                      <> " Current bucket size is:"
                      <> show bucketSize
                      <> " entries to LevelDB")
              threadDelay timeout
              (_, bucketSize') <- readMVar mvWriteBucket
              if ( bucketSize' == bucketSize
                || bucketSize < 100000 )
                && timeout < 16777216
                then return (timeout*2, False)
                else return (timeout, False)

    writeAndSwap = do
      putStrLn "Swapping"
      (writeBucket,bucketSize) <- swapMVar mvWriteBucket ([],0)
      putStrLn ("writing " <> show bucketSize <> " entries to LevelDB")
      batchWriteToLvlDB db writeBucket
      putStrLn "..wrote"

