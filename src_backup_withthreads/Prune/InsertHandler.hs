{-# LANGUAGE OverloadedStrings #-}

module Prune.InsertHandler where

import           Control.Concurrent
import           Data.Monoid         ((<>))
-- import           Control.Concurrent.MVar
import           Control.Monad.Loops
import qualified Database.LevelDB    as DB

-- import           Database

insertLoop :: DB.DB -> MVar DB.WriteBatch -> IO ()
insertLoop _db mvWriteBucket = do
  _ <- iterateUntilM id loop True
  return ()

  where
    loop _ =  do

      putMVar
      if length writeBucket >= 1000
        then undefined
        else do
          threadDelay 5000
          return (True,writeBuffer')


