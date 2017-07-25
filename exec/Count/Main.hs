{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Default
import           System.Environment           (getArgs)

import qualified Database.LevelDB             as DB



main :: IO ()
main = do
  dbDir <- fmap (!! 0) getArgs
  runResourceT $ do
    db <- DB.open dbDir def
    ldbCount db

ldbCount :: DB.DB -> ResourceT IO ()
ldbCount db = do
  i <- DB.iterOpen db def
  DB.iterFirst i
  let count = 0 :: Int
  totalCount <- getTotalCount count i
  liftIO . putStrLn $ show totalCount
  where
    getTotalCount :: Int -> DB.Iterator -> ResourceT IO Int
    getTotalCount c it = do
      valid <- DB.iterValid it
      if valid then do
          _ <- DB.iterNext it
          getTotalCount (c+1) it
      else return c
