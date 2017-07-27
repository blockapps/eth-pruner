{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Default
import           Data.Monoid                  ((<>))
import           System.Directory             (doesDirectoryExist)
import           System.Environment           (getArgs)

import qualified Database.LevelDB             as DB



main :: IO ()
main = do
  args <- getArgs
  if length args == 1
    then do
      let dbDir = head args
      exists <- doesDirectoryExist dbDir
      if exists
        then runResourceT $ do
          db <- DB.open dbDir def
          ldbCount db
        else putStrLn $ "Path `" <> dbDir <> "` to levelDB not found."
  else do
    putStrLn "No arguments found."
    putStrLn ""
    putStrLn "Usage - count /path/to/levelDB"


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
