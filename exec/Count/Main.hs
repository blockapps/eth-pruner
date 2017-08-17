{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Default
import           Options.Applicative
import           System.Directory             (doesDirectoryExist)

import qualified Database.LevelDB             as DB


main :: IO ()
main = execParser opts >>= (\cli -> do
  exists <- doesDirectoryExist (dbDir cli)
  if exists
    then runResourceT $ do
      db <- DB.open (dbDir cli) def
      ldbCount db
    else putStrLn $ "Path `" <> dbDir cli <> "` to levelDB not found." )
  where
    opts = info (cliVals <**> helper)
      ( fullDesc
     <> progDesc "Count the total keys in a levelDB"
     <> header "count - a tool to count all the items in a levelDB" )

ldbCount :: DB.DB -> ResourceT IO ()
ldbCount db = do
  i <- DB.iterOpen db def
  DB.iterFirst i
  let count = 0 :: Integer
  totalCount <- liftIO $ getTotalCount count i
  liftIO . putStrLn $ show totalCount
  where
    getTotalCount :: Integer -> DB.Iterator -> IO Integer
    getTotalCount c it = do
      valid <- DB.iterValid it
      if valid then do
          _ <- DB.iterNext it
          let c' = c+1
          c' `seq` getTotalCount c' it
      else return c


newtype DBDir = DBDir { dbDir :: String }

cliVals :: Parser DBDir
cliVals = DBDir
       <$> argument str
                    (metavar "DB_PATH"
                   <> help "directory of the levelDB ")
