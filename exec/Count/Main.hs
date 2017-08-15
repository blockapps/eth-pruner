{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Default
import qualified Database.LevelDB             as DB
import qualified Database.LevelDB.Streaming   as DB
import           Options.Applicative
import           System.Directory             (doesDirectoryExist)


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
ldbCount db =
  DB.withIterator db
                  def
                  (\ i -> do
                      let stream = DB.keySlice i DB.AllKeys DB.Desc :: DB.Stream (ResourceT IO) DB.Key
                      totalCount <- DB.foldl' (\c _ -> c + 1) (0::Int) stream
                      liftIO . putStrLn $ show totalCount
                  )


newtype DBDir = DBDir { dbDir :: String }

cliVals :: Parser DBDir
cliVals = DBDir
       <$> argument str
                    (metavar "DB_PATH"
                   <> help "directory of the levelDB ")
