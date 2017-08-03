{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Trans.Resource (runResourceT)
import           Options.Applicative
import           System.Directory             (doesDirectoryExist)

import           Restore

main :: IO ()
main = execParser opts >>= (\cli -> do
  let inDBDir = backupDBDir cli
      outDBDir = prunedDBDir cli
  [existsIn, existsOut] <- mapM doesDirectoryExist [inDBDir, outDBDir]
  case (existsIn, existsOut) of
    (True, True) -> runResourceT $ restore inDBDir outDBDir
    (False, True) -> putStrLn $ "Path to `" <> inDBDir <> "` to levelDB not found.\
                \ Make sure the chaindata directory is in the current\
                \ directory"
    (True, False) -> putStrLn $ "Path to `" <> outDBDir <> "` to levelDB not found.\
                \ Make sure the chaindata directory is in the current\
                \ directory"
    (False, False) -> putStrLn $ "Path to both `" <> inDBDir <> "` and `"
                                 <> outDBDir <> "` levelDBs not found." )
  where
    opts = info (cliVals <**> helper)
      ( fullDesc
     <> progDesc "Restore the State Trie of a pruned node using a backup of the\
                 \ un-pruned database"
     <> header "restore - a state trie restoration tool on pruned geth/quorum nodes" )


data DBDirs = DBDirs { backupDBDir :: String
                     , prunedDBDir :: String}

cliVals :: Parser DBDirs
cliVals = DBDirs
       <$> argument str
                    (metavar "BACKUP_DB"
                   <> help "directory of backup database")
       <*> argument str
                    (metavar "PRUNED_DB"
                   <> help "directory of pruned database")

