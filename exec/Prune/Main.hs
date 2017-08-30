{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Trans.Resource (runResourceT)
import           Options.Applicative
import           System.Directory             (doesDirectoryExist)
import           Prune


main :: IO ()
main = execParser opts >>= (\cli ->
  let inDBDir = inputDir cli
      outDBDir = outputDir cli
  in do
    exists <- doesDirectoryExist inDBDir

    if exists
      then runResourceT $ prune inDBDir outDBDir (blockNumber cli)
      else putStrLn $ "Path `" <> inDBDir <> "` to levelDB not found.\
                  \ Make sure the chaindata directory is in the current\
                  \ directory" )
  where
    opts = info (cliVals <**> helper)
      ( fullDesc
     <> progDesc "Prune the State Trie from given BLOCK"
     <> header "prune - a state trie pruning tool for geth/quorum" )



data BlockNum = BlockNum { blockNumber :: Int
                         , inputDir :: FilePath
                         , outputDir :: FilePath
                         }

cliVals :: Parser BlockNum
cliVals = BlockNum
       <$> argument auto (metavar "BlockNumber" <> help "block number to prune from")
       <*> strArgument (metavar "InputDirectory" <>  help "location of the database to prune")
       <*> strArgument (metavar "OutputDirectory" <> help "location to dump the pruned database")
