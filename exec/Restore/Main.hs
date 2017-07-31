{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Monoid                  ((<>))
import           System.Directory             (doesDirectoryExist)
import           System.Environment           (getArgs)

import           Restore

main :: IO ()
main = do
  args <- getArgs
  if length args == 2
    then do
      let [inDBDir,outDBDir] = args
      [existsIn, existsOut] <- mapM doesDirectoryExist args
      case (existsIn, existsOut) of
        (True, True) -> runResourceT $ restore inDBDir outDBDir
        (False, True) -> putStrLn $ "Path to `" <> inDBDir <> "` to levelDB not found. \
                    \ Make sure the chaindata directory is in the current \
                    \ directory"
        (True, False) -> putStrLn $ "Path to `" <> outDBDir <> "` to levelDB not found. \
                    \ Make sure the chaindata directory is in the current \
                    \ directory"
        (False, False) -> putStrLn $ "Path to both `" <> inDBDir <> "` and `"
                                     <> outDBDir <> "` to levelDB not found."
  else do
    putStrLn "No arguments found."
    putStrLn ""
    putStrLn "Usage - restore <path/to/backup> <path/to/pruned>"

