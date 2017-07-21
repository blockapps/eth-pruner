{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Trans.Resource (runResourceT)
import           System.Environment           (getArgs)

import           Prune


main :: IO ()
main = let inDBDir = "./chaindata"
           outDBDir = "./chaindata_new"
       in do
            arg <- fmap (flip (!!) 0) getArgs
            let blockNumber = read arg :: Int
            runResourceT $ prune inDBDir outDBDir blockNumber

