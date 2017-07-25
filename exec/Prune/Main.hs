{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Trans.Resource (runResourceT)
import           System.Environment           (getArgs)

import           Prune


main :: IO ()
main = let inDBDir = "./chaindata"
           outDBDir = "./pruned_chaindata"
       in do
            arg <- fmap (!! 0) getArgs
            print arg
            let blockNumber = read arg :: Int
            runResourceT $ prune inDBDir outDBDir blockNumber

