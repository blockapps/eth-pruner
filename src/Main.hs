{-# LANGUAGE OverloadedStrings #-}

import           Blockchain.Database.MerklePatricia.StateRoot (StateRoot (..))
import           Control.Monad.Trans.Resource                 (runResourceT)
import qualified Data.ByteString.Base16                       as B16
import qualified Data.ByteString.Char8                        as BC
import           System.Environment                           (getArgs)


import           Prune


main :: IO ()
main = let inDBDir = "./chaindata"
           outDBDir = "./chaindata_new"
       in do
            stateRootString <- fmap (flip (!!) 0) getArgs
            let maybeStateRoot = B16.decode $ BC.pack stateRootString
                sr =
                  case maybeStateRoot of
                   (x, "") | BC.length x == 32 -> x
                   (_, "") -> error $ "stateRoot length is wrong, should be 64, is : " ++ show (length stateRootString)
                   _ -> error $ "stateRoot is not a valid base16 encoded string: " ++ show stateRootString
            runResourceT $ prune inDBDir outDBDir (StateRoot sr)

