import           Control.Monad.Trans.Resource (runResourceT)

import           Prune


main :: IO ()
main = let inDBDir = "./chaindata"
           outDBDir = "./chaindata_new"
       in runResourceT $ prune inDBDir outDBDir
