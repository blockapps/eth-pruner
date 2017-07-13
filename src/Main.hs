import           Control.Monad.Trans.Resource (runResourceT)

import           Prune


main :: IO ()
main = let fromDir = "./chaindata"
           toDir = "./chaindata_new"
       in runResourceT $ prune fromDir toDir
