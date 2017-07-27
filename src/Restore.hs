module Restore where

import           Data.Default
import qualified Database.LevelDB as DB

import           Database


restore :: (DB.MonadResource m)
        => String
        -> String
        -> m ()
restore originDir toDir = do
  inDB <- DB.open originDir def
  outDB <- DB.open toDir def
  ldbForEach inDB (f outDB)
  return ()
  where
    f outDB k v = do
      mVal <- getValByKey outDB k
      case mVal of
        Nothing -> insertToLvlDB outDB k v
        Just _  -> return ()

