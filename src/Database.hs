module Database where

import           Control.Monad.Loops
import qualified Data.ByteString     as B
import           Data.Default
import qualified Database.LevelDB    as DB


insertToLvlDB :: (DB.MonadResource m)
              => DB.DB
              -> B.ByteString
              -> B.ByteString
              -> m ()
insertToLvlDB db = DB.put db DB.defaultWriteOptions

getValByKey :: (DB.MonadResource m)
            => DB.DB
            -> B.ByteString
            -> m (Maybe B.ByteString)
getValByKey db = DB.get db DB.defaultReadOptions

ldbForEach :: (DB.MonadResource m)
           => DB.DB
           -> (B.ByteString -> B.ByteString -> m ())
           -> m ()
ldbForEach db f = do
    i <- DB.iterOpen db def
    DB.iterFirst i
    whileM_ (DB.iterValid i) $ do
      Just key <- DB.iterKey i
      Just val <- DB.iterValue i
      f key val
      DB.iterNext i
      return ()
