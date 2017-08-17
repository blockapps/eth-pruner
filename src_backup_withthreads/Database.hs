module Database where

-- import           Control.Concurrent
-- import           Control.Monad
import           Control.Monad.Loops
import qualified Data.ByteString     as B
import qualified Database.LevelDB    as DB


insertToLvlDB :: DB.DB
              -> B.ByteString
              -> B.ByteString
              -> IO ()
insertToLvlDB db  = DB.put db DB.defaultWriteOptions

batchWriteToLvlDB :: DB.DB
                  -> DB.WriteBatch
                  -> IO ()
batchWriteToLvlDB db = DB.write db DB.defaultWriteOptions

getValByKey :: DB.DB
            -> B.ByteString
            -> IO (Maybe B.ByteString)
getValByKey db = DB.get db DB.defaultReadOptions{DB.fillCache=False}

ldbForEach :: DB.Iterator
           -> (B.ByteString -> B.ByteString -> IO ())
           -> IO ()
ldbForEach i f = do
    DB.iterFirst i
    whileM_ (DB.iterValid i) $ do
      Just key <- DB.iterKey i
      Just val <- DB.iterValue i
      f key val
      DB.iterNext i
      return ()
