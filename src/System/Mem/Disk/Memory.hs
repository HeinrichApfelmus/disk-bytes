-- | Simple, stupid implementation of a RAM-based 'Disk' for testing.
module System.Mem.Disk.Memory where

import Data.ByteString
    ( ByteString )
import Data.Foldable
    ( foldl' )
import Data.Int
    ( Int64 )
import Data.IORef

import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified System.Mem.Disk.DiskApi as DiskApi

{-----------------------------------------------------------------------------
    Disk
------------------------------------------------------------------------------}
data Disk = Disk
    { db :: IORef (Map.Map Int64 ByteString)
    , counter :: IORef Int64
    }

-- | Create a 'Disk' in memory for the purpose of testing and profiling
-- — by swapping v'System.Mem.Disk.withDiskMemory'
-- for  v'System.Mem.Disk.withDiskSqlite' and looking at the
-- heap profile of your program, you can quickly find out whether the
-- use of t'System.Mem.Disk.DiskBytes' really helps.
--
-- Ignores the 'FilePath' argument.
withDiskMemory :: FilePath -> (DiskApi.Disk -> IO a) -> IO a
withDiskMemory _ action = do
    db <- newIORef Map.empty
    counter <- newIORef 0
    action $ mkDiskApi $ Disk{db,counter}

getDiskSize_ :: Disk -> IO Integer
getDiskSize_ Disk{db} = sumBytes <$> readIORef db
  where
    sumBytes = foldl' (\n bs -> n + fromIntegral (BS.length bs)) 0

mkDiskApi :: Disk -> DiskApi.Disk
mkDiskApi disk = DiskApi.Disk
    { DiskApi.put = put_ disk
    , DiskApi.get = get_ disk
    , DiskApi.delete = delete_ disk
    , DiskApi.getDiskSize = getDiskSize_ disk
    }

{-----------------------------------------------------------------------------
    Disk operations
------------------------------------------------------------------------------}
put_ :: Disk -> ByteString -> IO Int64
put_ Disk{db,counter} bytes = do
    index <- atomicModifyIORef' counter $ \x -> (x+1,x)
    atomicModifyIORef' db $ \db_ -> (Map.insert index bytes db_, ())
    pure index

get_ :: Disk -> Int64 -> IO ByteString
get_ Disk{db} index = do
    Just bytes <- Map.lookup index <$> readIORef db
    pure bytes

delete_ :: Disk -> Int64 -> IO ()
delete_ Disk{db} index =
    atomicModifyIORef' db $ \db_ -> (Map.delete index db_, ())
