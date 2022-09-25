-- | Representation of 'Disk' as an object-like record —
-- this allows multiple storage space implementations.
module System.Mem.Disk.DiskApi where

import Data.ByteString
    ( ByteString )
import Data.Int
    ( Int64 )

{-----------------------------------------------------------------------------
    Disk
------------------------------------------------------------------------------}
-- | Represents the on-disk storage where
-- t'System.Mem.Disk.DiskBytes' are stored.
data Disk = Disk
    { put :: ByteString -> IO Int64
    , get :: Int64 -> IO ByteString
    , delete :: Int64 -> IO ()
    , getDiskSize :: IO Integer
        -- ^ Rough estimate of the current size of the 'Disk', in bytes.
    }
