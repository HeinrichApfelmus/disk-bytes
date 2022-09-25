-- | Simpler implementation of 'DiskBytes' using 'mkWeakIORef'.
-- It looks like this implementation is not significantly worse than the
-- one trying to use GHC-specific primitives.
module System.Mem.Disk.BytesPlain
    ( DiskBytes
    , toDiskBytes
    , fromDiskBytes
    ) where

import Data.ByteString
    ( ByteString )
import Data.Int
    ( Int64 )
import Data.IORef
    ( IORef, newIORef, readIORef, mkWeakIORef )
import System.IO.Unsafe
    ( unsafePerformIO )
import System.Mem.Disk.DiskApi
    ( Disk (..) )

{-----------------------------------------------------------------------------
    DiskBytes
------------------------------------------------------------------------------}
-- | A sequence of bytes that is stored on disk
-- -- if and only if the value is evaluated to WHNF.
--
-- The value is subject to normal garbage collection:
-- When the value is no longer referenced,
-- the disk memory will be freed (eventually).
newtype DiskBytes = DiskBytes { unDiskBytes :: IORef DiskBytesData }

data DiskBytesData = DiskBytesData
    { index :: !Int64
    , disk  :: Disk
    }

-- | Offload a sequence of bytes onto a 'Disk'.
-- 
-- NOTE: The result must be evaluated to WHNF before the data actually
-- on disk! Also keep in mind that the original 'ByteString' needs
-- to be garbage collected.
toDiskBytes :: Disk -> ByteString -> DiskBytes
toDiskBytes disk = unsafePerformIO . mkDiskBytes disk

mkDiskBytes :: Disk -> ByteString -> IO DiskBytes
mkDiskBytes disk bytes = do
    index <- put disk bytes
    ptr <- newIORef DiskBytesData{index, disk}
    _ <- mkWeakIORef ptr (finalizer ptr)
    pure $ DiskBytes ptr

finalizer :: IORef DiskBytesData -> IO ()
finalizer ptr = do
    DiskBytesData{index,disk} <- readIORef ptr
    delete disk index

-- | Read the sequence of bytes back into RAM.
fromDiskBytes :: DiskBytes -> ByteString
fromDiskBytes = unsafePerformIO . unpackM

unpackM :: DiskBytes -> IO ByteString
unpackM (DiskBytes ptr) = do
    DiskBytesData{index,disk} <- readIORef ptr
    get disk index
