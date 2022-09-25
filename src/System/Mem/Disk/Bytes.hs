{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Implementation of 'DiskBytes' using GHC-specific primitives
-- like 'mkWeak#'. Unfortunately,
-- I have not been able to bring down the RAM size of 'DiskByte'
-- significantly below 100 bytes yet.
module System.Mem.Disk.Bytes
    ( DiskBytes
    , toDiskBytes
    , fromDiskBytes
    ) where

import Data.ByteString
    ( ByteString )
import Data.Int
    ( Int64 )
import System.IO.Unsafe
    ( unsafePerformIO )
import System.Mem.Disk.DiskApi
    ( Disk (..) )

import GHC.Exts (Int#, Int(I#))
import GHC.Base
import GHC.Weak

{-----------------------------------------------------------------------------
    DiskBytes
------------------------------------------------------------------------------}
-- | A sequence of bytes that is stored on disk
-- — if and only if the value is evaluated to WHNF.
--
-- The value is subject to normal garbage collection:
-- When the value is no longer referenced,
-- the disk memory will be freed (eventually).
--
-- For estimating the memory cost:
-- Even though the bulk of the data is kept on disk,
-- each WHNF of 'DiskBytes' occupies roughly @~100@ bytes of RAM;
-- this is due to administrative overhead like weak pointers and finalizers.
data DiskBytes = DiskBytes
    { index :: Int#
    , disk  :: Disk
    }

-- | Make a weak pointer for our purposes.
addFinalizerBytes :: DiskBytes -> IO () -> IO ()
addFinalizerBytes v@(DiskBytes{}) (IO finalizer) = IO $ \s ->
    case mkWeak# v v finalizer s of (# s1, w #) -> (# s1, () #)

-- | Offload a sequence of bytes onto a 'Disk'.
-- 
-- NOTE: The result must be evaluated to WHNF before the data is actually
-- on disk!
-- Also, the original 'ByteString' needs to be garbage collected
-- in for its RAM to become free.
toDiskBytes :: Disk -> ByteString -> DiskBytes
toDiskBytes disk = unsafePerformIO . mkDiskBytes disk

mkDiskBytes :: Disk -> ByteString -> IO DiskBytes
mkDiskBytes disk bytes = do
    I# index <- fromEnum <$> put disk bytes
    let diskbytes = DiskBytes{index,disk}
    diskbytes `seq` (addFinalizerBytes diskbytes $ finalizerBytes diskbytes)
    pure diskbytes

{-# NOINLINE finalizerBytes #-}
finalizerBytes :: DiskBytes -> IO ()
finalizerBytes DiskBytes{index,disk} =
    delete disk $ toEnum $ I# index

-- | Read the sequence of bytes back into RAM.
fromDiskBytes :: DiskBytes -> ByteString
fromDiskBytes = unsafePerformIO . unpackM

unpackM :: DiskBytes -> IO ByteString
unpackM DiskBytes{index,disk} =
    get disk $ toEnum $ I# index
