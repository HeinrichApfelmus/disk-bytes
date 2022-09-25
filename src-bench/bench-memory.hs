module Main where

import Control.Concurrent
    ( threadDelay )
import Control.Exception
    ( evaluate )
import Control.Monad
    ( forM )
import Data.ByteString
    ( ByteString )
import System.Mem.Disk
import System.Mem
    ( performGC )
import Data.IORef

import qualified Data.ByteString as BS

{-----------------------------------------------------------------------------
    Benchmark parameters ------------------------------------------------------------------------------}
chunkSize, totalSize, maxSize :: Int
chunkSize = 2000 -- bytes
maxSize   = 1000 * chunkSize -- bytes
totalSize = 3 * maxSize -- bytes

{-----------------------------------------------------------------------------
    Benchmark execution ------------------------------------------------------------------------------}
main :: IO ()
main = withDiskSqlite "bench-mem.db" $ \disk -> do
    let batches = totalSize `div` maxSize
        batchSize = maxSize `div` chunkSize

    xss <- forM [1 .. batches] $ \i -> do
        say $ "Batch number " <> show i
        say $ ".. create `DiskBytes` and garbage collect `ByteString`"
        say $ "   invariant: `DiskBytes` in WHNF does not use RAM"
        xs <- forM [1 .. batchSize] $ \j -> do
            let x = toDiskBytes disk (mkBytes chunkSize j) :: DiskBytes
            x `seq` performGC
            pure x

        say $ ".. evaluate and discard `fromDiskBytes`"
        say $ "   invariant: `DiskBytes` store valid data"
        mapM_ (evaluate . fromDiskBytes) xs

        say $ ".. [pause]"
        threadDelaySeconds 2

        say $ ".. garbage collect on-disk `DiskBytes`"
        say $ "   invariant: disk memory is released for `DiskBytes`"
              <> " that are not alive"
        performGC

        say . (\x -> "Disk size: " <> show x <> " bytes") =<< getDiskSize disk
        pure []

    evaluate $ concat xss

    pure ()

mkBytes :: Int -> Int -> ByteString
mkBytes n seed = BS.pack $ map (\x -> toEnum $ (x+seed) `mod` 2^8) [1..n]

say :: String -> IO ()
say = putStrLn

threadDelaySeconds :: Int -> IO ()
threadDelaySeconds s = threadDelay (s * 1000 * 1000)
