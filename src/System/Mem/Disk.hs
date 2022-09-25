module System.Mem.Disk
    ( -- * Synopsis
      -- $synopsis

      -- * DiskBytes 
      DiskBytes
    , toDiskBytes
    , fromDiskBytes
      -- * Disk storage space
    , Disk
    , getDiskSize
    , withDiskSqlite
    , withDiskMemory
    ) where

import Data.ByteString
    ( ByteString )
import System.Mem.Disk.Bytes
import System.Mem.Disk.DiskApi
    ( Disk, getDiskSize )
import System.Mem.Disk.Memory
    ( withDiskMemory )
import System.Mem.Disk.Sqlite
    ( withDiskSqlite )

{- $synopsis

This module introduces a data type 'DiskBytes' which represents a sequence
of bytes.
However, unlike most Haskell data types, values of this type
are not stored in volatile RAM, but on the hard disk (e.g. magnetic or SSD).
But just like ordinary Haskell data types,
this type is referentially transparent — no 'IO' is needed to access the disk!

The purpose of this data type is to allow you to make
trade-offs between RAM (scarce but fast) and disk (plenty but slow)
simply by switching between the 'ByteString' and 'DiskBytes' types —
while keeping the Haskell code pure. In order to test whether this worked out in your favor, you can use the 'withDiskMemory' function.

The on-disk storage space is represented by the 'Disk' type.
Typically, this storage is managed through a file on the file system,
e.g. using 'withDiskSqlite'.
-}
