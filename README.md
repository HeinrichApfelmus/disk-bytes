# disk-bytes

This package provides a data type `DiskBytes` which represents a sequence of bytes that is stored on disk — but in a referentially transparent manner.

The key invariant is that a value of type `DiskBytes` that has been evaluated *to weak-head normal form* (WHNF) occupies just a few words of RAM, but many bytes of on-disk storage. (We can't guarantee anything about expressions that are not in WHNF.)

The main use case for `DiskBytes` is when you have a pure Haskell program which is storing too much data, and you want to offload some of this data in a controlled, yet transparent way to disk — without `IO` doing violence to your beautiful, pure Haskell code.

The interface for `DiskBytes` consists of two *pure* functions which convert to/from an in-memory (RAM) `ByteString`:

```hs
toDiskBytes :: Disk -> ByteString -> DiskBytes
fromDiskBytes :: DiskBytes -> ByteString
```

Here, `Disk` represents the on-disk storage, typically obtained by opening a file on the file system. One can interpret `Disk` as virtual memory.

## Implementation Details

### Disk storage

Currently, the `Disk` data type is implemented as an open sqlite database file. In other words, sqlite is used to manage on-disk memory in a file. I decided to use an existing library for on-disk storage, because managing the on-disk storage (B+ trees, trade-offs between read and write speed, …) is an interesting problem, but it's not a problem that I want to solve *here*.

However, sqlite is a bit overkill, because all that we need is a key-value store. In the future, one might consider on-disk storage libraries such as [lmdb][] or [RocksDB][] — I picked sqlite simply because it has Haskell bindings that I have used before. Pull requests (with benchmarks) are welcome.

### Sqlite

TODO: Implement batching? We may want to batch *insertions* and *deletions* until the total number of bytes to process reaches a certain threshold, e.g. 10kB? Rumor has it that sequences of database operations such as `INSERT INTO` become faster if they are batched into a single transaction, rather than run as separate queries with just a few bytes each.

### Referential transparency

Internally, the `DiskBytes` type uses `unsafePerformIO`. However, this use is referentially transparent as long as the library has exclusive access to the on-disk storage. In other words, we assume that the on-disk memory is as exclusive to the Haskell run-time as we assume that RAM is exclusive to the Haskell run-time. 

TODO: Make an honest attempt to ensure that no other process can read or write to the file, e.g. by setting file permissions.

### Testing

Currently, the benchmark `memory` serves as a basic test that everything is working as intended. You can run the benchmark and look at its heap profile by executing the commands

```shell
$ cabal bench
$ hp2pretty memory.hp
```

This tests the following properties:

* `DiskBytes` that are alive do not use much RAM. (Currently, ~`100` bytes per WHNF of `DiskBytes`.)
* `DiskBytes` that are not alive are garbage collected and disk memory is freed. (This works as the value returned by `getDiskSize` stops growing.)
* The bytes of 'DiskBytes' that are alive can be loaded back into RAM. (`fromDiskBytes` does not throw an error.)

  [lmdb]: https://en.wikipedia.org/wiki/Lightning_Memory-Mapped_Database
  [rocksdb]: https://en.wikipedia.org/wiki/RocksDB
