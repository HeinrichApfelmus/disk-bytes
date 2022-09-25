# Revision history for disk-bytes

All notable changes to this project will (hopefully) be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org).

## Unreleased

(This section tracks upcoming changes.)

## 0.1.0.0 — 2022-09-25

Initial release.

### Added

* Module `System.Mem.Disk` which introduces the `DiskBytes` and `Disk` data types.
* Benchmark `memory` that tests whether RAM usage and garbage collection work as claimed.
