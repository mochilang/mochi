# MEP-75 Phase 13: Phar Distribution Path

**Status**: LANDED 2026-05-30 00:38 (GMT+7)

## Goal

Implement Phar archive generation support for the PHP library distribution path. Given a package name, PSR-4 namespace, and optional compression config, emit `stub.php` (the Phar bootstrap) and `build.php` (the builder script) so a Mochi-emitted PHP library can be distributed as a single self-contained `.phar` file.

## Design

`Build(cfg Config)` is the main entry point. It validates `PharName` and `PSR4Namespace`, then produces two files:

**stub.php** - the Phar stub embedded at the archive head:
- Calls `Phar::mapPhar` to register the archive
- Registers an `spl_autoload_register` PSR-4 loader rooted at `phar://archive.phar/src/`
- Ends with `__HALT_COMPILER()` as required by the Phar format

**build.php** - the builder script run by the developer:
- Uses `RecursiveIteratorIterator` + `RecursiveDirectoryIterator` to add all `.php` files from `src/`
- Calls `$phar->setStub(file_get_contents('stub.php'))`
- Optionally calls `compressFiles(Phar::GZ)` or `compressFiles(Phar::BZ2)`
- Usage: `php -d phar.readonly=0 build.php`

`Compression` enum provides `CompressNone` (default), `CompressGZ`, and `CompressBZ2` options.

`Config.StubPreamble` allows injecting a banner comment into the stub.

## Files Landed

- `package3/php/pharemit/pharemit.go` -- Build + renderStub + renderBuild
- `package3/php/pharemit/pharemit_test.go` -- 13 test functions

## Test Coverage

- Required field validation (PharName, PSR4Namespace)
- Both expected files produced (stub.php, build.php)
- PharFileName set correctly
- stub.php contains Phar::mapPhar and __HALT_COMPILER
- stub.php contains spl_autoload_register with PSR-4 namespace
- stub.php references the phar file name
- build.php contains RecursiveIteratorIterator, addFile, setStub
- build.php uses default src/ dir
- build.php respects custom SrcDir
- CompressGZ emits Phar::GZ call
- CompressBZ2 emits Phar::BZ2 call
- CompressNone emits no compressFiles call
- StubPreamble injected as comment into stub
