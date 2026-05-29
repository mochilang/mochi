# MEP-75 Phase 7: vendor/autoload.php Generator (PSR-4, no composer install)

**Status**: LANDED 2026-05-30 00:00 (GMT+7)

## Goal

Implement the pre-computed `vendor/autoload.php` generator under `package3/php/autoload`. The generator produces a valid PHP autoloader from PSR-4 namespace maps and classmap entries without requiring a live `composer install`.

## Design

The `Generate(Config)` function takes a `Config` struct with PSR-4, classmap, and files entries, and produces PHP source text for `vendor/autoload.php`.

The generated PHP registers a single `spl_autoload_register` callback that:
1. Looks up the class in the PSR-4 prefix table (sorted alphabetically for determinism)
2. Converts the namespace suffix to a file path using `str_replace('\\', '/', $rel)`
3. Falls back to classmap lookup if PSR-4 lookup doesn't find the file

Always-include files (`FilesEntry`) are emitted as `require_once` before the autoloader.

The `BuildConfig([]PackageAutoload)` helper aggregates multiple packages' autoload data into a single `Config`, prefixing each path with the package's `VendorDir`.

## Files Landed

- `package3/php/autoload/autoload.go` -- Generate() + BuildConfig()
- `package3/php/autoload/autoload_test.go` -- 12 test functions

## Test Coverage

- Empty config produces PHP header but no autoloader body
- PSR-4 single entry: prefix, dir, spl_autoload_register, str_replace
- Multiple PSR-4 entries sorted alphabetically for determinism
- Classmap entry with FQCN lookup
- Files entry with require_once
- Combined PSR-4 + classmap + files
- BuildConfig aggregates multiple packages with VendorDir prefix
- BuildConfig classmap and files support
- normalisePrefix ensures trailing backslash
- Header comment ("Mochi PHP bridge", "do not edit")
