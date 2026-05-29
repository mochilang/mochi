# MEP-75 Phase 8: mochi.lock [[php-package]] Integration + --check Mode

**Status**: LANDED 2026-05-30 00:00 (GMT+7)

## Goal

Implement the `mochi.lock` `[[php-package]]` table under `package3/php/lock`. Each entry records the Composer package name, version, dist-sha256 (content-addressed cache key), reflection-sha256, PSR-4 autoload map, PHP version constraint, and direct dependencies.

Also implements `--check` mode: drift detection that compares locked SHA-256 values against on-disk state.

## Design

### PhpPackage struct

Each `[[php-package]]` entry stores:
- `Name`: Composer package name (vendor/package)
- `Version` + `VersionNormalized`: resolved version
- `DistURL` + `DistSHA256`: content-addressed cache key
- `ReflectionSHA256`: SHA-256 of reflection CLI output
- `PHPConstraint`: php requirement from composer.json
- `PSR4`: namespace prefix -> source directory
- `Require`: direct runtime dependencies

### Check() -- drift detection

`Check(entries []PhpPackage, hashes map[string]OnDiskHashes)` compares:
- If package not in hashes map: DriftEntry with "(not cached)"
- If `DistSHA256` differs: DriftEntry for "dist-sha256"
- If `ReflectionSHA256` differs: DriftEntry for "reflection-sha256"
- Empty locked hash: no drift reported (opt-out)

Returns `(true, nil)` when all match; `(false, []DriftEntry)` on drift.

### Format() -- TOML rendering

`Format([]PhpPackage)` renders entries as TOML `[[php-package]]` blocks for appending to `mochi.lock`.

## Files Landed

- `package3/php/lock/lock.go` -- PhpPackage, Check(), Format(), Vendor(), Pkg()
- `package3/php/lock/lock_test.go` -- 11 test functions

## Test Coverage

- All match: Check returns ok=true
- Dist-sha256 drift: detected and reported
- Reflection-sha256 drift: detected separately
- Not-cached package: drift with "(not cached)"
- Empty locked hash: no false-positive drift
- Multiple packages: only drifted ones reported
- Format single + multiple entries
- Vendor() and Pkg() helpers
- DriftEntry.String() rendering
