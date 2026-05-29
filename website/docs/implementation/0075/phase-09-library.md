# MEP-75 Phase 9: TargetPhpLibrary (PSR-4 src/ + composer.json + README)

**Status**: LANDED 2026-05-30 00:00 (GMT+7)

## Goal

Implement the `TargetPhpLibrary` build target under `package3/php/library`. Given a `Config` (package metadata) and a list of `ClassFile` entries, the emitter produces the complete PHP library file set ready for git tag and Packagist publish.

## Emitted files

| File | Description |
|---|---|
| `composer.json` | Composer metadata with PSR-4 autoload, require, authors, keywords |
| `README.md` | Generated documentation stub with installation instructions |
| `LICENSE` | MIT license text with year and first author |
| `src/<FQCN>.php` | One PHP file per class/interface/enum (one class per file, PSR-4 layout) |

## Design

`Emit(Config, []ClassFile)` is the main entry point. It:
1. Validates required fields (`ComposerName`, `PSR4Namespace`)
2. Renders `composer.json` via `json.MarshalIndent` with PSR-4 namespace
3. Renders `README.md` with package name and `composer require` instructions
4. Renders MIT `LICENSE` with current year and first author name
5. For each `ClassFile`, converts FQCN to a PSR-4 file path and writes to `src/`

The PSR-4 namespace in `composer.json` always has a trailing backslash: `"Acme\\MyLib\\"`.

## Files Landed

- `package3/php/library/library.go` -- Emit() + helpers
- `package3/php/library/library_test.go` -- 12 test functions

## Test Coverage

- Required field validation (ComposerName and PSR4Namespace)
- Expected output files: composer.json, README.md, LICENSE
- composer.json valid JSON with correct fields
- PSR-4 namespace with trailing backslash in composer.json
- require entries: php + additional packages
- Authors in composer.json
- README.md content with composer require command
- LICENSE MIT text with author name
- Class file PSR-4 path generation
- Multiple classes at different namespace depths
- classFilePath unit tests
- Default ^8.4 PHP require when PHPRequire is empty
