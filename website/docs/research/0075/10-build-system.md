---
title: "10. Build system"
sidebar_position: 11
sidebar_label: "10. Build system"
description: "Build system for the PHP bridge: `package3/php/` Go module structure, the reflection CLI design (Go invokes `php` with a synthesised script), PHP version pinning, content-addressed cache design, integration with MEP-55 Driver.Build vendor/ injection, and CI matrix (PHP 8.4.0, 8.4 latest, 8.5 allow-failure)."
---

# 10. Build system

Author: research pass for MEP-75 (Mochi and PHP package bridge). Date: 2026-05-29 22:11 (GMT+7).

This note describes the `package3/php/` Go module structure, the reflection CLI design, the content-addressed cache layout, and the CI matrix for MEP-75.

## 1. package3/php/ structure

Following `package3/rust/` and `package3/go/` (which were established by MEP-73 and MEP-74 respectively), MEP-75 lives under:

```
package3/php/
  packagist/           # Packagist v2 sparse API client
    client.go          # HTTP GET p2/<vendor>/<package>.json
    client_test.go
    cache.go           # Content-addressed cache (~/.cache/mochi/php-deps/)
    cache_test.go
    entry.go           # Packagist response types (JSON deserialization)
    entry_test.go
    semver.go          # Composer semver constraint parsing and matching
    semver_test.go
  reflect/             # PHP reflection CLI integration
    cli.go             # Go-side: exec.Command("php", "reflect.php", ...) + parse
    cli_test.go
    surface.go         # ReflectionSurface JSON types
    surface_test.go
    reflect.php        # The PHP-side reflection script (embedded in Go binary)
  typemap/             # PHP-to-Mochi type translation table
    kind.go            # PHP type kind discriminants
    kind_test.go
    map.go             # Translation table (PHP kind → Mochi type)
    map_test.go
    mapping.go         # TranslatedSurface + SkipReport types
    mapping_test.go
  externemit/          # Mochi extern fn/type emitter
    emit.go            # TranslatedSurface → .mochi shim file
    emit_test.go
  glue/                # PHP-side glue stub emitter
    emit.go            # TranslatedSurface → PHP forwarding stubs
    emit_test.go
  autoload/            # PSR-4 autoload map pre-computer
    autoload.go        # [[php-package]] lockfile data → vendor/autoload.php
    autoload_test.go
  build/               # MEP-55 driver extension
    driver.go          # Bridge.PrepareVendor(workdir, mochiLock)
    driver_test.go
    phase00_test.go    # Phase 0 gate
    workspace.go       # Vendor directory materialisation
    workspace_test.go
  errors/              # Package-level error types
    errors.go
```

This structure mirrors `package3/go/` (which has `moduleproxy/`, `apisurface/`, `sumdb/`, `typemap/`, `build/`, `errors/`, `cmd/go-ingest/`). The key differences:

- No `sumdb/` equivalent (Packagist has no transparency log).
- `reflect/` instead of `apisurface/` (the PHP reflection CLI replaces the `go-ingest` helper binary).
- `autoload/` (no Go equivalent; Go's autoloading is handled by the compiler, not a runtime registry).
- `glue/` (PHP-side forwarding stubs; the Go bridge generates both the Mochi shim and the PHP glue).

## 2. Reflection CLI design

The reflection CLI is a PHP script embedded in the Go binary as a `//go:embed` asset:

```go
//go:embed reflect/reflect.php
var reflectPhpScript string
```

When the Go bridge needs to reflect a package, it:

1. Writes `reflectPhpScript` to a temporary file (or uses a precomputed path in the package cache).
2. Runs `exec.Command("php", tmpReflectScript, packagePath, "--php-version=8.4")`.
3. Captures stdout (the JSON surface document).
4. Parses the JSON into a `ReflectionSurface` struct.
5. Computes SHA-256 of the JSON output.
6. Caches both the JSON and the SHA-256 alongside the dist zip in `~/.cache/mochi/php-deps/<sha256>/<sha256>.reflect.json`.

The `reflect.php` script structure:

```php
<?php
declare(strict_types=1);

require_once $argv[1] . '/vendor/autoload.php';

$surface = [];

// Walk classes
$classNames = getPackageClassNames($argv[1]);
foreach ($classNames as $className) {
    $rc = new ReflectionClass($className);
    if (!$rc->isPublic() || $rc->isInternal()) { continue; }
    $surface[] = reflectClass($rc);
}

// Walk functions
$funcNames = getPackageFunctionNames($argv[1]);
foreach ($funcNames as $funcName) {
    $rf = new ReflectionFunction($funcName);
    if (!$rf->isPublic() || $rf->isInternal()) { continue; }
    $surface[] = reflectFunction($rf);
}

echo json_encode(['surface' => $surface]);
```

The script requires the package's `vendor/autoload.php` to load the classes before reflecting them. This means the bridge first runs a lightweight `composer install --no-scripts --no-plugins --no-autoloader` pass to generate the autoload map, then runs the reflection script. The full `composer install` is only needed for the reflection step; the bridge's production vendor sandbox is pre-computed from the lockfile.

## 3. PHP version pinning

The reflection CLI requires PHP >= 8.1 (for `ReflectionUnionType` and `ReflectionIntersectionType`). The Go bridge checks the PHP version before invoking the CLI:

```go
cmd := exec.Command("php", "--version")
out, _ := cmd.Output()
// parse "PHP 8.4.1" from out
if phpVersion < "8.1" {
    return fmt.Errorf("php bridge: require PHP >= 8.1; found %s", phpVersion)
}
```

The bridge records the PHP version used for reflection in the lockfile as a comment:

```toml
# reflection-php-version = "8.4.1"
```

(Stored as a lockfile comment, not a structured field, to avoid breaking MEP-57's lockfile schema.)

## 4. Content-addressed cache

The cache layout under `~/.cache/mochi/php-deps/`:

```
~/.cache/mochi/php-deps/
  <sha256-prefix>/
    <sha256-full>.zip           # dist zip
    <sha256-full>.reflect.json  # reflection CLI output
    <sha256-full>.autoload.php  # pre-computed autoload fragment for this package
```

The cache key is the SHA-256 of the dist zip. Two packages at different Packagist dist URLs that produce the same zip content share a cache entry. The reflection CLI output is tied to the zip (the reflection sees the same classes regardless of URL); a re-lock that downloads the same zip is a full cache hit.

Cache invalidation: the cache entry is invalidated only if `dist-sha256` changes in `mochi.lock`. No TTL-based expiry; the content-addressed key is the only eviction trigger (plus manual `mochi pkg cache prune`).

## 5. Integration with MEP-55 Driver.Build

The MEP-55 build driver (`transpiler3/php/build/build.go`) is extended by `package3/php/build/Bridge.PrepareVendor`:

```go
// Bridge.PrepareVendor materialises the vendor directory for all
// [[php-package]] entries in the mochi.lock.
func (b *Bridge) PrepareVendor(workDir string, lock *pkglock.Lock) error {
    for _, pkg := range lock.PhpPackages {
        // 1. Extract the dist zip from the content-addressed cache.
        if err := b.extractPackage(workDir, pkg); err != nil {
            return err
        }
        // 2. Write the PHP glue stubs for this package.
        if err := b.writeGlueStubs(workDir, pkg); err != nil {
            return err
        }
    }
    // 3. Write the combined vendor/autoload.php.
    return b.writeAutoloadMap(workDir, lock.PhpPackages)
}
```

This is called by the MEP-55 driver before the `php main.php` (or `php out.phar`, etc.) invocation. The driver's existing `Build` function gains a `PhpBridge *package3/php/build.Bridge` field; when non-nil, `PrepareVendor` is called as a pre-build step.

## 6. CI matrix

```yaml
php-versions: ["8.4.0", "8.4", "8.5"]
allow-failure:
  php-version: "8.5"
```

| PHP version | Role |
|---|---|
| 8.4.0 | The pinned baseline. Tests must pass against the exact version MEP-55 targets. |
| 8.4 latest | Tracks the latest PHP 8.4 patch. Tests must pass. |
| 8.5 allow-failure | Forward-compatibility smoke test. Failures are tracked but do not block CI. |

PHP 8.5 is not yet released as of May 2026; the allow-failure matrix entry is reserved for when it ships. The CI image uses `shivammathur/setup-php@v2` (the standard GitHub Actions PHP setup action) to install the requested PHP version.

The reflection CLI is tested against all three matrix entries. The PHP glue stubs and the full end-to-end test (import php + run program) are tested against 8.4.0 and 8.4 latest.

## 7. The Go module for package3/php/

`package3/php/` is a Go module (`go.mod` with module path `mochi/package3/php`). It imports:

- `mochi/transpiler3/php/build` (for the driver extension point).
- `mochi/pkg/pkglock` (for `[[php-package]]` lockfile types).
- `mochi/pkg/pkgblob` (for the content-addressed cache).
- Standard library: `net/http`, `crypto/sha256`, `os/exec`, `encoding/json`, `archive/zip`.
- No external Composer or PHP-specific Go dependencies.

The module is ~4,500 LOC of Go across the `packagist/`, `reflect/`, `typemap/`, `externemit/`, `glue/`, `autoload/`, and `build/` packages.

## Cross-references

- [[04-packagist-ingest]] for the Packagist v2 client.
- [[05-type-mapping]] for the type translation table.
- [[09-psr-autoloading]] for the autoload map computation.
- [[11-testing-gates]] for the CI matrix and fixture corpus details.
- [MEP-55 build.go](/docs/mep/mep-0055) for the MEP-55 driver that the bridge extends.
- [MEP-73 research/10](/docs/research/0073/) for the analogous build system discussion for the Rust bridge.
