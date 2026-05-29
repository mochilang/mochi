---
title: "11. Testing gates"
sidebar_position: 12
sidebar_label: "11. Testing gates"
description: "Testing strategy for MEP-75: the 24-package fixture corpus with per-package test angles, reflection snapshot tests (SHA-256 of JSON surface pinned in test), extern shim compilation tests, end-to-end composer install + exec tests, mochi.lock round-trip tests, and TargetPhpLibrary emit tests."
---

# 11. Testing gates

Author: research pass for MEP-75 (Mochi and PHP package bridge). Date: 2026-05-29 22:11 (GMT+7).

This note describes the testing strategy for MEP-75: the fixture corpus, the categories of tests, and the gate criteria for each phase.

## 1. The 24-package fixture corpus

The gate for each phase is empirical: the bridge must handle the 24-package fixture corpus. The packages were selected from the April 2026 top-downloaded packages on Packagist, with coverage chosen to exercise different aspects of the type mapping table, the reflection CLI, and the async bridge.

| Package | Version | Test angle |
|---------|---------|-----------|
| `guzzlehttp/guzzle` | ^7.8 | HTTP client with async methods (promises); exercises the async bridge and the `Closure` type mapping |
| `symfony/console` | ^7.0 | Command-line framework; exercises abstract class and interface reflection (`InputInterface`, `OutputInterface`) |
| `symfony/http-foundation` | ^7.0 | HTTP request/response objects; exercises readonly class properties and PHP 8.1 union types |
| `laravel/framework` | ^11.0 | Full-stack framework; exercises large-surface reflection (many classes), PSR-4 namespace collision avoidance, and the SkipReport count |
| `phpunit/phpunit` | ^11.3 | Testing framework; exercises abstract class bridge (TestCase) and attribute reflection (PHP 8.0 attributes) |
| `monolog/monolog` | ^3.6 | Logging framework; exercises PSR-3 interface bridge (`Psr\Log\LoggerInterface`) and the interface-to-protocol-handle mapping |
| `doctrine/orm` | ^3.1 | ORM; exercises reflection of classes with typed and untyped properties, `mixed` return types (SkipReport), and the `db` capability flag |
| `psr/log` | ^3.0 | PSR-3 logger interfaces; exercises pure-interface reflection (all items should translate as `extern type`) |
| `nesbot/carbon` | ^3.3 | Date/time library; exercises named class reflection with many methods, `DateTimeInterface` bridge, and static factory methods |
| `vlucas/phpdotenv` | ^5.6 | `.env` file loader; exercises the `fs` capability flag and simple class reflection |
| `league/flysystem` | ^3.28 | Filesystem abstraction; exercises the `fs` capability flag and interface reflection (`FilesystemOperator`) |
| `paragonie/random_compat` | ^9.99 | Compatibility shim; exercises minimal surface reflection and PHP version constraint validation |
| `ramsey/uuid` | ^4.7 | UUID generation; exercises backed enum reflection (`UuidInterface`, `Uuid` class) and static factory methods |
| `bacon/bacon-qr-code` | ^3.0 | QR code generation; exercises class reflection with output-type parameters and resource-type SkipReport |
| `spatie/laravel-permission` | ^6.4 | Permission management; exercises trait-bearing class reflection (traits are SkipReport in v1) |
| `barryvdh/laravel-debugbar` | ^3.14 | Debug toolbar; exercises large-class reflection and IDE helper stubs |
| `composer/composer` | ^2.7 | The Composer CLI itself; exercises recursive reflection (Composer is itself a Composer package) and the packagist-client |
| `phpmailer/phpmailer` | ^6.9 | Email library; exercises the `net` capability flag and class reflection with optional typed properties |
| `symfony/mailer` | ^7.0 | Symfony mailer component; exercises interface bridge and the `net` capability flag |
| `league/oauth2-server` | ^9.0 | OAuth2 server; exercises abstract class bridge, interface reflection, and the async bridge opt-in |
| `firebase/php-jwt` | ^6.10 | JWT library; exercises static class methods (no constructor) and string-return typed methods |
| `socialiteproviders/google` | ^4.6 | Google OAuth2 provider; exercises interface implementation pattern and the `net` capability flag |
| `stripe/stripe-php` | ^15.0 | Stripe PHP SDK; exercises deep class hierarchies, untyped `array` SkipReport, and GuzzleHTTP dependency bridge |
| `psr/http-client` | ^1.0 | PSR-18 HTTP client interface; exercises minimal-interface reflection (one method, all types in-table) |

## 2. Reflection snapshot tests

For each package in the corpus, the bridge stores a golden snapshot of the JSON surface document (SHA-256 pinned in the test file):

```go
func TestReflectGuzzle(t *testing.T) {
    surface, err := reflect.RunCLI(t.TempDir(), "guzzlehttp/guzzle", "7.8.1")
    require.NoError(t, err)
    digest := sha256.Sum256(surface.JSON)
    assert.Equal(t, "fedcba9876...", hex.EncodeToString(digest[:]),
        "reflection surface hash changed; recheck type mapping")
}
```

A change in the snapshot triggers a test failure, prompting manual review. The snapshots are committed to the repository under `tests/php-fixtures/<vendor>_<package>/<version>/surface.json`.

The snapshot tests do not require a live PHP installation for their pass/fail assertion; they compare against the committed golden file. The actual reflection CLI invocation (which requires `php` on PATH) is gated behind `//go:build php_reflection` and runs only in the CI matrix.

## 3. Extern shim compilation tests

For each package, the bridge generates a Mochi shim file and asserts it parses cleanly via the MEP-1 parser:

```go
func TestShimParses_GuzzleHTTP(t *testing.T) {
    shim := loadGoldenShim(t, "guzzlehttp_guzzle")
    _, err := parser.Parse("guzzlehttp_guzzle_shim.mochi", []byte(shim))
    require.NoError(t, err, "synthesised shim must parse without errors")
}
```

These tests require no PHP installation; they only exercise the Go-side type-translation and shim-emit code paths.

## 4. SkipReport coverage tests

For each package, the bridge asserts that the SkipReport contains the expected set of skipped items (no more, no less):

```go
func TestSkipReport_DoctrineORM(t *testing.T) {
    report := loadGoldenSkipReport(t, "doctrine_orm")
    // assert mixed-return methods are skipped
    assert.Contains(t, report.Skipped, "Doctrine\\ORM\\EntityManager::flush")
    // assert total count is within expected range (25 ± 5)
    assert.InDelta(t, 25, len(report.Skipped), 5)
}
```

The SkipReport is committed as a golden file alongside the snapshot. A new SkipReport entry triggers a test failure; a removed entry also triggers a failure (to prevent silent improvements that mask regressions).

## 5. End-to-end composer install + exec tests

For selected packages (those that do not require a live server), the bridge runs a complete end-to-end test:

1. Lock the package: `mochi pkg lock` (against the fixture cache; no live Packagist network).
2. Write a Mochi source file that imports the package.
3. Build via `mochi build --target=php-run`.
4. Assert the PHP output matches the golden output.

Example for `ramsey/uuid`:

```mochi
// tests/php-e2e/uuid_basic.mochi
import php "ramsey/uuid" as uuid

fn main() {
    let id = uuid.uuid4()
    print(id.to_string())
}
```

The test asserts that the output is a valid UUID v4 string (matching `[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}`).

These tests require `php` on PATH and run in the CI matrix (PHP 8.4.0 and 8.4 latest; PHP 8.5 allow-failure).

## 6. mochi.lock round-trip tests

For each package in the corpus, the bridge asserts that running `mochi pkg lock` twice produces byte-identical `mochi.lock` output (the reproducibility gate):

```go
func TestLockRoundTrip_GuzzleHTTP(t *testing.T) {
    lock1 := runLock(t, "guzzlehttp/guzzle@7.8.1")
    lock2 := runLock(t, "guzzlehttp/guzzle@7.8.1")
    assert.Equal(t, lock1, lock2, "mochi.lock must be byte-identical on two runs")
}
```

The round-trip test uses the fixture cache (no network); it exercises the lockfile serialiser's canonical output.

## 7. TargetPhpLibrary emit tests

For a small set of Mochi source programs, the bridge tests the `TargetPhpLibrary` emit path:

1. Compile a Mochi source file with `mochi build --target=php-library`.
2. Assert the emitted `composer.json` is valid JSON (via `json.Unmarshal`).
3. Assert the emitted `composer.json`'s `autoload.psr-4` section is non-empty.
4. Assert the emitted PHP files parse cleanly via `php -l`.
5. Assert the emitted `src/` tree has the expected PSR-4 structure.

The TargetPhpLibrary tests do not require live Packagist; they test the emitter in isolation.

## 8. Interface and abstract class bridge tests

Phase 11 tests the `extern type` protocol handle pattern for PHP interfaces and abstract classes:

```go
func TestInterfaceBridge_PsrLog(t *testing.T) {
    surface := reflectPackage(t, "psr/log@3.0.0")
    shim := synthesiseShim(surface)
    // assert LoggerInterface is emitted as extern type
    assert.Contains(t, shim, "extern type LoggerInterface")
    // assert all interface methods are emitted as extern fn
    assert.Contains(t, shim, "extern fn logger_emergency(l: LoggerInterface, message: string): unit")
}
```

## 9. Async bridge tests (Phase 12)

Phase 12 tests require `php`, `react/event-loop`, and a running test HTTP server:

```go
func TestAsyncBridge_GuzzleHTTP(t *testing.T) {
    if !hasPhp() || !hasComposer() { t.Skip("php+composer required") }
    // start a mock HTTP server
    ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        w.Write([]byte("hello"))
    }))
    defer ts.Close()
    // compile with async enabled
    out := runMochiWithAsync(t, mochiSrc_GuzzleAsync, ts.URL)
    assert.Equal(t, "hello", out)
}
```

## Cross-references

- [[04-packagist-ingest]] for the fixture cache design.
- [[05-type-mapping]] for the SkipReport items the tests validate.
- [[10-build-system]] for the CI matrix.
- [MEP-75 §Phases](/docs/mep/mep-0075#phases) for the phase gate criteria.
