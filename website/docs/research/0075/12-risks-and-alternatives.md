---
title: "12. Risks and alternatives"
sidebar_position: 13
sidebar_label: "12. Risks"
description: "The MEP-75 risk register (PHP type system looseness, PHP version spread, Packagist no OIDC, reflection API instability, PSR-4 namespace collisions, Composer solver vs PubGrub divergence, ReactPHP vs RevoltPHP fragmentation, PHP fibers vs OS threads, TargetPhpLibrary breaking on PHP < 8.1, composer install network dependency in CI) and the alternatives considered and rejected."
---

# 12. Risks and alternatives

Author: research pass for MEP-75 (Mochi and PHP package bridge). Date: 2026-05-29 22:11 (GMT+7).

This note collects the risks MEP-75 carries and the alternative approaches that were considered and rejected.

## R1: PHP type system looseness (mixed, untyped array)

**Risk**: PHP's type system is optional in many older packages. A `guzzlehttp/guzzle` method might declare its `$options` parameter as `array` (untyped). The Reflection API returns `array` with no shape information. The bridge cannot translate this to a Mochi type and emits a SkipReport.

**Likelihood**: high. Many Packagist packages, even well-maintained ones, have some untyped `array` parameters.

**Impact**: medium. The SkipReport documents the gap; the user can hand-write an `extern fn` override for critical items. The majority of a package's surface is still translatable.

**Mitigation**:
- The SkipReport includes a suggestion for the override: `add extern fn ... from php "..." custom`.
- PHPDoc augmentation (reading `@param array<K, V>` from docblocks) reduces the SkipReport count for well-documented packages.
- The 24-package corpus tests measure the SkipReport count per package; regressions are tracked.

**Residual**: packages with pervasive `mixed` and untyped `array` (particularly older packages that predate PHP 7 type annotations) have large SkipReport lists and require significant hand-written overrides. The user is informed of this upfront via the SkipReport diagnostics.

## R2: PHP version spread in the wild

**Risk**: the PHP ecosystem in production spans PHP 8.0 through 8.4 (and PHP 7.4 on legacy systems). A package that declares `php: >=7.4` but uses PHP 8.1-only features in its code (fibers, readonly, enums) may fail to run on older PHP.

**Likelihood**: medium. Major packages track PHP compatibility carefully; less-maintained packages do not.

**Impact**: low for MEP-75's consume direction (MEP-55 targets PHP 8.4; any package that runs on 8.4 works). Higher for the produce direction (a library emitted by `TargetPhpLibrary` requires PHP 8.4; downstream consumers on older PHP cannot install it).

**Mitigation**:
- The bridge records `php-version-constraint` from the package's `composer.json` and warns when the floor is below 8.1.
- `TargetPhpLibrary` always emits `"php": "^8.4"` in the `composer.json`; there is no way to emit a lower-floor library.
- The CI matrix tests against PHP 8.4.0 and 8.4 latest to confirm the floor is enforced.

**Residual**: users who need to target PHP 8.1-8.3 with their published library cannot use `TargetPhpLibrary` without adjusting the emitted code. This is a known limitation of the bridge's PHP 8.4 target.

## R3: Packagist no OIDC

See [[07-packagist-trusted-publishing-gap]] for the full analysis. Summary: the publish direction has a known supply-chain gap (long-lived API tokens). The GPG + Sigstore mitigation is credible but not equivalent to OIDC. The gap is documented; the roadmap item is to add OIDC when Packagist ships it.

## R4: Reflection API instability across PHP versions

**Risk**: PHP's Reflection API has evolved across versions. `ReflectionUnionType` was added in PHP 8.0; `ReflectionIntersectionType` in PHP 8.1; `ReflectionEnum` in PHP 8.1; `ReflectionProperty::isReadOnly()` in PHP 8.1; `ReflectionClass::isReadOnly()` in PHP 8.2. A reflection CLI that targets PHP 8.4 may behave differently than expected on PHP 8.1.

**Likelihood**: low. The bridge targets PHP 8.4; the CI matrix runs PHP 8.4.0 and 8.4 latest. PHP 8.1 and 8.2 are tested only in the `--minimum-floor` path.

**Impact**: low. The reflection CLI would fail or return incorrect results on older PHP; the bridge checks the PHP version before running the CLI.

**Mitigation**:
- The bridge requires PHP >= 8.1 for the reflection CLI (checked before invocation).
- The PHP 8.5 allow-failure entry in the CI matrix catches forward-compatibility issues early.

## R5: PSR-4 namespace collisions

**Risk**: two imported packages declare the same PSR-4 root namespace. For example, `symfony/console` and `symfony/http-foundation` both use `Symfony\` as their root. The autoload map would have two `Symfony\` entries pointing to different directories.

**Likelihood**: certain for popular namespace families (`Symfony\`, `Illuminate\`, `Doctrine\`).

**Impact**: medium. Without explicit conflict resolution, the second entry would overwrite the first in a naive implementation.

**Mitigation**:
- PSR-4 allows multiple entries for the same prefix (with different base directories). The bridge's autoload emitter combines multiple packages under the same prefix into a list of base directories, matching Composer's behaviour.
- Symfony packages, for example, all declare sub-namespaces of `Symfony\`, and Composer correctly resolves them by checking each base directory in order.
- The bridge tests include Symfony Console + Symfony HTTP Foundation together to verify combined autoload.

**Residual**: packages that declare overlapping but not identical prefixes (e.g., `App\` in one package and `App\Models\` in another) may still collide. The bridge emits a SkipReport for these cases.

## R6: Composer solver vs PubGrub divergence

**Risk**: MEP-57's PubGrub solver and Composer's solver may resolve the same set of constraints to different versions, producing inconsistent results.

**Likelihood**: low. The bridge delegates PHP dep resolution entirely to Composer's solver (`composer update --dry-run --format=json`) and records the result in `mochi.lock`. MEP-57's PubGrub solver is not used for PHP deps; it records the Composer result.

**Impact**: low. The `mochi.lock` faithfully records what Composer resolved; downstream `mochi pkg lock --check` verifies the same resolution.

**Mitigation**: delegate to Composer, not PubGrub, for PHP dependency resolution. The rationale is in [[04-packagist-ingest]] §3.

## R7: ReactPHP vs RevoltPHP ecosystem fragmentation

**Risk**: some packages target ReactPHP's promise API; others target amphp/amp's coroutine API. The two have incompatible async models at the application level (promises vs coroutines), even though they share `revolt/event-loop` as the scheduler.

**Likelihood**: medium. The fragmentation is real but affects a minority of Packagist packages (the majority are synchronous).

**Impact**: low for the MEP-75 bridge, which only wraps async calls as synchronous adapters. The user's Mochi program is always synchronous (from its perspective); the bridge's async glue decides which event-loop to use.

**Mitigation**:
- `[php.async] event-loop = "react"` uses ReactPHP's promise API.
- `[php.async] event-loop = "revolt"` uses RevoltPHP's fiber API.
- The bridge documents that mixing packages from both worlds in one program requires careful manual bridge code.

## R8: PHP fibers vs OS threads

**Risk**: PHP fibers (PHP 8.1) are cooperative coroutines on a single OS thread. A PHP fiber that blocks on a synchronous I/O call (e.g., `file_get_contents` without a ReactPHP stream wrapper) blocks the entire PHP process, defeating the async bridge's purpose.

**Likelihood**: certain for any synchronous I/O call inside a fiber.

**Impact**: medium for programs that mix async and synchronous I/O.

**Mitigation**: the bridge documents that the async bridge is only useful when the imported package's I/O is ReactPHP-native (uses non-blocking stream wrappers). Packages that use synchronous PHP I/O (e.g., `file_get_contents`, `PDO::query`) cannot benefit from the async bridge. The user is warned in the SkipReport when a package has synchronous I/O methods.

## R9: TargetPhpLibrary breaking on PHP < 8.1

**Risk**: `TargetPhpLibrary` emits PHP 8.4 constructs (readonly class, enum, first-class callables). A downstream PHP user on PHP 8.1 or 8.2 trying to `composer require` the library would get a fatal error.

**Likelihood**: certain for PHP 8.1-8.3 users trying to install MEP-75-emitted libraries.

**Impact**: medium. The `composer.json` constraint `"php": "^8.4"` prevents installation on PHP < 8.4, so the user gets a Composer error rather than a runtime error. This is correct behaviour, but it limits the audience for MEP-75-emitted libraries.

**Mitigation**: document the PHP 8.4 floor clearly. A future sub-phase could add a `TargetPhpLibraryLegacy` target with PHP 8.1 compatibility (dropping readonly classes, enums, and first-class callables in favour of workarounds), but v1 targets only PHP 8.4.

## R10: `composer install` network dependency in CI

**Risk**: Phase 7 (autoload + composer install integration) and Phase 12 (async bridge) require `composer install` against live Packagist in some test paths.

**Likelihood**: certain; CI environments with no outbound network fail these tests.

**Impact**: medium. Network-dependent tests are flaky; CI environments behind firewalls cannot run them.

**Mitigation**:
- The 24-package fixture corpus is pre-fetched and committed to the repository under `tests/php-fixtures/`. CI does not need live Packagist for the fixture corpus tests.
- End-to-end tests that exercise live `composer install` are gated behind `//go:build php_live_network` and run only in the designated CI environment with outbound network access.
- The bridge's production code path uses the pre-computed autoload map (not a live `composer install`); `composer install` is only used in the reflection step (once per package version, results cached).

## Alternatives considered

### A1: Use PHPStan as the primary surface source

PHPStan's reflection system is the most accurate PHP type model available: it understands PHPDoc generics, array shapes, and conditional return types. Rejected for the primary path because PHPStan requires ~40MB of Composer dependencies and a full PHP toolchain; it is too heavy for a bridge that targets the default case. PHPStan output is used as an augmentation layer when Psalm stubs are not available and PHPStan is installed, but it is not a hard dependency.

### A2: Use PHP-Parser (nikic/php-parser) as the primary surface source

PHP-Parser provides a full PHP AST without executing the code. Rejected because PHP-Parser does not resolve types across files; it sees the raw AST before class-loading. The Reflection API is post-loading and has resolved types. PHP-Parser could be used as a fallback for packages that cannot be loaded without a running web server (e.g., Laravel's `bootstrap/app.php`), but this is a rare case addressed by the SkipReport mechanism.

### A3: Generate PHP extension (phpize/ext-ffi/PHP-CPP)

Rejected. No stable C ABI for PHP; extension authoring violates the zero-boilerplate promise. See [[02-design-philosophy]] §2 and [[03-prior-art-bridges]] §ext-ffi.

### A4: Use Psalm stubs as the mandatory surface format

Rejected because the vast majority of Packagist packages do not ship Psalm stubs. Psalm stubs are used as an augmentation layer when present.

### A5: Use an annotation-based approach (require `@mochi-bridge` PHPDoc annotations in the upstream package)

Rejected. The zero-boilerplate promise means the user does not annotate upstream packages. The bridge works on any Packagist package without requiring upstream changes.

### A6: Use `composer create-project` to extract the surface

`composer create-project` runs `composer install` on the package as a project root. Rejected: it requires live Packagist network access at reflection time, is slower than the content-addressed dist fetch, and produces a different source tree than the dist zip.

### A7: Use the Packagist REST API v1 (not v2) for metadata

The v1 API (`/packages/<vendor>/<package>.json`) returns identical information but requires paginated fetches for full version history. Rejected in favour of v2 which returns all versions in a single request.

### A8: Require PSR-12 or PER-CS2.0 style in emitted TargetPhpLibrary code

The MEP-55 emit pass already uses `php-cs-fixer` with PER-CS2.0 rules (Phase 15 gate). `TargetPhpLibrary` inherits this requirement. Not a rejected alternative; just documented here for completeness.

## Cross-references

- [[02-design-philosophy]] for the load-bearing decisions that drove these choices.
- [[03-prior-art-bridges]] for the broader PHP interop landscape.
- [[07-packagist-trusted-publishing-gap]] for risk R3 in depth.
- [[08-async-bridge]] for risk R7 and R8 in depth.
- [MEP-75 §Risks](/docs/mep/mep-0075#risks) for the normative risk register.
- [MEP-75 §Alternatives](/docs/mep/mep-0075#alternatives-considered) for the normative alternatives list.
