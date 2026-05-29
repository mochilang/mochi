---
title: MEP-75 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 75. Mochi+PHP package bridge"
description: "Per-phase implementation tracking for MEP-75 (Mochi+PHP package bridge). Status + commit columns capture how each phase landed on main."
---

# MEP-75 implementation tracking

Per-phase tracking for [MEP-75 Mochi+PHP package bridge](/docs/mep/mep-0075). Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main`.

A phase is LANDED only when its gate is green for every target (consume direction + publish direction where applicable). Missing surfaces become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

## Phase status

| Phase | Title | Status | Commit | Tracking page |
|-------|-------|--------|--------|---------------|
| 0 | Skeleton: package3/php/ layout + Driver/Workspace scaffold + errors | LANDED | — | [phase-00](/docs/implementation/0075/phase-00-skeleton) |
| 1 | Packagist v2 sparse-index client (version resolution + dist URL) | LANDED | — | — |
| 2 | Composer dist fetcher + SHA-256 content-addressed cache | LANDED | — | — |
| 3 | PHP Reflection CLI (reflect.php + Go invoker + JSON surface parser) | LANDED | — | — |
| 4 | Closed PHP-to-Mochi type-mapping table | LANDED | — | [phase-04](/docs/implementation/0075/phase-04-typemap) |
| 5 | Mochi extern fn / extern type emitter + SKIPPED.txt | LANDED | — | [phase-05](/docs/implementation/0075/phase-05-externemit) |
| 6 | `import php` grammar wiring + MEP-55 build orchestration | LANDED | — | [phase-06](/docs/implementation/0075/phase-06-glue) |
| 7 | vendor/autoload.php generator (PSR-4, no composer install) | LANDED | — | [phase-07](/docs/implementation/0075/phase-07-autoload) |
| 8 | mochi.lock `[[php-package]]` integration + `--check` mode | LANDED | — | [phase-08](/docs/implementation/0075/phase-08-lock) |
| 9 | `TargetPhpLibrary` emit (PSR-4 src/ + composer.json + README) | LANDED | — | [phase-09](/docs/implementation/0075/phase-09-library) |
| 10 | Packagist publish flow (GPG tag + Sigstore OIDC + Update API) | LANDED | — | [phase-10](/docs/implementation/0075/phase-10-publish) |
| 11 | Interface and abstract class bridge | LANDED | — | [phase-11](/docs/implementation/0075/phase-11-abstract) |
| 12 | Async PHP bridge (ReactPHP / RevoltPHP event-loop injection) | LANDED | — | [phase-12](/docs/implementation/0075/phase-12-async) |
| 13 | Phar distribution path | LANDED | — | [phase-13](/docs/implementation/0075/phase-13-phar) |
| 14 | Full 24-package fixture corpus gate + mochi.lock round-trip | LANDED | — | [phase-14](/docs/implementation/0075/phase-14-corpus) |

## Per-phase fields

Each phase tracking page documents (or will document, once the phase begins):

- **Gate**: the test or check that must pass for the phase to be LANDED.
- **Files to touch**: the bridge-side files (Go) the phase introduces or modifies.
- **Fixtures**: which of the 24-package fixture corpus the phase validates against.
- **Skip count**: the expected SkipReport count per fixture package (golden numbers).
- **Sub-phase decomposition** (if needed): N.1, N.2, ... entries when an upstream constraint forces splitting.

## Fixture corpus

The 24-package fixture corpus from the MEP-75 spec (top Composer packages by download count):

guzzlehttp/guzzle, symfony/console, symfony/http-foundation, laravel/framework,
phpunit/phpunit, monolog/monolog, doctrine/orm, psr/log, nesbot/carbon,
vlucas/phpdotenv, league/flysystem, paragonie/random_compat, ramsey/uuid,
bacon/bacon-qr-code, spatie/laravel-permission, barryvdh/laravel-debugbar,
composer/composer, phpmailer/phpmailer, symfony/mailer, league/oauth2-server,
firebase/php-jwt, socialiteproviders/google, stripe/stripe-php,
spatie/laravel-medialibrary.

Each phase that touches the type-mapping or extern emit layer asserts golden counts against this corpus. The corpus is regenerated quarterly to track package API drift.

## PHP-to-Mochi type table (closed set)

| PHP type | Mochi type | Notes |
|----------|------------|-------|
| int | int | |
| float | float | |
| string | string | |
| bool | bool | |
| ?T | T\|nil | nullable wrapper |
| array (typed) | list/map | heuristic from docblock or typed property |
| class | record/handle | |
| interface | protocol handle | |
| void | unit | |
| never | panic boundary | SkipNever in v1 |
| mixed | SKIP | SkipMixed |
| object | SKIP | SkipObject |
| callable | SKIP | SkipCallable |
| resource | SKIP | SkipResource |
| A\&B (intersection) | SKIP | SkipIntersection |

## Implementation location

The bridge lives at `package3/php/` in the repo root:

```
package3/php/
  README.md               # pointer to MEP-75 spec
  errors/                 # SkipReason + BridgeError (phase 0)
  build/                  # Driver + Options + Workspace scaffold (phase 0)
  packagist/              # Packagist v2 sparse-index client (phase 1)
  cache/                  # content-addressed Composer dist fetcher (phase 2)
  reflect/                # PHP CLI reflection invoker + JSON parser (phase 3)
  typemap/                # closed PHP-to-Mochi type table (phase 4)
  externemit/             # Mochi extern fn / extern type emitter (phase 5)
  glue/                   # PHP-side use + forwarding stubs (phase 6)
  autoload/               # vendor/autoload.php generator (phase 7)
  lock/                   # mochi.lock [[php-package]] read/write (phase 8)
  library/                # TargetPhpLibrary: PSR-4 src/ + composer.json (phase 9)
  publish/                # Packagist publish: GPG tag + Sigstore + Update API (phase 10)
  externemit/hierarchy.go # PHP class/interface hierarchy analysis (phase 11)
  asyncemit/              # async extern fn emitter for promise/future returns (phase 12)
  pharemit/               # Phar stub + build script generator (phase 13)
  corpus/                 # 24-package fixture corpus + integration tests (phase 14)
```

## CI matrix

| Target | PHP version | OS | Allow failure |
|--------|-------------|-----|--------------|
| php-8.4-ubuntu | PHP 8.4.0 | ubuntu-24.04 | no |
| php-8.4-latest | PHP 8.4 latest | ubuntu-24.04 | no |
| php-8.5-dev | PHP 8.5 | ubuntu-24.04 | yes |

## Status snapshot

As of 2026-05-30 00:43 (GMT+7): all 15 phases (0-14) LANDED. The full 24-package fixture corpus gate is green. The mochi.lock round-trip (Format/Check) passes against all fixtures. Async bridge detects ReactPHP/Amp/Revolt promise methods. Phar distribution path generates stub + build scripts.

## Cross-references

- [MEP-75 spec](/docs/mep/mep-0075) for the normative design.
- [MEP-75 research bundle](/docs/research/0075/) for the 13-note deep-research collection.
- [MEP-55 implementation tracking](/docs/implementation/0055) for the PHP transpiler that MEP-75 extends.
- [MEP-57 implementation tracking](/docs/implementation/0057) for the polyglot package system MEP-75 builds on.
