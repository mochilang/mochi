# MEP-75 Phase 14: Full 24-Package Fixture Corpus Gate + mochi.lock Round-Trip

**Status**: LANDED 2026-05-30 00:43 (GMT+7)

## Goal

Define and exercise the full 24-package fixture corpus against every prior MEP-75 phase component. Validate that the extern emitter, glue emitter, typemap, async detector, and mochi.lock round-trip all work correctly across the complete set of representative PHP package surfaces.

## Corpus Packages (24)

| # | Package | Key patterns exercised |
|---|---|---|
| 1 | guzzlehttp/guzzle | HTTP client, async promise returns, interface |
| 2 | symfony/console | Abstract command class, interface params |
| 3 | symfony/http-foundation | Request/Response classes, primitive returns |
| 4 | laravel/framework | mixed return (skip), container pattern |
| 5 | phpunit/phpunit | Abstract test case, void returns, mixed params |
| 6 | monolog/monolog | PSR-3 interface implementation |
| 7 | doctrine/orm | Object param (skip), entity manager interface |
| 8 | psr/log | Pure interface with 8 void methods |
| 9 | nesbot/carbon | Static factory methods, class return type |
| 10 | vlucas/phpdotenv | Static constructor, void methods |
| 11 | league/flysystem | Filesystem interface + implementation |
| 12 | paragonie/random_compat | Top-level functions (not class methods) |
| 13 | ramsey/uuid | Static UUID factory, interface return |
| 14 | bacon/bacon-qr-code | Simple class with string/void methods |
| 15 | spatie/laravel-permission | Static finder, fluent return type |
| 16 | barryvdh/laravel-debugbar | mixed param (skip), bool returns |
| 17 | composer/composer | Cross-package class dependencies |
| 18 | phpmailer/phpmailer | bool returns, void SMTP config |
| 19 | symfony/mailer | Mailer interface + implementation |
| 20 | league/oauth2-server | OAuth2 server, interface response return |
| 21 | firebase/php-jwt | Static encode/decode, array param (skip) |
| 22 | socialiteproviders/google | OAuth provider, class return types |
| 23 | stripe/stripe-php | Payment intent static factory |
| 24 | pestphp/pest | Test suite static factory, int return |

## Gate Criteria

- `corpus.All()` returns exactly 24 fixtures
- All fixture names are unique
- `externemit.Emit` succeeds (no error, non-nil result) for all 24 fixtures
- Packages with mappable methods produce non-empty MochiSource
- Packages with `mixed` returns produce at least one SkipReport
- `glue.Emit` succeeds for all 24 fixtures
- All glue namespaces begin with `MochiGlue\`
- mochi.lock `Format` + `Check` round-trip is clean (no false-positive drift)
- Drift is detected when dist SHA-256 changes
- Async detection finds promise methods in guzzlehttp/guzzle
- `typemap.Map` correctly maps primitive types and skips `mixed`/`object`
- All fixtures have non-nil surfaces with non-empty PackageName

## Files Landed

- `package3/php/corpus/corpus.go` -- 24 Fixture definitions + All()
- `package3/php/corpus/corpus_test.go` -- 13 integration test functions
- `website/docs/implementation/0075/index.md` -- all phases marked LANDED

## Test Coverage (13 tests)

- TestCorpusSize: exactly 24 fixtures
- TestCorpusNoDuplicateNames: all names unique
- TestCorpusExternEmitDoesNotPanic: all 24 surfaces emit without error
- TestCorpusExternEmitProducesOutput: method-bearing surfaces have output
- TestCorpusExternEmitExternFnPresent: mappable packages have extern fn
- TestCorpusMixedTypeSkipped: mixed-returning packages have SkipReports
- TestCorpusGlueEmitDoesNotPanic: all 24 surfaces produce glue files
- TestCorpusGlueNamespaceCorrect: all namespaces start with MochiGlue\
- TestCorpusLockRoundTrip: Format + Check round-trip is clean
- TestCorpusLockDriftDetected: changed hash triggers drift entry
- TestCorpusAsyncDetection: guzzlehttp/guzzle has async extern fn
- TestCorpusTypemapPrimitiveTypes: string/int/bool/float/void map; mixed/object skip
- TestCorpusAllFixturesHaveSurface: no nil surfaces or empty PackageName
