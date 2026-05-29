---
title: "Risks and alternatives: scheduling, PHP_INT_MAX overflow, Psalm 5 vs 6, abstract readonly, phar.readonly, amphp removed, live LLM deferred, PHP 8.5"
description: "Risk register for MEP-55: PHP scheduling, PHP_INT_MAX DJB2 overflow (GMP mitigation), Psalm 5 vs 6 PHP 8.4 incompatibility, abstract readonly inheritance, phar.readonly default, amphp/revolt removal, live LLM providers deferred, PHP 8.5 allow_failure."
sidebar_position: 12
---

# Risks and alternatives

Author: research pass for MEP-55 (Mochi-to-PHP 8.4 transpiler).
Date: 2026-05-29 15:00 (GMT+7).
Sources: `transpiler3/php/colour/colour.go`,
`transpiler3/php/lower/lower.go`,
`transpiler3/php/runtime/composer.json`,
`transpiler3/php/build/phase13_test.go`,
`transpiler3/php/build/phase17_test.go`,
`.github/workflows/transpiler3-php-test.yml`,
`website/docs/mep/mep-0055.md`.

This note collects the risks MEP-55 accepts, the mitigations applied,
and the alternatives explicitly rejected. Each risk entry has a
severity, a description, and a mitigation or current status.

## Risk register

### R1: PHP has no preemptive scheduler

**Severity**: medium (design constraint, not a defect).

**Description**: PHP has no preemptive scheduler in the standard library.
Mochi agents and streams on PHP are synchronous: an agent's `recv()`
call does not yield to other agents while waiting. All Phase 9-10
fixtures use emit-before-recv patterns specifically because of this.

If a Mochi program is written with interleaved concurrent agent behaviour
(agent A sends to agent B which replies to agent A), that program cannot
be correctly lowered to the synchronous PHP model.

**Mitigation**: The lowerer emits sequential code. Programs that require
genuine concurrency must target a different Mochi backend (BEAM MEP-46,
JVM MEP-47). The MEP-55 spec documents this limitation explicitly.

**Status**: Accepted design constraint. `colour/colour.go` documents:
"no PHP function ever needs an `Amp\Future<T>` return type."

### R2: PHP_INT_MAX overflow for DJB2 cassette keys

**Severity**: high (would produce wrong cassette keys for some prompts).

**Description**: PHP's native `int` is 63-bit signed on 64-bit platforms
(PHP_INT_MAX = 9223372036854775807 = 2^63 - 1). The DJB2 hash of some
(provider, model, prompt) combinations produces a 64-bit unsigned value
greater than PHP_INT_MAX. Under PHP's native integer arithmetic, the
value would be silently truncated or wrapped as a negative number,
producing a key that does not match the cassette file.

Example: `djb2("openai", "", "Say hello.") = 15023835511162652990`,
which is larger than 2^63 - 1 = 9223372036854775807. PHP would
miscompute this without GMP.

**Mitigation**: The `mochi_llm_cassette_key` helper (lower.go lines
388-403) uses GMP to perform all hash arithmetic in unsigned 64-bit
space:
```php
$h = gmp_init(5381);
$mask = gmp_init('FFFFFFFFFFFFFFFF', 16);
$h = gmp_and(gmp_mul($h, 33), $mask);
$h = gmp_xor($h, gmp_init(ord($buf[$i])));
return gmp_strval($h, 10);
```
GMP is a required extension (`"ext-gmp": "*"` in `composer.json`).

`TestPhase13DJB2HashMatchesCassetteFilenames` pins the exact expected
hash for every known tuple, confirming that the GMP path produces the
same result as Go's native `uint64` arithmetic.

**Status**: Mitigated. Requires `ext-gmp`, documented in README and
composer.json.

### R3: Psalm 5.x PHP 8.4 incompatibility

**Severity**: high (blocks CI).

**Description**: Psalm 5.x did not recognise PHP 8.4 as a valid platform
version in its internal version table. When run against the runtime
package with `platform: {php: 8.4}` in `psalm.xml`, Psalm 5 reported
the PHP version as unknown and flagged `abstract readonly class` syntax
as invalid PHP, producing false-positive errors.

**Mitigation**: Migrated to `"vimeo/psalm": "^6.0"` in `composer.json`.
Psalm 6 adds full PHP 8.4 support. This migration happened in audit
round 1 after the initial umbrella PR.

**Status**: Resolved. `composer.json` requires `^6.0`.

### R4: `final readonly class extends abstract readonly class` requires PHP 8.4

**Severity**: high (would break sum-type lowering on PHP < 8.4).

**Description**: PHP 8.4 introduced the rule that a `final readonly`
class can only extend an `abstract readonly` base. In PHP 8.3 and earlier,
`abstract readonly class` is not valid syntax. Attempting to use a plain
`abstract class` base for a sum type breaks in PHP 8.4 because
`final readonly` cannot extend a non-readonly parent.

This constraint was discovered during Phase 5 CI on the umbrella PR.
The CI run failed with a PHP parse error on the emitted sum-type class
hierarchy.

**Mitigation**: The `ClassDecl` ptree node has an `Abstract` field. When
`Abstract: true`, the emit pass writes `abstract readonly class NAME {}`
(nodes.go lines 738-739). The `lowerUnion` function (lower.go lines
1006-1030) sets `Abstract: true` on the base class. The fix was a single
line in the lowerer.

**Status**: Fixed in umbrella PR Phase 5. No further action needed.

### R5: `phar.readonly = 1` default on most distributions

**Severity**: medium (blocks Phar creation without workaround).

**Description**: Most Linux distributions and macOS Homebrew ship PHP
with `phar.readonly = 1` in the global `php.ini`. This prevents the
`Phar::startBuffering()` and `addFile()` calls in the Phase 17 stager
from executing.

**Mitigation**: The stager is invoked with `php -d phar.readonly=0` in
`runPharFixture` (phase17_test.go line 74). Documentation notes that
production deployments can use `humbug/box compile` (which handles
`phar.readonly` internally) or add `phar.readonly = Off` to a project
`php.ini`.

The `-d` flag works per-invocation without modifying the system
configuration. CI uses `shivammathur/setup-php@v2` which sets up a
clean PHP environment; the flag is still needed there.

**Status**: Mitigated. No change to default distribution configuration
required.

### R6: `amphp/revolt` removed from runtime

**Severity**: low (was never shipped, removed in audit round 1).

**Description**: An early draft of `composer.json` listed `amphp/revolt`
in `require-dev` as the intended async event loop backend for Phase 11.
Phase 11 was implemented as synchronous wrappers instead, making the
dependency unnecessary. The dependency was removed in audit round 1.

If `amphp/revolt` had been left in `require-dev`, it would have added a
large transitive dependency tree and potentially caused Psalm/PHPStan
analysis to take longer. More importantly, it would have created a
false expectation that Phase 11 supports true async.

**Mitigation**: Removed. `composer.json` has no `amphp/revolt` entry.
The `colour.go` package comment documents the decision permanently.

**Status**: Resolved.

### R7: Live LLM providers deferred

**Severity**: low (Phase 13 is cassette-only; acknowledged in spec).

**Description**: Phase 13 ships cassette-only LLM dispatch. The
`mochi_llm_generate` helper reads `$MOCHI_LLM_CASSETTE_DIR/<hash>.txt`.
When the env var is unset or the cassette is missing, the helper writes
a stderr diagnostic and returns `""`:
```php
fwrite(STDERR, "mochi_llm_generate: MOCHI_LLM_CASSETTE_DIR not set; live mode not yet implemented for PHP\n");
return '';
```
Live OpenAI, Anthropic, Google, and llama.cpp integrations are not
implemented.

**Mitigation**: The C runtime (MEP-45) similarly defers live providers
until a later phase. The cassette-only approach is sufficient for
testing and for offline evaluation scenarios.

**Status**: Accepted deferral. Documented in MEP-55 spec and in the PHP
helper's stderr message.

### R8: PHP 8.5 allow_failure

**Severity**: low (early warning, not a current breakage).

**Description**: PHP 8.5 was in alpha/beta at MEP-55 ship time (May
2026). Its full release may introduce changes to:
- Strict-type handling (new coercion rules).
- Deprecation of functions used by the runtime or lowered code.
- PHPStan/Psalm compatibility.

**Mitigation**: The CI matrix runs PHP 8.5 as `allow_failure: true`
(`.github/workflows/transpiler3-php-test.yml`). This provides early
warning of 8.5 breakage without blocking CI on main. When 8.5 reaches
stable, the `allow_failure` flag will be set to `false`.

**Status**: Monitored.

### R9: `Mochi\Runtime\IO` flagged as dead code by Psalm

**Severity**: low (causes false-positive CI failure without `@api`).

**Description**: Psalm's `UnusedClass` check reports `IO` as dead code
because no code within `src/` calls its methods. Lowered Mochi programs
call it from outside the scanned source tree.

**Mitigation**: The `@api` PHPDoc tag on `IO` (runtime/src/Mochi/
Runtime/IO.php lines 17-18) suppresses the finding. See [[04-runtime]]
for details.

**Status**: Mitigated by `@api`. TRUST.md documents the convention.

### R10: PHP reserved word collision with user agent/record names

**Severity**: low (affects programs that name a type after a PHP keyword).

**Description**: PHP rejects class names that are reserved words. A
Mochi program with `agent Switch { ... }` or `record Int { ... }` would
emit invalid PHP without the collision guard.

**Mitigation**: `phpClassName(name)` (lower.go lines 965-972) appends
`_` to any name in `phpReservedClassNames` (lines 939-963). The
`agent_bool.mochi` fixture pins the `Switch_` case. The list covers
all PHP 8.4 reserved words and soft-reserved type names (`int`, `float`,
`bool`, `string`, `void`, etc.).

**Status**: Mitigated. Fragment test `agent_bool` pins it.

## Rejected alternatives

### A1: Amp/Revolt for async

**Evaluated**: amphp/amp v3 and revolt/event-loop for Phase 11 async.

**Decision**: Rejected. Adds runtime dependencies, changes function
signatures, requires event-loop configuration. Sync wrappers cover all
Phase 11 fixture use cases.

### A2: Haxe PHP backend

**Evaluated**: route Mochi through a Mochi→Haxe transpiler using Haxe's
existing PHP backend.

**Decision**: Rejected. Two translation layers. Haxe's PHP backend not
under Mochi's control. Would add a large build-time dependency.

### A3: HHVM/Hack as the primary PHP-family target

**Evaluated**: target HHVM instead of Zend PHP.

**Decision**: Rejected. HHVM dropped PHP backward compatibility in 2018.
HHVM is not available on shared hosting. Standard Zend PHP 8.4 reaches
a wider deployment surface.

### A4: humbug/box for Phase 17 CI gate

**Evaluated**: use `humbug/box compile` in the Phase 17 CI gate.

**Decision**: Rejected for the in-tree gate. Requires `composer global
require humbug/box` and network access in every test run. The built-in
`Phar` class is sufficient for structural validation. Production builds
can use humbug/box.

### A5: PHP 8.3 minimum floor

**Evaluated**: lower the floor to PHP 8.3 for wider compatibility.

**Decision**: Rejected. PHP 8.3 has no `abstract readonly class`. Sum
types cannot be encoded cleanly. See [[02-design-philosophy]] for the
full version history analysis.
