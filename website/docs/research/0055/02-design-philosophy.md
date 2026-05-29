---
title: "Design philosophy: why PHP 8.4, why strict_types, why sync wrappers, why final readonly, why DJB2/GMP, why Phar"
description: "The 'why' behind every load-bearing MEP-55 design choice: PHP 8.4 floor, strict_types=1, abstract readonly bases, sync wrappers, GMP DJB2 hashing, and Phar packaging."
sidebar_position: 2
---

# Design philosophy: why PHP 8.4, why `declare(strict_types=1)`, why sync wrappers not Amp/Revolt, why `final readonly class` for records, why `abstract readonly class` for sum-type bases, why DJB2/GMP for cassette keys, why Phar over box/humbug for the Phase 17 gate

Author: research pass for MEP-55 (Mochi-to-PHP 8.4 transpiler).
Date: 2026-05-29 15:00 (GMT+7).
Sources: `transpiler3/php/lower/lower.go`, `transpiler3/php/ptree/nodes.go`,
`transpiler3/php/colour/colour.go`, `transpiler3/php/build/packaging.go`,
`transpiler3/php/runtime/composer.json`, `website/docs/mep/mep-0055.md`.

This note explains the load-bearing design choices behind MEP-55 and
the constraints they impose. It is the "why" companion to
[[01-language-surface]]'s "what" and [[05-codegen-design]]'s "how".

## 1. Why a PHP target

Mochi already covers ten lowering targets: vm3 (reference tree-walker),
MEP-45 (C, AOT), MEP-46 (BEAM), MEP-47 (JVM bytecode), MEP-48 (.NET),
MEP-49 (Swift), MEP-50 (Kotlin), MEP-51 (Python), MEP-52 (TypeScript),
MEP-53 (Rust), MEP-54 (Erlang/OTP). Each picks up an ecosystem Mochi
cannot reach from the others.

PHP is the outlier in the list above: it is the server-side language
running the largest installed base of web applications. WordPress powers
approximately 43% of the public web as of 2026-Q1. Laravel, Symfony,
Drupal, MediaWiki, and Magento together cover most of the remainder of
the PHP-powered web tier. Packagist hosts over 400,000 Composer packages.

Every other Mochi target can run web workloads, but none of them
integrates with the Composer package registry, none deploys as a Phar
archive on a shared hosting tier, and none runs in a FrankenPHP or
RoadRunner worker pool managed by the existing PHP app-server
ecosystem. MEP-55 closes that gap.

## 2. Why PHP 8.4 as the minimum

The `require` constraint in `transpiler3/php/runtime/composer.json` is
`"php": "^8.4"`. This is not arbitrary:

**Readonly inheritance (PHP 8.4, Nov 2024).** Mochi sum types require
that `final readonly class Shape_Circle` extends an `abstract readonly
class Shape` base. PHP 8.4 introduced the rule that a `final readonly`
class can extend an `abstract readonly` base. PHP 8.3 and earlier have
no `abstract readonly` combination. In those versions, the base would
need to be a plain `abstract class`, but then the subclass cannot be
`readonly`. The entire sum-type lowering strategy depends on PHP 8.4.

**PHP 8.3.** No `abstract readonly class` syntax. Sum-type bases would
need a different encoding (perhaps a sealed interface plus a trait), which
would add complexity and lose the property that `instanceof` pattern
matching is a single `if ($x instanceof Shape_Circle)` check.

**PHP 8.2.** Same problem, plus readonly classes (`final readonly class`)
are new in 8.2, so records would also need a different encoding.

**PHP 8.1.** Readonly properties exist but not readonly classes.
Constructor promotion exists (8.0) but the `readonly` modifier on a
class is unavailable.

**PHP 8.0.** `match` expression and named arguments are available, but
readonly properties and classes are not. Records would need custom
`__construct` bodies with manual `$this->x = $x;` assignments.

**PHP 7.x.** No `match`, no named arguments, no `fn(...)` arrow
functions. The entire surface used by MEP-55's lowering would need
workarounds.

The conclusion is that PHP 8.4 is the earliest version where all five
Mochi type-system constructs (records, sum types, closures, streams,
agents) lower cleanly without workarounds.

## 3. Why `declare(strict_types=1)`

Every emitted PHP file opens with `declare(strict_types=1);` emitted by
`ptree.PhpFile.PhpSource()` (nodes.go lines 40-42). This flag:

- Prevents PHP from silently coercing `"42"` to `42` when an `int`
  parameter receives a string. Without it, `mochi_print_i64("hello")`
  would not fail at the call site.
- Makes `===` the only form of equality that matches Mochi's strict
  type identity semantics. The `==` operator in PHP with loose types
  applies coercions that have no Mochi equivalent.
- Makes PHPStan level-9 analysis more precise. Many PHPStan rules are
  only fully actionable under strict mode.

The flag applies per-file in PHP. Since the emitter writes exactly one
file per Mochi module, every emitted file gets it unconditionally.

## 4. Why `final readonly class` for records

Mochi records are structurally immutable value types. The PHP encoding
needs:

1. Immutability: fields cannot be reassigned after construction.
2. Value semantics: two records with the same fields are equal.
3. No subclassing: records are not open for extension in Mochi.

`final readonly class` satisfies all three: `readonly` enforces (1),
PHP's value-by-copy semantics for objects passed to typed parameters
satisfies (2) for the purposes of the lowerer, and `final` enforces (3).
Constructor promotion (PHP 8.0) eliminates the boilerplate
`$this->x = $x;` pattern.

The alternative would be a plain `class` with `readonly` properties on
each field. That works but requires explicit `public function
__construct(public readonly int $x) {}` syntax, which constructor
promotion already achieves more concisely.

## 5. Why `abstract readonly class` for sum-type bases

Mochi sum types need a base class that:

1. Can be the parent of a `final readonly class` variant.
2. Is not directly instantiable.
3. Carries no fields of its own (the fields live on variants).

PHP 8.4's `abstract readonly class` satisfies all three. The base body
is intentionally empty (nodes.go `ClassDecl.PhpString` line 754 comment).
Attempting to use `abstract class` (without `readonly`) for the base
breaks in PHP 8.4 because `final readonly` cannot extend a non-readonly
base (the `readonly` modifier must be consistent across the hierarchy).
The CI failure during Phase 5 of the umbrella PR confirmed this
constraint and drove the `Abstract = true` flag on `ClassDecl` to emit
`abstract readonly class` specifically.

## 6. Why `===` for all comparisons including float

PHP's `==` operator applies type coercion. For floats, `0 == false`,
`0 == ""`, and `0.0 == 0` are all true under `==`. Mochi's equality
semantics require strict type identity: `0.0 == 0` is false in Mochi
(different types), and `0.0 == 0.0` is true. The `===` operator in PHP
performs no coercion and matches Mochi's semantics exactly.

The `BinaryExpr` ptree node always uses `===` for the Mochi `==`
operator (lower.go, `lowerBinaryExpr`). This is enforced by the lowerer
regardless of the operand types.

## 7. Why sync wrappers for async, not Amp/Revolt

PHP has no preemptive scheduler in the standard library. Two
third-party options exist: `amphp/amp` (a coroutine-based event loop)
and `revolt/event-loop` (the spiritual successor). Both require
adding Composer runtime dependencies and change the function signature
conventions (Amp functions return `Future<T>` instead of `T`).

Phase 11 chose sync wrappers instead:

- `mochi_future_make($v)` wraps an already-computed value in a
  `MochiFuture` object.
- `mochi_future_await($f)` unwraps it by returning `$f->value`.
- `mochi_future_await_all($fs)` maps over a list of `MochiFuture`
  objects.

No PHP fibers, no event loop, no runtime dependency. The
`colour/colour.go` package documents this explicitly: every function is
permanently Blue (synchronous); the Red colour constant is reserved but
never produced. The `transpiler3/php/runtime/composer.json` originally
drafted `amphp/revolt` in `require`; audit round 1 removed it after
Phase 11 confirmed sync-only operation.

This choice has a cost: Mochi agents and streams are synchronous on PHP.
Phase 11 fixtures all use emit-before-recv patterns (no interleaving
needed) specifically because the sync model cannot support blocking
recv-before-emit. The trade-off buys zero external dependencies and
full compatibility with every PHP deployment environment, including
shared hosting that disallows fibers.

## 8. Why DJB2 and GMP for cassette keys

Phase 13 LLM cassette keys must be byte-equal across the PHP and C
(MEP-45) targets so the same cassette directory can be shared. The C
runtime uses DJB2 (hash = 5381, then `h = (h * 33) ^ c` for each
byte, modulo 2^64). PHP's native integer is 63-bit signed on 64-bit
platforms (PHP_INT_MAX = 9223372036854775807). Some DJB2 hash values
for real prompts exceed PHP_INT_MAX, causing PHP's integer overflow
to produce the wrong (negative or truncated) result.

GMP (`ext-gmp`) solves this: the `mochi_llm_cassette_key` helper
initialises the hash as a GMP integer, applies `gmp_and(gmp_mul($h,
33), $mask)` with a 64-bit mask `FFFFFFFFFFFFFFFF`, then calls
`gmp_strval($h, 10)` to produce the decimal string key. The GMP
operations keep the value in 64-bit unsigned space throughout. Both
`ext-gmp` and `ext-mbstring` are listed as required extensions in
`runtime/composer.json` (lines 8-9).

The Go-side `djb2CassetteKey` reimplementation in
`build/phase13_test.go` (lines 219-226) uses Go's native `uint64`
which wraps modulo 2^64 correctly, confirming equivalence with the
GMP path without needing PHP to be installed.

## 9. Why Phar over humbug/box for the Phase 17 gate

The Phase 17 packaging gate tests that a runnable Phar archive can be
produced from any emitted `main.php`. Two tools can produce Phars:

1. **humbug/box** (also called box): a Composer package that compiles,
   compresses, and optionally GPG-signs a Phar. Production builds
   prefer it. But requiring `humbug/box` as a CI dependency adds a
   `composer global require humbug/box` step and network access to
   every test run.

2. **PHP's built-in `Phar` class**: available in every PHP 8.4
   installation, no extra tools required. Less featureful (no
   compression, no GPG signing), but sufficient for structural
   validation.

The Phase 17 gate uses a generated stager script (emitted by
`emitPharStager` in `build/packaging.go` lines 53-74) that calls
`Phar::startBuffering()`, `addFile()`, `setStub()`, and
`stopBuffering()`. The stager runs under `php -d phar.readonly=0`
because `phar.readonly` defaults to `1` on many distributions. The
gate confirms the resulting `.phar` runs and produces the correct
stdout.

Production deployments can substitute `humbug/box compile` for the
stager; the gate's job is purely structural validation.

## 10. Why a direct aotir → ptree → emit approach

MEP-55 reuses the aotir IR from MEP-45 (C target) rather than
introducing a new IR or piping through a different PHP-generating tool.
The aotir represents the shared lowering obligations (monomorphisation,
closure conversion, match exhaustiveness) that are target-independent.
Below aotir, the PHP lowerer builds a PHP-specific syntax tree (ptree)
that maps 1:1 to PHP source constructs, then the emit pass renders
ptree to text by calling `ptree.PhpFile.PhpSource()`.

This two-level architecture (aotir → ptree → text) mirrors MEP-45, -46,
-47, -48, -49, -50, -51, -52, and -54. It ensures that all targets
share the same front-end guarantees (type-checking, exhaustiveness, no
unbound variables) while each target's lowerer can make PHP-specific
choices without affecting the shared path.
