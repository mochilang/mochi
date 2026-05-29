---
title: "Prior art: source-to-PHP tools, PHP version history, and why aotir→ptree→emit"
description: "Survey of compile-to-PHP tools (Hack/HHVM, Haxe PHP target, Dart2PHP), PHP version history by relevant feature, and why MEP-55 chose a direct aotir→ptree→emit pipeline."
sidebar_position: 3
---

# Prior art: source-to-PHP tools, PHP version history, and why aotir→ptree→emit

Author: research pass for MEP-55 (Mochi-to-PHP 8.4 transpiler).
Date: 2026-05-29 15:00 (GMT+7).
Sources: `transpiler3/php/runtime/composer.json`,
`website/docs/mep/mep-0055.md`, PHP release notes (php.net/ChangeLog),
Haxe PHP target documentation, Hack/HHVM documentation,
Dart2PHP deprecation announcement (2015).

This note surveys the prior art in source-to-PHP compilation and explains
why MEP-55 chose a direct aotir→ptree→emit approach rather than reusing
any existing tool or IR.

## 1. Hack / HHVM

Facebook's Hack language (2014) is the most prominent "compile a typed
language to PHP-compatible runtime" project. Hack added gradual typing,
generics, async/await, algebraic data types, and enums on top of PHP
syntax. HHVM (HipHop Virtual Machine) is the runtime that executes Hack
and PHP.

**What Hack solved**: Hack demonstrated that a strict type system over
a PHP-like language is viable at scale and that sum types (`shape`,
`enum`, nullable types) can be added to PHP-family syntax without
breaking the ecosystem.

**Why MEP-55 is different**: Hack targets the HHVM runtime, not standard
PHP. HHVM diverged significantly from Zend Engine PHP in terms of
extension support, hosting availability, and community adoption. By 2018
HHVM dropped backward compatibility with PHP. MEP-55 targets stock PHP
8.4 on Zend Engine, running anywhere `php` is installed, not HHVM.

Additionally, Hack is its own language with its own source syntax. MEP-55
is a transpiler from a different source language (Mochi) to PHP source.

## 2. Haxe PHP target

Haxe is a typed language that can transpile to multiple targets:
JavaScript, C++, C#, Java, Python, Lua, and PHP. The Haxe PHP target
(maintained as `haxe --target php`) emits PHP source from Haxe programs.

**What Haxe's PHP target shows**: a working source-to-PHP transpiler
can be maintained long-term. Haxe's PHP backend emits `declare
(strict_types=1)` on its output files (as of Haxe 4.2), the same
choice MEP-55 makes. Haxe also emits class hierarchies for algebraic
types.

**Why MEP-55 is not Haxe**: Haxe's type system and Mochi's are
different. Haxe uses structural types and its closures, records, and
async models differ from Mochi's. MEP-55 reuses Mochi's existing aotir
IR and the shared front-end (parser, type checker, closure conversion,
exhaustiveness) rather than building a separate pipeline. A Haxe
target for Mochi would require writing a Mochi→Haxe transpiler and
then using Haxe→PHP on top, adding a large dependency without benefit.

## 3. Dart2PHP (deprecated 2015)

Google's Dart language had an experimental Dart2PHP backend until 2015,
when Google deprecated it in favour of Dart2JS. The Dart2PHP backend
emitted PHP 5.x-era code with explicit closures (since PHP 5.3) and
lacked support for modern PHP constructs.

**What Dart2PHP showed**: the impedance mismatch between Dart's
type-safe closures and PHP 5.x closures was significant. Dart2PHP needed
explicit `use ($var)` capture lists everywhere, which required the
backend to track free variables carefully.

**Why MEP-55 avoids this**: PHP 8.0's arrow functions (`fn(...)`) capture
by value automatically from the enclosing scope. MEP-55's closure
lowering uses `ClosureExpr` (arrow functions) for all closures, which
eliminates the need to enumerate captured variables in the emitted
source. The `fn(...)` syntax was not available when Dart2PHP was
written.

## 4. AssemblyScript

AssemblyScript targets WebAssembly from TypeScript-like syntax. It has
no PHP target and is not relevant, but it demonstrates that language
targets do not need to share a runtime with the source language.
Noted as explicitly skipped (no PHP Wasm runtime was evaluated for MEP-55).

## 5. Parrot (dead)

The Parrot virtual machine (PHP via mod_parrot) was a research effort
to run multiple languages on a common VM. It never reached production
PHP deployment. Dead since 2014.

## 6. Other compile-to-PHP tools

- **transpile/php**: small open-source Python-to-PHP transpiler, maintained sporadically.
- **Snowscript**: Python-like syntax to PHP, unmaintained since 2016.
- **Symbiose**: OCaml-style syntax to PHP, prototype-only.

None of these tools produce PHP 8.4+ code, use `declare(strict_types=1)`
throughout, or target `final readonly class` for value types. None
integrate with aotir.

## 7. PHP version history: features relevant to MEP-55

| PHP version | Release | Feature relevant to MEP-55 |
|-------------|---------|----------------------------|
| 5.3 (2009) | 2009-06 | Anonymous functions with `use ($var)` capture |
| 7.0 (2015) | 2015-12 | Return types, scalar type hints with strict_types |
| 7.4 (2019) | 2019-11 | Arrow functions `fn(...)`, spread in arrays `[...$a]` |
| 8.0 (2020) | 2020-11 | Constructor promotion, named arguments, `match`, `str_contains` |
| 8.1 (2021) | 2021-11 | Readonly properties |
| 8.2 (2022) | 2022-12 | `final readonly class` |
| 8.3 (2023) | 2023-11 | No abstract readonly syntax (gap year for sum types) |
| 8.4 (2024) | 2024-11 | `abstract readonly class`, asymmetric visibility, `#[\Override]` |

The `^8.4` floor in `runtime/composer.json` means every PHP feature in
the table above through 8.4 is available unconditionally. MEP-55 uses:

- Arrow functions (7.4): `ClosureExpr` in ptree.
- Spread in arrays (7.4): `ArrayAppendExpr` (`[...$xs, $v]`).
- Constructor promotion (8.0): all record and agent classes.
- Named arguments (8.0): `NewExpr` uses named args.
- `match` expression (8.0): not used directly; MEP-55 lowers Mochi
  `match` to `ChainedIfStmt` instead for the `instanceof` discrimination.
- `str_contains` (8.0): used inside `mochi_str_contains`.
- `final readonly class` (8.2): all record classes.
- `abstract readonly class` (8.4): all sum-type bases.

PHP 8.5 runs as `allow_failure: true` in the CI matrix (see
`.github/workflows/transpiler3-php-test.yml`). PHP 8.4.0 and PHP 8.4
latest are both required passes.

## 8. Why not PHP 8.5+

PHP 8.5 was in alpha/beta at the time MEP-55 was written (2026-05). The
CI matrix includes it as `allow_failure: true` to catch breakage early
without blocking the merge. The `^8.4` constraint in `composer.json`
does not exclude 8.5 (Composer's `^` operator allows compatible minor
and patch updates within the major), so Composer would install on
8.5 freely. The `allow_failure` is a CI-policy choice, not a
compatibility exclusion.

## 9. Why a direct aotir → ptree → emit approach

Three alternatives were evaluated:

**A. Emit PHP through a Haxe intermediate**: would require writing
a Mochi→Haxe transpiler on top of the Haxe→PHP backend. Two
translation layers instead of one, and Haxe's PHP target is not
under Mochi's control.

**B. Emit PHP through a template engine**: a `text/template`-based
approach (similar to `build/packaging.go` which uses templates for
static artifacts) would work for simple programs but quickly becomes
unreadable for nested expressions and complex control flow.

**C. Build a PHP-specific ptree and emit pass**: this is the chosen
approach, mirroring MEP-45 (C ptree), MEP-51 (Python ptree), and
MEP-52 (TypeScript AST). The ptree represents PHP-specific IR at a
level of abstraction that makes it easy to reason about generated code
while keeping the emit pass simple (each node's `PhpString` method is
self-contained). The ptree approach also makes it easy to unit-test the
lowerer without running PHP: the fragment tests in `phase*_test.go` call
`Driver.Build` to get the emitted source text and call
`strings.Contains` on it.

The direct approach also means that every aotir improvement (closure
conversion, exhaustiveness, monomorphisation) propagates to PHP
automatically, without maintaining a Haxe or template-based glue layer.
