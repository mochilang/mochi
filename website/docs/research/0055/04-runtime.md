---
title: "Runtime: the mochi/runtime Composer package"
description: "Directory structure, IO class, @api annotation, devDependencies (Psalm 6, PHPStan 1.12, php-cs-fixer), zero runtime deps, and required PHP extensions for the mochi/runtime Composer package."
sidebar_position: 4
---

# Runtime: the `mochi/runtime` Composer package

Author: research pass for MEP-55 (Mochi-to-PHP 8.4 transpiler).
Date: 2026-05-29 15:00 (GMT+7).
Sources: `transpiler3/php/runtime/composer.json`,
`transpiler3/php/runtime/src/Mochi/Runtime/IO.php`,
`transpiler3/php/runtime/psalm.xml`,
`transpiler3/php/runtime/phpstan.neon`,
`.github/workflows/transpiler3-php-test.yml`.

The `mochi/runtime` Composer package is the PHP-side runtime library for
MEP-55-generated programs. It ships under the `Mochi\Runtime` PSR-4
namespace, has zero Composer runtime dependencies, and requires exactly
two PHP extensions (`ext-mbstring` and `ext-gmp`). See
[[02-design-philosophy]] for the rationale behind zero runtime deps and
the GMP requirement.

## 1. Directory structure

```
transpiler3/php/runtime/
  composer.json          # package manifest
  psalm.xml              # Psalm 6 configuration
  phpstan.neon           # PHPStan level-9 configuration
  phpunit.xml.dist       # PHPUnit 11.3 configuration
  TRUST.md               # package trust and signing notes
  src/
    Mochi/
      Runtime/
        IO.php           # print primitives
  tests/
    Mochi/
      Runtime/           # PHPUnit test suite (autoloaded via autoload-dev)
```

The `autoload` stanza in `composer.json` maps `Mochi\Runtime\` to
`src/Mochi/Runtime/` (PSR-4). The `autoload-dev` stanza maps
`Mochi\Runtime\Tests\` to `tests/Mochi/Runtime/`.

## 2. The IO class

`src/Mochi/Runtime/IO.php` defines `final class IO` in the `Mochi\Runtime`
namespace with four static methods:

- `IO::printString(string $value): void` — echoes `$value` plus `"\n"`.
- `IO::printInt(int $value): void` — echoes `$value` plus `"\n"`.
- `IO::printBool(bool $value): void` — echoes `"true\n"` or `"false\n"`.
- `IO::printFloat(float $value): void` — mirrors Go's
  `strconv.FormatFloat('g', -1, 64)` contract: NaN → `"NaN"`, +Inf →
  `"+Inf"`, -Inf → `"-Inf"`, whole-number floats → integer digits (so
  `4.0` prints as `"4"`), others echo PHP's default float-to-string.

The float formatter in IO.php (lines 57-73) is identical in intent to
the `mochi_print_f64` inline helper in `lower.go` (lines 171-184). The
inline helper is used in Phase 1-14 (before the Composer package lands);
Phase 15 switches to the Composer-autoloaded `\Mochi\Runtime\IO` class.

## 3. The `@api` annotation and why it is needed

The IO class is referenced only from lowered Mochi programs, which live
outside the `src/` directory tree that Psalm scans. From Psalm's
perspective, no code in `src/` calls `IO::printString`, so Psalm's
`UnusedClass` check would report the entire class as dead code and fail
the CI gate.

The `@api` PHPDoc tag on the class docblock tells Psalm that `IO` is an
externally-consumed public surface and suppresses the `UnusedClass`
finding. This pattern is standard in Psalm for library packages whose
entry points are not exercised from within the package's own source tree.

The `TRUST.md` file in the runtime directory documents this annotation
convention and why it cannot be removed.

## 4. devDependencies

The `require-dev` stanza in `composer.json` (lines 13-19) lists:

```json
"phpstan/phpstan": "^1.12",
"phpstan/phpstan-strict-rules": "^1.6",
"phpstan/phpstan-deprecation-rules": "^1.2",
"vimeo/psalm": "^6.0",
"friendsofphp/php-cs-fixer": "^3.65",
"phpunit/phpunit": "^11.3"
```

None of these appear in `require` (runtime dependencies). They are
dev-only and do not ship inside a `vendor/` tree when a consuming project
runs `composer install --no-dev`.

### 4.1 PHPStan 1.12

PHPStan at level 9 (the strictest level) is configured via
`phpstan.neon`. Level 9 enables:
- All type-mismatch checks.
- Dead code detection (unused variables, unreachable branches).
- Array/property access safety.

The CI gate runs `vendor/bin/phpstan analyse --no-progress`.

### 4.2 Psalm 6 (not 5)

The constraint `"vimeo/psalm": "^6.0"` is significant. Psalm 5.x
had a PHP 8.4 platform detection failure: Psalm 5 did not recognise
PHP 8.4 in its `phpversion` analysis and flagged `abstract readonly class`
syntax as invalid. Upgrading to Psalm 6 was required during the first
audit round of MEP-55. Psalm 6 added PHP 8.4 support.

The CI runs `vendor/bin/psalm --no-progress --no-cache`.

### 4.3 php-cs-fixer 3.65

Configured to enforce PER-CS2.0 (PHP-FIG Extended Coding Style 2.0)
plus the PHP 8.4 migration ruleset. The CI gate runs
`vendor/bin/php-cs-fixer fix --dry-run --diff` (zero rewrites required).

The formatter enforces 4-space indentation (which the ptree emit pass
also uses: `indent(n)` in `nodes.go` returns `strings.Repeat("    ",
n)`), single blank lines between declarations, and PSR-4 file structure.

### 4.4 PHPUnit 11.3

The runtime test suite lives under `tests/Mochi/Runtime/`. PHPUnit 11.3
requires PHP 8.2+, so it is compatible with the ^8.4 platform floor.

## 5. Runtime (production) dependencies

The `require` stanza has exactly three entries:

```json
"php": "^8.4",
"ext-mbstring": "*",
"ext-gmp": "*"
```

No Composer packages appear in `require`. This means `composer install`
on a project that depends on `mochi/runtime` does not pull in any
third-party PHP code. The two extensions are:

- `ext-mbstring`: multibyte string functions. Required for UTF-8-aware
  string operations in Phase 12+ FFI and fetch layers.
- `ext-gmp`: GNU Multiple Precision Arithmetic. Required for the Phase 13
  DJB2 cassette key computation (the hash can exceed PHP_INT_MAX, so
  GMP is used for correct uint64 arithmetic). See [[02-design-philosophy]]
  for the rationale.

Both extensions are available on all major PHP distributions (Ubuntu,
Debian, Alpine, macOS Homebrew). The CI matrix installs them via
`shivammathur/setup-php@v2` with `extensions: mbstring, gmp`.

## 6. Minimum-stability

`"minimum-stability": "stable"` in `composer.json` ensures that
`composer install` only resolves stable releases of dev dependencies.
No alpha or RC builds of PHPStan or Psalm are allowed in the lock file.

## 7. Composer configuration

```json
"config": {
    "sort-packages": true,
    "preferred-install": "dist"
}
```

`sort-packages: true` keeps `composer.json` sorted deterministically,
matching the Phase 16 reproducibility goal for the source tree.
`preferred-install: dist` prefers zip distributions over source clones,
which speeds up CI.

## 8. Phase 15: Composer package staging

Phase 15 wires the Composer package into the build sandbox. The
`runtimeSourceDir()` function in `build.go` (lines 183-195) locates the
`transpiler3/php/runtime/` directory relative to the Go source file using
`runtime.Caller(0)`. The `copyFile(dst, src)` helper (lines 199-215)
copies individual files into the sandbox, creating parent directories as
needed.

The inline runtime helpers (`mochi_print_str`, `mochi_str_contains`, etc.)
that Phase 1-14 inject directly into the emitted PHP file are superseded
in Phase 15 by the Composer-autoloaded equivalents. The transition is
transparent to the fixtures because the public interface (function name,
parameter types, return type) is identical.
