---
title: "02. Design philosophy"
sidebar_position: 3
sidebar_label: "02. Design philosophy"
description: "Why a bidirectional PHP bridge, why PHP Reflection API over PHP-Parser and PHPStan as the primary surface source, why PHP-only scope with no C wrapper or cross-target support, why PSR-4 is the only viable library layout, why API-token plus GPG plus Sigstore for publish (and the honest accounting of the missing OIDC gap), why the async bridge is opt-in, and why the type-mapping table is closed not open."
---

# 02. Design philosophy

Author: research pass for MEP-75 (Mochi and PHP package bridge). Date: 2026-05-29 22:11 (GMT+7).

This note frames the seven load-bearing design decisions in MEP-75 alongside the alternatives that were considered and rejected. Each section follows the same structure: the decision, the alternatives, the trade-offs.

## 1. Why the PHP Reflection API (not PHP-Parser or PHPStan)

The bridge needs a machine-readable description of every Composer package's public PHP surface. Four candidate sources existed:

**PHP Reflection API** (`new ReflectionClass($name)`, `new ReflectionFunction($name)`, `ReflectionNamedType`, `ReflectionUnionType`, `ReflectionIntersectionType`). Available in every PHP installation since PHP 5.0 (substantially improved through PHP 8.x). Returns the public surface of a loaded class: properties, methods, parameter types, return types, constants, attributes. Supplemented by PHPDoc parsing for type information the runtime API cannot express (array shapes, generic annotations, narrowed conditional returns). Supplemented by Psalm stubs when the package ships them.

**nikic/php-parser**. A pure PHP library (no PHP execution needed) that parses PHP source into an AST. Returns the raw syntactic surface: class declarations, method declarations, parameter names. Does NOT resolve types across files (cross-file type resolution requires a type checker, not just a parser). Would need a full Go-side PHP parser re-implementation to work without `php` on the build host.

**PHPStan's reflection layer**. PHPStan ships a sophisticated reflection system that understands PHPDoc generics, array shapes, and conditional return types. However, PHPStan is a Composer dev-dependency that requires the full package's source plus its own dependencies (~40MB installed); it requires `php` on the build host; and it runs the entire PHPStan pipeline just to extract the surface. The overhead is substantial.

**phpDocumentor's Reflection component**. A separate Composer library that parses PHP source and PHPDoc. Has the same heavy-dependency and PHP-required issues as PHPStan.

The PHP Reflection API wins because it is available everywhere `php` is installed, it is stable (part of the PHP core), and its output corresponds to the runtime type system (what PHP actually enforces). PHPDoc + Psalm stubs fill the gaps for type-level information the runtime API cannot express. The reflection CLI is a self-contained ~200-line PHP script with zero Composer dependencies.

The residual limitation: `mixed`, untyped `array`, and `object` remain opaque to the Reflection API even with PHPDoc help. These are handled by the SkipReport mechanism: see [[05-type-mapping]] §R1.

## 2. Why a PHP-only bridge scope (no C wrapper, no cross-target)

MEP-73 (Rust) and MEP-74 (Go) generate an `extern "C"` wrapper crate or a `//export` cgo package, respectively, that any Mochi build target can link against. The rationale is that those languages compile to native code and expose a stable C ABI. PHP does not.

PHP is a dynamic scripting language that runs inside a PHP process. There is no stable C ABI for PHP code that another compiled language can call. `ext-ffi` (PHP's FFI extension) works in the other direction (PHP calling C), not C calling PHP. The Zend Engine C API (`zend_function`, `zend_class_entry`) is not a public stable ABI across PHP versions.

Consequently, the PHP bridge can only be used from Mochi programs that target the MEP-55 PHP output. A Mochi program compiled to Rust (via MEP-53) cannot call a PHP package via the MEP-75 bridge; it would need to spawn a PHP subprocess, which is a different problem. The bridge's scope is therefore:

- **Consumer direction**: Mochi source compiled via MEP-55 can `import php "..."` and call PHP packages.
- **Producer direction**: `TargetPhpLibrary` emits a Composer library that PHP users can `composer require`.
- **No cross-target support**: the bridge has no effect on any non-PHP Mochi build target.

This narrower scope is correct, not a limitation. The bridge does exactly what is possible given PHP's architecture.

## 3. Why a closed type-mapping table

The same argument as MEP-73 §6 applies with PHP-specific texture. PHP's type system is richer than it looks:

PHP 8.4 supports: scalar types (`int`, `float`, `string`, `bool`), nullable (`?T`), union types (`int|string`), intersection types (`A&B`), named classes, interfaces, `void`, `never`, `mixed`, `object`, `array`, typed arrays via PHPDoc (`array<int, string>`), enums (pure and backed), readonly class properties, fibers, first-class callables, and `Closure`.

A closed table that covers the straightforward cases (scalar types, nullable, named classes, interfaces, void, never, typed arrays via PHPDoc) and refuses the ambiguous ones (mixed, object, untyped array, self, static, parent, multi-member union types, intersection types) provides:

- **Predictable user surface.** The user reads the table and knows whether a given PHP item will translate.
- **Refusal as information.** The SkipReport names the item and the reason, guiding the user toward hand-written overrides.
- **No silent miscompilation.** An open table that guessed `mixed -> any` or `array -> list<unknown>` would produce Mochi declarations that type-check but behave incorrectly at runtime.

The closed table approach is the same philosophy as MEP-73 and MEP-74. The PHP version of the table is described fully in [[05-type-mapping]].

## 4. Why PSR-4 is the only viable library layout

Packagist requires published Composer packages to declare their autoload mapping in `composer.json`. Two autoloading standards exist:

**PSR-0** (deprecated). Maps class `Foo_Bar_Baz` to file `Foo/Bar/Baz.php`. Deprecated by PHP-FIG in 2014. Still accepted by Composer and Packagist but not recommended. New packages should not use PSR-0.

**PSR-4**. Maps namespace `Foo\Bar\` to directory `src/Foo/Bar/`. Class `Foo\Bar\Baz` lives in `src/Foo/Bar/Baz.php`. This is the standard for all modern PHP packages.

`TargetPhpLibrary` emits PSR-4 layout exclusively. PSR-0 is not offered. The bridge derives the PSR-4 namespace root from `[php.publish] psr4-namespace` (defaulting to a PascalCase transformation of the Packagist package name).

The namespace derivation rule:
- `my-vendor/my-package` becomes `MyVendor\\MyPackage\\`.
- `symfony/console` becomes `Symfony\\Console\\` (though for actual Symfony packages the user would set `psr4-namespace = "Symfony\\Component\\Console\\"`).
- The user overrides with an explicit `psr4-namespace` when the default derivation does not match the target ecosystem's convention.

PSR-4 is the only viable layout because every PHP static analysis tool (PHPStan, Psalm, Intelephense), every IDE (PhpStorm, VS Code with PHP Intelephense), and every modern PHP framework (Laravel, Symfony, Drupal 10) assumes PSR-4.

## 5. Why API-token plus GPG plus Sigstore (and honest gap accounting)

MEP-73 (Rust/crates.io) and MEP-74 (Go/module proxy + cosign) ship OIDC-native signing stories. MEP-75 cannot, because Packagist does not (as of May 2026) support OIDC trusted publishing.

Packagist's current publish story:
1. The author pushes a semver-tagged commit to a public git repo.
2. Packagist discovers the tag via the GitHub/GitLab webhook integration (GitHub App) or the Update API.
3. The Update API requires a long-lived API token in the `apiToken` query parameter.
4. Packagist does NOT verify the publisher's identity via OIDC; it trusts whoever holds the API token.

MEP-75's mitigation stack:
- **GPG-signed git tags**: the signed tag proves the tagger's identity (their GPG key). Downstream users who verify the tag signature can confirm the author identity.
- **Sigstore `actions/attest-build-provenance@v1` attestation**: a Sigstore keyless OIDC attestation on the dist zip, tied to the GitHub Actions workflow identity. This does not prevent a bad Packagist API token from being abused, but it provides a transparency-log entry for the published artifact.
- **GitHub App integration**: registering the GitHub App on the repository allows Packagist to auto-detect new tags via webhook, removing the need to call the Update API with a token manually. The App token is managed by GitHub, not by the user.

This is a materially weaker supply-chain story than crates.io's RFC #3724. MEP-75 does not pretend otherwise. The gap is documented here and in [[07-packagist-trusted-publishing-gap]]. The roadmap: when Packagist ships OIDC (it is on their roadmap but has no GA date as of May 2026), MEP-75 will add a `mochi pkg publish --to=packagist --oidc` path that uses short-lived CI tokens, matching MEP-73's model.

## 6. Why bidirectional

The same core argument as MEP-73 §1:

- **Symmetric distribution.** A Mochi author writes a PHP library, depends on Packagist packages, publishes to Packagist. A PHP user `composer require`s it. A Mochi-to-PHP bridge without the producer direction would mean PHP users cannot consume Mochi libraries.
- **Shared infrastructure.** The Packagist v2 client, the reflection CLI, the PSR-4 emitter, the `composer.json` synthesiser, and the dist-zip tooling are all shared between the consume and produce directions. Shipping both in one MEP amortises the work.
- **MEP-55 already ships the PHP output pipeline.** The MEP-55 pipeline already lowers Mochi to PHP and packages it as a Composer library. MEP-75's producer direction extends that with `TargetPhpLibrary` and the Packagist publish flow. The extension is small relative to what MEP-55 already built.

The alternative (split into MEP-75 consume + MEP-76 produce) was rejected because the seam between them is artificial and would require two lockfile-section migrations.

## 7. Why the async bridge is opt-in

PHP has no native async runtime in the standard library. PHP 8.1 introduced fibers (cooperative green threads), but fibers do not provide an event loop; they are low-level building blocks. The two major event loops in the PHP ecosystem are:

- **ReactPHP** (`react/event-loop`, first release 2012, mature, widely adopted, compatible with PHP 8.4). Provides a non-blocking I/O event loop backed by libuv or PHP stream_select.
- **RevoltPHP** (`revolt/event-loop`, extracted from amphp in 2022, shared by amphp/amp and ReactPHP since ReactPHP 3.0). The lower-level fiber-backed event loop that both ReactPHP 3 and amphp use.

MEP-55 Phase 11 (async colouring) concluded that the PHP target lowers Mochi async to synchronous wrappers, with no event-loop dependency. This decision holds for the common case. The async bridge opt-in exists for programs that:

1. Import Packagist packages that are written for ReactPHP (e.g., `react/http`, `react/socket`, `clue/reactphp-buzz`).
2. Need non-blocking I/O in their PHP programs and want to leverage these packages.

The async opt-in has non-trivial cost: injecting `react/event-loop` or `revolt/event-loop` into the vendor sandbox adds ~1.2MB of PHP code and changes the execution model from synchronous to event-loop-driven. Most Mochi programs targeting PHP do not need this. Making it opt-in keeps the common path simple and fast.

The detailed async bridge design is in [[08-async-bridge]].

## Cross-references

- [[01-language-surface]] for the user-visible surface.
- [[03-prior-art-bridges]] for the PHP interop landscape comparison.
- [[04-packagist-ingest]] for the Packagist v2 API and reflection CLI.
- [[05-type-mapping]] for the closed table contents.
- [[07-packagist-trusted-publishing-gap]] for the OIDC gap detail.
- [[08-async-bridge]] for the ReactPHP/RevoltPHP opt-in detail.
- [[09-psr-autoloading]] for the PSR-4 layout decision.
- [MEP-75](/docs/mep/mep-0075) for the normative spec.
