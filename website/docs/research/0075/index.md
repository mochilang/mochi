---
title: "MEP-75 research bundle"
sidebar_position: 1
sidebar_label: "Overview"
description: "Twelve research notes covering the design space behind MEP-75: language surface, design philosophy, prior-art PHP bridges, Packagist ingest, the closed type-mapping table, the Composer publish flow, the Packagist trusted-publishing gap, the async bridge, PSR-4 autoloading, the build system, testing gates, plus the risks and rejected alternatives register."
---

# MEP-75 research bundle

This bundle is the informative companion to [MEP-75](/docs/mep/mep-0075). It documents the design space the bridge sits in: prior art, the choices considered and rejected, the trade-offs accepted, and the open risks. The bundle is meant to be read alongside the spec, not in place of it.

## Notes

| Note | Subject |
|------|---------|
| [01. Language surface](01-language-surface.md) | The `import php "vendor/package@^semver" as alias` import shape, the `[php-dependencies]` / `[php]` / `[php.publish]` / `[php.capabilities]` manifest tables, and the CLI surface (`mochi pkg add php`, `mochi pkg publish --to=packagist`, `mochi pkg sync php`). |
| [02. Design philosophy](02-design-philosophy.md) | Why a bidirectional bridge, why PHP Reflection API over PHP-Parser and PHPStan, why PHP-only scope with no C wrapper, why PSR-4 is the only library layout, why API-token plus GPG plus Sigstore for publish, why the async bridge is opt-in, why the type-mapping table is closed not open. |
| [03. Prior-art bridges](03-prior-art-bridges.md) | Survey of HHVM/Hack, PHP FFI extension (ext-ffi), PHP-CPP, Node.js edge-php, Python php-serialize, Go/PHP via exec, phpize, PhpBrew, and other PHP interop stories; comparison with MEP-73's Rust bridge and MEP-74's Go bridge; why MEP-75 chose reflection-first over annotation-first. |
| [04. Packagist ingest](04-packagist-ingest.md) | The Packagist v2 sparse API in depth: `p2/<vendor>/<package>.json` format, dist/source URL resolution, SHA-256 checksums in Packagist responses, comparison with Cargo sparse index and Go module proxy, Packagist webhook model, and the Packagist mirror protocol. |
| [05. Type mapping table](05-type-mapping.md) | The complete closed PHP-to-Mochi type table: every PHP 8.4 type with translation rationale, union types, intersection types, nullable, void, never, mixed, callable, object, self, static, parent, array shapes (list vs map heuristic), typed array PHPDoc annotations, enum (pure and backed), readonly class, fibers. |
| [06. Composer publish flow](06-composer-publish-flow.md) | Packagist publish flow in detail: `composer.json` schema, VCS-based Packagist discovery, git tag plus webhook vs API token ping, dist zip vs source, GPG-signed tags, Sigstore attestation, and the GitHub App integration as partial mitigation. |
| [07. Packagist trusted-publishing gap](07-packagist-trusted-publishing-gap.md) | Deep dive on the Packagist trusted-publishing gap: comparison with npm Trusted Publishing, PyPI OIDC, and crates.io RFC #3724; supply-chain risks the gap enables; Packagist's current mitigations; what a hypothetical OIDC-trusted Packagist would look like; and the MEP-75 workaround plus roadmap. |
| [08. Async bridge](08-async-bridge.md) | PHP async story: fibers (PHP 8.1), ReactPHP event loop, RevoltPHP (from amphp/revolt), PHP cooperative multitasking vs MEP-55's sync-wrapper philosophy, when the async opt-in makes sense, and the async-capability flag. |
| [09. PSR-4 autoloading](09-psr-autoloading.md) | PSR-4 autoloading deep dive: why PSR-4 is the only viable layout, how class FQCN maps to file paths, how Composer generates autoload maps, how MEP-75 emits PSR-4-compliant `src/` trees for TargetPhpLibrary, why PSR-0 is deprecated, and vendor-prefix collision avoidance. |
| [10. Build system](10-build-system.md) | Build system for the bridge: `package3/php/` Go module structure, the reflection CLI design, PHP version pinning, content-addressed cache design, integration with MEP-55 Driver.Build vendor/ injection, and CI matrix (PHP 8.4.0, 8.4 latest, 8.5 allow-failure). |
| [11. Testing gates](11-testing-gates.md) | Testing strategy: 24-package fixture corpus with per-package test angles, reflection snapshot tests, extern shim compilation tests, end-to-end composer install + exec tests, mochi.lock round-trip tests, and TargetPhpLibrary emit tests. |
| [12. Risks and alternatives](12-risks-and-alternatives.md) | Risk register: PHP type system looseness, PHP version spread, Packagist no OIDC, reflection API instability, PSR-4 namespace collisions, Composer solver vs PubGrub divergence, ReactPHP vs RevoltPHP fragmentation, PHP fibers vs OS threads, TargetPhpLibrary breaking on PHP < 8.1, and composer install network dependency in CI. |

## Cross-references

- [MEP-75 spec](/docs/mep/mep-0075) — the normative document.
- [MEP-55](/docs/mep/mep-0055) — the PHP transpiler this bridge builds on.
- [MEP-57](/docs/mep/mep-0057) — the source-level package system whose manifest and lockfile the bridge extends.
- [MEP-73](/docs/mep/mep-0073) — the Rust bridge whose philosophy and template MEP-75 follows.
- [MEP-74](/docs/mep/mep-0074) — the Go bridge, the immediate predecessor in the bridge series.
- [Implementation tracking](/docs/implementation/0075/) — the per-phase delivery status.
