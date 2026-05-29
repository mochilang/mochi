---
title: "MEP-76 research bundle"
sidebar_position: 1
sidebar_label: "Overview"
description: "Twelve research notes covering the design space behind MEP-76: language surface, design philosophy, prior art, RBS/YARD ingest, type mapping, RubyGems publish flow, OIDC trusted publishing, native extensions, Bundler/lockfile, Ractor concurrency, version resolution, and risks."
---

# MEP-76 research bundle

This bundle is the informative companion to [MEP-76](/docs/mep/mep-0076). It documents the design space the bridge sits in: prior art, the choices considered and rejected, the trade-offs accepted, and the open risks. The bundle is meant to be read alongside the spec, not in place of it.

## Notes

| Note | Subject |
|------|---------|
| [01. Language surface](01-language-surface.md) | The `import ruby "..."` import shape, the `mochi.toml` `[ruby-dependencies]` and `[ruby]` tables, the CLI surface (`mochi pkg add ruby`, `mochi pkg publish --to=rubygems.org`), and the per-import alias semantics. |
| [02. Design philosophy](02-design-philosophy.md) | Why a bidirectional bridge, why RBS over YARD as the primary type source, why a synthesised shim over direct FFI, why OIDC trusted publishing is the only publish path. |
| [03. Prior-art bridges](03-prior-art-bridges.md) | PyO3-pyo3-sys, JRuby FFI, TruffleRuby Polyglot, ruby-ffi, fiddle, Sorbet, RBS. What each gets right and what MEP-76 borrows. |
| [04. RBS bundled-sig ingest](04-rbs-ingest.md) | Walking `sig/**/*.rbs` from the gem tarball, the RBS 3.x grammar subset the bridge parses, the Go-side RBS parser shape, and the `rbs-source = "bundled"` path. |
| [05. Type mapping](05-type-mapping.md) | The closed translation table from RBS types to Mochi types, the SkipReport cases, the generic class handling, the `nil` / `untyped` desugar. |
| [06. RubyGems publish flow](06-rubygems-publish-flow.md) | The rubygems.org upload protocol, the gemspec metadata requirements, the `.gem` tarball shape, the publish-side gate. |
| [07. OIDC trusted publishing](07-oidc-trusted-publishing.md) | The rubygems.org OIDC flow, the JWT claim requirements, the `gem push` provenance attestation, and the API key fallback transition path. |
| [08. Native C extension gems](08-native-extensions.md) | Pre-built binary gem selection, the platform-suffix matching, pure-Ruby fallback gems, source build opt-in, and the SkipReport path when no option is available. |
| [09. Bundler and mochi.lock](09-bundler-lockfile.md) | Gemfile.lock format, the `[[ruby-package]]` mochi.lock schema, `--check` mode, version conflict resolution, and bundle install orchestration without a Gemfile in the user's source tree. |
| [10. Ractor and Fiber concurrency](10-ractor-concurrency.md) | Ruby 3.x Ractors, Fibers and the Fiber::Scheduler interface, the async gem opt-in, GVL implications, and TruffleRuby compatibility notes. |
| [11. Version resolution](11-version-resolution.md) | RubyGems version operators, the compact index format, pre-release handling, platform-suffix selection, and the two-tier resolution strategy (Go resolver + Bundler PubGrub fallback). |
| [12. Risks and alternatives](12-risks-and-alternatives.md) | The risk register (RBS coverage gaps, schema instability, native extension availability, Bundler drift, OIDC API stability, GVL assumption, Ruby version fragility) and rejected alternatives (YARD-only, runtime introspection, API key publish, Ractor-default, skip native extensions, delegate all resolution to Bundler). |

## Cross-references

- [MEP-76 spec](/docs/mep/mep-0076) — the normative document.
- [MEP-56](/docs/mep/mep-0056) — the Ruby transpiler this bridge builds on.
- [MEP-57](/docs/mep/mep-0057) — the source-level package system whose manifest and lockfile the bridge extends.
- [Implementation tracking](/docs/implementation/0076/) — the per-phase delivery status.
