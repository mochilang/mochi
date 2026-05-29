---
title: "02. Design philosophy"
sidebar_position: 3
sidebar_label: "02. Design philosophy"
description: "Why bidirectional, why direct Ruby call (no C FFI layer), why RBS over YARD, why gem_rbs_collection for missing sigs, why Sigstore-keyless, why closed type table."
---

# 02. Design philosophy

This note frames the six load-bearing design decisions in MEP-76 alongside the alternatives that were considered and rejected. Each section follows the same structure: the decision, the alternatives considered, and the trade-offs that settled the question.

## 1. Why bidirectional

Mochi could ship only the consume direction (`import ruby "..."`) and leave Mochi-as-gem-publisher to a future MEP. Or only the publish direction. The two directions are technically independent: consuming a gem uses RBS ingest + shim synthesis; publishing uses the Mochi emit pipeline plus RubyGems.org's upload API. They share the manifest tables, the lockfile sections, and the OIDC trusted-publishing flow, but no code paths.

Shipping both directions in one MEP is the right scope because:

- **Symmetric distribution.** A library author writes Mochi, depends on Ruby gems, publishes to RubyGems.org. A library consumer either writes Mochi (uses MEP-76 `import ruby`) or writes Ruby (uses the published gem via `gem install`). A unidirectional bridge leaves one side of this symmetry broken.

- **Shared OIDC infrastructure.** Sigstore-keyless trusted publishing requires a working OIDC token exchange in CI, a Fulcio cert pull, and a Rekor log entry write. Implementing this once for the publish direction (instead of leaving it to a future MEP) amortises the work. The consume direction does not use OIDC, so there is no redundancy.

- **Shared capability surface.** The `[ruby.capabilities]` table that audits which capabilities the imported gem graph requires has its mirror in MEP-57's `[capabilities]` table that audits which capabilities a Mochi-published gem requires. Both directions share the monotonicity rule and the audit pass.

The alternative (split into MEP-76 consume + MEP-77 publish) was rejected because the seam between them is artificial and would force two separate lockfile-section migrations.

## 2. Why direct Ruby require/call instead of C FFI

MEP-73 (Rust bridge) synthesises a C-ABI wrapper crate because Rust is a memory-managed, borrow-tracked language that has no stable runtime embedding API. The Mochi-to-Rust path must cross a C-ABI seam to get lifetime-free, generic-free callability.

MEP-76 does not need that seam. The target runtime for MEP-76 is Ruby via MEP-56 (the Ruby emit target). The Mochi program already runs inside a CRuby process at runtime. Calling a gem method is a direct Ruby method dispatch: `require 'nokogiri'` followed by `Nokogiri::HTML(input)`. No translation layer exists between the shim and the gem.

The alternatives considered:

| Approach | How it works | Why rejected |
|----------|--------------|--------------|
| C FFI via `fiddle`/`ffi` gem | Mochi shim calls into C-exported symbols in the gem's `.so` | Only works for gems with `extern "C"` symbols. Nokogiri, redis, sinatra have no such surface. Fiddle is for calling C libraries, not Ruby gems. |
| C extension on top of the gem | Write a thin C extension that wraps the gem methods | Requires distributing compiled native code per platform. Same complexity as MEP-73 but without the type-safety benefit, because gem internals are Ruby objects anyway. |
| `eval` / `send` reflection | Call gem methods via `Object#send` or `eval` | Dynamic dispatch defeats static type checking. Every call becomes `untyped` at the Mochi side. |
| Direct require + method dispatch | `require 'gem'; MochiShim::Gem.method(args)` | Works. No ABI seam. Static types available via RBS. This is what MEP-76 uses. |

The concrete shim that MEP-76 generates for a nokogiri call illustrates the point. There is no `dlopen`, no C struct marshalling, no memory layout concern:

```ruby
# ruby_wrap/nokogiri/shim.rb
require 'nokogiri'

module MochiShim
  module Nokogiri
    def self.html_parse(input)
      ::Nokogiri::HTML(input)
    end
  end
end
```

Contrast with MEP-73's Rust bridge: Rust does not have an embeddable runtime that a Mochi program hosts. Rutie and Magnus (see [[03-prior-art]]) require the Rust code to link against `libruby`. MEP-76 is already inside `libruby`; the shim is a thin dispatch wrapper, not an FFI crossing.

The asymmetry between MEP-73 and MEP-76 is intentional: MEP-73 must cross a C-ABI seam to reach Rust because Rust has no runtime; MEP-76 crosses no seam to reach Ruby because Mochi is already running Ruby. The design surface aligns with the target language's runtime model.

## 3. Why RBS over YARD

MEP-76 needs a machine-readable description of each gem's public API surface. Two major documentation systems exist for Ruby:

**RBS** (Ruby Signature) is the official Ruby 3.0+ type system. It ships in the Ruby standard library (`rbs` gem), has a deterministic machine-parseable grammar, and is validated by the Ruby type checker (`steep` / `sorbet` / `typeprof`). The `sig/` directory convention was introduced in Ruby 3.0 (December 2020) and has been growing in adoption. RBS files carry versioned grammar: the `rbs-version` field in `mochi.lock` records the exact grammar version per gem.

**YARD** is a documentation generation tool that extracts type information from `@param [Type]` and `@return [Type]` comment tags. YARD has been the de-facto documentation standard since 2008. Type expressions in YARD tags are freeform strings (e.g., `Array<String>`, `Hash{Symbol => Integer}`, `Boolean`, `nil`). They have no grammar schema, no version number, and no machine-validation step. The YARD type parser in the bridge must apply heuristics to extract a Mochi-representable type from a string like `"Array<String, nil>"`.

The decision table:

| Property | RBS | YARD |
|----------|-----|------|
| Official Ruby standard | Yes (Ruby 3.0+) | No (third-party gem) |
| Machine-parseable grammar | Yes (formal BNF) | No (freeform comment strings) |
| Grammar versioning | Yes (`rbs-version`) | No |
| Type checker validation | Yes (steep, sorbet) | No |
| Stdlib coverage | Ships with `ruby/ruby` | Partial |
| Community gem coverage | `gem_rbs_collection` (400+ gems) | rubydoc.info (many gems) |
| Bridge parse complexity | Moderate (full grammar subset) | High (heuristic extraction) |

RBS wins on correctness and predictability. YARD remains the fallback for gems that ship neither bundled RBS nor `gem_rbs_collection` coverage (see §4 below and [[04-rbs-yard-ingest]] for the accuracy statistics).

## 4. Why gem_rbs_collection as fallback

Not every gem ships `sig/` RBS files. For the top-50 RubyGems (May 2026 survey), 38 have RBS coverage through bundled sigs or `gem_rbs_collection`; 9 have YARD-only coverage; 3 have neither. `gem_rbs_collection` (the GitHub repository `ruby/gem_rbs_collection`) is the community-maintained source of RBS signatures for popular gems that have not yet migrated their own `.rbs` files upstream.

Three fallback strategies were considered:

| Strategy | How it works | Problem |
|----------|--------------|---------|
| Generate stubs by running the gem | Load the gem in a subprocess, introspect via `Module#instance_methods` + `Method#parameters` | Runtime execution at lock time is a supply-chain risk. Locks cannot run user gems in a sandbox. Type information from `#parameters` does not include types, only names. |
| YARD-only fallback | Parse `@param`/`@return` tags from YARD docs | Heuristic, lossy, no machine validation. Accepted as last resort (see [[04-rbs-yard-ingest]] §YARD). |
| gem_rbs_collection | Fetch the pinned subtree of community-maintained `.rbs` files | Deterministic. SHA256-pinned in `mochi.lock`. Same quality as bundled RBS for the covered gems. |

`gem_rbs_collection` is fetched at lock time: the bridge downloads the `gems.json` manifest from the `ruby/gem_rbs_collection` repository (pinned to a specific commit SHA in `mochi.lock`), resolves the relevant gem/version entry, and downloads the corresponding `.rbs` files. The SHA256 of each downloaded file is recorded in `mochi.lock`.

The `mochi.lock` entry for a `gem_rbs_collection`-sourced gem records full provenance:

```toml
[[ruby-package]]
name = "redis"
version = "5.0.8"
source = "rubygems"
gem-sha256 = "aabbcc..."
rbs-source = "gem_rbs_collection"
rbs-collection-commit = "f3a9b1..."
rbs-sha256 = "ddeeff..."
```

The fallback chain is: bundled RBS (best) > `gem_rbs_collection` > YARD (worst). Items unreachable via any of these three paths produce a `SkipReport` and are not surfaced in the shim. The `rbs-source` field in `mochi.lock` is the audit record for which tier provided each gem's types.

## 5. Why RubyGems.org OIDC only (no API key)

RubyGems.org has historically used long-lived API keys (`gem push --key`) for `gem push` authentication. Trusted publishing via OIDC became generally available on RubyGems.org in 2023 (following GitHub's `id-token: write` support and the `trusted_publishing` key in RubyGems.org's gem settings).

MEP-76 supports only the OIDC trusted-publishing path. Long-lived API keys are rejected for the same reasons as MEP-73 §5 (crates.io) and MEP-57 (Mochi central registry):

- **Token compromise history.** Long-lived RubyGems.org API keys have appeared in compromised gem releases (the `rest-client` 1.6.13 / 1.6.14 incident, December 2019; the `bootstrap-sass` compromise, March 2019). Both were traced to stolen long-lived tokens. The supply-chain attack surface is structural: every maintainer's `.gemrc` that holds an API key is a potential vector.

- **Industry convergence.** npm, Maven Central, PyPI, crates.io, and RubyGems.org all reached trusted publishing GA within an 18-month window (2023-2025). Shipping a 2026 bridge that routes around OIDC is shipping a decade-old supply-chain story.

- **Symmetry with MEP-57.** MEP-57 mandates Sigstore-keyless for the Mochi central registry. Mochi-to-RubyGems.org publish using the same mechanism is the consistent choice; the OIDC exchange, Fulcio cert pull, and Rekor log entry write are already implemented.

The transition accommodation: `mochi pkg publish --to=rubygems.org` accepts an `--allow-api-key-fallback` flag for maintainers who have not yet migrated their CI to OIDC. The flag is default off and emits a deprecation warning. It will be removed once RubyGems.org trusted publishing reaches 90% adoption among the top-1000 gems (estimated Q2 2027).

## 6. Why a closed type table

The bridge translates RBS types to Mochi types via a fixed enumerated table (documented in [[05-type-mapping]]). Items whose parameter or return types fall outside the table are skipped with a `SkipReport`. The alternative is an open table that synthesises a Mochi struct wrapper for every Ruby type the bridge encounters.

The closed table wins for the same reasons as MEP-73 §6:

- **Predictability.** A Mochi user can read the type-mapping table and predict whether a given gem method will translate. An open table would require reading the bridge's synthesis logic to predict outcomes.

- **Refusal is information.** When the bridge refuses to translate a method, the `SkipReport` tells the user exactly which RBS type caused the refusal. The user can then write a `custom` `extern fn` override, taking explicit responsibility for the mapping at that call site.

- **Ruby's open-class problem.** Ruby classes are open: any gem can reopen `String`, `Integer`, or `Array` and add methods. An open table would face the question of which reopened methods belong to which namespace. The closed table sidesteps this entirely by committing to a fixed mapping for the primitive types and skipping everything else.

- **Auditability.** The closed table fits in a single source file (approximately 200 LOC of Go, parallel to MEP-73's table). Changes are reviewable as a unit.

The cost: the closed table refuses gem methods that take or return complex Ruby-specific types (`Proc`, `Method`, open `Struct` definitions, `BasicObject`, arbitrary `Class` values). The mitigation is the `extern fn ... custom` override path, which lets the user take responsibility for the mapping item-by-item.

## Cross-references

- [[01-language-surface]] for the user-visible surface these decisions produce.
- [[03-prior-art]] for how Rutie, Magnus, PyCall, and the ffi gem compare to the direct-require approach.
- [[04-rbs-yard-ingest]] for the RBS vs YARD ingest detail.
- [MEP-76](/docs/mep/mep-0076) for the normative spec.
- [MEP-73](/docs/mep/mep-0073) §5-6 for the parallel Rust bridge decisions this note draws on.
- [MEP-57](/docs/mep/mep-0057) for the broader capability and OIDC infrastructure.
