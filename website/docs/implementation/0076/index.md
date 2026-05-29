---
title: MEP-76 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 76. Mochi+Ruby package manager"
description: "Per-phase implementation tracking for MEP-76 (Mochi+Ruby package bridge). Status + commit columns capture how each phase lands on main."
---

# MEP-76 implementation tracking

Per-phase tracking for [MEP-76 Mochi+Ruby package bridge](/docs/mep/mep-0076). Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main`.

A phase is LANDED only when its gate is green for every target in the runtime matrix. Missing surfaces become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

## Phase status

| Phase | Title | Status | Commit | Tracking page |
|-------|-------|--------|--------|---------------|
| 0 | Skeleton: package3/ruby/ layout (rbs/, index/, typemap/, wrapper/, build/, errors/, gemspec/) | NOT STARTED | — | [phase-00](/docs/implementation/0076/phase-00-skeleton) |
| 1 | RubyGems compact-index client (https://index.rubygems.org/info/\<gem\>, SHA256 + BLAKE3 verify) | NOT STARTED | — | [phase-01](/docs/implementation/0076/phase-01-compact-index) |
| 2 | RBS bundled-sig ingest (walk sig/\*\*/*.rbs from gem tarball) | NOT STARTED | — | [phase-02](/docs/implementation/0076/phase-02-rbs-ingest) |
| 3 | gem_rbs_collection fallback ingest (fetch + pin gems.json manifest) | NOT STARTED | — | [phase-03](/docs/implementation/0076/phase-03-gem-rbs-collection) |
| 4 | YARD doc fallback ingest (yard doc JSON, best-effort type extraction) | NOT STARTED | — | [phase-04](/docs/implementation/0076/phase-04-yard-ingest) |
| 5 | RBS-to-Mochi type mapping + SkipReport emit | NOT STARTED | — | [phase-05](/docs/implementation/0076/phase-05-type-mapping) |
| 6 | Ruby shim emit (shim.rb require + method wrappers + shim.mochi extern fn declarations) | NOT STARTED | — | [phase-06](/docs/implementation/0076/phase-06-shim-emit) |
| 7 | `import ruby "<gem>@<semver>" as <alias>` grammar + parser | NOT STARTED | — | [phase-07](/docs/implementation/0076/phase-07-import-grammar) |
| 8 | Build orchestration (Gemfile injection into MEP-56 emit pipeline + bundle install) | NOT STARTED | — | [phase-08](/docs/implementation/0076/phase-08-build) |
| 9 | mochi.lock `[[ruby-package]]` integration + --check mode | NOT STARTED | — | [phase-09](/docs/implementation/0076/phase-09-lockfile) |
| 10 | TargetRubyGem publish metadata + gemspec emit from mochi.toml `[ruby.publish]` | NOT STARTED | — | [phase-10](/docs/implementation/0076/phase-10-gemspec-emit) |
| 11 | Trusted publishing (rubygems.org OIDC flow, gem push, provenance attestation) | NOT STARTED | — | [phase-11](/docs/implementation/0076/phase-11-trusted-publish) |
| 12 | Native C extension gems (pre-built binary gem selection + pure-Ruby fallback map) | NOT STARTED | — | [phase-12](/docs/implementation/0076/phase-12-native-ext) |
| 13 | Ractor/Fiber async bridge (`[ruby.async]` framework opt-in) | NOT STARTED | — | [phase-13](/docs/implementation/0076/phase-13-ractor-fiber) |

## Runtime matrix

| Phase range | CI target | Notes |
|-------------|-----------|-------|
| 0-9 | CRuby 3.2, CRuby 3.4 on ubuntu-latest | Gated against the 20-gem fixture corpus |
| 10-11 | CRuby 3.2, CRuby 3.4 on ubuntu-latest | Gated against the mock-rubygems-registry harness (no live network) |
| 12 | CRuby 3.2, CRuby 3.4 on ubuntu-latest + macos-latest | Fixture corpus extended to include native extension gems (nokogiri, pg) |
| 13 | CRuby 3.2, CRuby 3.4, TruffleRuby 24.x on ubuntu-latest | Fixture corpus extended to include async gems (async, falcon) |

## Fixture corpus

The 20-gem fixture corpus (May 2026 top-downloaded-on-rubygems.org, excluding deprecated or stdlib-replaced gems):

nokogiri, rails, rake, bundler, activesupport, activerecord, actionpack, devise, rspec, rspec-core, rubocop, pry, httparty, faraday, redis, pg, grpc, aws-sdk-s3, oj, dry-types.

Phases 0-9 assert golden SkipReport counts against this corpus. Phases 12-13 extend the corpus with native extension gems (nokogiri, pg) and async gems (async, falcon).

## Implementation location

The bridge lives at `package3/ruby/` in the repo root:

```
package3/ruby/
  README.md               # pointer to MEP-76 spec
  errors/                 # SkipReason + BridgeError (phase 0)
  build/                  # Gemfile synthesiser + bundle install driver (phase 0)
  semver/                 # rubygems-flavoured semver parser (phase 1)
  index/                  # compact-index client + content-addressed cache (phase 1)
  rbs/                    # RBS parser + bundled-sig walker (phase 2)
  typemap/                # RBS-to-Mochi type table + SkipReport (phase 5)
  wrapper/                # shim.rb + shim.mochi emitter (phase 6)
  gemspec/                # gemspec renderer from mochi.toml (phase 10)
  publish/                # rubygems.org OIDC publish + gem push (phase 11)
```

## Cross-references

- [MEP-76 spec](/docs/mep/mep-0076) for the normative design.
- [MEP-76 research bundle](/docs/research/0076/) for the 12-note deep-research collection.
- [MEP-56 implementation tracking](/docs/implementation/0056) for the Ruby transpiler that MEP-76 extends.
- [MEP-57 implementation tracking](/docs/implementation/0057) for the polyglot package system MEP-76 builds on.
