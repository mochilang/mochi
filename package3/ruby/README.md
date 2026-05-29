# package3/ruby

Bidirectional Ruby gem bridge for Mochi. Implementation lives under this directory as phases land per [MEP-76 implementation tracking](../../website/docs/implementation/0076/index.md).

## Status

Phase 0 skeleton. No functional code yet. The per-phase delivery plan is tracked in [MEP-76](../../website/docs/mep/mep-0076.md).

## What this is

The MEP-76 bridge sits between MEP-56 (Mochi-to-Ruby transpiler) and MEP-57 (Mochi source-level package system). Two directions:

- **Consume**: `import ruby "<gem>@<semver>" as <alias>` in Mochi source. The bridge resolves the gem from the RubyGems compact index, extracts the API surface via RBS (Ruby 3.0+ type signatures), lowers through a closed type-mapping table, and emits a thin Ruby shim (`shim.rb`) plus a Mochi `extern fn` file (`shim.mochi`). Because MEP-56 already transpiles Mochi to Ruby, no C FFI wrapper is needed: the shim is a plain `require 'gem'` followed by method-forwarding Ruby modules.

- **Publish**: `mochi pkg publish --to=rubygems.org`. The bridge builds via MEP-56's existing `TargetRubyGem` target, populates gemspec fields from `mochi.toml [ruby.publish]`, and uploads through RubyGems.org OIDC trusted publishing (GA since 2023). No long-lived API token path is supported.

## Directory layout

```
package3/ruby/
  errors/     # SkipReport + cross-cutting error types (phase 0)
  rbs/        # RBS type-sig parser + gem_rbs_collection fetcher (phases 2-3)
  index/      # RubyGems compact-index client + SHA256 verify (phase 1)
  typemap/    # closed RBS-to-Mochi type-mapping table (phase 5)
  wrapper/    # Ruby shim emitter + Mochi extern-fn emitter (phase 6)
  build/      # Gemfile synthesis + bundle install orchestration (phase 8)
  gemspec/    # gemspec emit from mochi.toml [ruby.publish] (phase 10)
```

## References

- [MEP-76 spec](../../website/docs/mep/mep-0076.md) for the normative design.
- [MEP-76 research bundle](../../website/docs/research/0076/index.md) for the 12-note deep-research collection.
- [MEP-76 implementation tracking](../../website/docs/implementation/0076/index.md) for the 14-phase rollout.
- [package3/rust/](../rust/) for the parallel Rust crate bridge (MEP-73).
