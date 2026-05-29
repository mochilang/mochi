---
title: "12. Risks and alternatives"
sidebar_position: 13
sidebar_label: "12. Risks"
description: "Risk register (RBS coverage gaps, Ruby version fragility, native extension pre-built availability, Bundler version drift, OIDC token scope, RubyGems.org API stability) and rejected alternatives (YARD-only, runtime introspection, API key publish, Ractor-default, GVL assumption)."
---

# 12. Risks and alternatives

This note collects the risks MEP-76 carries and the alternative approaches considered and rejected. The risk register is a forward-looking inventory; the alternatives section documents the reasoning so future maintainers can understand why the chosen path was taken.

## Risk register

| ID | Risk | Likelihood | Impact | Mitigation |
|----|------|------------|--------|------------|
| R1 | RBS coverage gap: gem has no RBS and gem_rbs_collection has no entry | Medium | Medium | YARD fallback; if YARD also absent, SkipReport all items; unblock via `extern fn ... custom` |
| R2 | RBS schema instability: rbs gem major version bump changes .rbs syntax | Low | High | Bridge pins `rbs-version` in mochi.lock; test against rbs 2.x and 3.x fixtures; `--check` catches format drift |
| R3 | Native extension gem: pre-built binary not available for target platform | Medium | Medium | Pure-Ruby fallback gem if exists; source build opt-in; SkipReport if neither |
| R4 | Bundler version drift: bundle install behavior changes between Bundler versions | Low | Medium | Bridge pins `bundler-version` in `[ruby]` table; CI tests against Bundler 2.4, 2.5, 2.6 |
| R5 | RubyGems.org OIDC API change: trusted publishing endpoint changes | Low | High | Bridge wraps `gem push` (not raw API); rubygems.org OIDC has been stable since 2023; `--allow-key-fallback` escape hatch |
| R6 | GVL assumption: code assumes CRuby GVL serializes gem calls; breaks on TruffleRuby | Medium | Medium | Emit thread-safety warnings in shim for flagged gems; test on TruffleRuby CI (MEP-56 phase 26 already has TruffleRuby CI) |
| R7 | Ruby version fragility: gem requires Ruby >= 3.2 but user has Ruby 3.1 | Low | Low | Bridge checks `ruby-version` in `[ruby]` against each gem's `required_ruby_version`; lock fails with diagnostic |

## R1: RBS coverage gap

**Risk**: a gem has no bundled RBS signatures and is not in `gem_rbs_collection`. The YARD fallback produces no useful type information either (gem is poorly documented or uses freeform type strings).

**Likelihood**: medium. Approximately 30-40% of gems outside the top-200 have neither bundled RBS nor a `gem_rbs_collection` entry (May 2026 survey).

**Impact**: medium. All items from the gem produce `SkipReport` entries in the Mochi type surface. The gem is still usable via `extern fn ... custom` declarations, but the user must author the type signatures by hand.

**Mitigation**:
- The bridge runs the three-tier ingest pipeline: bundled RBS first, then `gem_rbs_collection`, then YARD. Each tier reduces the SkipReport count.
- The bridge emits a diagnostic listing the skipped items, ordered by estimated usage frequency (based on download counts for similar gems).
- The user can unblock individual items via `[[ruby.extern]]` in `mochi.toml`:
  ```toml
  [[ruby.extern]]
  gem = "some-gem"
  ruby-class = "SomeGem::Client"
  ruby-method = "fetch"
  mochi-signature = "(url: String) -> Result<String, Error>"
  ```

**Residual**: gems with zero type information are usable but require user-authored signatures for every method. This is acceptable for niche gems; widely-used gems are expected to gain RBS coverage over time.

## R2: RBS schema instability

**Risk**: the `rbs` gem ships a major version that changes `.rbs` file syntax or the programmatic parse API (the `RBS::Parser` interface). The bridge's Go-side `.rbs` parser fails or produces wrong results.

**Likelihood**: low. The RBS format has been stable since rbs 2.0 (2021). A major syntax change is unlikely without a long deprecation period.

**Impact**: high. If the bridge misparses `.rbs` files, type-mapping produces wrong Mochi signatures without a hard error, causing silent miscompilation.

**Mitigation**:
- The bridge pins `rbs-version` in `mochi.lock` and records the SHA256 of the `.rbs` files at lock time.
- `mochi pkg lock --check` re-parses the `.rbs` files and compares the resulting type surface hash; a format change causes a detectable mismatch.
- The bridge ships `.rbs` fixture tests against rbs 2.x and 3.x file formats; a parse regression fails CI before release.
- If rbs 4.x introduces an incompatible syntax, the bridge ships a new parser alongside the old one, gated by `rbs-version`.

**Residual**: between an rbs major release and the bridge update, users on the new rbs gem version get a parse error at lock time. The error message instructs the user to pin the rbs gem version via `[ruby] rbs-gem-version = "3.x"`.

## R3: Native extension gem pre-built availability

**Risk**: the user targets a platform (e.g., musl-linux, arm64-windows) for which the gem publisher has not built a pre-built binary gem. The bridge cannot install the gem without a full native toolchain.

**Likelihood**: medium. Most major gems publish pre-built binaries for `x86_64-linux`, `arm64-darwin`, and `x64-mingw-ucrt`. Niche platforms (musl, RISC-V, s390x) are often missing.

**Impact**: medium. The build fails with a Bundler "Could not find a valid gem" error, which the bridge surfaces as a `NativeExtNotAvailable` error.

**Mitigation**:
- For gems with a known pure-Ruby fallback (e.g., `nokogiri-pure` for nokogiri), the bridge records the fallback in its capability database and offers to use it via `mochi pkg add ruby nokogiri --use-pure-ruby`.
- For source builds, the user can opt in via `[ruby.native-ext] build-from-source = true`, which instructs Bundler to compile the C extension locally.
- If neither is possible, the bridge emits a `SkipReport` for the gem and records `capabilities-native-ext = true` with a `build-status = "unavailable"` note.

**Residual**: source builds require a C toolchain and potentially gem-specific system libraries (libxml2 for nokogiri, libpq for pg). The user is responsible for providing these on unsupported platforms.

## R4: Bundler version drift

**Risk**: Bundler changes the behavior of `bundle install --deployment --frozen` between minor versions, causing the bridge's orchestration to fail or produce different results.

**Likelihood**: low. Bundler's core install behavior has been stable since Bundler 2.x. Breaking changes are rare between minor versions.

**Impact**: medium. A behavior change could break CI builds silently (e.g., gems installed to a different path, `--frozen` no longer refusing updates).

**Mitigation**:
- The bridge pins `bundler-version` in the `[ruby]` table of `mochi.lock`:
  ```toml
  [ruby]
  bundler-version = "2.5.6"
  ```
  The build invocation ensures this exact Bundler version is used via `bundle _2.5.6_ install ...`.
- CI tests against Bundler 2.4, 2.5, and 2.6 to catch behavioral regressions before they reach users.
- If the pinned Bundler version is not installed, the bridge emits an installation hint: `gem install bundler:2.5.6`.

**Residual**: users on corporate environments that restrict gem installs may not be able to install the pinned Bundler version. The bridge accepts any Bundler >= 2.4 with a deprecation warning if the pinned version is unavailable.

## R5: RubyGems.org OIDC API change

**Risk**: rubygems.org changes the OIDC trusted publishing API (the endpoint URL, the JWT claim requirements, or the token exchange protocol), breaking `mochi pkg publish --to=rubygems.org`.

**Likelihood**: low. The rubygems.org trusted publishing API has been stable since its launch in late 2023 and follows the same OIDC patterns as PyPI and crates.io.

**Impact**: high. A breaking change would prevent Mochi packages from being published to rubygems.org until the bridge is updated.

**Mitigation**:
- The bridge wraps `gem push` (the official rubygems CLI) rather than calling the raw HTTP API. This means rubygems.org API changes are absorbed by the `gem` CLI without requiring a bridge update.
- The `--allow-key-fallback` flag enables legacy API key publish as an escape hatch during a transition period.
- The bridge monitors the rubygems.org changelog and ships updates within 48 hours of a breaking API change.

**Residual**: the window between an API change and a bridge update requires the `--allow-key-fallback` escape hatch.

## R6: GVL assumption

**Risk**: gem shim code generated by the bridge assumes CRuby's GVL serialises concurrent gem calls. On TruffleRuby (which has no GVL), two Mochi agent threads calling the same gem method simultaneously may produce data races.

**Likelihood**: medium. TruffleRuby is an explicit MEP-56 target; users who opt into TruffleRuby and use concurrent agents will exercise this path.

**Impact**: medium. Data races in gem code produce non-deterministic results, which may be hard to diagnose.

**Mitigation**:
- The bridge's capability database flags gems with known GVL-dependent state (`capabilities-gvl-dependent = true`).
- For flagged gems, the bridge emits a warning comment in the generated shim (see note 10 for the exact comment format).
- MEP-56 phase 26 already runs TruffleRuby CI; MEP-76 extends the CI matrix to include gem shim tests on TruffleRuby.

**Residual**: the warning is informational. The bridge does not inject locks around gem calls; doing so would be too coarse and would serialize all gem access, eliminating any concurrency benefit.

## R7: Ruby version fragility

**Risk**: a gem declares `required_ruby_version >= 3.2` in its gemspec but the user's environment has Ruby 3.1. Bundler refuses to install the gem.

**Likelihood**: low. Most gems support a wide range of Ruby versions. New gems targeting Ruby 3.2+ features are an exception.

**Impact**: low. The error is caught at lock time (not silently at runtime) and the message is clear.

**Mitigation**:
- At lock time, the bridge checks the `required_ruby_version` field from the compact index against `[ruby] ruby-version` in `mochi.toml`:
  ```
  LockError: nokogiri 1.17.0 requires Ruby >= 3.2.0 but mochi.toml declares ruby-version = "3.1.4".
  Hint: upgrade to Ruby 3.2 or pin to nokogiri ~> 1.16.
  ```
- The bridge selects the highest gem version whose `required_ruby_version` is satisfied by the user's declared Ruby version.

**Residual**: if the user has not declared `ruby-version` in `mochi.toml`, the bridge assumes the system Ruby and may select a version the user cannot actually run on a different target.

## Rejected alternatives

### YARD-only (no RBS)

YARD type annotations are freeform strings (`@param foo [String, Integer]`) with no normative schema. Using YARD as the sole type source was rejected because:

- YARD types are not machine-readable in a reliable way; parsers disagree on edge cases.
- Many gems annotate complex return types as freeform prose ("returns a Hash of symbols to values") rather than structured type expressions.
- Empirical testing against the 20-gem fixture corpus showed a SkipReport rate of 60-70% with YARD-only vs. 10-20% with RBS as the primary source.
- RBS is the Ruby 3.x standard; YARD is legacy. Investing primarily in YARD would require maintaining a custom parser against a moving target.

YARD remains as a third-tier fallback for gems with no RBS coverage.

### Runtime introspection (require + ObjectSpace)

The bridge could load each gem into a Ruby process at lock time and use `ObjectSpace` and `Module#instance_methods` to enumerate the API surface.

Why rejected:

- Requires running Ruby at lock time. On CI environments without a Ruby installation, this blocks lock.
- Gem loading has side effects (`at_exit` hooks, background threads, file writes). Running gems at lock time is unsafe for gems with aggressive `require`-time side effects.
- The introspection surface is the runtime shape (what methods exist), not the type shape (what types they accept). Without type information, every method becomes `SkipReport`.
- Platform-dependent behavior: some gems behave differently at require-time on different platforms.

Runtime introspection is rejected in favor of static sources (RBS, YARD).

### API key publish path

The bridge could use a long-lived RubyGems API key (stored in CI secrets) for `gem push`.

Why rejected: same supply-chain rationale as MEP-73 and MEP-57. Long-lived registry tokens are the primary attack vector in package registry compromises (xz-utils backdoor, event-stream). OIDC trusted publishing ties the publish identity to the CI workflow, not to a stored secret.

### Ractor-default for gem calls

Making Ractor the default isolation mechanism for gem method calls was considered as a way to provide true parallelism.

Why rejected:

- Fewer than 5% of the top-100 gems are Ractor-safe (May 2026 survey). Forcing Ractor use would raise `Ractor::IsolationError` for 95% of gem calls silently at runtime.
- The Ractor isolation requirement (deeply frozen objects at boundaries) is incompatible with the mutable objects most gems return (Hash, Array, custom classes).
- Ractor can be a future opt-in (phase 13) for users who curate a Ractor-safe gem set; it cannot be the default.

### GVL assumption (no warning)

Emitting no warning for GVL-dependent gems on TruffleRuby targets was considered as a simpler approach.

Why rejected: silent data races are harder to diagnose than explicit warnings. The warning comment has negligible code-generation cost and directly helps TruffleRuby users.

### Skip all native extension gems

Refusing to bridge any gem with a native C extension was considered as a way to avoid the pre-built binary problem entirely.

Why rejected:

- nokogiri (HTML/XML parsing) and pg (PostgreSQL client) are among the top-10 most-downloaded gems. Excluding them would make the bridge useless for a large fraction of real-world Ruby workloads.
- Pre-built binary gems solve the installation problem for 95% of users on supported platforms (x86_64-linux, arm64-darwin, x64-mingw-ucrt).
- The bridge already tracks `capabilities-native-ext = true` in `mochi.lock`, giving users full visibility.

Native extension gems are supported with pre-built binary preference; source builds and pure-Ruby fallbacks are available for other platforms.

### Delegate all resolution to Bundler subprocess

Using Bundler for all resolution (not just conflict fallback) was considered as a way to avoid implementing a Go-side resolver.

Why rejected:

- Bundler requires a Ruby runtime on the user's machine at lock time. Developers who have not installed Ruby cannot lock.
- Starting a Bundler subprocess for every lock adds ~2-5 seconds of Ruby startup overhead even for trivial gem graphs.
- The Go compact-index resolver handles 95% of real-world gem graphs (those without complex cross-gem conflicts) without needing Ruby.
- The two-tier strategy gets the best of both: fast lock for simple cases, correct lock for complex cases.

## Cross-references

- [09. Bundler and mochi.lock](09-bundler-lockfile.md) for the dual-file strategy and `--check` mode (R2 mitigation).
- [10. Ractor and Fiber concurrency](10-ractor-concurrency.md) for the GVL and Ractor discussion (R6).
- [11. Version resolution](11-version-resolution.md) for the two-tier resolver (rejected alternative: delegate all to Bundler).
- [MEP-76 spec](/docs/mep/mep-0076) for the normative risk register.
- [MEP-73 research note 12](/docs/research/0073/12-risks-and-alternatives) for the analogous Rust bridge risks.
