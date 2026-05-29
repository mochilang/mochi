---
title: "09. Bundler and mochi.lock integration"
sidebar_position: 10
sidebar_label: "09. Bundler and mochi.lock"
description: "Gemfile.lock format, the mochi.lock [[ruby-package]] table schema, the --check mode, version conflict resolution, and how the bridge orchestrates bundle install without a Gemfile in the user's source tree."
---

# 09. Bundler and mochi.lock integration

This note documents how the MEP-76 bridge coordinates with Bundler, the canonical Ruby dependency manager. The bridge generates a synthetic Gemfile at build time (not committed to the user's source tree), runs `bundle install`, and records the result in `mochi.lock`. The dual-file strategy (Gemfile.lock + mochi.lock) prevents silent re-resolution on subsequent builds.

## Gemfile.lock format

Bundler's own lockfile (`Gemfile.lock`) has four top-level sections:

```
GEM
  remote: https://rubygems.org/
  specs:
    nokogiri (1.16.2-x86_64-linux)
      racc (~> 1.4)
    racc (1.8.1)
  platforms:
    x86_64-linux
  dependencies:
    nokogiri (~> 1.16)
  BUNDLED WITH
    2.5.6
```

- **GEM / remote**: the gem source(s) used for resolution.
- **GEM / specs**: the complete resolved graph with pinned versions and their own dependency lines.
- **PLATFORMS**: the Bundler platform strings for which the resolution is valid.
- **DEPENDENCIES**: the top-level constraints declared in the Gemfile (not transitive).
- **BUNDLED WITH**: the Bundler version that performed the resolution.

The bridge generates a synthetic `Gemfile` at `<workdir>/.mochi-ruby/Gemfile` at build time. This file is never committed to the user's repository. Its content is derived purely from the `[ruby-dependencies]` table in `mochi.toml`. The resulting `Gemfile.lock` is written to `<workdir>/.mochi-ruby/Gemfile.lock` and is also not committed; its SHA256 is recorded in `mochi.lock` (see below).

## mochi.lock `[[ruby-package]]` table schema

Each resolved gem is recorded as a `[[ruby-package]]` array entry in `mochi.lock`:

```toml
[[ruby-package]]
name = "nokogiri"
version = "1.16.2"
platform = "x86_64-linux"
gem-sha256 = "abc123..."
gem-blake3 = "def456..."
rbs-source = "bundled"        # "bundled" | "gem_rbs_collection" | "yard" | "none"
rbs-version = "3"             # RBS schema version
rbs-sha256 = "789abc..."      # SHA256 of the .rbs files used
capabilities-net = false
capabilities-fs = false
capabilities-native-ext = true
```

Field semantics:

| Field | Type | Description |
|-------|------|-------------|
| `name` | string | Gem name as it appears on RubyGems |
| `version` | string | Exact resolved version (no operators) |
| `platform` | string | Bundler platform string; `"ruby"` for pure-Ruby gems |
| `gem-sha256` | string | SHA256 of the `.gem` tarball fetched from the remote |
| `gem-blake3` | string | BLAKE3 of the same tarball for fast local verification |
| `rbs-source` | enum | Where the RBS type information was obtained |
| `rbs-version` | string | RBS schema version used when the types were ingested |
| `rbs-sha256` | string | SHA256 of the `.rbs` files used for type ingest; empty string if `rbs-source = "none"` |
| `capabilities-net` | bool | True if static scan found `Net::HTTP`, `TCPSocket`, or equivalent |
| `capabilities-fs` | bool | True if static scan found `File.open`, `Dir.glob`, or equivalent |
| `capabilities-native-ext` | bool | True if the gem tarball contains a `ext/` directory with a `extconf.rb` |

The `rbs-source` values follow the same priority as the ingest pipeline (notes 02-04): `bundled` > `gem_rbs_collection` > `yard` > `none`.

## `--check` mode

`mochi pkg lock --check` is the CI gate. It:

1. Recomputes the SHA256 for each gem tarball recorded in `mochi.lock` by re-fetching (or re-reading from the local Bundler cache) and comparing against `gem-sha256`.
2. Recomputes the SHA256 for each RBS set (by re-walking the `.rbs` files from the gem or the collection) and comparing against `rbs-sha256`.
3. Recomputes the SHA256 of `<workdir>/.mochi-ruby/Gemfile.lock` and compares against the `gemfile-lock-sha256` entry in the `[ruby]` table of `mochi.lock`.
4. Exits with status 0 if all checks pass; exits non-zero with a per-package diff on any mismatch.

Example CI job step:

```yaml
- name: Verify mochi.lock integrity
  run: mochi pkg lock --check
```

A mismatch output looks like:

```
MISMATCH nokogiri 1.16.2 gem-sha256
  expected: abc123...
  actual:   def789...
Lock is stale or tampered. Run `mochi pkg lock` to refresh.
```

## Bundle install orchestration

The bridge orchestrates `bundle install` as follows:

1. **Gemfile synthesis**: the bridge writes `<workdir>/.mochi-ruby/Gemfile` with a `source "https://rubygems.org"` line and one `gem "<name>", "<constraint>"` line per entry in `[ruby-dependencies]`.

2. **Environment setup**: the bridge sets:
   - `BUNDLE_GEMFILE=<workdir>/.mochi-ruby/Gemfile`
   - `BUNDLE_PATH=<workdir>/.mochi-ruby/vendor/bundle`

3. **Install invocation**:
   ```sh
   bundle install --deployment --frozen --gemfile=<workdir>/.mochi-ruby/Gemfile
   ```
   The `--deployment` flag instructs Bundler to install gems under the `vendor/bundle/` path relative to the Gemfile location. The `--frozen` flag prevents Bundler from updating the lockfile during install (the lockfile must already be consistent).

4. **MEP-56 build handoff**: after install, the bridge sets `BUNDLE_GEMFILE` and `BUNDLE_PATH` in the environment before invoking the MEP-56 Ruby build pipeline so that the emitted Ruby code finds the correct gem load paths.

The `vendor/bundle/` tree is written to `<workdir>/.mochi-ruby/` and is ephemeral (rebuild-able from `mochi.lock`). It is not committed to the user's repository.

## Version conflict resolution

The bridge delegates conflict resolution to Bundler. The flow is:

1. The bridge runs `bundle lock --gemfile=<workdir>/.mochi-ruby/Gemfile` (without `--frozen`) to let Bundler attempt resolution.
2. If Bundler resolves successfully, the result is written to `Gemfile.lock` and then translated into `mochi.lock` entries.
3. If Bundler detects a conflict, it writes a conflict report to stderr. The bridge parses the report and surfaces it as a `LockConflict` error:

```
LockConflict: cannot resolve gem dependencies
  nokogiri ~> 1.15 (required by your mochi.toml)
  nokogiri >= 1.16 (required by grpc 1.63.0)
Hint: run `mochi pkg add ruby nokogiri@~>1.16` to relax the constraint.
```

The user resolves via `mochi pkg add ruby <gem>@<constraint>` with an explicit constraint. The bridge does not silently pick a resolution; all constraint changes are user-initiated.

## Gemfile.lock + mochi.lock dual-file strategy

The two files serve different purposes:

| File | Owner | Committed? | Purpose |
|------|-------|------------|---------|
| `Gemfile.lock` | Generated at `<workdir>/.mochi-ruby/` | No | Bundler's resolution record; input to `bundle install --frozen` |
| `mochi.lock` | Repo root | Yes | Mochi's content-addressed snapshot: gem SHA256, BLAKE3, RBS SHA256, capabilities |

The bridge records the SHA256 of the generated `Gemfile.lock` in the `[ruby]` table of `mochi.lock`:

```toml
[ruby]
bundler-version = "2.5.6"
ruby-version = "3.2.4"
gemfile-lock-sha256 = "aaabbb..."
```

On the next build, if the `gemfile-lock-sha256` matches the freshly generated `Gemfile.lock`, the bridge skips `bundle install` and uses the cached `vendor/bundle/`. If the SHA256 does not match (because `mochi.toml` constraints changed), the bridge re-runs `bundle lock` and `bundle install`.

This prevents Bundler from silently re-resolving: the lock SHA must match or the build fails with an actionable error.

## Cross-references

- [08. Native C extension gems](08-native-extensions.md) for how native extension gems interact with the `vendor/bundle/` layout.
- [11. Version resolution](11-version-resolution.md) for the two-tier resolver (Go compact-index + Bundler PubGrub).
- [MEP-57](/docs/mep/mep-0057) for the `mochi.lock` format that `[[ruby-package]]` extends.
- [MEP-56](/docs/mep/mep-0056) for the Ruby transpiler whose build pipeline the bridge injects into.
- [Implementation tracking phase 9](/docs/implementation/0076/phase-09-lockfile) for the delivery status of this feature.
