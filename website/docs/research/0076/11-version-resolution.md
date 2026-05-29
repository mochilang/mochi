---
title: "11. Version resolution"
sidebar_position: 12
sidebar_label: "11. Version resolution"
description: "The RubyGems version spec operators (~>, >=, =, >, <), the compact index format parsed by the bridge, version conflict resolution strategy, and how mochi.toml version constraints map to mochi.lock pinned versions."
---

# 11. Version resolution

This note documents how the MEP-76 bridge translates user-supplied version constraints in `mochi.toml` into pinned gem versions in `mochi.lock`. The bridge implements a two-tier resolution strategy: a Go-side compact-index parser for 95% of cases, falling back to a Bundler subprocess for complex conflicts.

## RubyGems version operators

RubyGems supports five version operators:

| Operator | Example | Semantics |
|----------|---------|-----------|
| `~>` (pessimistic) | `~> 1.2` | `>= 1.2, < 2.0` (patch wildcard) |
| `~>` (pessimistic patch) | `~> 1.2.3` | `>= 1.2.3, < 1.3.0` (micro wildcard) |
| `>=` | `>= 1.0` | At least 1.0; no upper bound |
| `=` | `= 1.0.0` | Exactly 1.0.0 |
| `>` | `> 1.0` | Strictly greater than 1.0 |
| `<` | `< 2.0` | Strictly less than 2.0 |

The pessimistic operator (`~>`) is the most common in gemspecs because it allows safe patch and minor upgrades while preventing major-version breaking changes.

The bridge stores constraints in `mochi.toml` using the same operator syntax:

```toml
[ruby-dependencies]
nokogiri = "~> 1.16"
grpc = ">= 1.60, < 2.0"
```

Multiple constraints on a single gem are comma-separated within the string value (matching the RubyGems DSL convention).

## Compact index format

The bridge fetches gem metadata from the RubyGems compact index at:

```
https://index.rubygems.org/info/<gem-name>
```

Each line in the response describes one gem version:

```
1.16.2 |checksum:sha256:<hex>|deps:racc:~> 1.4,mini_portile2:~> 2.8.2
1.16.2-x86_64-linux |checksum:sha256:<hex>|deps:racc:~> 1.4
1.16.1 |checksum:sha256:<hex>|deps:racc:~> 1.4,mini_portile2:~> 2.8.2
```

Fields (pipe-separated):

| Field | Description |
|-------|-------------|
| `<version>` | Gem version string; may include platform suffix |
| `checksum:sha256:<hex>` | SHA256 of the `.gem` file (matches `gem-sha256` in `mochi.lock`) |
| `deps:<name>:<constraint>,...` | Comma-separated runtime dependency list |

The bridge parses the compact index in Go at lock time. No Ruby runtime or RubyGems CLI is required. The parsed data feeds the two-tier resolver (see below).

## Pre-release versions

RubyGems pre-release versions use string suffixes appended after the numeric parts:

- `1.0.0.pre`
- `2.0.0.alpha`
- `3.0.0.rc1`
- `1.0.0.beta.2`

Pre-release versions are excluded from constraint matching by default. A constraint of `~> 1.0` will not select `1.1.0.rc1`. The user must explicitly request a pre-release constraint to include them:

```toml
[ruby-dependencies]
my-gem = ">= 1.1.0.rc1"
```

The bridge's Go resolver treats any version with a non-numeric suffix component as a pre-release and filters it out of candidate sets unless the constraint itself references a pre-release version.

## Platform suffix

Many gems ship platform-specific pre-built variants in addition to the pure-Ruby (source) version. The compact index lists these as separate version entries:

```
1.16.2 |checksum:sha256:<hex>|...       # pure-Ruby / source build
1.16.2-x86_64-linux |checksum:sha256:<hex>|...
1.16.2-arm64-darwin |checksum:sha256:<hex>|...
1.16.2-x64-mingw-ucrt |checksum:sha256:<hex>|...
```

The bridge selects the platform variant matching `RUBY_PLATFORM` from the build environment. The platform string is normalised to Bundler's platform vocabulary (`x86_64-linux`, `arm64-darwin`, `x64-mingw-ucrt`, `java`).

If no platform-specific variant is available for the current platform, the bridge falls back to the `ruby` (pure-Ruby / source) variant. The fallback is recorded in `mochi.lock` with `platform = "ruby"`.

## Two-tier resolution strategy

### Tier 1: Go compact-index resolver

The bridge implements a greedy resolver in Go:

1. Fetch the compact-index entry for each gem in `[ruby-dependencies]`.
2. For each gem, collect all versions that satisfy the user's constraint, sorted highest-first.
3. Pick the highest satisfying version.
4. Recursively add transitive dependencies, applying the same rule.
5. On conflict (two constraints select disjoint sets for the same gem), fall through to Tier 2.

The greedy resolver handles 95% of real-world gem graphs without running Ruby.

### Tier 2: Bundler PubGrub subprocess

If Tier 1 encounters a conflict, the bridge falls back to running:

```sh
bundle lock --gemfile=<workdir>/.mochi-ruby/Gemfile
```

Bundler uses a PubGrub-based solver (since Bundler 2.2) that backtracks correctly over complex version constraints. The bridge captures the resulting `Gemfile.lock`, parses it, and translates it into `[[ruby-package]]` entries.

The two-tier approach means the user's machine needs a Ruby + Bundler installation only if the Go resolver encounters a conflict. The Go resolver is always tried first.

## mochi.toml to mochi.lock flow

```
mochi.toml [ruby-dependencies]
        |
        v
  Bridge reads constraints
        |
        v
  Compact-index fetch (Go)
        |
        v
  Greedy resolve (Go Tier 1)
        |  conflict?
        +--------> bundle lock (Bundler Tier 2)
        |
        v
  Pinned versions + SHA256
        |
        v
  mochi.lock [[ruby-package]] entries written
```

On a subsequent `mochi pkg lock`, the bridge re-resolves using the constraints in `mochi.toml` but keeps the pinned version from `mochi.lock` if it still satisfies the constraint. This avoids unexpected upgrades on lock refresh. To force an upgrade, the user runs `mochi pkg lock --upgrade`.

## Constraint storage in mochi.toml vs mochi.lock

| File | Content | Example |
|------|---------|---------|
| `mochi.toml` | User-authored constraint | `nokogiri = "~> 1.16"` |
| `mochi.lock` | Pinned exact version + SHA256 | `version = "1.16.2"`, `gem-sha256 = "abc..."` |

The constraint in `mochi.toml` is what the user manages. The pinned version in `mochi.lock` is what the build uses. This matches the `Gemfile` / `Gemfile.lock` model but extends it with content-addressed hashes and RBS metadata.

## Cross-references

- [09. Bundler and mochi.lock](09-bundler-lockfile.md) for the full `[[ruby-package]]` schema and `--check` mode.
- [01. Language surface](/docs/research/0076/01-language-surface) for the `[ruby-dependencies]` mochi.toml syntax.
- [12. Risks and alternatives](12-risks-and-alternatives.md) for the rejected "delegate all resolution to Bundler subprocess" alternative.
- [MEP-57](/docs/mep/mep-0057) for the polyglot package system whose `mochi.lock` format this extends.
- [Implementation tracking phase 1](/docs/implementation/0076/phase-01-compact-index) for the compact-index client delivery status.
