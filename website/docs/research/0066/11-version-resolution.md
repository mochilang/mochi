---
title: "11. Version resolution"
sidebar_position: 12
sidebar_label: "11. Version resolution"
description: "Hex.pm's version constraint operators (~>, >=, <, and), the Hex.pm HTTP API v2 compact index format, pre-release handling, the bridge's two-tier resolution strategy (Go resolver + rebar3 fallback), and Elixir-compat package selection."
---

# 11. Version resolution

## Hex.pm version constraint syntax

Hex.pm uses a subset of semantic versioning with a small set of operators:

| Operator | Meaning | Example |
|----------|---------|---------|
| `~> X.Y` | `>= X.Y and < X+1.0` (if Y is the minor version) | `~> 2.12` means `>= 2.12.0 and < 3.0.0` |
| `~> X.Y.Z` | `>= X.Y.Z and < X.Y+1.0` (patch-level pessimistic) | `~> 2.12.0` means `>= 2.12.0 and < 2.13.0` |
| `>= X.Y.Z` | Greater than or equal | `>= 1.2.0` |
| `> X.Y.Z` | Strictly greater than | `> 1.2.0` |
| `<= X.Y.Z` | Less than or equal | `<= 2.0.0` |
| `< X.Y.Z` | Strictly less than | `< 3.0.0` |
| `== X.Y.Z` | Exact match | `== 2.12.0` |
| `!= X.Y.Z` | Exclusion | `!= 1.99.0` |
| `and` | Logical AND (compound) | `>= 1.0.0 and < 2.0.0` |
| `or` | Logical OR (compound) | `>= 1.5.0 or == 1.3.2` |

The bridge's Go-side constraint parser (`package3/erlang/hexsemver/`) implements this grammar. The parser is separate from MEP-73's cargo-semver parser because Hex.pm's constraint language differs from Cargo's in several ways: Hex.pm uses `and`/`or` keywords (not comma and `|`), the `~>` operator's semantics depend on whether the version has two or three components, and Hex.pm allows `or` between ranges.

## Hex.pm HTTP API v2 index format

The Hex.pm package index is available via two protocols:

**Full package list**: `GET https://hex.pm/api/packages?page=N` returns a JSON array of package summaries. Iterating all pages is slow (18,000+ packages as of 2026). The bridge does not use this endpoint for resolution.

**Per-package metadata**: `GET https://hex.pm/api/packages/<name>` returns a JSON object with:
```json
{
  "name": "cowboy",
  "releases": [
    {
      "version": "2.12.0",
      "inserted_at": "2024-01-15T12:00:00Z",
      "requirements": {
        "cowlib": {"requirement": "~> 2.13", "optional": false, "app": "cowlib"},
        "ranch": {"requirement": "~> 2.1", "optional": false, "app": "ranch"}
      },
      "checksum": "9f8e7d...",  // SHA-512 of contents.tar.gz
      "meta": {
        "links": {"GitHub": "https://github.com/ninenines/cowboy"},
        "licenses": ["ISC"],
        "description": "Small, fast, modern HTTP server for Erlang/OTP."
      }
    }
  ]
}
```

The bridge queries this endpoint for each package in `[erlang-dependencies]` and its transitive dependencies, building a dependency graph.

**Compact index** (experimental, analogous to crates.io sparse index): Hex.pm provides a compressed binary index at `https://hex.pm/registry.ets.gz` (the `registry.ets` format, a DETS/ETS table encoded as Erlang terms). The bridge uses the per-package JSON API for resolution (no bulk download needed), not the ETS registry.

## Two-tier resolution

The bridge uses a two-tier strategy:

**Tier 1: Go-side resolver** (`package3/erlang/resolver/`). The bridge implements a simple greedy resolver in Go that:
1. Starts with the user-declared `[erlang-dependencies]`.
2. For each package, fetches its version list from the Hex.pm API and selects the newest version satisfying the constraint.
3. Recursively adds transitive dependencies.
4. Checks for version conflicts (two packages require incompatible versions of a third).

For most `[erlang-dependencies]` sets, the greedy resolver produces a valid solution in O(N) API calls.

**Tier 2: rebar3 fallback**. When the Go resolver detects a conflict or encounters an `override` directive in `[erlang-dependencies]`, it falls back to generating a candidate `rebar.config`, running `rebar3 upgrade` or `rebar3 lock`, and reading the resulting `rebar3.lock` file. The rebar3 lock is then imported back into `mochi.lock`. This tier requires rebar3 to be installed but handles complex dependency graphs correctly.

## Override directive

Erlang dependency graphs occasionally have version conflicts where two packages require incompatible versions of a shared library. rebar3 resolves this via `override`: one package version wins. The `[erlang-dependencies]` `override = true` field maps to rebar3's `{overrides, [{override, Name, Config}]}` mechanism. The Go resolver recognises `override = true` entries and immediately triggers the rebar3 fallback tier rather than attempting to resolve them itself.

## Pre-release handling

Hex.pm pre-release versions use a `-rcN`, `-betaN`, `-alphaN` suffix convention. The bridge excludes pre-release versions by default. A `[erlang].allow-prerelease = true` flag in `mochi.toml` opts in.

## Elixir-compat packages

When `[erlang].elixir-compat = true`, the bridge also resolves Elixir packages from Hex.pm. An Elixir package is considered Erlang-compatible if:

1. Its compiled BEAM files contain Erlang-style `-spec` annotations (many Elixir libraries add `-spec` directives to their `.beam` files for Dialyzer compatibility).
2. Its public API does not use Elixir `__struct__` or `Protocol` dispatch (which require the Elixir runtime).

The bridge adds a `SkipReport: elixir_runtime_required` entry for any Elixir module function that calls `Elixir.Kernel` or any module whose name begins with `Elixir.`.

## Cross-references

- [[09-rebar3-lockfile]] for how the resolved versions are recorded.
- [[06-hex-publish-flow]] for the Hex.pm API used at publish time.
- [[12-risks-and-alternatives]] §R3 for the risk of rebar3 version fragility in the fallback tier.
