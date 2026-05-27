---
title: "Phase 19. v1.0 release"
sidebar_position: 21
sidebar_label: "Phase 19. v1.0 release"
description: "MEP-46 Phase 19. v1.0 release: user docs, changelog, Hex.pm publish, MEP-46 Final status."
---

# Phase 19. v1.0 release

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-46 §Phases · Phase 19. v1.0 release](/docs/mep/mep-0046#phase-19-v10-release) |
| Status         | LANDED |
| Started        | 2026-05-27 (GMT+7) |
| Landed         | 2026-05-27 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`website/docs/manual/build-beam.mdx` is published and accurate, `CHANGELOG.md` has a `[0.12.0]`
entry, `mochi` Hex package `0.1.0` is published to Hex.pm, and `mep-0046.md` status is `Final`.

## Goal-alignment audit

Phase 19 closes the MEP-46 BEAM pipeline. The gate has four components, each serving a distinct
user-facing purpose:

**User documentation (19.0):** Without documentation, users cannot discover or correctly use the
BEAM targets. The `build-beam.mdx` page is the canonical user entry point for the Mochi BEAM
backend. It must ship simultaneously with the implementation; documentation that lags the feature
is documentation that does not exist in practice. The no-caveats rule ensures the page does not
describe unfinished work.

**Changelog (19.1):** The `CHANGELOG.md` entry gives users upgrading from v0.11 a clear,
structured summary of what the BEAM backend provides. It also serves as the basis for the GitHub
release notes and the Hex.pm release description.

**Hex.pm publication (19.2):** Publishing the `mochi` OTP runtime to Hex.pm makes it a first-class
Erlang/Elixir dependency. Erlang teams who want to call Mochi-compiled agents or streams from their
own Erlang code can add `{mochi, "0.1.0"}` to their `rebar.config` and depend on it like any other
Hex package. Without Hex publication, the runtime is only available via path reference, which is
not usable for third-party adoption.

**MEP Final (19.3):** Flipping the MEP to Final marks the specification as normative and complete.
It signals to contributors that the design is stable and that changes to the BEAM pipeline should
go through a new MEP or MEP amendment, not informal code changes.

## Sub-phases

### 19.0 `website/docs/manual/build-beam.mdx`

User-facing documentation page. Published on the Mochi website alongside `build.mdx` (the C/AOT
backend docs from MEP-45 Phase 19.0).

**Page structure:**

```
# Building for BEAM/OTP

## Quick start
  (two code blocks: escript hello-world, release packaging)

## Tier-1 platform support
  (table: platform x arch x OTP versions x JIT x status)

## Build targets reference
  ### --target=beam-escript
  ### --target=beam-release
  ### --target=beam-rebar3-project
  ### --target=beam-mix-project
  ### --target=beam-atomvm

## OTP version requirements
  (minimum OTP 27.0; tested on 27.0, 27.latest, 28.latest)

## Packaging (escript vs release vs project export)
  (when to use each target; size comparison table)

## Erlang FFI
  (syntax, calling conventions, type mapping)

## Agents (gen_server)
  (agent.new, agent.call, agent.cast; lifetime, supervision)

## Streams (pg process groups)
  (stream.pub, stream.sub, stream.map, stream.filter; cross-node)

## Async/await
  (async {} blocks, await, recv-marker optimization)

## LLM functions
  (mochi.llm.query, cassette mode, --cassette flag)

## HTTP fetch (gun)
  (fetch, fetch_json, error handling, TLS)

## AtomVM profile
  (--profile=atomvm, .avm output, supported OTP subset)

## Reproducible builds
  (CInf stripping, SOURCE_DATE_EPOCH, SHA-256 verification)

## CLI reference
  (full flag table for mochi build --target=beam-*)
```

**No-caveats rule:** The page describes only shipped, working features. If a feature is not yet
implemented (e.g., `mix release` in the mix target), it does not appear on this page. The AtomVM
section appears because Phase 15.3 shipped the AtomVM profile. The reproducible builds section
appears because Phase 18 shipped CInf stripping.

**CLI help sync:** `mochi build --help` and `mochi build --target=beam-* --help` output must match
the CLI reference section of the docs page. This is tested by `TestPhase19DocsCliSync` which
extracts flag names and descriptions from the CLI reference section of the MDX source and asserts
they match `mochi build --help` output line-by-line.

**Platform table:**

| Platform | Architecture | OTP versions | JIT | Status |
|----------|-------------|--------------|-----|--------|
| Linux | x86_64 | 27.0, 27.x, 28.x | BeamAsm | Tier 1 |
| macOS | ARM64 (Apple silicon) | 27.0, 27.x, 28.x | BeamAsm | Tier 1 |
| Linux | ARM64 | 27.x | BeamAsm | Tier 2 (nightly) |
| Windows | x86_64 | 27.x | BeamAsm | Tier 2 (nightly) |
| AtomVM | ESP32/STM32/Pico | AtomVM 0.6+ | None | Profile target |

**Files changed:**

- `website/docs/manual/build-beam.mdx` (new)
- `transpiler3/beam/build/phase19_test.go` (new): `TestPhase19DocsCliSync`.

### 19.1 Release notes and changelog entry

`CHANGELOG.md` gets a `[0.12.0]` entry immediately below the `[Unreleased]` header.

**Entry structure (abbreviated):**

```markdown
## [0.12.0] - 2026-MM-DD

### Added: BEAM/OTP backend (MEP-46)

- `mochi build --target=beam-escript`: compile Mochi programs to self-contained OTP escripts.
- `mochi build --target=beam-release`: OTP release tarball via relx (includes ERTS, ~30-80 MB).
- `mochi build --target=beam-rebar3-project`: emit a complete rebar3 project with readable .erl source.
- `mochi build --target=beam-mix-project`: emit a Mix project for Elixir tooling.
- `mochi build --target=beam-atomvm`: compile for AtomVM microcontrollers (.avm output).
- OTP 27.0 minimum; OTP 27.x and 28.x tested in blocking CI matrix (x86_64-linux, aarch64-darwin).
- Agents: `agent<S>` lowers to gen_server processes; agent.new, agent.call, agent.cast.
- Streams: `stream<T>` lowers to pg process groups; pub/sub, stream.map, stream.filter, cross-node.
- Async/await: `async {}` blocks lower to spawn_monitor with recv-marker optimization.
- Erlang FFI: `@erlang_module("module") fun f(x: T): R` calls native Erlang from Mochi.
- LLM functions: cassette mode for testing; --cassette flag for reproducible LLM tests.
- HTTP fetch: fetch(url) and fetch_json(url) via the gun HTTP client.
- Dialyzer-clean output: -spec attributes emitted from Mochi types; CI gate with rebar3 dialyzer -Werror.
- Reproducible .beam output: CInf timestamp stripped, functions sorted; SHA-256 reproducibility CI.
- mochi OTP runtime published as mochi 0.1.0 on Hex.pm.
```

The changelog entry is comprehensive because users upgrading from v0.11 have not seen any BEAM
backend features yet. Subsequent BEAM releases use incremental changelogs.

The GitHub release notes for `v0.12.0` are generated from the `[0.12.0]` changelog section by
`.github/workflows/release.yml` (existing release workflow, already reads from `CHANGELOG.md`).

**Files changed:**

- `CHANGELOG.md`: add `[0.12.0]` section.

### 19.2 `mochi` runtime published to Hex.pm

The `transpiler3/beam/runtime/` directory is packaged as the `mochi` Hex package. It contains the
OTP runtime modules that every Mochi-compiled BEAM program depends on at runtime.

**Package layout:**

```
transpiler3/beam/runtime/
├── mix.exs                  (Hex package metadata + Mix project definition)
├── rebar.config             (rebar3 dep compatibility)
├── src/
│   ├── mochi.app.src        (OTP application descriptor)
│   ├── mochi_agent.erl      (gen_server agent runtime)
│   ├── mochi_stream.erl     (pg stream runtime)
│   ├── mochi_async.erl      (spawn_monitor async runtime)
│   ├── mochi_llm.erl        (LLM + cassette runtime)
│   └── mochi_fetch.erl      (gun HTTP client wrapper)
└── test/                    (runtime unit tests)
```

**`mix.exs` for Hex publishing:**

```elixir
defmodule Mochi.MixProject do
  use Mix.Project
  def project, do: [
    app: :mochi,
    version: "0.1.0",
    description: "OTP runtime library for Mochi-compiled BEAM programs",
    package: package(),
    deps: [{:gun, "~> 2.0"}]
  ]
  defp package, do: [
    licenses: ["Apache-2.0"],
    links: %{"GitHub" => "https://github.com/mochilang/mochi",
             "Docs" => "https://mochilang.dev/docs/manual/build-beam"},
    files: ~w(src rebar.config mix.exs LICENSE README.md)
  ]
end
```

**`rebar.config` for rebar3 users:**

```erlang
{deps, [{gun, "2.0.1"}]}.
{erl_opts, [debug_info]}.
{hex, [{doc, ex_doc}]}.
```

**Publication CI step** (in `.github/workflows/hex-publish.yml`):

```yaml
name: hex-publish
on:
  push:
    tags: ["v*.*.*"]
jobs:
  publish:
    runs-on: ubuntu-latest
    steps:
      - uses: erlef/setup-beam@v1
        with:
          otp-version: "27"
          elixir-version: "1.17"
      - run: mix hex.publish --yes
        working-directory: transpiler3/beam/runtime
        env:
          HEX_API_KEY: ${{ secrets.HEX_API_KEY }}
```

The `HEX_API_KEY` secret is the Hex.pm API key for the `mochilang` organization account,
configured in the GitHub repository secrets by a maintainer.

The `gun` 2.0 dependency is declared in both `mix.exs` and `rebar.config`. `TestPhase19HexManifest`
verifies both files declare the same version to prevent skew.

**Files changed:**

- `transpiler3/beam/runtime/mix.exs` (new)
- `transpiler3/beam/runtime/rebar.config`: add Hex metadata fields (`{hex, [...]}` block).
- `.github/workflows/hex-publish.yml` (new)
- `transpiler3/beam/build/phase19_test.go`: `TestPhase19HexManifest`.

### 19.3 MEP-46 status flipped to Final

`website/docs/mep/mep-0046.md` receives two changes:

**1. Status line update:**

```markdown
| Status | Final 2026-MM-DD HH:MM (GMT+7) |
```

(Timestamp from `date` at the time of the PR merge commit.)

**2. Closeout block** appended at the end of the MEP:

```markdown
## Closeout

Landed: 2026-MM-DD HH:MM (GMT+7)

All Phase 0-19 gates green. Shipped in Mochi v0.12.0.

What shipped:
- Phases 0-8: Core Erlang lowering, escript, primitives, collections, records, sums,
  closures, query, Datalog.
- Phase 9: Agents (gen_server).
- Phase 10: Streams (pg process groups).
- Phase 11: Async/await (spawn_monitor, recv-marker optimization).
- Phase 12: Erlang FFI.
- Phase 13: LLM functions (cassette mode).
- Phase 14: HTTP fetch (gun).
- Phase 15: Release packaging (escript, OTP release via relx, rebar3 project, mix project,
  AtomVM profile, Docker base images).
- Phase 16: OTP 27/28 x linux/macos CI matrix (6 cells, blocking).
- Phase 17: Dialyzer-clean output (-spec emission, opaque runtime types, CI gate).
- Phase 18: Reproducible .beam files (CInf stripping, function sort); benchmark gate (3x of C).
- Phase 19: User docs (build-beam.mdx), changelog (v0.12.0), Hex.pm (mochi 0.1.0).

Deferred post-v1.0: relup/appup hot reload, AtomVM hardware-in-the-loop CI,
cross-compilation, mix release, EDoc/ExDoc, ARM Linux Tier-1, Windows Tier-1,
OTP 29 Tier-1, build cache integration, NIF numeric optimization.
```

**Implementation tracking index** (`website/docs/implementation/0046/index.md`): Phase 15-19
rows in the phase status table are updated from `NOT STARTED` to `LANDED`.

**Files changed:**

- `website/docs/mep/mep-0046.md`: Status -> Final, closeout block.
- `website/docs/implementation/0046/index.md`: Phase 15-19 status -> LANDED.

## Decisions made

**Why `build-beam.mdx` not `build-beam.md`.** All other Docusaurus manual pages in this project
use `.mdx` for MDX support (JSX-in-Markdown, which enables interactive code examples and imported
React components). Consistency with `build.mdx` (MEP-45 Phase 19.0) means the BEAM docs page can
use the same `<CodeBlock>`, `<Tabs>`, and `<PlatformTable>` components used elsewhere in the
manual. A `.md` file cannot use JSX, which would make the BEAM docs page look and feel different
from the rest of the manual.

**Why publish to Hex.pm at all.** The `mochi` OTP runtime library is useful independently of the
Mochi build tool. Erlang and Elixir developers who want to call Mochi-compiled agents, streams, or
query functions from their own Erlang code need the runtime as a dep. Publishing to Hex.pm makes it
a first-class community dependency with versioned releases, a changelog, and discoverability via
`hex.pm/packages/mochi`. Without Hex publication, users must add the runtime via a git path
reference, which is not practical for production dependencies and signals that the library is not a
stable artifact.

**Why `[0.12.0]` for the BEAM backend release.** The current release is `v0.11.1`. The BEAM
backend is a major new feature. Following semver, a new feature warrants a minor version bump:
`0.11.1 -> 0.12.0`. The BEAM backend does not break existing Mochi programs (the C AOT backend is
unchanged), so a major version bump is not warranted. A patch bump (`0.11.2`) would undersell the
scope of the change.

**Why Final and not Accepted.** The MEP lifecycle for this project is `Draft -> Final`. There is
no `Accepted` or `Active` state. A MEP moves to Final when the implementation is complete and all
phase gates are green. At that point the MEP becomes normative documentation of a shipped feature,
not a proposal. Marking it Accepted when the implementation is already shipped would be inaccurate.

**Why include all Phase 0-19 highlights in the changelog, not just Phase 15-19.** Users upgrading
from v0.11 have not seen any BEAM backend features yet (Phases 0-14 were not shipped in v0.11
releases; they were behind the in-progress MEP-46 flag). The changelog for the first BEAM release
should be comprehensive: it is the primary communication channel for what the BEAM backend can do.
A changelog that says only "Phase 15-19 landed" is not useful to a user who has never seen Phase
0-14. Subsequent BEAM patch releases use incremental changelogs.

## Files changed

| File | Change |
|------|--------|
| `website/docs/manual/build-beam.mdx` | New: user-facing BEAM backend documentation |
| `CHANGELOG.md` | Add `[0.12.0]` section with full BEAM backend feature list |
| `transpiler3/beam/runtime/mix.exs` | New: Hex.pm package metadata + Mix project definition |
| `transpiler3/beam/runtime/rebar.config` | Add `{hex, [...]}` metadata block |
| `.github/workflows/hex-publish.yml` | New: `mix hex.publish` on `v*.*.*` tags |
| `website/docs/mep/mep-0046.md` | Status -> Final, closeout block added |
| `website/docs/implementation/0046/index.md` | Phase 15-19 rows updated to LANDED |
| `transpiler3/beam/build/phase19_test.go` | New: `TestPhase19DocsCliSync`, `TestPhase19HexManifest`, `TestPhase19MEPFinal`, `TestPhase19ChangelogEntry` |

## Test set

- `TestPhase19DocsCliSync`: extracts CLI flag entries from the `## CLI reference` section of
  `build-beam.mdx` via regex, runs `mochi build --target=beam-escript --help` and other target
  help commands, and asserts the documented flags match the actual CLI output. Mismatched or
  missing flags are a test failure.
- `TestPhase19HexManifest`: reads `transpiler3/beam/runtime/mix.exs` and `rebar.config`, verifies
  both declare the same package version (`0.1.0`) and the same `gun` dependency version, preventing
  version skew between the two manifests.
- `TestPhase19MEPFinal`: reads `website/docs/mep/mep-0046.md`, asserts `Status: Final` is present
  with a timestamp, and asserts the `## Closeout` section is present with a `Landed:` line.
  Prevents the MEP from being marked Final without the closeout block.
- `TestPhase19ChangelogEntry`: reads `CHANGELOG.md`, asserts a `[0.12.0]` section is present and
  contains at least the key feature keywords: `beam-escript`, `beam-release`, `gen_server`, `pg`,
  `Dialyzer`, `Hex.pm`, `AtomVM`. A changelog entry missing key features is a test failure.

## Deferred work

- Interactive code examples in `build-beam.mdx` using the Mochi playground widget (embedded WASM
  compilation in the browser). Deferred; the playground is not yet available for the BEAM target.
- `mix release` support in the `--target=beam-mix-project` target: currently only `mix compile` is
  supported. `mix release` requires generating a `rel/` directory and `config/runtime.exs`.
  Deferred post-v1.0.
- ExDoc HTML documentation uploaded to Hex.pm alongside the package: requires EDoc comments in the
  runtime `.erl` files. Deferred until the EDoc deferred item from Phase 17 is addressed.
- OTP 29 promotion to Tier-1 in the platform table: update `build-beam.mdx` when OTP 29 final is
  released and the nightly has been clean for two weeks. No MEP amendment needed; this is a
  documentation update.
- Windows Tier-1 promotion in the platform table: same criteria as OTP 29 promotion.
- `mochi` on other package registries (npm for WASM/WebAssembly BEAM, crates.io): no WASM BEAM
  target is planned for v1.0. Deferred indefinitely.

## Closeout notes

All sub-phases landed in the final batch commit `f088b884be`. Sub-phase 19.0 (`docs/manual/build-beam.mdx`) landed as `e13b3a1643`. Sub-phase 19.1 (release notes for v0.13.0) landed as `2526c7c5f7`. Sub-phases 19.2 (Hex.pm Erlang runtime publish) and 19.3 (MEP-46 Final status) landed as `f088b884be`. The Mochi OTP runtime is published to Hex.pm as `mochi 0.1.0` and `mep-0046.md` status is Final.
