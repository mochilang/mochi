---
title: "Phase 0. Spec freeze and skeleton trees"
sidebar_position: 2
sidebar_label: "Phase 0. Skeleton"
description: "MEP-46 Phase 0 tracking: spec freeze, transpiler3/beam/ skeleton trees, OTP app stub, implementation tracking pages, sidebar wiring."
---

# Phase 0. Spec freeze and skeleton trees

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-46 §Phases · Phase 0](/docs/mep/mep-0046#phase-0-spec-freeze-and-skeleton-trees) |
| Status         | LANDED 2026-05-26 12:47 (GMT+7) |
| Started        | 2026-05-26 12:38 (GMT+7) |
| Landed         | 2026-05-26 12:47 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

MEP-46 merged on `main`; `transpiler3/beam/{lower,emit,build}/doc.go` files exist and `go vet ./transpiler3/beam/...` is clean; `transpiler3/beam/runtime/src/{mochi_app,mochi_sup,mochi_atoms}.erl` stubs and `mochi.app.src` exist; `rebar3 compile` on the OTP app skeleton exits 0; `tests/transpiler3/beam/README.md` documents fixture layout; implementation tracking pages for every phase exist under `/docs/implementation/0046/`.

## Goal-alignment audit

The user-facing goal of MEP-46 is "ship a Mochi program as an OTP application or standalone escript targeting the BEAM VM". Phase 0 does not move that goal directly. It plants the four structural anchors that make every later phase cheap to open: (1) the Go package tree tells a contributor which stage owns which concern without reading the MEP end-to-end, (2) the OTP app skeleton means every downstream phase can `rebar3 compile` its Erlang runtime pieces against a known module namespace, (3) the fixture README establishes the naming convention so fixture authors don't ask the same orientation questions twice, and (4) the per-phase tracking pages give a contributor a landing page with gate, decisions, and deferred work before the phase even opens. The cost is one PR; without it every later phase repeats this orientation cost inline.

## Sub-phases

| #   | Scope | Status | Commit | PR |
|-----|-------|--------|--------|----|
| 0.0 | MEP-46 merged with §Phases section; implementation tracking pages created under `/docs/implementation/0046/`; Docusaurus sidebar wired | LANDED 2026-05-26 12:47 (GMT+7) | — | — |
| 0.1 | `transpiler3/beam/{lower,emit,build}/doc.go` created; `go vet ./transpiler3/beam/...` clean | LANDED 2026-05-26 12:47 (GMT+7) | — | — |
| 0.2 | `tests/transpiler3/beam/README.md` documents fixture layout and naming convention; `runVm3` test helper defined in `transpiler3/beam/build/build_test.go` | LANDED 2026-05-26 12:47 (GMT+7) | — | — |
| 0.3 | OTP app skeleton: `mochi.app.src`, `mochi_app.erl`, `mochi_sup.erl`, `mochi_atoms.erl`; `rebar3 compile` exits 0 | LANDED 2026-05-26 12:47 (GMT+7) | — | — |

## Sub-phase 0.0

MEP-46 spec merged on `main`. The spec includes the §Phases section that lists every phase, its gate, and the fixture target counts. Implementation tracking pages (the files under `/docs/implementation/0046/`) are created as stubs; each stub links back to the MEP §Phases anchor so a contributor can navigate spec -> tracking page and back without ambiguity. The Docusaurus sidebar entry for MEP-46 is wired so the pages appear in the website's implementation section alongside MEP-45.

The `website/docs/mep/mep-0046.md` file is the normative spec; the tracking pages under `/docs/implementation/0046/` are the mutable rolling status. These are two different documents with two different purposes.

## Sub-phase 0.1

Three Go packages are created under `transpiler3/beam/`:

- `transpiler3/beam/lower/` -- owns the `Lower(prog *aotir.Program) (*cerl.Module, error)` entry point; converts the MEP-45 aotir IR to a tree of `cerl` records.
- `transpiler3/beam/emit/` -- owns `Emit(mod *cerl.Module, workDir string) ([]BeamFile, error)`; serialises a `cerl.Module` to binary Erlang term format and drives `compile:forms/2` via an `erl -noshell` subprocess.
- `transpiler3/beam/build/` -- owns `Driver.Build(src, out string, target Target)`; glues lower -> emit -> escript packaging.

Each package gets a `doc.go` with a one-paragraph package doc that (a) states what the package owns, (b) names the entry-point function, and (c) cross-references the adjacent packages. A `transpiler3/beam/doc.go` at the root carries the pipeline diagram:

```
Mochi source
  -> parser/typechecker (shared with MEP-45)
  -> aotir.Program (MEP-45 IR, reused unchanged)
  -> beam/lower: aotir -> cerl.Module
  -> beam/emit: cerl.Module -> .beam bytes (via erl -noshell compile:forms/2)
  -> beam/build: .beam bytes -> escript or OTP release
```

The `go vet ./transpiler3/beam/...` gate catches import cycles and missing doc comments before any real code lands in these packages.

A `transpiler3/beam/cerl/` package is also created in 0.1. It defines the Go-side `cerl.Module`, `cerl.FunctionDef`, and the expression types that mirror the Core Erlang AST (`c_int`, `c_float`, `c_atom`, `c_var`, `c_let`, `c_case`, `c_call`, `c_cons`, `c_nil`, `c_tuple`, `c_map`, `c_try`, `c_catch`, `c_fun`, `c_values`). These are pure Go structs; no Erlang is invoked in this package. The package also defines `Module.MarshalBinary() ([]byte, error)` which serialises the module to an Erlang external term (ETF) binary that can be passed to `compile:forms/2` via stdin or a temp file.

## Sub-phase 0.2

`tests/transpiler3/beam/README.md` documents:

- Fixture directory layout: `tests/transpiler3/beam/fixtures/phase{N}/{NNN}_{name}.mochi` and `{NNN}_{name}.out`. The three-digit prefix sorts fixtures visually and lets the gate test walker pick them up in a deterministic order without sorting by name.
- How `.out` files are generated: run `vm3 run {NNN}_{name}.mochi > {NNN}_{name}.out`. The `.out` file is the ground truth; the BEAM-compiled binary's stdout must match it byte-for-byte.
- The `runVm3(t *testing.T, src string) []byte` helper in `transpiler3/beam/build/build_test.go`: it finds the `vm3` binary on `PATH`, runs it on the given source file, and returns the captured stdout. Used only in the rare case where a `.out` file needs to be regenerated programmatically during fixture authoring; in CI the `.out` files are committed and `runVm3` is not called.
- The `runBeamFixture(t *testing.T, mochiPath, outPath string)` helper: calls `Driver.Build` on `mochiPath`, runs the resulting escript, captures stdout, and diffs against `outPath`. Shared across all phase gate tests.
- Naming conventions: `001_` through `099_` for Phase 1, `100_` through `199_` for Phase 2 (sub-phases 2.0-2.4 share the range), `200_` through `299_` for Phase 3, `300_` through `399_` for Phase 4.

## Sub-phase 0.3

The OTP app skeleton is the Erlang-side foundation that every later phase links against. It ships in Phase 0 because: (a) `mochi_str.erl` (Phase 1) must belong to a compiled OTP application to be loadable by an escript, (b) `rebar3` needs `mochi.app.src` to know how to build the project, and (c) establishing the module namespace (`mochi_*`) now prevents collisions later.

`mochi.app.src` content:
```erlang
{application, mochi, [
  {description, "Mochi runtime for BEAM"},
  {vsn, "0.1.0"},
  {modules, []},
  {registered, []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {mochi_app, []}}
]}.
```

`mochi_app.erl` implements the `application` behaviour:
```erlang
-module(mochi_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mochi_sup:start_link().

stop(_State) ->
    ok.
```

`mochi_sup.erl` implements a `one_for_one` supervisor with an empty child list. This is intentional: no long-lived processes are needed in Phase 0. Later phases (agents, streams) will add supervised children.

```erlang
-module(mochi_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    {ok, {SupFlags, []}}.
```

`mochi_atoms.erl` is a compile-time atom registry. It declares atoms that are referenced across modules so that beam's atom table is populated at load time:
```erlang
-module(mochi_atoms).
-export([all/0]).

all() ->
    [mochi_record_tag, mochi_error, mochi_break, mochi_continue,
     mochi_err_divzero, mochi_err_index, mochi_err_type].
```

The `rebar3 compile` gate runs on `transpiler3/beam/runtime/` using the project's `rebar.config`. OTP 27 is the minimum version.

## Decisions made

- **OTP app skeleton ships in Phase 0, not Phase 1.** Every BEAM-compiled escript must load its runtime modules from somewhere. Without `mochi.app.src` and `mochi_app.erl`, `Phase 1`'s escript packaging step has nowhere to put `mochi_str.beam`. Moving the skeleton to Phase 1 would mean 1.0 conflates two concerns (pipeline wiring and OTP app setup) in one PR. Separating them keeps Phase 0 cheap and Phase 1 focused on the actual transpiler.
- **`go vet` is the Go-side gate, not `go build`.** The packages in 0.1 have no real implementation yet, just `doc.go` stubs. `go build` would succeed vacuously. `go vet` is the minimal signal that imports, package names, and doc comment structure are correct.
- **`rebar3 compile` is the Erlang-side gate.** It gives exactly the same signal for the OTP skeleton: no implementation, but the module names, behaviours, export lists, and `app.src` are syntactically valid and type-consistent with OTP 27.
- **Three `doc.go` files, not one.** Same rationale as MEP-45 Phase 0: `go doc mochi/transpiler3/beam/lower` is the first page a contributor opens when debugging a lowering bug; it should tell them exactly what the package does and where the entry point is.
- **`cerl/` package uses Go structs, not cgo.** The Core Erlang records are represented as Go structs that serialise to ETF via `encoding/binary`. This avoids a cgo dependency and keeps the Go build hermetic. The Erlang runtime is invoked only at `emit` time via `os/exec`.
- **Module namespace is `mochi_*`.** All runtime modules use this prefix to avoid collisions with user code. The name-mangling spec (`mochi_{pkg}__{mod}__{name}`) applies to transpiled user code; runtime modules use the flat `mochi_*` prefix.

## Files changed

| File | Purpose |
|------|---------|
| `website/docs/mep/mep-0046.md` | Normative spec (merged in 0.0) |
| `website/docs/implementation/0046/index.md` | Phase index (created in 0.0) |
| `website/docs/implementation/0046/phase-{00..19}.md` | Per-phase tracking stubs (created in 0.0) |
| `transpiler3/beam/doc.go` | Pipeline diagram (created in 0.1) |
| `transpiler3/beam/lower/doc.go` | Package doc for the lowerer (created in 0.1) |
| `transpiler3/beam/emit/doc.go` | Package doc for the emitter (created in 0.1) |
| `transpiler3/beam/build/doc.go` | Package doc for the build driver (created in 0.1) |
| `transpiler3/beam/cerl/cerl.go` | Go-side Core Erlang AST types + ETF serialiser (created in 0.1) |
| `tests/transpiler3/beam/README.md` | Fixture layout and naming convention (created in 0.2) |
| `transpiler3/beam/build/build_test.go` | `runVm3` and `runBeamFixture` helpers (created in 0.2) |
| `transpiler3/beam/runtime/src/mochi_app.erl` | OTP application callback (created in 0.3) |
| `transpiler3/beam/runtime/src/mochi_sup.erl` | One-for-one supervisor skeleton (created in 0.3) |
| `transpiler3/beam/runtime/src/mochi_atoms.erl` | Compile-time atom registry (created in 0.3) |
| `transpiler3/beam/runtime/src/mochi.app.src` | OTP application resource file (created in 0.3) |
| `transpiler3/beam/runtime/rebar.config` | rebar3 project config (created in 0.3) |

## Test set

- `go vet ./transpiler3/beam/...` -- no test functions, but this is the 0.1 gate.
- `rebar3 compile` in `transpiler3/beam/runtime/` -- no Erlang unit tests yet; this is the 0.3 gate.
- `transpiler3/beam/cerl/cerl_test.go::TestCerlETFRoundTrip` -- 6 cases verifying that small `cerl.Module` trees serialise to valid ETF that `erl -noshell` can decode with `binary_to_term/1`.

## Deferred work

- The `cerl/` package's ETF serialiser handles only the Core Erlang subset needed through Phase 4. Tuples with arity > 8, bit strings, and compressed terms are not supported; they are added as needed in later phases.
- `mochi_sup.erl`'s child list is empty through Phase 8. Supervised workers for streams and agents land in Phase 9.
- `rebar3 dialyzer` is not part of the Phase 0 gate; it requires PLT construction which is slow and belongs in Phase 17 (Dialyzer clean pass).
- OTP release packaging (`relx` config) is Phase 15; Phase 0 only ships the OTP app, not a packaged release.

## Closeout notes

Phase 0 COMPLETE 2026-05-26 12:47 (GMT+7). All four sub-phases landed in one PR.

Sub-phases 0.0-0.3 green:

- `go vet ./transpiler3/beam/...` exits 0 with no diagnostics.
- `TestCerlETFRoundTrip` (6 cases) passes; all six module trees serialise to valid ETF and binary_to_term/1 decodes them without error under OTP 28.
- `rebar3 compile` on `transpiler3/beam/runtime/` exits 0; three modules compile (mochi_app, mochi_sup, mochi_atoms).
- `tests/transpiler3/beam/README.md` and `build_test.go` helpers are in place for Phase 1.

One deviation from the spec: the `rebar.config` removes the `{parse_transform, lager_transform}` opt because lager is not a listed dependency. The erl_opts entry is now just `[debug_info]`, which is correct for an OTP 27+ project.
