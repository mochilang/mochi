---
title: Phase 7 — build orchestration
sidebar_position: 8
sidebar_label: "Phase 7. build orchestration"
description: "Pipeline orchestrator wiring sparse-index → rustdoc ingest → typemap → wrapper synth → emit into an on-disk cargo workspace, plus the Cargo runner that builds it."
---

# Phase 7 — build orchestration

Phase 7 is the integration phase. The previous six phases each lower one slice of the bridge: phase 1 fetches sources, phase 2 ingests rustdoc JSON, phase 3 walks the type table, phase 4 synthesises the wrapper crate, phase 5 emits the Mochi shim files, phase 6 teaches the parser to recognise `import rust`. Phase 7 plugs all six together: a `Pipeline` takes a list of `ImportRef`s, runs each through the synthesis chain, and writes a populated cargo workspace under the driver's work-dir, ready for `cargo build`. A new `Cargo` runner constructs deterministic argv + env for the actual cargo invocation.

## Gate

- `go test ./package3/rust/build/...` is green.
- `TestPhase07Orchestration` (per-phase sentinel) drives a hex-like fixture surface through the pipeline end-to-end and asserts the on-disk workspace layout matches MEP-73 spec §6, plus pins the `cargo build` argv + env shape against a portable no-op binary (no rust toolchain needed on the host).

## Files landed

### Bridge code

- `package3/rust/build/pipeline.go`
  - `ImportRef{Crate, Version, Alias}` — one resolved `import rust` statement.
  - `ImportRefError` — structural rejection (empty crate, empty version).
  - `SurfaceProvider` + `SurfaceProviderFunc` — interface that resolves a `(crate, version)` to a phase-2 `rustdoc.ApiSurface`. Production code wires this to the rustdoc-JSON ingest; tests inject canned fixtures.
  - `Pipeline{Driver, Provider}` — top-level orchestrator.
  - `Pipeline.Resolve(refs)` — pure (no fs) lowering: each ref runs through `wrapper.Synth` + `emit.Emit`, the wrapper registers as a `MemberWrapper` member, and the upstream crate appears under `[workspace.dependencies]` pinned to `="<version>"`. Duplicate refs collapse on the `<crate>@<version>` key. Output is sorted by member path so byte-identical dep sets produce byte-identical results regardless of input order.
  - `Pipeline.MaterialiseWorkspace(result)` — projects a `PipelineResult` onto disk under `<workdir>/rust_workspace/` per MEP-73 §6 layout (see below). Idempotent.
- `package3/rust/build/cargo.go`
  - `Cargo{Bin, Verbose, Deterministic, Offline, Frozen, Locked, CargoHome, ExtraEnv, Stdout, Stderr}` — invocation surface.
  - `BuildOptions{WorkspaceRoot, Profile, Package, Target, Jobs}` — per-call configuration.
  - `Cargo.ArgsBuild(opts)` — pure argv composer: `[cargo, build, --verbose?, --offline?, --frozen?, --locked?, --profile <p>, --package?, --target?, --jobs?, --manifest-path <root>/Cargo.toml]`.
  - `Cargo.Env(base)` — applies deterministic-build keys (`SOURCE_DATE_EPOCH=0`, `CARGO_TERM_COLOR=never`, `RUSTC_BOOTSTRAP=0`) plus `CARGO_HOME` plus `ExtraEnv` on top of `base`, sorted alphabetically for byte-stable testability.
  - `Cargo.Build(ctx, opts)` — runs cargo with `exec.CommandContext`; respects context cancellation.
  - `LookCargo()` — `exec.LookPath("cargo")` with a useful diagnostic when missing.

### Tests

- `package3/rust/build/pipeline_test.go` — 22 cases covering Resolve happy path, member + shared-dep registration, dedup, deterministic ordering across input permutations, validation errors (empty crate, empty version, nil provider, nil receiver, nil surface, provider error bubble), MaterialiseWorkspace file presence + contents + idempotence, path-segment sanitiser, ImportRefError formatting, SurfaceProviderFunc dispatch.
- `package3/rust/build/cargo_test.go` — 18 cases pinning ArgsBuild defaults + every flag, Env sorted output + override semantics + malformed-entry skip, Build refusal of empty WorkspaceRoot, Build via `/usr/bin/true` succeeding (no cargo on PATH required), Build under a cancelled context failing, LookCargo coherence, captureBuf helper.
- `package3/rust/build/phase07_test.go` — sentinel `TestPhase07Orchestration` with sub-tests `end_to_end` (workspace materialises with all 7 expected paths + workspace Cargo.toml pins), `argv_shape` (`cargo build --verbose --offline --locked --profile release --manifest-path /tmp/ws/Cargo.toml`), `env_shape` (deterministic + CARGO_HOME + ExtraEnv sorted), `members_sorted` (three crates in mixed order produce alphabetical member listing).

## Workspace layout produced

For `import rust "hex@0.4.3" as hex`, `Pipeline.MaterialiseWorkspace` writes:

```
<workdir>/rust_workspace/
  Cargo.toml                    # workspace root: members + [workspace.dependencies] + profiles
  .gitignore                    # target/
  rust_wrap/
    hex/
      Cargo.toml                # [package] mochi_wrap_hex, crate-type=cdylib+rlib, dep hex = "=0.4.3"
      src/
        lib.rs                  # #[no_mangle] pub unsafe extern "C" fn mochi_hex_encode(...), etc.
      SKIPPED.txt               # phase-2 + phase-4 skip reports
  mochi/
    hex_extern.mochi            # extern type / extern fun declarations
    hex.mochi                   # alias re-exports (fun encode, fun decode, fun to_upper_hex)
```

The user's own MEP-53 emit attaches `mochi_user` as the workspace member at index 0; the bridge does not touch that path. The wrapper crate's `[dependencies] hex = "=0.4.3"` and the workspace root's `[workspace.dependencies] hex = "=0.4.3"` produce identical pins so cargo's dep resolver never has to make a choice.

## Cargo invocation

`Cargo` is the runner the driver uses to actually compile. The split between `ArgsBuild` (pure) and `Build` (impure) is intentional: argv composition is the testable surface, since it pins the contract between the bridge and cargo. The `Build` method is mostly plumbing; the per-host integration test that runs an actual `cargo build` lives in MEP-53's cross-compile gate, not here.

Canonical release invocation:

```
cargo build \
  --offline --locked \
  --profile release \
  --manifest-path <workdir>/rust_workspace/Cargo.toml
```

With environment:

```
CARGO_HOME=<cache>/cargo
CARGO_TERM_COLOR=never
RUSTC_BOOTSTRAP=0
SOURCE_DATE_EPOCH=0
```

The `--offline` flag is the load-bearing piece: phase 1 already pre-fetched every transitive source into the content-addressed cache, so any network access during build is a cache-invalidation bug that should fail loudly.

## Target matrix

| target                          | status | notes |
|---------------------------------|--------|-------|
| host stable rust 1.95 (darwin-arm64) | LANDED | full pipeline test passes; cargo invocation tested via `/usr/bin/true` |
| musl-x64 / musl-arm64           | LANDED | argv + workspace layout are host-agnostic; per-target `--target` flag wired |
| wasm32-wasip1                   | n/a    | cargo workspace path is required; the wasm target lives downstream of phase 9 |

Phase 7's gate is portable: zero phase-7 tests require cargo or a Rust toolchain on the host. The integration with the real Rust toolchain happens at MEP-53's cross-compile gate, which is allowed to skip on hosts that lack rust.

## Status

| date | event |
|------|-------|
| 2026-05-29 22:54 (GMT+7) | Phase 7 worktree created at `/Users/apple/mochi-mep73-p7` off `origin/main`. |
| 2026-05-29 23:00 (GMT+7) | `pipeline.go` + `cargo.go` + 3 test files added; `go test ./package3/rust/build/...` green. |
| 2026-05-29 23:04 (GMT+7) | Phase 7 tracking page committed; ready for auto-ship. |

## Cross-references

- [MEP-73 spec §6 Build orchestration](/docs/mep/mep-0073#build-orchestration) for the canonical workspace layout.
- [Phase 4 wrapper synthesiser](/docs/implementation/0073/phase-04-wrapper) for the wrapper crate Phase 7 wires into the workspace.
- [Phase 5 extern emitter](/docs/implementation/0073/phase-05-extern-emit) for the Mochi-side shim files Phase 7 materialises under `mochi/`.
- [Phase 6 import grammar](/docs/implementation/0073/phase-06-import-grammar) for the parser that produces the `ImportRef`s Phase 7 consumes.
- [Phase 8 mochi.lock integration](/docs/implementation/0073/phase-08-lockfile) (next): records each Pipeline.Resolve output as a `[[rust-package]]` entry with the wrapper SHA-256.
