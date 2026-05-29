---
sidebar_label: "Phase 11: Async bridge"
sidebar_position: 12
---

# MEP-73 Phase 11: Async bridge (tokio runtime singleton + block_on)

**Status:** LANDED (2026-05-30)
**Spec section:** [MEP-73 §3 — Async bridge](/docs/mep/mep-0073)
**Worktree:** `/Users/apple/mochi-mep73-p11`

## Gate

Land the async-fn bridge: when an upstream crate exposes
`async fn` items, the synthesised wrapper crate stops generating
"todo!()" placeholder bodies and instead routes each async call
through a process-wide tokio runtime constructed lazily on first use,
via `mochi_rt::block_on(async { upstream(...).await })`.

## Why it matters

Phases 4-5 produced wrapper bodies that called sync Rust fns
directly. Async fns were left as `todo!()` because the bridge had no
runtime to execute them. Most modern crates (tokio, reqwest, sqlx,
axum, hyper, serde_json's async variants) expose `async fn` as a
substantial part of their surface. Without Phase 11, importing any of
those crates produces a wrapper that compiles but panics at runtime.

Phase 11 closes that gap. The wrapper synthesiser detects
`Header.IsAsync` from rustdoc JSON, marks the SynthFn as async, and at
emit time:

1. Prepends `pub mod mochi_rt;` to `src/lib.rs`.
2. Renders a `src/mochi_rt.rs` module declaring a `OnceCell<Runtime>`
   plus a `get_rt()` accessor and an inline `block_on` helper.
3. Emits each async wrapper body as `mochi_rt::block_on(async {
   upstream(args).await })` instead of the direct call.
4. Adds `tokio = { version = "^1.42", features = [...] }` and
   `once_cell = "^1.20"` to the wrapper's Cargo.toml.

When the crate's surface has zero async fns, none of (1)-(4) fire and
the wrapper is byte-identical to the pre-Phase-11 output. Pure-sync
imports pay zero compile-time and zero runtime cost.

## What landed

### `package3/rust/asyncbridge/asyncbridge.go`

The new package owning the deterministic text shape of the async
bridge.

- `Flavor` enum (`FlavorCurrentThread`, `FlavorMultiThread`) with
  `ParseFlavor(s)` from `[rust.runtime] flavor` in `mochi.toml`.
- `MinTokioVersion = "1.42"` floor — the version that ships
  `Builder::new_current_thread` + `new_multi_thread` + the
  stabilised `Runtime::block_on` signature.
- `Flavor.TokioFeatures()` returns the cargo features the wrapper
  Cargo.toml needs (`rt` + `macros` always, `rt-multi-thread`
  additionally for the multi-thread flavor).
- `RuntimeModule(flavor)` returns the verbatim `mochi_rt.rs` body:
  `OnceCell<Runtime>` + `get_rt()` + `block_on` helper. The body
  matches the spec text at MEP-73 §3 (Async bridge).
- `FnBody(upstreamPath, args)` renders `mochi_rt::block_on(async {
  upstream(args).await })`.
- `CargoDepRow(flavor)` and `OnceCellDepRow()` render the
  `[dependencies]` rows for tokio + once_cell.

### `package3/rust/wrapper/crate.go`

Threaded `IsAsync bool` through `SynthFn`. `Synth(...)` propagates
`fn.Header.IsAsync` from the rustdoc surface; `Crate.HasAsync()`
helper reports whether any function is async. New `Crate.AsyncFlavor
asyncbridge.Flavor` field defaults to current-thread; callers
override from mochi.toml.

### `package3/rust/wrapper/emit.go`

- `EmitLibRS` adds a `pub mod mochi_rt;` line when `HasAsync()`.
- `emitBody` calls `asyncbridge.FnBody(...)` for async wrappers
  instead of building the direct call string.
- New `EmitMochiRT(c)` returns the `src/mochi_rt.rs` body or empty
  string when no async fns are present.
- `EmitCargoTOML` injects the tokio + once_cell dep rows when
  `HasAsync()`. Sync-only crates retain byte-identical output.

### Tests

- `asyncbridge_test.go` (16 cases): flavor parsing, default flavor,
  feature lists, runtime module shape (OnceCell + Builder + block_on
  +  enable_all), multi-thread flavor flip, FnBody shape for 0, 1,
  multi-arg cases, FnBody rejects empty path, Cargo dep row
  determinism, OnceCellDepRow, MinTokioVersion non-empty.
- `wrapper/async_test.go` (12 cases): Synth propagates IsAsync,
  HasAsync detects + rejects, EmitLibRS wires `pub mod mochi_rt;`
  only on async, EmitLibRS async body uses block_on, EmitLibRS sync
  body does not, EmitMochiRT returns module for async / empty for
  sync, EmitMochiRT honors multi-thread flavor, EmitCargoTOML adds /
  omits tokio + once_cell, multi-thread flavor flips features in
  Cargo.toml, EmitLibRS deterministic.
- `asyncbridge/phase11_test.go` (sentinel) with subtests:
  `runtime_module_uses_oncecell_and_runtime`,
  `current_thread_is_default`,
  `multi_thread_opt_in_flips_features_and_builder`,
  `block_on_body_awaits_upstream`, `dep_pins_min_tokio_version`,
  `byte_stable_outputs`.

## Target matrix

| Target           | Status   | Notes |
|------------------|----------|-------|
| Async detection  | ✅       | `Header.IsAsync` from rustdoc surface drives `SynthFn.IsAsync`. |
| Runtime module   | ✅       | `OnceCell<Runtime>` + `get_rt()` + `block_on` helper. |
| Body wrap        | ✅       | `mochi_rt::block_on(async { upstream(args).await })`. |
| Cargo deps       | ✅       | tokio + once_cell injected only when HasAsync(). |
| Flavor toggle    | ✅       | `current-thread` (default) vs. `multi-thread` opt-in. |
| Sync regression  | ✅       | Pure-sync crates pay zero cost. |
| Byte stability   | ✅       | RuntimeModule + CargoDepRow + FnBody all deterministic. |
| Embedded subset  | ⚠️       | Out of scope for Phase 11. Phase 13 will refuse async-fn-exposing crates under `profile = "embedded"` per spec §3 ("no tokio under no_std"). |

## How this phase plugs in to the larger pipeline

```
  rustdoc.FunctionEntry          wrapper.Synth                    EmitLibRS
  + Header.IsAsync       ───►   SynthFn{IsAsync: true}    ───►   pub mod mochi_rt;
                                                                  ...
                                                                  #[no_mangle]
                                                                  pub unsafe extern "C" fn
                                                                  mochi_<crate>_<fn>(args) {
                                                                    mochi_rt::block_on(
                                                                      async {
                                                                        <crate>::<fn>(args).await
                                                                      })
                                                                  }

                                                                EmitMochiRT
                                                                  static MOCHI_RT:
                                                                    OnceCell<Runtime>
                                                                  pub fn get_rt() -> ...
                                                                  pub fn block_on<F>(...)

                                                                EmitCargoTOML
                                                                  [dependencies]
                                                                  <crate> = "=..."
                                                                  tokio = { version = "^1.42",
                                                                    features = [...] }
                                                                  once_cell = "^1.20"
```

The bridge keeps the wrapper crate sync at the FFI boundary (no
`async extern "C" fn` — they don't exist in Rust), so Mochi's existing
sync-only FFI loader needs no changes. The tokio runtime singleton
lives entirely inside the wrapper crate.

## Timeline

| Time (GMT+7)        | Step |
|---------------------|------|
| 2026-05-30 00:08    | Worktree branch `mep/0073-phase-11` created off `origin/main`. |
| 2026-05-30 00:12    | `package3/rust/asyncbridge/asyncbridge.go` written. |
| 2026-05-30 00:14    | `wrapper/crate.go` + `wrapper/emit.go` threaded with `IsAsync` + tokio dep injection. |
| 2026-05-30 00:16    | Per-package tests + Phase 11 sentinel written. |
| 2026-05-30 00:17    | `go test ./package3/rust/...` green. |
| 2026-05-30 00:18    | Tracking page + spec sync. |
