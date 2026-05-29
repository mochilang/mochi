---
title: "Phase 4. extern-C wrapper synthesiser"
sidebar_position: 6
sidebar_label: "Phase 4. extern-C wrapper synth"
description: "MEP-73 Phase 4 lands the package3/rust/wrapper/ pipeline: given an ApiSurface and the typemap closed table, Synth produces a Crate carrying src/lib.rs (extern-C signatures + scalar/string marshalling), Cargo.toml (cdylib + rlib + pinned upstream dep), and SKIPPED.txt (grouped, sorted SkipReports)."
---

# Phase 4. extern-C wrapper synthesiser

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-73 §Phases](/docs/mep/mep-0073#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 21:30 (GMT+7) |
| Landed         | 2026-05-29 21:39 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`package3/rust/wrapper.Synth(upstream, version, *rustdoc.ApiSurface) *Crate` lowers a Phase 2 ApiSurface into a Crate. For each function in the surface, the parameter types and return type are run through Phase 3's `typemap.Map` (`Direction=In` for params, `Direction=Out` for the return). Successful mappings produce a `SynthFn`; failures produce a `SkipReport` whose Detail prefixes which parameter (or return) refused, so the user sees `"param 1 (data): dyn Trait has no Mochi surface in v1"` rather than a generic refusal.

The Crate carries three files that later phases consume:

- `src/lib.rs`: the static runtime prologue (`#[repr(C)]` definitions of `MochiString`, `MochiSlice`, `MochiOption`, `MochiResult`, `MochiTuple`, `MochiMap`, `MochiHandle`, plus `mochi_string_to_str` / `mochi_string_from_owned` helpers and an `extern "C" mochi_string_free` deallocator), followed by one `#[no_mangle] pub unsafe extern "C" fn <mangled>(...)` per `SynthFn`.
- `Cargo.toml`: a Cargo package declaring `crate-type = ["cdylib", "rlib"]` and a pinned `=<version>` dependency on the upstream crate. `publish = false` keeps it out of crates.io.
- `SKIPPED.txt`: SkipReports grouped by `Reason`, sorted by `ItemPath` within each group, rendered via `SkipReport.String`. Empty surfaces render `"# No skipped items.\n"`.

The 16 test functions across `crate_test.go` (7) and `emit_test.go` (9) pin the synth pipeline and the rendered output against a hex-like ApiSurface fixture. Full `go test ./package3/rust/...` is green across all 7 packages (errors, build, semver, sparse, rustdoc, typemap, wrapper).

## Body-emission rules

For each `SynthFn` the emitter decides between a *concrete* and a *deferred* body:

- A body is **concrete** when every parameter is a scalar or a `KindString`, and the return is `None` or scalar or `KindString`. In that case `EmitLibRS` emits:
  - one `let <name>: &str = unsafe { mochi_string_to_str(<name>) };` binding per string input,
  - a direct upstream call `let __ret = <UpstreamPath>(<args>);` (or, if the return is unit, just the call statement),
  - a return marshalling line: scalar returns are passed through, `String` returns flow through `mochi_string_from_owned` which leaks the `String` and emits a `MochiString` (deallocated by the caller via `mochi_string_free`).
- A body is **deferred** when any parameter or the return is a compound (list / bytes / map / option / result / tuple / struct / enum / handle). The body becomes `todo!()` preceded by `// TODO: marshal compound input/output (phase 4.1).`. The signature is still emitted with the correct `FFIRepr` rendering so `cbindgen` can produce a header, and the Mochi-side emitter (Phase 5) sees the symbol. Filling in the bodies is the gate for the Phase 4.1 sub-phase.

In the hex-like fixture this means:

- `hex::to_upper_hex(value: u8) -> char` lowers to a concrete body: `mochi_hex_to_upper_hex(value: uint8_t) -> uint32_t { hex::to_upper_hex(value) }`.
- `hex::encode(data: &[u8]) -> String` keeps its signature (`(data: MochiSlice) -> MochiString`) but the body is `todo!()` because `MochiSlice -> &[u8]` marshalling is a Phase 4.1 deliverable.
- `hex::decode(input: &str) -> Result<Vec<u8>, FromHexError>` keeps its signature (`(input: MochiString) -> MochiResult`) but the body is `todo!()` for the same reason on the return side.

## Mangling

The wrapper crate's name is `mochi_wrap_<sanitized_upstream>`. Hyphens become underscores, upper-case becomes lower-case, anything else becomes `_`. Examples:

| Upstream | Crate name |
|----------|------------|
| `hex` | `mochi_wrap_hex` |
| `once_cell` | `mochi_wrap_once_cell` |
| `some-Crate` | `mochi_wrap_some_crate` |
| `Foo123` | `mochi_wrap_foo123` |

Each `SynthFn.ExternName` is `mochi_<sanitized_upstream>_<sanitized_path_tail...>`. For `once_cell::sync::Lazy::new` the result is `mochi_once_cell_sync_lazy_new`. The first path segment is the crate name itself; mangling re-prepends `mochi_<crate>` rather than duplicating the leading crate segment.

Anonymous parameters (`name == ""` in rustdoc) become `arg0`, `arg1`, ... so emitted Rust source compiles.

## Determinism

Both `EmitLibRS` and `EmitSkippedTXT` are deterministic. Tests run them five times each and assert byte-equality. `EmitSkippedTXT` sorts groups by `Reason.String()` and entries within each group by `ItemPath`, so the output stays stable across rustdoc walker non-determinism (though Phase 2 already documents a deterministic walker order).

## Files changed

| File | Purpose |
|------|---------|
| `package3/rust/wrapper/crate.go` | `Crate`, `SynthFn`, `SynthParam`, `Synth`, name mangling helpers |
| `package3/rust/wrapper/emit.go` | `EmitLibRS`, `EmitCargoTOML`, `EmitSkippedTXT`, body emission rules |
| `package3/rust/wrapper/runtime.go` | the static FFI prologue (`MochiString` etc., `mochi_string_to_str`, `mochi_string_from_owned`, `mochi_string_free`) |
| `package3/rust/wrapper/crate_test.go` | counts, walker-skip propagation, fn-failure skip, return-skip detail prefix, anon-param substitution, crate/extern name mangling |
| `package3/rust/wrapper/emit_test.go` | prologue presence, scalar body concreteness, deferred-body `todo!()` selection, Cargo.toml fields, SKIPPED.txt grouping + sorting + determinism |
| `website/docs/implementation/0073/phase-04-wrapper.md` | this page |

## Test set

- All `package3/rust/wrapper/...` unit tests (16 functions).
- Full `go test ./package3/rust/...` regression across 7 packages.

## Closeout notes

Phase 4 introduces no external dependencies (Rust or Go side). The wrapper crate's Cargo.toml pins the upstream version with `=<ver>` so the synthesised tree matches the rustdoc-JSON the surface was produced from; the lock-file integration in Phase 8 inherits the exact-pin semantics.

Phase 4.1 (sub-phase) fills in the deferred bodies: `MochiSlice -> &[u8]` and `MochiSlice -> Vec<T>` marshalling, `MochiOption` / `MochiResult` / `MochiTuple` construction and deconstruction, and `MochiHandle` lifetime hooks. The split is justified by the umbrella-phase coverage rule: today's gate (every fn produces a parseable Rust signature) is green for every target, but the run-time wrap of compound types needs a separate test surface.

Phase 5 (Mochi-side extern fn emitter) consumes the SynthFn list to emit `extern fn` declarations on the Mochi side; the same `FFIRepr` and `MochiType` renderings flow through unchanged.
