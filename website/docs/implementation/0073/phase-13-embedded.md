---
sidebar_label: "Phase 13: Embedded / no_std subset"
sidebar_position: 14
---

# MEP-73 Phase 13: Embedded / no_std subset (`profile = "embedded"` + alloc opt-in)

**Status:** LANDED (2026-05-30)
**Spec section:** [MEP-73 §3 — Embedded subset](/docs/mep/mep-0073)
**Worktree:** `/Users/apple/mochi-mep73-p13`

## Gate

Land the embedded build profile: when the user sets `[rust] profile =
"embedded"` in `mochi.toml`, the wrapper crate the bridge synthesises
compiles for bare-metal targets (`thumbv7em-none-eabihf` and
friends). The wrapper carries `#![no_std]` + `extern crate alloc;`,
pins the upstream Cargo dep to `default-features = false`, and
refuses every `async fn` at synth time (tokio requires std).

## Why it matters

Phases 0-12 produced wrappers that always linked against `std`.
Embedded Rust users (firmware authors, RTOS integrators, anyone
shipping to thumbv7em / riscv32imc / similar bare-metal targets)
cannot include those wrappers because `std` is unavailable on those
triples. Bringing Mochi to embedded Rust without Phase 13 would
require hand-editing every generated `src/lib.rs` after the bridge
runs.

Phase 13 closes that gap. The wrapper synth grows a profile field,
the emit prepends the no_std header, the Cargo.toml row flips
`default-features = false` so the upstream cannot leak `std` in
silently, and async fns get a clear up-front `SkipEmbedded` refusal
instead of a confusing link-time tokio missing-`std` error.

Pure-hosted builds pay zero cost: `LibRSHeader(ProfileHosted)` is
empty, the Cargo row falls back to the plain `name = "=version"` form,
and async fns continue to route through Phase 11's tokio runtime
singleton. The embedded profile is purely additive.

## What landed

### `package3/rust/embedded/embedded.go`

The new package owning the deterministic text shape of the embedded
profile and the closed parser for the `[rust] profile = "..."` row.

- `Profile` enum (`ProfileHosted`, `ProfileEmbedded`) with `String()`
  and `ParseProfile(s)` round-tripping through the manifest spelling.
- `ParseTOMLBody(body)` parses the `profile = "..."` row from a
  table fragment. Accepts blank lines + `#` comments; rejects
  unquoted values, unknown keys, and trailing junk after the closing
  quote.
- `LibRSHeader(profile)` returns the `src/lib.rs` prologue.
  ProfileHosted returns the empty string; ProfileEmbedded returns
  `#![no_std]\nextern crate alloc;\n` (terminating newline so callers
  concatenate cleanly).
- `CargoUpstreamDepRow(profile, name, version)` renders the upstream
  dep row. ProfileEmbedded emits the inline-table form with
  `default-features = false`; ProfileHosted emits the existing
  `name = "=version"` shape unchanged.
- `RefuseAsync(profile)` reports whether async fns must be skipped
  at synth time (true iff ProfileEmbedded).
- `AsyncRefusalReason` is the canonical detail string so test
  fixtures and downstream tooling can match it byte-stable.
- `AllowedTriples(profile)` lists the embedded target triples the
  bridge expects to compile against (sorted; thumbv6m / thumbv7em /
  thumbv7m / thumbv8m / riscv32imc / riscv32imac).

### `package3/rust/errors/errors.go`

New `SkipEmbedded` reason. Renders as `SkipEmbedded` in
`SKIPPED.txt` so it sorts and groups predictably alongside the
existing reasons.

### `package3/rust/wrapper/crate.go`

- New `Crate.Profile embedded.Profile` field. Defaults to
  ProfileHosted (the pre-Phase-13 behaviour).
- `SynthWithProfile(upstream, version, surface, profile)` extends
  `Synth` with the profile. Plumbs the profile and runs the
  async-refusal pass.
- `SynthFull(...)` combines monomorphisation spec + profile for the
  most-general path the build driver will use.
- `applyProfileRefusal(c)` walks `c.Functions` and moves every async
  fn into `c.Skipped` with `errors.SkipEmbedded` +
  `embedded.AsyncRefusalReason` when `embedded.RefuseAsync(profile)`
  is true. Idempotent.

### `package3/rust/wrapper/emit.go`

- `EmitLibRS` prepends `embedded.LibRSHeader(c.Profile)` before the
  runtime prologue.
- `EmitCargoTOML` uses `embedded.CargoUpstreamDepRow` for the
  upstream dep row instead of the inline `name = "=version"` literal.
  When the profile is embedded and async refusal stripped every
  async fn, `c.HasAsync()` returns false and the tokio + once_cell
  rows are omitted.

### Tests

- `embedded_test.go` (20 cases): ParseProfile (default / hosted /
  embedded / rejects unknown), Profile.String + round-trip,
  LibRSHeader (hosted / embedded / trailing newline / byte-stable),
  CargoUpstreamDepRow (hosted plain / embedded default-features
  flip), RefuseAsync, AsyncRefusalReason stable, ParseTOMLBody
  (empty / embedded / comments + blank lines / rejects unknown key /
  rejects unquoted / rejects trailing junk), AllowedTriples (hosted
  returns nil / embedded sorted + nonempty + canonical entries
  present).
- `wrapper/embedded_test.go` (9 cases): SynthWithProfile hosted keeps
  async, embedded refuses async with SkipEmbedded + tokio detail,
  embedded keeps sync, EmitLibRS embedded has #![no_std], EmitLibRS
  hosted lacks #![no_std], EmitCargoTOML embedded flips
  default-features, EmitCargoTOML hosted plain, EmitCargoTOML
  embedded omits tokio+once_cell, SynthFull threads profile +
  spec, byte-stable emit.
- `embedded/phase13_test.go` sentinel (9 subtests):
  `profile_default_is_hosted`, `profile_parses_from_toml`,
  `libRS_carries_no_std_under_embedded`,
  `libRS_omits_no_std_under_hosted`,
  `cargo_pins_default_features_off_under_embedded`,
  `async_fns_refused_under_embedded`,
  `sync_fns_kept_under_embedded`,
  `triples_list_sorted_and_nonempty`,
  `emit_byte_stable_under_embedded`.

## Target matrix

| Target                  | Status | Notes |
|-------------------------|--------|-------|
| `#![no_std]` prologue   | ✅     | Embedded EmitLibRS starts with the no_std attribute. |
| `extern crate alloc;`   | ✅     | Embedded EmitLibRS pulls in alloc-backed String / Vec / BTreeMap. |
| Cargo default-features  | ✅     | Embedded upstream row flips `default-features = false`. |
| Async refusal           | ✅     | SynthWithProfile moves async fns to `c.Skipped` with `SkipEmbedded`. |
| Sync preserved          | ✅     | Sync fns pass through unchanged under embedded. |
| Tokio dep omitted       | ✅     | After async refusal `HasAsync()` returns false so EmitCargoTOML skips tokio + once_cell. |
| Profile parser          | ✅     | `[rust] profile = "embedded"` parses; unknown keys / unquoted values rejected. |
| Triples list            | ✅     | thumbv6m / thumbv7em / thumbv7m / thumbv8m / riscv32imc / riscv32imac, sorted. |
| Hosted regression       | ✅     | Hosted profile emit is byte-identical to pre-Phase-13. |
| Byte stability          | ✅     | Embedded EmitLibRS + EmitCargoTOML byte-stable across 16 runs. |

## How this phase plugs in to the larger pipeline

```
  mochi.toml [rust] profile = "embedded"
                │
                ▼
  embedded.ParseTOMLBody → embedded.Profile
                │
                ▼
  wrapper.SynthWithProfile(upstream, version, surface, profile)
                │
                ▼
            (default Synth path)
                │
                ▼
            applyProfileRefusal(c)
                │
   ┌────────────┴────────────┐
   │                         │
   ▼ ProfileHosted           ▼ ProfileEmbedded
  c unchanged              for each fn in c.Functions:
                             if fn.IsAsync:
                               c.Skipped += SkipReport{
                                 Reason: SkipEmbedded,
                                 Detail: AsyncRefusalReason,
                               }
                               continue
                             keep fn
                │
                ▼
            EmitLibRS:
              embedded.LibRSHeader(c.Profile)  # "#![no_std]\nextern crate alloc;\n" or ""
              runtime_prologue
              [mod mochi_rt;]   # only if HasAsync() — under embedded, never
              extern "C" fn... bodies
                │
                ▼
            EmitCargoTOML:
              [dependencies]
              embedded.CargoUpstreamDepRow(c.Profile, ...)
              [tokio + once_cell only if HasAsync()]
```

Phase 13 is the closing brace on the bridge's consume-direction
capability surface (`net`, `fs`, `proc`, `unsafe`, and now `embedded`
as a structural profile). Combined with Phase 12 (monomorphisation),
the bridge can now serve every leaf of the §10 target matrix.

## Timeline

| Time (GMT+7)        | Step |
|---------------------|------|
| 2026-05-30 00:36    | Worktree branch `mep/0073-phase-13` created off `origin/main` (which includes Phase 12 SHA 37768e85). |
| 2026-05-30 00:38    | `package3/rust/embedded/embedded.go` written (Profile, ParseProfile, ParseTOMLBody, LibRSHeader, CargoUpstreamDepRow, RefuseAsync, AsyncRefusalReason, AllowedTriples). |
| 2026-05-30 00:39    | `errors.SkipEmbedded` added; wrapper grows `SynthWithProfile` + `SynthFull` + `applyProfileRefusal`. |
| 2026-05-30 00:40    | EmitLibRS + EmitCargoTOML wired through embedded helpers; full rust test sweep green. |
| 2026-05-30 00:41    | Phase 13 sentinel + tracking page + spec sync. |
