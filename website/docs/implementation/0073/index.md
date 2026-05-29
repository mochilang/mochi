---
title: MEP-73 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 73. Mochi+Rust package manager"
description: "Per-phase implementation tracking for MEP-73 (Mochi+Rust package manager). Status + commit columns capture how each phase landed on main."
---

# MEP-73 implementation tracking

Per-phase tracking for [MEP-73 Mochi+Rust package manager](/docs/mep/mep-0073). Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main` (or, for the umbrella PR, the in-branch commit on `mep/0073-rust-package`).

A phase is LANDED only when its gate is green for every target (consume direction + publish direction where applicable). Missing surfaces become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

## Phase status

| Phase | Title | Status | Commit | Tracking page |
|-------|-------|--------|--------|---------------|
| 0 | Skeleton: package3/rust/ layout + cargo workspace plumbing | LANDED | [2dc3b34f](https://github.com/mochilang/mochi/commit/2dc3b34f) | [phase-00](/docs/implementation/0073/phase-00-skeleton) |
| 1 | Sparse-index client (`index.crates.io` reader, sha256 + blake3 download verify) | LANDED | [a3c263fb](https://github.com/mochilang/mochi/commit/a3c263fb) | [phase-01](/docs/implementation/0073/phase-01-sparse-index) |
| 2 | Rustdoc-JSON ingest + ApiSurface emit | LANDED | [1cd97c1b](https://github.com/mochilang/mochi/commit/1cd97c1b) | [phase-02](/docs/implementation/0073/phase-02-rustdoc-ingest) |
| 3 | Closed type-mapping table (scalars / strings / collections / Option / Result / Tuple / Struct / Enum) | LANDED | [0be87e7f](https://github.com/mochilang/mochi/commit/0be87e7f) | [phase-03](/docs/implementation/0073/phase-03-type-mapping) |
| 4 | Wrapper crate synthesiser (extern "C" surface, MochiString / MochiSlice / opaque handles) | LANDED | [2bf1474c](https://github.com/mochilang/mochi/commit/2bf1474c) | [phase-04](/docs/implementation/0073/phase-04-wrapper) |
| 5 | Mochi-side extern fn emitter + alias shim file generation | LANDED | [6da45d5d](https://github.com/mochilang/mochi/commit/6da45d5d) | [phase-05](/docs/implementation/0073/phase-05-extern-emit) |
| 6 | `import rust "<crate>@<semver>" as <alias>` grammar + parser | LANDED | [8a4e5b8d](https://github.com/mochilang/mochi/commit/8a4e5b8d) | [phase-06](/docs/implementation/0073/phase-06-import-grammar) |
| 7 | Build orchestration: workspace synth + cargo build invocation + artifact link | LANDED | (pending) | [phase-07](/docs/implementation/0073/phase-07-build) |
| 8 | mochi.lock `[[rust-package]]` integration + `--check` mode | NOT STARTED | — | [phase-08](/docs/implementation/0073/phase-08-lockfile) |
| 9 | `TargetRustLibrary` emit (rlib + cdylib + Cargo.toml + cbindgen) | NOT STARTED | — | [phase-09](/docs/implementation/0073/phase-09-rust-library-emit) |
| 10 | Trusted publishing (`mochi pkg publish --to=crates.io`) Sigstore OIDC flow | NOT STARTED | — | [phase-10](/docs/implementation/0073/phase-10-trusted-publish) |
| 11 | Async bridge (tokio runtime singleton + block_on entry) | NOT STARTED | — | [phase-11](/docs/implementation/0073/phase-11-async-bridge) |
| 12 | Monomorphisation (`[rust.monomorphise]` manifest + per-instantiation wrapper) | NOT STARTED | — | [phase-12](/docs/implementation/0073/phase-12-monomorphise) |
| 13 | Embedded / no_std subset (`profile = "embedded"` + alloc opt-in) | NOT STARTED | — | [phase-13](/docs/implementation/0073/phase-13-embedded) |

## Per-phase fields

Each phase tracking page documents (or will document, once the phase begins):

- **Gate**: the test or check that must pass for the phase to be LANDED.
- **Files to touch**: the bridge-side files (Go) and emit-side files (Rust template) the phase introduces or modifies.
- **Fixtures**: which of the 24-crate fixture corpus the phase validates against.
- **Skip count**: the expected SkipReport count per fixture crate (golden numbers).
- **Sub-phase decomposition** (if needed): N.1, N.2, ... entries when an upstream constraint forces splitting.

## Fixture corpus

The 24-crate fixture corpus (April 2026 top-25-most-downloaded-on-crates.io minus the long-deprecated lazy_static):

anyhow, thiserror, serde, regex, rayon, itertools, once_cell, time, uuid, url, base64, hex, sha2, blake3, rand, rand_chacha, num_cpus, bytes, smallvec, indexmap, ahash, parking_lot, crossbeam, tokio.

Each phase that touches the type-mapping or wrapper layer asserts golden counts against this corpus. The corpus is regenerated quarterly to track crate API drift.

## Implementation location

The bridge lives at `package3/rust/` in the repo root:

```
package3/rust/
  README.md               # pointer to MEP-73 spec
  errors/                 # SkipReason + BridgeError (phase 0)
  build/                  # Workspace + Driver + Cargo.toml renderer (phase 0)
  semver/                 # cargo-flavoured semver parser (phase 1)
  sparse/                 # sparse-index client + content-addressed cache (phase 1)
  rustdoc/                # rustdoc-types Go parser + ApiSurface walker (phase 2)
  typemap/                # closed type table + Mochi/FFI rendering (phase 3)
  wrapper/                # extern-C wrapper synthesiser (phase 4)
  emit/                   # Mochi extern fn emitter (phase 5)
  publish/                # crates.io publish + Sigstore (phase 10)
  embedded/               # no_std subset (phase 13)
```

The `package3/rust/` location is shared with the broader MEP-57 polyglot package work (where `package3/` is the v3 package-system tree).

## Status snapshot

As of 2026-05-29 23:04 (GMT+7): phases 0, 1, 2, 3, 4, 5, 6, and 7 LANDED (skeleton + sparse-index client + cargo semver + rustdoc-JSON ingest + closed type-mapping table + extern-C wrapper synthesiser + Mochi extern-fn emitter + `import rust` grammar + build orchestration pipeline). Phases 8-13 pending. The MEP spec and research bundle are landed; implementation is progressing one phase per PR with auto-merge.

## Cross-references

- [MEP-73 spec](/docs/mep/mep-0073) for the normative design.
- [MEP-73 research bundle](/docs/research/0073/) for the 12-note deep-research collection.
- [MEP-53 implementation tracking](/docs/implementation/0053) for the underlying Rust transpiler that MEP-73 extends.
- [MEP-57 implementation tracking](/docs/implementation/0057) for the polyglot package system MEP-73 builds on.
