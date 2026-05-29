---
title: "Phase 15. Publish-ready crate"
sidebar_position: 17
sidebar_label: "Phase 15. Publish"
description: "MEP-53 Phase 15, publish-ready crate metadata (license, description, repository) plus opt-in cargo publish --dry-run gate."
---

# Phase 15. Publish-ready crate metadata + dry-run gate

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-53 §Phases](/docs/mep/mep-0053#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 (GMT+7) |
| Landed         | 2026-05-29 (GMT+7) |
| Tracking issue | [#22610](https://github.com/mochilang/mochi/issues/22610) |
| Tracking PR    | [#22499](https://github.com/mochilang/mochi/pull/22499) |
| Commit         | 458120b2d3 |

## Gate

`TestPhase15Publish` in `transpiler3/rust/build/phase15_test.go` runs two steps:

1. **Always-on**: build a fixture to `TargetRustCrate`, then parse the emitted `Cargo.toml` and assert it carries `license`, `description`, and `repository` fields. These are the three fields crates.io requires for `cargo publish` to succeed.
2. **Opt-in** (gated by `MOCHI_RUN_PUBLISH_DRYRUN=1`): inside the emitted crate, run `cargo publish --dry-run --no-verify --allow-dirty` and assert exit zero. This confirms cargo accepts the metadata as publishable. Off by default because cargo's dry-run still requires registry index access (it talks to crates.io to check for name collisions).

## Lowering decisions

The `TargetRustCrate` target was added in this phase. Until phase 15, `TargetRustSource` rendered the source files but not a self-contained crate; `TargetNativeExecutable` built into the cargo cache; neither produced an artefact a user could `cargo publish` from. `TargetRustCrate` copies the full `workDir` (Cargo.toml + src/ + cffi/ + build.rs) to `outDir` via a tree-walk that skips `target/`. The result is a directory that `cargo publish` accepts.

`generateCargoToml` was extended to emit:

```toml
[package]
name = "mochi-emitted"
version = "0.1.0"
edition = "2021"
license = "Apache-2.0"
description = "Mochi program emitted by transpiler3/rust"
repository = "https://github.com/mochilang/mochi"
```

The `license` is `Apache-2.0` matching the Mochi project license. `description` and `repository` are static defaults; future phases could expose them as `mochi.toml` fields.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/rust/build/build.go` | Add `TargetRustCrate`, `copyTree`, extend `generateCargoToml` |
| `transpiler3/rust/build/phase15_test.go` | Metadata-assertion gate + opt-in dry-run gate |
| `tests/transpiler3/rust/fixtures/phase15-publish/publish_hello.mochi` + `.out` | Single fixture |

## Test set

- `TestPhase15Publish/metadata` (always-on)
- `TestPhase15Publish/cargo_publish_dryrun` (opt-in, gated on `MOCHI_RUN_PUBLISH_DRYRUN=1`)

## Closeout notes

`--no-verify` is necessary because the dry-run with verify-on would itself run `cargo build` on the emitted crate inside an isolated workspace, which fails if any cc-rs dep is missing or if the crate name collides with an in-flight upload. The trade-off: we lose the build-verifies-the-package check, but we still verify the metadata-correctness piece (which is what fails most often when authors first publish to crates.io).

`--allow-dirty` is necessary because the test runs against a freshly written-out crate that isn't a git repo. Cargo otherwise refuses to publish from non-clean git trees.
