---
title: "11. Testing gates"
sidebar_position: 12
sidebar_label: "11. Testing gates"
description: "Per-phase test set, vm3 stdout oracle, cargo check vs cargo build vs cargo run, reproducibility SHA-256 gate, publish dry-run gate, embedded no_std cargo check gate."
---

# 11. Testing gates

This note covers MEP-53's gate-per-phase strategy: which test runner is used, what the oracle is, and how the gates compose.

## The vm3 stdout oracle

For each `.mochi` fixture, the gate:

1. Runs the fixture under vm3 (the canonical Mochi VM) and captures stdout.
2. Lowers and builds the fixture with the Rust target.
3. Runs the emitted binary and captures stdout.
4. Asserts byte-equal match.

This is the same oracle the C, BEAM, JVM, .NET, Swift, Kotlin, Python, TypeScript, and Ruby targets use. Reusing vm3 (rather than authoring a separate fixture corpus per target) means a single source of truth for expected behavior and a single point of fixture maintenance.

For test cases where Mochi's spec says "implementation-defined" (e.g., HashMap iteration order), the fixture is either avoided or the test wraps the output through a sort step before comparison.

## Per-phase test runner

Each phase has a Go test in `transpiler3/rust/build/phaseNN_test.go` that:

- Iterates over fixtures in `tests/transpiler3/rust/phaseNN/`.
- Calls `Driver.Build` for each fixture.
- Runs the emitted binary.
- Diffs against the vm3 oracle.

Per-fixture timeouts cap at 30 seconds (most lower in under 2 seconds; the longest are wasm builds). Test failures surface as Go test failures with the fixture path and a unified diff against the expected stdout.

## Cargo check vs cargo build vs cargo run

- **cargo check** (used by the embedded gate, phase 18): typechecks only, no codegen, no link. Fastest, ~3 seconds per fixture.
- **cargo build --release** (used by phases 1-15, 17, 18 default): typechecks + codegens + links. Most fixtures: 8-15 seconds per fixture on a warm cache.
- **cargo run --release** (implicit in the phase test runners): builds + runs. Adds &lt; 0.5 seconds.

The phase 16 reproducibility gate runs `cargo build --release` twice on the same source (clean target/ between), SHA-256s both binaries, asserts equality.

## Phase test set sizes

| Phase | Fixture count | Notes |
|-------|---------------|-------|
| 1 | 1 | Hello world. |
| 2 | 20 | Scalars and basic ops. |
| 3 | 12 | Control flow. |
| 4 | 14 | Strings and chars. |
| 5 | 27 | Lists, maps, sets. |
| 6 | 34 | Records and sum types. |
| 7 | 43 | Closures and capture. |
| 8 | 20 | Query DSL. |
| 9 | 44 | Agents and streams. |
| 10 | 31 | Async and channels. |
| 11 | 36 | Datalog. |
| 12 | 24 | extern fn / cffi. |
| 13 | 11 | Linux static via cargo-zigbuild (subdirs per triple). |
| 14 | 17 | Panic, try, catch, fetch, JSON, LLM. |
| 15 | 1 | Publish dry-run. |
| 16 | 7 | Reproducibility (macOS skipped). |
| 17 | 4 | wasm32-wasip1 + wasmtime. |
| 18 | 12 | Embedded subset (cargo check only). |

Fixtures stack: a fixture exercising phase N is also exercised by every phase &gt;= N. The phase-N gate is the new fixtures only.

## Reproducibility gate (phase 16)

The reproducibility gate:

```go
func TestPhase16Reproducible(t *testing.T) {
    if runtime.GOOS == "darwin" {
        t.Skip("LC_UUID is randomised per link on Mach-O")
    }
    d := &Driver{Deterministic: true}
    for _, fixture := range fixtures {
        a, _ := d.Build(fixture, t.TempDir(), TargetNativeExecutable)
        // clean cache to force rebuild
        os.RemoveAll(d.CacheDir)
        b, _ := d.Build(fixture, t.TempDir(), TargetNativeExecutable)
        if sha256File(a) != sha256File(b) {
            t.Errorf("non-deterministic: %s", fixture)
        }
    }
}
```

The platform-skip on macOS is necessary because Mach-O's `LC_UUID` load command is randomised by ld64 per link (no flag suppresses it). The reproducibility property is asserted on Linux only.

Cleaning the cache between runs is required: otherwise the second build would be a cache hit and trivially byte-identical without testing anything.

## Publish dry-run gate (phase 15)

The publish dry-run:

```sh
cargo package --allow-dirty --target-dir /tmp/mochi-publish-check
```

This packages the runtime crate (without publishing) and verifies that:

- The `Cargo.toml` has the required publish-side fields (license, description, repository, documentation).
- All source files are bundled correctly.
- No unrelated files (target/, .git/) are included.
- The README.md path is correct.

The gate does **not** actually publish (no crates.io credentials in CI). A future sub-phase could add a release-tag-triggered publish workflow.

## Embedded gate (phase 18)

The embedded gate:

```sh
cargo check --no-default-features --features embedded
```

Run against the `mochi-runtime` crate workspace. The crate must compile cleanly under `no_std + alloc` with only the embedded-allowed modules (conv, strings) active.

This is `cargo check`, not `cargo build`: there is no embedded executable to produce (the embedded target is a "library subset" available for downstream MCU crates to depend on). Build would require a no_std-compatible target triple like `thumbv7em-none-eabihf` which CI doesn't currently install.

## wasm gate (phase 17)

The wasm gate builds the fixture for wasm32-wasip1 and runs it under wasmtime:

```sh
cargo build --release --target wasm32-wasip1
wasmtime run target/wasm32-wasip1/release/foo.wasm
```

The stdout is captured and diffed against the vm3 oracle. Fixtures that depend on net (TCP) or threads (std::thread) are excluded from the wasm fixture set because wasm32-wasip1 doesn't support them.

The gate skips cleanly if `wasmtime` is not on PATH (the driver detects this via `exec.LookPath`), which means CI without wasmtime installed reports "wasm gate skipped" rather than red-x.

## cargo-zigbuild gate (phase 13)

The Linux-static gate cross-builds with cargo-zigbuild:

```sh
cargo zigbuild --release --target x86_64-unknown-linux-musl
cargo zigbuild --release --target aarch64-unknown-linux-musl
```

The emitted binaries are stamped with `file` to verify "statically linked, ELF 64-bit." The gate does not execute the binaries on macOS host (would require QEMU / docker). A Linux CI runner could execute the x86_64 binary directly; the aarch64 case still requires QEMU on x86 hosts.

The gate skips cleanly if `cargo-zigbuild` is not on PATH.

## Test parallelism

`go test -parallel N` with N = number of CPU cores. Per-fixture builds are CPU-bound (rustc) and not cache-contended (each fixture has its own `<sha8>` subdir under `~/.cache/mochi/rust/`). The bottleneck is rustc throughput, not Go test framework overhead.

On a 10-core M1 Pro, the full MEP-53 test set runs in ~12 minutes warm-cache. Cold cache (after `rm -rf ~/.cache/mochi/rust/`) takes ~28 minutes.

## CI integration

`.github/workflows/transpiler3-rust.yml` (phase 0 added) runs `go test ./transpiler3/rust/...` on every push touching `transpiler3/rust/`, `runtime3/rust/`, or `tests/transpiler3/rust/`. The CI cache is keyed on `~/.cache/mochi/rust/` so subsequent runs reuse the cargo target directory.

Failed gates surface as a red x on the PR with a link to the fixture path and the diff. Most failures are oracle drift (vm3 output changed) or fixture authoring errors; genuine bugs in the Rust emitter are caught at lower-time test assertion failures rather than at run-time diff.

## Cross-references

- [[build-system]] for the driver invocation per gate.
- [[rust-target-portability]] for target triple availability per gate.
- [[runtime]] for the embedded module subset.
- [MEP-53 §5](/docs/mep/mep-0053#5-build-targets) for the normative target list.
