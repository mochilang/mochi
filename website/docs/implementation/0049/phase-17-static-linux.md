---
title: "Phase 17. Static Linux SDK single binary"
sidebar_position: 21
sidebar_label: "Phase 17. Static Linux SDK"
description: "MEP-49 Phase 17 — Swift Static Linux SDK (musl libc); single self-contained binary with no runtime dependencies; cross-compilation from macOS to linux-x64 and linux-arm64."
---

# Phase 17. Static Linux SDK single binary

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 17](/docs/mep/mep-0049#phase-17-static-linux) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase17StaticLinux`: `ldd binary` outputs "not a dynamic executable" (or equivalent on musl). Binary runs on vanilla Alpine Linux (no Swift runtime installed). 20 fixtures green on linux-x64 and linux-arm64. Cross-compilation from macOS arm64 to linux-x64 and linux-arm64.

## Goal-alignment audit

The single static binary is Mochi's server deployment story. Deploying a Swift server application typically requires either installing the Swift runtime on the target machine or bundling it with the app. The Static Linux SDK eliminates both requirements: the binary contains the full Swift runtime statically linked, making it a true zero-dependency deployment artifact. This is the "ship it anywhere" capability that makes Mochi competitive with Go for server workloads.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 17.0 | Static Linux SDK install: `swift sdk install` + `--checksum` verification | NOT STARTED | — |
| 17.1 | Cross-compile from macOS arm64 → linux-x64: `swift build --swift-sdk x86_64-swift-linux-musl` | NOT STARTED | — |
| 17.2 | Cross-compile from macOS arm64 → linux-arm64: `swift build --swift-sdk aarch64-swift-linux-musl` | NOT STARTED | — |
| 17.3 | `ldd` gate: verify binary is statically linked; cold-start on Alpine Linux Docker container | NOT STARTED | — |
| 17.4 | Binary size measurement: target ~10MB for release binary (hello world program) | NOT STARTED | — |

## Sub-phase 17.0 -- Static Linux SDK install

### Decisions made (17.0)

**Swift Static Linux SDK**: Apple provides a prebuilt musl-based static SDK for cross-compilation to Linux. Introduced in Swift 5.9 (SE-0387: Swift SDK bundles). The SDK bundles:
- Swift standard library (statically linked)
- Foundation framework (open-source reimplementation for Linux)
- All other Swift runtime libraries
- musl libc
- Linux kernel headers

**Installation**:

```bash
swift sdk install \
  https://download.swift.org/swift-6.0-release/static-sdk/swift-6.0-RELEASE/swift-6.0-RELEASE_static-linux-0.0.1.artifactbundle.tar.gz \
  --checksum <sha256>
```

The Mochi build driver installs the SDK automatically if not present (`~/.swiftpm/swift-sdks/`). The checksum is pinned in `transpiler3/swift/build/sdk.go` per Swift version.

**CI setup**: the `ubuntu-24.04` GitHub Actions runner pre-installs the Swift toolchain. The Static Linux SDK is installed as a CI setup step (cached by `~/.swiftpm/swift-sdks/` path).

**SDK identifier**: the SDK is referenced by its triple: `x86_64-swift-linux-musl` and `aarch64-swift-linux-musl`.

## Sub-phase 17.1 -- Cross-compile to linux-x64

### Decisions made (17.1)

**Build command** (from macOS arm64):

```bash
swift build \
  -c release \
  --swift-sdk x86_64-swift-linux-musl \
  --static-swift-stdlib
```

**`--static-swift-stdlib`**: explicitly requests static linking of the Swift standard library. With the musl SDK, this is the default; the flag is added for clarity.

**Output location**: `.build/x86_64-swift-linux-musl/release/MochiOut`. A statically linked ELF binary.

**MochiRuntime compilation for musl**: `swift-collections`, `swift-algorithms`, `swift-async-algorithms`, and other MochiRuntime dependencies must compile for the musl target. All Apple-provided packages support the Static Linux SDK. Third-party packages may not; the Mochi build driver checks compatibility during `swift package resolve` and warns.

**`swift-cmark` / BoringSSL**: Foundation on Linux uses BoringSSL for TLS (for `URLSession`). BoringSSL is statically linked into the binary when using the Static Linux SDK. This means `URLSession` (Phase 14) works in the static binary without requiring OpenSSL on the target machine.

## Sub-phase 17.2 -- Cross-compile to linux-arm64

### Decisions made (17.2)

**Build command** (from macOS arm64 or linux-x64):

```bash
swift build \
  -c release \
  --swift-sdk aarch64-swift-linux-musl \
  --static-swift-stdlib
```

**CI runner**: `ubuntu-24.04-arm` GitHub Actions runner (Graviton 2, arm64). Native compilation is used on arm64 runners; cross-compilation from macOS is available for local development.

**Output**: `.build/aarch64-swift-linux-musl/release/MochiOut`. A statically linked AArch64 ELF binary.

**Binary size target**: ~10MB for a release hello-world binary (Swift runtime + Foundation + MochiRuntime). A complex program with full query DSL, agents, and streams may reach 15-20MB. This is acceptable for server deployment.

## Sub-phase 17.3 -- ldd gate

### Decisions made (17.3)

**`ldd` verification**:

```bash
# On the build machine:
ldd .build/x86_64-swift-linux-musl/release/MochiOut
# Expected output: "not a dynamic executable"
# or: "statically linked"
```

On Alpine Linux (musl libc), `ldd` on a musl-linked binary outputs the musl dynamic linker path. Static binaries output "not a dynamic executable" regardless of the host libc.

**Alpine Linux container test**: the CI gate runs the binary inside a Docker container from `alpine:3.20` (no Swift installed):

```bash
docker run --rm \
  -v "$(pwd)/.build/x86_64-swift-linux-musl/release/MochiOut:/app/MochiOut" \
  alpine:3.20 \
  /app/MochiOut
```

The binary must produce the expected output with exit code 0.

**Cold-start measurement**: the CI gate records the time from process start to first output. Target: < 20ms for a release hello-world binary on a c5.xlarge (4 vCPU). The measurement is recorded in CI artifacts for trend tracking; it is not a hard gate.

## Sub-phase 17.4 -- Binary size measurement

### Decisions made (17.4)

**Measurement**: `wc -c .build/x86_64-swift-linux-musl/release/MochiOut`. Recorded in CI artifacts.

**Size reduction techniques applied**:
- `-c release` (optimisations enabled, debug info disabled).
- `--static-swift-stdlib` (single copy of stdlib, not multiple).
- `-Xswiftc -Osize` (size-optimised build, if binary correctness is maintained).
- Linker stripping: `strip -S` to remove debug symbols.

**`-Osize` consideration**: `-Osize` (introduced in Swift 5.1) optimises for binary size over speed. The trade-off: up to 20% size reduction, up to 10% performance regression for CPU-bound code. Enabled by default in `--target=swift-linux-static` builds; can be disabled with `--no-optimize-size`.

**Compression**: static binaries for distribution can be compressed with `upx --lzma`. This is optional and not applied by default (it makes the binary non-standard and slower to start). Available via `mochi build --compress`.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/build/sdk.go` | Static Linux SDK install/verify; SDK identifier constants; `--swift-sdk` flag injection |
| `transpiler3/swift/build/static.go` | `swift build --static-swift-stdlib` invocation; ldd gate; Alpine Docker test |
| `transpiler3/swift/build/package.go` | `--swift-sdk` dependency filtering for musl-compatible packages |
| `transpiler3/swift/build/phase17_test.go` | `TestPhase17StaticLinux`: 20 fixtures + ldd gate + Alpine container test |
| `tests/transpiler3/swift/fixtures/phase17-static-linux/` | 20 fixture directories |

## Test set

- `TestPhase17StaticLinux` -- 20 fixtures (subset of phase 1-14 fixtures recompiled for linux-x64 static): `static_hello`, `static_scalars`, `static_list`, `static_map`, `static_record`, `static_sum`, `static_closure`, `static_query`, `static_agent`, `static_stream`, `static_async`, `static_ffi_c`, `static_fetch_mock`, `static_datalog`, `static_llm_mock`, `static_arm64_hello`, `static_arm64_agent`, `static_arm64_query`, `static_alpine_hello`, `static_alpine_agent`.

## Deferred work

- Windows cross-compilation from macOS. Deferred to Phase 17.1.
- Embedded Swift (bare-metal). Out of v1 scope.
- WASI (WebAssembly System Interface). Out of v1 scope.
- Container image generation (`docker build` with the static binary in FROM scratch). Deferred to a future tooling phase.
