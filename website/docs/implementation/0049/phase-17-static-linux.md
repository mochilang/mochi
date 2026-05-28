---
title: "Phase 17. Static Linux SDK single binary"
sidebar_position: 21
sidebar_label: "Phase 17. Static Linux SDK"
description: "MEP-49 Phase 17 — Swift Static Linux SDK (musl libc); SDKTripleX64/Arm64 constants; StaticLinuxSDKAvailable helper; cross-compile path in Driver; skipped when SDK absent."
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

`TestPhase17StaticLinux`: validates Swift source generation for static Linux targets. Actual `swift build --swift-sdk` cross-compilation is skipped when the Static Linux SDK is not installed (`StaticLinuxSDKAvailable` returns false). 3 fixtures.

## Goal-alignment audit

The static Linux binary is Mochi's zero-dependency server deployment story. Phase 17 ships the SDK availability check, the SDK triple constants, and the `TargetLinuxStaticX64`/`TargetLinuxStaticArm64` build targets in the Driver. The actual `ldd` gate (verifying "not a dynamic executable") and Alpine container test are deferred to a sub-phase that requires a Linux CI runner with the SDK installed.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 17.0 | `SDKTripleX64`, `SDKTripleArm64` constants; `StaticLinuxSDKAvailable(triple)` helper | LANDED | mep/0049-phase-17 |
| 17.1 | `TargetLinuxStaticX64`, `TargetLinuxStaticArm64` in `Driver.Build`; `--swift-sdk` flag injection | LANDED | mep/0049-phase-17 |
| 17.2 | `ldd` gate: verify binary is statically linked on linux-x64 | DEFERRED | — |
| 17.3 | Alpine Linux Docker container cold-start test | DEFERRED | — |
| 17.4 | Binary size measurement and `-Osize` flag | DEFERRED | — |

## Sub-phase 17.0 -- SDK availability

### Decisions made (17.0)

**`SDKTripleX64 = "x86_64-swift-linux-musl"`** and **`SDKTripleArm64 = "aarch64-swift-linux-musl"`**: the Swift SDK triple identifiers used with `--swift-sdk`.

**`StaticLinuxSDKAvailable(triple string) bool`**: runs `swift sdk list` and checks whether the output contains the given triple. Returns `false` if `swift` is not on PATH.

```go
func StaticLinuxSDKAvailable(triple string) bool {
    out, err := exec.Command("swift", "sdk", "list").Output()
    if err != nil { return false }
    return strings.Contains(string(out), triple)
}
```

The gate test calls `StaticLinuxSDKAvailable(SDKTripleX64)` and calls `t.Skip` when the SDK is not installed, so the test always passes in CI without the SDK.

## Sub-phase 17.1 -- Static build targets

### Decisions made (17.1)

**`TargetLinuxStaticX64` and `TargetLinuxStaticArm64`**: new target constants in `build.go`. When selected, `Driver.Build` passes `--swift-sdk <triple>` and `--static-swift-stdlib` to `swift build`.

**Cross-compilation from macOS**: supported when the Static Linux SDK is installed. The build output goes to `.build/<triple>/release/<binary>`.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/build/sdk.go` | `SDKTripleX64`, `SDKTripleArm64`, `StaticLinuxSDKAvailable` |
| `transpiler3/swift/build/build.go` | `TargetLinuxStaticX64`, `TargetLinuxStaticArm64`; `--swift-sdk` flag injection |
| `transpiler3/swift/build/phase17_test.go` | `TestPhase17StaticLinux`: 3 fixtures; skips if SDK absent |
| `tests/transpiler3/swift/fixtures/phase17-static-linux/` | 3 fixture directories |

## Test set

- `TestPhase17StaticLinux` -- 3 fixtures: `static_func`, `static_hello`, `static_int`. Test skips when `StaticLinuxSDKAvailable(SDKTripleX64)` returns false.

## Deferred work

- `ldd` gate: assert binary output is "not a dynamic executable" on a linux-x64 runner. Deferred to Phase 17.2.
- Alpine Linux Docker container test (`docker run alpine:3.20 /app/MochiOut`). Deferred to Phase 17.3.
- Binary size measurement; `-Osize` flag support. Deferred to Phase 17.4.
- `strip -S` + optional `upx --lzma` compression. Deferred.
- Windows cross-compilation from macOS. Deferred.
- WASI (WebAssembly System Interface). Out of v1 scope.
