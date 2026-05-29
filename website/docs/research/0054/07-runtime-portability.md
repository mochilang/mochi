---
title: "07. Go target and portability"
sidebar_position: 8
sidebar_label: "07. Portability"
description: "Go 1.21 / 1.26 matrix, the GOOS+GOARCH cross-compile story, the wasm/js vs wasip1 split, vendor vs proxy mode, why no TinyGo for the default path."
---

# 07. Go target and portability

This note describes the Go-toolchain-and-target matrix the transpiler ships against.

## Go version floor and gate

- **Floor: Go 1.21 (Aug 2023).** First version with stable `GOOS=wasip1`. We need wasip1 for Phase 17. Below 1.21 we would only ship `GOOS=js` for the browser case, which excludes wasmtime / wasmer / WasmEdge.
- **Production gate: Go 1.26.0 + Go 1.26.x latest.** The active stable as of 2026-05.
- **Forward-compat smoke: Go 1.27 beta.** Runs as `allow_failure: true` to catch upcoming breaking changes.

We do not gate against Go 1.18 / 1.19 / 1.20 because they predate the `slices` and `maps` stdlib packages and the reproducible-build defaults. Supporting them would mean reimplementing those helpers in the runtime module, doubling LOC for negligible reach.

## OS / arch matrix

Go's first-class cross-compile via `GOOS` + `GOARCH` env vars is the load-bearing feature for the Phase-17 targets. The matrix:

| Target | GOOS | GOARCH | Linker | Notes |
|--------|------|--------|--------|-------|
| `TargetGoBinaryLinuxAmd64` | linux | amd64 | internal | Default Linux server target. |
| `TargetGoBinaryLinuxArm64` | linux | arm64 | internal | Raspberry Pi 4+, AWS Graviton. |
| `TargetGoBinaryDarwinAmd64` | darwin | amd64 | external (`ld64`) | Intel Macs. |
| `TargetGoBinaryDarwinArm64` | darwin | arm64 | external (`ld64`) | Apple silicon. |
| `TargetGoBinaryWindowsAmd64` | windows | amd64 | internal | Most Windows servers and desktops. |
| `TargetGoBinaryFreeBSDAmd64` | freebsd | amd64 | internal | The "is this still maintained" target. |
| `TargetGoWasmJS` | js | wasm | internal | Browser via `wasm_exec.js`. |
| `TargetGoWasiP1` | wasip1 | wasm | internal | wasmtime / wasmer / WasmEdge. |

All targets except darwin link with Go's internal linker — no external toolchain required. The darwin targets need Apple's `ld64`, which is installed by `xcode-select --install` on macOS; on Linux developers wanting to cross-compile to darwin install osxcross (out of scope for the MEP).

The Linux arm64 target is gateable from an Ubuntu x86_64 CI runner because Go's internal linker handles cross-arch without a special linker. This is the killer cross-compile feature.

## Cgo across the matrix

`import "C"` works on every target where Go has a host C compiler available:

| Target | cgo |
|--------|-----|
| linux / amd64 | yes (gcc) |
| linux / arm64 | yes (with cross-gcc) |
| darwin / amd64, arm64 | yes (clang) |
| windows / amd64 | yes (MinGW) |
| freebsd / amd64 | yes (clang) |
| js / wasm | no |
| wasip1 / wasm | no |

Phase 12 fixtures skip cgo when the target is js/wasm or wasip1/wasm. The skip is at fixture-discovery time, not build-failure time.

## Wasm: the js vs wasip1 split

`GOOS=js GOARCH=wasm` targets the browser. Produces a `.wasm` that requires `wasm_exec.js` (shipped with the Go toolchain at `$(go env GOROOT)/lib/wasm/wasm_exec.js`) as the JS-side glue. The `wasm_exec.js` provides the syscalls (`fs.read`, `fs.write`, `time.now`, etc.) the wasm module needs.

`GOOS=wasip1 GOARCH=wasm` targets the [WASI Preview 1](https://wasi.dev/) standard. Produces a `.wasm` that runs under any wasmtime / wasmer / WasmEdge host with no extra glue. wasip1 was renamed from `wasi` in Go 1.21 (matching the upstream naming change).

Differences:

| Feature | js/wasm | wasip1/wasm |
|---------|---------|-------------|
| `os.ReadFile` | requires `fs.readFile` shim in `wasm_exec.js` | works directly under wasmtime's `--dir=...` |
| `net/http` server | not possible (no `net.Listen`) | not possible (wasip1 has no socket API) |
| `net/http` client | works via the browser's `fetch` API + a shim | not possible (wasip1 has no socket API) |
| `time.Now` | works via `Date.now()` | works via wasi's `clock_time_get` |
| `runtime.GOMAXPROCS` | always 1 | always 1 |
| cgo | not supported | not supported |
| goroutines | cooperative; single OS thread; goroutines yield via scheduler points | same |

The cooperative-single-thread constraint means streams and agents work but never parallelise. Phase 9 / 10 fixtures targeting wasm exclude any parallelism assertion.

## Vendor vs proxy mode

By default the driver writes `vendor/dev.mochilang/runtime/go/...` into every emitted module and invokes `go build -mod=vendor`. This makes the build offline-capable: no `proxy.golang.org` contact required.

`Driver.NoVendor=true` switches to proxy mode. The emitted `go.mod` lists `dev.mochilang/runtime/go v1.x.y` as a normal `require`; `go build` resolves the dependency from `~/go/pkg/mod` (filling from the proxy if needed). This is faster for incremental builds (no vendor copy) but requires a populated module cache.

## Why no TinyGo for the default path

[TinyGo](https://tinygo.org/) (2018-present) is a separate Go compiler producing LLVM IR; it targets microcontrollers (ATSAMD, STM32, ESP32, RP2040) and wasm with smaller binaries than gc. TinyGo binary size on wasm32 is typically 5-50x smaller than gc.

The trade: TinyGo does not support the full Go stdlib. Notable gaps:

- No `reflect.Type.NumMethod`, no `reflect.Value.Call`. Breaks any reflection-based helper (`mochiruntime.AnyEqual` uses `reflect.DeepEqual`).
- Limited `encoding/json` (works but slow; no streaming decoder).
- No `net/http` server. Limited `net/http` client (depends on target).
- Limited goroutines (cooperative scheduling, no preemption). Channels work.
- No cgo (on most targets).

For Phase 17 wasm fixtures we hit at least the reflect gap. So TinyGo is out for the default. It is a candidate sub-target for Phase 17.x once the source-language surface that uses reflect is gated off.

## CI matrix

The phase gate runs in CI as a matrix of (OS, Go version):

```yaml
strategy:
  matrix:
    os: [ubuntu-24.04, macos-15, windows-2025]
    go: ['1.26.0', '1.26.x']
```

The cross-arch targets (`linux/arm64`, `darwin/amd64`, etc.) are exercised from the ubuntu-24.04 runner via `GOOS=... GOARCH=... go build` cross-compile. The compiled binary is not executed there — execution gating for cross-arch is Phase 16.x.

The wasm gate uses wasmtime 26 installed via `cargo install wasmtime-cli`; if absent the wasm fixtures skip.

## Reach by region

Go's installed base is concentrated in the cloud-native ecosystem. CNCF's 2025 annual survey reported >70% of CNCF-graduated projects are dominantly Go. Go is also the default Lambda Custom Runtime language (AWS), the default Cloud Functions language (GCP), and the default Azure Function language (Microsoft) for low-cold-start serverless. A Mochi program compiled to Go reaches every cloud where serverless is a deployment option.

## Reach by deployment shape

| Deployment shape | Go fit | Notes |
|------------------|--------|-------|
| Single static binary on Linux server | A+ | The default `go build` produces this. |
| Sidecar in a Kubernetes pod | A+ | Same. Plus the entire k8s API is Go. |
| Lambda function | A | `GOOS=linux GOARCH=arm64 go build` produces a Lambda-compatible binary for the arm64 runtime. |
| Cloud Run / Knative serverless | A | Distroless container with a single binary. |
| Edge worker | B+ | wasip1 fits Cloudflare Workers' WASI runtime; latency varies. |
| Browser web app | C | Wasm binary size (~3-5MB minified with gc) is heavy for browsers; TinyGo helps but loses reflect. |
| Mobile (iOS / Android) | C | `gomobile bind` exists but mobile is better served by MEP-49 (Swift) and MEP-50 (Kotlin). |
| Embedded MCU | D | TinyGo only; out of scope for default path. |
