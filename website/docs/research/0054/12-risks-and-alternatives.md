---
title: "12. Risks and alternatives"
sidebar_position: 13
sidebar_label: "12. Risks"
description: "Risk register (macOS LC_UUID, wasm-js no fetch glue, wasip1 no cgo, cassette drift, generic-method gap, hermetic-build gap, vet false positives) plus rejected alternatives."
---

# 12. Risks and alternatives

This note enumerates the risks identified during the MEP-54 design pass and the alternatives that were considered and rejected.

## Risk register

### R1: macOS LC_UUID non-determinism

**Symptom:** Two `Driver.Build` invocations on macOS produce binaries that differ in exactly the `LC_UUID` load command (16 bytes).

**Cause:** Apple's `ld64` linker generates a random UUID per link. The UUID is used by Crash Reporter to correlate crash logs with debug symbols. There is no rustc-equivalent `-no-uuid` flag for Go on darwin.

**Mitigation:** Phase 16 platform-skips the macOS reproducibility gate. The Linux and Windows gates are enforced. Users who need bit-reproducible darwin binaries can run a post-link patcher that zeroes the LC_UUID payload (`ldid -S` works, but is itself a third-party tool).

### R2: Browser wasm has no `net/http` server

**Symptom:** A Mochi program that uses `httpGet` works on Linux but produces a runtime "operation not supported" error on `GOOS=js GOARCH=wasm`.

**Cause:** The browser's wasm runtime does not provide a `net.Listen` or `net.Dial` capability. Go's `net/http` client falls back to the browser's `fetch` API via a shim in `wasm_exec.js`, but only for HTTPS to same-origin or CORS-allowed hosts.

**Mitigation:** Phase 17 fixtures targeting `wasm-js` exclude `httpGet`. Documentation flags the restriction. A future sub-phase may ship a `MOCHI_FETCH_GLUE` shim that wires `httpGet` to the browser's `fetch` directly, but that requires JS-side cooperation we do not have for the default test harness.

### R3: wasip1 has no cgo

**Symptom:** A Mochi program that uses `import "C"` fails to compile under `GOOS=wasip1 GOARCH=wasm`.

**Cause:** wasip1 has no syscall surface for cgo; the Go toolchain rejects cgo for that target tuple.

**Mitigation:** Phase 12 fixtures detect cgo usage at the lower stage and skip when the target is wasip1. The skip is at fixture-discovery time, not build-failure time.

### R4: LLM cassette drift

**Symptom:** A fixture using `generate openai { prompt: "..." }` passes when the cassette was first recorded but fails after a prompt edit because the SHA-256 key changed.

**Cause:** The cassette is keyed by `sha256(provider + ":" + model + ":" + prompt)`. Any character change in the prompt produces a different key, so the cassette lookup misses and the runtime helper returns the empty string.

**Mitigation:** Fixtures pin the exact prompt string and commit the cassette file alongside the `.mochi` source. The Phase 13 documentation flags the brittleness. Re-recording requires `MOCHI_RECORD_CASSETTES=1` env var so accidental re-records are prevented.

### R5: Generic-method gap (Go 1.21)

**Symptom:** A runtime helper that we would naturally write as a method on a generic type (e.g., `func (s *Stream[T]) Map[U](f func(T) U) *Stream[U]`) cannot be expressed because Go 1.21 does not allow generic methods.

**Cause:** Go's generics design intentionally excludes generic methods to preserve interface compatibility. The Go team has signalled openness to adding them in a future version but it has not landed as of 2026-05.

**Mitigation:** Every generic helper is a free function (`func Map[T, U any](s *Stream[T], f func(T) U) *Stream[U]`). This works but produces less idiomatic call sites (`Map(stream, f)` instead of `stream.Map(f)`). The lowerer adapts by always emitting the free-function form.

### R6: `go build` hermeticity gap

**Symptom:** `Driver.Build` reads `$HOME/.netrc` (for proxy auth) and `$GIT_TERMINAL_PROMPT` (for prompt suppression) from the user's environment. A hostile `.netrc` could exfiltrate the build to a malicious proxy.

**Cause:** `go build` invokes `git fetch` internally to resolve module dependencies; `git` reads `.netrc` for HTTP auth.

**Mitigation:** Vendor mode is on by default; in vendor mode `go build` does not invoke `git fetch` and does not read `.netrc`. Proxy mode is opt-in via `Driver.NoVendor=true` and the user accepts the auth-config risk in that mode. The driver also clears `GOPATH` and `GOCACHE` to per-build directories so a hostile module cache cannot poison the build.

### R7: `go vet` false positives on lowered code

**Symptom:** A lowered function triggers a `go vet` warning that is not actually a bug (e.g., "self-assignment" on `x = x` synthesised by a sum-type variant unpack).

**Cause:** Go vet does not understand the lowering context; it reasons about the emitted source as if it were hand-written.

**Mitigation:** The lowerer avoids known-triggering patterns. When a true positive is impossible to avoid (e.g., the synthesised `_ = m` after an agent message handler unpacks all fields), the harness's `MOCHI_GO_VET_ALLOW` env var skips specific warnings without disabling vet entirely.

### R8: Channel capacity overflow

**Symptom:** `make_chan(N)` with a very large `N` allocates more memory than expected at the runtime helper layer.

**Cause:** Go's `make(chan T, cap)` is O(1) in capacity (it just records the cap), but if the capacity is `INT_MAX` and the channel fills, the buffer holds `INT_MAX` elements of `T`, which can exhaust memory.

**Mitigation:** The lowerer caps channel capacity at `1 << 24` (16M elements); larger requests are rejected at lower time with a clear error. This is a soft limit that can be overridden via `MOCHI_MAX_CHAN_CAP` env var.

### R9: Cross-module name collisions

**Symptom:** A Mochi program imports two modules that both export a type named `Foo`; the Go lowering produces two `type Foo` declarations in the same package.

**Cause:** Mochi's namespace model allows the imports to live in distinct module namespaces; Go's package model flattens everything into one namespace.

**Mitigation:** The lowerer mangles colliding names with a module-derived prefix (`module1_Foo`, `module2_Foo`). Phase 4 wires this. The mangling is deterministic so re-builds produce the same names.

### R10: Cassette directory not set

**Symptom:** `generate openai { ... }` returns the empty string with no clear indication why.

**Cause:** `MOCHI_LLM_CASSETTE_DIR` env var is unset, so the cassette lookup short-circuits to the empty-string fallback.

**Mitigation:** The runtime helper writes a one-line stderr diagnostic on the first cassette-miss per process: `mochi: LLM cassette dir not set ($MOCHI_LLM_CASSETTE_DIR); returning empty string`. The diagnostic is suppressed after the first emit so chatty fixtures do not flood the log.

## Alternatives considered

### A1: Use `go/ast` for code generation

**Why considered:** Stdlib AST, no separate maintenance burden.

**Why rejected:** `go/ast` is designed for parsing existing Go, not for synthesising new Go. Every node has a `token.Pos` field that must be populated for `go/printer` to produce sensible output. The structural `gotree` is shaped for synthesis (no position fields; positions are reconstructed by the renderer). See [[codegen-design]] and [[prior-art-transpilers]].

### A2: Use `text/template` for code generation

**Why considered:** Simpler than a structural AST; widely used by `protoc-gen-go`, `sqlc`, `stringer`.

**Why rejected:** Template-based emitters accumulate whitespace bugs. The "fix one bug, break two others" failure mode is well-documented in the kubebuilder issue tracker. Structural AST avoids the class entirely.

### A3: Use `sync.Mutex`-wrapped queues for channels

**Why considered:** Direct port of the C target's lowering.

**Why rejected:** Go's native `chan T` is exactly the right primitive. Wrapping it would add a synchronisation cost the source language does not require.

### A4: Use TinyGo as the default backend

**Why considered:** TinyGo produces smaller wasm binaries (5-50x smaller than gc) and supports more embedded targets.

**Why rejected:** TinyGo does not support the full Go stdlib. Notable gaps: reflect (used by `mochiruntime.AnyEqual`), `encoding/json` (limited), `net/http` (limited). TinyGo is a Phase 17.x sub-target for users who need it; not the default.

### A5: Inline the runtime per-emission

**Why considered:** No external dep, single-file builds work without `go.mod`.

**Why rejected:** Makes generated code untraceable under `go doc`. Breaks `go install` for users who want to consume Mochi-emitted Go programs the standard way. Inflates the per-fixture diff because every emit ships a copy of the runtime.

### A6: Skip the published runtime module

**Why considered:** Simpler distribution (no pkg.go.dev story).

**Why rejected:** Same reasons as A5. Publication is one git tag per release; the cost is negligible and the discoverability gain is large.

### A7: Use a green-thread library for agents

**Why considered:** Could give finer-grained control over scheduling than goroutines.

**Why rejected:** Goroutines are already cooperatively scheduled by the Go runtime with M:N to OS threads. A green-thread library would duplicate that work without adding capability. We use `errgroup`-style supervision internally as an implementation detail in the agent runtime, but the lowering pattern is plain `go` statements.

### A8: Use `go/printer` directly without re-formatting

**Why considered:** Skips one parse-print pass.

**Why rejected:** `go/printer` is finicky about leading comments, trailing semicolons in single-line composite literals, and tab vs space rendering. `go/format.Source` (the function `gofmt` uses) handles all of these by re-parsing and re-printing. The cost is one extra parse; the win is no whitespace bugs.

### A9: Lower to native Go generics throughout

**Why considered:** Smaller emitted code (one generic function instead of one per instantiation).

**Why rejected:** Monomorphisation runs in clower (shared with C / Rust / etc.); duplicating that pass for Go alone would diverge the cross-target semantics. Generic helpers in the runtime module (`mochiruntime.Map[T, U]`) are an exception: they are runtime helpers, not lowered code, so they can be generic without affecting the cross-target shape.

### A10: Skip the deterministic build flags by default

**Why considered:** `Driver.Deterministic=true` is opt-in; default builds produce timestamps and build IDs.

**Why rejected:** Reproducibility is on a per-call basis: `Driver.Build` callers who want it set `Deterministic=true`, callers who want a normal dev build do not. This is the same shape as MEP-53. Always-on determinism would surprise users debugging via `go tool objdump` (the stripped symbol table makes objdump less useful).

## Future candidates

Risks marked deferred-mitigation become candidates for future sub-phases:

- **F1:** Browser wasm `fetch` shim (Phase 17.x). Wire `httpGet` to the browser's `fetch` API.
- **F2:** Darwin LC_UUID post-link patcher (Phase 16.x). Bundle an `ldid -S`-equivalent in the driver so darwin reproducibility lights up.
- **F3:** TinyGo sub-target for embedded wasm (Phase 17.x). Gate the source surface that does not use reflect or `encoding/json`.
- **F4:** Generic method support (depends on upstream Go). When generic methods land in Go, the runtime helpers move to method form for ergonomics.
- **F5:** Sandboxed `go build` (Phase 16.x). Run `go build` inside a Linux user namespace so the hermetic-build gap (R6) is closed structurally rather than by convention.
