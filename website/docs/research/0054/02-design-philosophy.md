---
title: "02. Design philosophy"
sidebar_position: 3
sidebar_label: "02. Design philosophy"
description: "Why Go, why the 1.21 floor, why native goroutines, why gotree not go/ast, why the runtime module is published."
---

# 02. Design philosophy

This note is opinionated. It records the five load-bearing decisions for MEP-54 and the rejected alternatives. The MEP body cites these as load-bearing.

## Why Go at all

The orthogonal targets cover most of the deployment surface: C (MEP-45) for native single-file binaries with minimal runtime, BEAM (MEP-46) for hot-reload OTP services, JVM (MEP-47) for Maven Central interop, .NET (MEP-48) for NuGet and Windows, Swift (MEP-49) for Apple, Kotlin (MEP-50) for Android, Python (MEP-51) for PyPI and Jupyter, TypeScript (MEP-52) for npm and web, Rust (MEP-53) for crates.io and no_std, PHP (MEP-55) for WordPress, Ruby (MEP-56) for RubyGems. Go's specific niche is cloud-native deployment (Kubernetes, Docker, Terraform, Prometheus, the entire CNCF graduated-project list) plus the easiest cross-compile story of any modern language.

The market signal is overwhelming: every cloud-native sidecar / operator / agent / collector / kubectl-plugin / Terraform-provider in 2026 ships as a Go binary because that is what the ecosystem expects. A Mochi program intended for that surface needs to be a Go binary, not a C binary that happens to run on Linux.

## Why the Go 1.21 floor (production gate on 1.26)

Go 1.21 (August 2023) is the first version with stable `GOOS=wasip1`. We need that for the Phase 17 wasm target. Below 1.21 we would have to ship `GOOS=js` only for the browser case, which excludes wasmtime / wasmer / WasmEdge entirely.

The production gate (1.26.0 and 1.26.x latest) tracks the active stable as of 2026-05. This gives us PGO (1.20+), generics (1.18+, used in `mochiruntime.Map[T, U]`), `slices` and `maps` stdlib packages (1.21+, used heavily in the query lowering), `log/slog` (1.21+, used in the agent supervisor), and the reproducible-by-default flags (1.22+).

We do not gate against Go 1.18 or 1.19 because they predate `slices` / `maps` stdlib and would require manual reimplementation of those helpers in the runtime, doubling the runtime LOC for negligible reach gain.

## Why native goroutines and `chan T`

Mochi's agents and channels map directly onto Go's `go` statement and `chan T` with zero translation tax. Goroutines are stack-managed by the Go runtime: per-agent cost is ~2KB at spawn, growing as needed. 10,000 agents fit in ~20MB total runtime memory. Compare:

- **Rust (MEP-53):** single-thread `Rc<RefCell<VecDeque<T>>>` because there is no built-in M:N scheduler; pulling in tokio would be a runtime tax the source language does not require.
- **Python (MEP-51):** `asyncio.Queue` with cooperative event loop; no preemption, requires explicit `await` points.
- **TypeScript (MEP-52):** `AsyncIterableQueue` + `AbortController`; same cooperative model as Python.
- **PHP (MEP-55):** userland Channel<Message> with synchronous receive loop because PHP has no preemptive scheduler.
- **Ruby (MEP-56):** `Thread::SizedQueue` + `Thread.new`, similar to Go but with the GIL forcing serial execution.

Go is the only target where the source-language concurrency model translates 1:1 to the runtime's native concurrency model with no compromise. This is a structural reason to ship a Go target even if every other target were already shipping.

## Why `gotree` not `go/ast`

Go's stdlib `go/ast` is the AST that `go/parser` produces, designed for analysing existing Go code. Constructing a valid `go/ast.File` from scratch requires populating dozens of position fields (`token.Pos`) for every node so `go/printer` produces sensible output, plus a `token.FileSet` to back the positions.

The structural `gotree` is shaped for synthesis: no `token.Pos` field on any node; positions are reconstructed by the renderer. The renderer pipes through `go/format.Source` (the same formatter that `gofmt` uses) so the output is canonically formatted by construction. This gives us:

1. **Compact node types.** A `gotree.FuncDecl` has 6 fields (Name, Params, Returns, Body, Receiver, Doc); the equivalent `ast.FuncDecl` has 5 fields plus a recursive `token.Pos` chain across every child.
2. **No `go/printer` whitespace bugs.** `go/printer` is finicky about leading comments, trailing semicolons in single-line composite literals, and tab vs space rendering. `go/format.Source` handles all of these automatically by re-parsing and re-printing.
3. **Free `gofmt` correctness.** The output passes `gofmt -l` clean by construction. Any divergence from canonical form is a renderer bug, not a generator bug.

The same structural-AST pattern is used by the Rust target (`rtree`, MEP-53) and the Ruby target (`rtree`, MEP-56). It is a portable design.

## Why the runtime module is published

`dev.mochilang/runtime/go` (Apache-2.0) is a real Go module published to the public module proxy, not an inlined helper library bundled per emission. Three reasons:

1. **`go install` works.** A user can `go install dev.mochilang/runtime/go/cmd/...@latest` and get a working binary from a published Mochi program with no extra setup. An inlined runtime would force every emitted module to ship a copy of the helpers, which `go doc` would render as opaque code.
2. **`go get` works.** A user can `go get dev.mochilang/runtime/go/collections` and use the typed `Map`, `Filter`, `Reduce` helpers from any Go program. This makes the runtime useful beyond the Mochi-emitted-Go path.
3. **pkg.go.dev indexes the runtime.** Documentation, version history, license, vulnerability database (`govulncheck`) coverage all come for free.

The runtime is vendored into every emitted module under `vendor/dev.mochilang/runtime/go/` so the produced binary builds offline without hitting `proxy.golang.org`. Vendor mode is on by default; `Driver.NoVendor=true` switches to proxy mode for users who prefer their own caching.

## Rejected alternatives

- **`go/ast` for emission.** Rejected for the position-field reasons above.
- **`text/template` for emission.** Rejected for the whitespace-bug class. Template engines accumulate "fix one bug, break two others" failures.
- **`sync.Mutex`-wrapped channels.** Rejected; Go's `chan T` is exactly the right primitive.
- **TinyGo as the default backend.** Rejected; TinyGo is a separate compiler with limited reflect / cgo / concurrency support. It is a great Phase 17 sub-target for embedded but not the default.
- **A `Source` enum with multiple emit strategies.** Rejected; one canonical emit (gotree → `go/format`) is simpler. Variations live behind `Driver.Target`, not behind alternative emitters.
- **Inlined runtime.** Rejected for the publish-story reasons above.
- **Green-thread library** (e.g., `golang.org/x/sync/errgroup`). Rejected for direct lowering; the agent supervisor uses `errgroup` internally as an implementation detail but the lowering pattern is plain `go` statements.
