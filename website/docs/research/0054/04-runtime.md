---
title: "04. Runtime building blocks"
sidebar_position: 5
sidebar_label: "04. Runtime"
description: "The dev.mochilang/runtime/go module: agent, collections, datalog, llm, option, query, result, stream, stringz, timez. Why zero third-party deps in the default build."
---

# 04. Runtime building blocks

This note describes the `dev.mochilang/runtime/go` Go module that supports the Mochi-to-Go transpiler. The module ships at Apache-2.0, ~1200 LOC of Go across ~10 packages, with zero third-party deps in the default build.

## Module layout

```
dev.mochilang/runtime/go/
├── go.mod                       // module dev.mochilang/runtime/go
├── doc.go                       // package-level overview
├── agent/                       // agent supervisor + restart policy
├── collections/                 // Map, Filter, Reduce, Sort, OMap
├── datalog/                     // semi-naive runtime fallback
├── llm/                         // cassette dispatch
│   ├── llm.go                   // base package: cassette replay
│   └── openai/                  // provider plugin (separate sub-module)
├── option/                      // Option[T] helper
├── query/                       // join / group-by / top-K helpers
├── result/                      // Result[T, E] helper
├── stream/                      // bounded broadcast stream
├── stringz/                     // Substring, ReverseString, RuneAt
├── timez/                       // deterministic Now (overridable via MOCHI_NOW_NS)
└── *.go                         // panic/recover helpers, Str, AbsI64, DivI64, ModI64, file I/O, CSV, JSON, fetch
```

## Why zero third-party deps in the default build

Three reasons:

1. **`go install` works offline after first install.** A user running `go install dev.mochilang/runtime/go/cmd/...@latest` once gets the full runtime cached in `~/go/pkg/mod`. No transitive dep resolution chain that breaks when the user is on a flight.

2. **`govulncheck` surface is minimal.** Every additional dep is another CVE-rotation surface. Sticking to stdlib (which has a single CVE rotation surface across the entire toolchain) keeps the security surface small.

3. **Vendor mode is small.** `vendor/` directories that include `github.com/foo/bar` plus 30 transitive deps balloon the emitted module to 100MB. Sticking to stdlib keeps the runtime vendor at ~1200 LOC, ~80KB.

The exception is the `llm/openai/` sub-module which depends on `github.com/openai/openai-go`. This is a separate Go module (different `go.mod`), so a Mochi program that never uses `generate openai { ... }` does not pull the SDK.

## Package descriptions

### `agent/`

The agent supervisor. Each Mochi agent has a corresponding `*FooAgent` struct with an `in chan FooMsg` field. The supervisor wraps `New<AgentType>()` to add a restart policy: when the agent goroutine panics, the supervisor restarts it up to N times before propagating. Phase 10 documents the exact restart policy.

The supervisor uses `golang.org/x/sync/errgroup` internally — wait, no it does not, because that would be a third-party dep. The supervisor uses `sync.WaitGroup` + a `chan error` for fan-out; the implementation is ~80 LOC.

### `collections/`

Typed helpers for Mochi's list / map / set / omap. The generic functions:

```go
func Map[T, U any](xs []T, f func(T) U) []U
func Filter[T any](xs []T, p func(T) bool) []T
func Reduce[T, A any](xs []T, acc A, f func(A, T) A) A
func Sort[T cmp.Ordered](xs []T) []T   // returns a sorted copy
func MapKeys[K comparable, V any](m map[K]V) []K
func MapValues[K comparable, V any](m map[K]V) []V
```

`OMap[K, V]` is a small generic struct that pairs a `map[K]V` with an insertion-order `[]K` for deterministic iteration. Phase 7.11 wires it up; the OMap is mutable by design (Mochi's omap is mutable).

### `datalog/`

A runtime fallback for cases where compile-time Datalog evaluation is not viable (currently never; the entire Datalog surface evaluates at compile time per [[dataset-pipeline]]). Phase 8 lands this as a vestigial package; if a future surface (recursive rules with parametric queries) requires runtime evaluation, the implementation goes here.

### `llm/`

Base package handles cassette dispatch. The cassette key is `sha256(provider + ":" + model + ":" + prompt)`; the cassette file lives at `$MOCHI_LLM_CASSETTE_DIR/<sha256-hex>.txt`. If the env var is unset or the cassette is missing, the helper writes a stderr diagnostic and returns the empty string. This matches the cross-target cassette pattern from MEP-45 / MEP-53 / MEP-55.

Provider plugins (`openai/`, `anthropic/`, `google/`) live in separate sub-modules so a Mochi program that uses one provider does not pull the SDKs for the others.

### `option/`, `result/`

Generic `Option[T]` and `Result[T, E]` wrappers. Used for the lowering of Mochi's sum types when the source uses the standard-library `Option<T>` and `Result<T, E>` directly. Custom user-defined sum types lower to their own discriminated interface + variant structs (see [[type-lowering]]); these wrappers are only for the stdlib cases.

### `query/`

Helpers for the query DSL beyond what `slices` and `maps` stdlib cover. Notable:

- `HashJoin[K comparable, L, R any](left []L, right []R, lk func(L) K, rk func(R) K, combine func(L, R) any) []any` — hash-join with a left-side hash table.
- `TopK[T any](xs []T, k int, less func(T, T) bool) []T` — heap-backed top-K via `container/heap`.
- `GroupBy[T any, K comparable](xs []T, key func(T) K) map[K][]T` — group-by with insertion-order preservation via OMap.

The `arena_query` helper from Phase 7.5 allocates a single backing slice and slices into it for each query stage; this avoids the GC pressure of allocating a fresh slice per stage.

### `stream/`

Bounded broadcast stream. The struct holds `subs []chan T` with bounded capacity per subscriber. `Emit(v)` sends to every subscriber; if a subscriber's channel is full, the emit drops (matching the stream-backpressure semantics from MEP-55 §Stream).

### `stringz/`

UTF-8-aware string helpers. `Substring(s string, lo, hi int64) string` slices on rune boundaries, not byte boundaries; `ReverseString(s string) string` reverses by rune; `RuneAt(s string, i int64) rune` indexes by rune. These match the Mochi source-language semantics where string indices are rune-based.

### `timez/`

Deterministic time. `Now() time.Time` reads `MOCHI_NOW_NS` from the environment if set, falling back to `time.Now()` otherwise. This makes fixtures that print timestamps deterministic.

### Top-level package

The top-level `mochiruntime` package exports:

- `Panic(code int64)` — panics with `int64` payload (Mochi's panic code).
- `Recover(f func()) (panicked bool, code int64)` — wraps the try/catch lowering's `defer recover()` block.
- `Str(v any) string` — dispatch on type at lowering time; emits the right `strconv.FormatInt` / `strconv.FormatFloat` / identity for string.
- `AbsI64(n int64) int64`, `DivI64(a, b int64) int64`, `ModI64(a, b int64) int64` — runtime-checked arithmetic.
- `ReadFile`, `Lines`, `AppendFile` — file I/O wrappers with vm3-matching error semantics.
- `LoadCSV`, `SaveCSV` — `encoding/csv` wrappers.
- `JsonDecode` — `encoding/json.Unmarshal` to `map[string]any` with vm3-matching error handling.
- `HttpGet` — `net/http.Get` wrapper.
- `LlmGenerate` — cassette dispatch entry point.

## Vendor strategy

The runtime is vendored into every emitted module under `vendor/dev.mochilang/runtime/go/` so the produced binary builds offline. `Driver.Build` writes a complete `go.mod` + `go.sum` + `vendor/modules.txt` so `go build -mod=vendor` works without contacting the proxy.

Vendor mode is on by default. `Driver.NoVendor=true` switches to proxy mode for users who prefer their own caching; in that case the emitted `go.mod` lists `dev.mochilang/runtime/go v1.x.y` as a normal require and `go build` resolves via `proxy.golang.org`.

## Versioning

The runtime module follows semver via git tags (`v1.0.0`, `v1.1.0`, ...). Mochi-emitted modules pin a specific runtime version that matches the transpiler version that produced them: `mochi build` v1.2.0 emits modules requiring `dev.mochilang/runtime/go v1.2.0`. Cross-version compatibility (a Mochi v1.2 emit running against runtime v1.3) is not guaranteed but tracked as a Phase 18 sub-goal.
