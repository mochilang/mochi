---
title: "08. Goroutine bridge"
sidebar_position: 9
sidebar_label: "08. Goroutine bridge"
description: "The cgo boundary cost, the Go runtime scheduler inside the c-archive, the channel-as-handle pattern, the callback-as-handle pattern, the cgo.Handle lifetime story, why no runtime singleton is needed unlike MEP-73's tokio singleton."
---

# 08. Goroutine bridge

This note details how the bridge exposes Go's first-class concurrency (goroutines, channels, `select`) to Mochi. The story is materially simpler than MEP-73's Rust async bridge because Go's runtime ships with the goroutine scheduler built in.

## No runtime singleton

MEP-73 §08 documents an entire singleton pattern: a process-wide `tokio::runtime::Runtime` constructed lazily on first async-rust call, with the runtime's `block_on` dispatching every async call. The Rust async story requires a host executor.

The Go story does not. Every Go binary, including c-archives, ships the goroutine scheduler. When Mochi cgo-calls into a wrapper-exported function:

- The cgo call acquires a per-call goroutine on the Go side.
- The function runs inside the Go runtime's scheduler.
- If the function spawns goroutines (`go some_func()`), the scheduler picks them up automatically.
- If the function blocks on a channel receive (`<-ch`), the scheduler parks the goroutine.
- When the function returns, the cgo call returns.

The bridge has no runtime to construct. No `OnceLock<Runtime>`. No lazy init.

## The cgo boundary cost

Each cgo call from Mochi into Go costs ~200ns on darwin-arm64 (Go 1.23, May 2026, measured on Apple M3). Breakdown:

- ~50ns: cgo state machine transition (push the C call frame, switch to the Go execution context).
- ~100ns: per-call goroutine acquire from the goroutine pool (Go reuses goroutines from a per-thread pool; the first call on a thread is slightly slower).
- ~50ns: argument marshalling for the call (varies with argument count and size).

Going the other direction (Go callback into Mochi via a registered handle) adds another ~150ns. So a Mochi callback invoked from inside a Go function costs ~350ns per round-trip.

The cost is materially higher than Mochi-native calls (~5ns). Programs that hot-loop across the cgo boundary pay this per iteration. The mitigation is the batched-variant wrapper.

## The batched-variant wrapper

For functions identified as hot-path candidates (the synthesiser inspects the user's Mochi source for `for x in xs { go_fn(x) }`-shaped patterns), the bridge offers a batched variant:

```go
// Original signature
func ProcessOne(x int64) int64

// Synthesised batched variant
//export mochi_go_<module>_ProcessOne_batched
func ProcessOne_batched(xs []int64) []int64 {
    out := make([]int64, len(xs))
    for i, x := range xs {
        out[i] = ProcessOne(x)
    }
    return out
}
```

The Mochi side calls the batched variant with a slice; the per-call cgo cost is amortised across the batch. A 1M-element batch pays one cgo crossing instead of 1M.

The batched variant is opt-in via a `[go.batched]` table in `mochi.toml`:

```toml
[go.batched]
items = [
    "github.com/example/processor.ProcessOne",
    "github.com/example/hasher.Hash",
]
```

## Channel handle pattern

Go's `chan T` cannot be passed directly across the cgo boundary (cgo only handles primitive C types and pointers). The bridge exposes channels as opaque cgo handles via the `runtime/cgo` package (in-stdlib since Go 1.17, August 2021).

```go
// Wrapper for a Go function returning a chan.
//export mochi_go_<module>_Tick
func mochi_go_time_Tick(duration int64) uint64 {
    ch := time.Tick(time.Duration(duration))
    return acquireHandle(ch)
}

//export mochi_go_<module>_chan_recv_time
func mochi_go_chan_recv_time(handle uint64) (int64, bool) {
    ch := resolveHandle(handle).(<-chan time.Time)
    t, ok := <-ch
    if !ok { return 0, false }
    return t.UnixNano(), true
}

//export mochi_go_<module>_chan_close
func mochi_go_chan_close(handle uint64) {
    releaseHandle(handle)
}
```

The Mochi side gets a `stream<int>` whose iterator calls `mochi_go_<module>_chan_recv_<elem-type>` per element and stops on `ok = false`. The closing of the stream calls `mochi_go_<module>_chan_close`.

### Handle lifetime

The handle pool is a `sync.Map[uint64]any` keyed by a monotonically incrementing uint64. The wrapper module's `mochi_rt.go` file holds:

```go
var (
    handlesMu sync.Mutex
    handles   = map[uint64]cgo.Handle{}
    nextID    uint64
)

func acquireHandle(v any) uint64 {
    handlesMu.Lock()
    defer handlesMu.Unlock()
    nextID++
    h := cgo.NewHandle(v)
    handles[nextID] = h
    return nextID
}

func releaseHandle(id uint64) {
    handlesMu.Lock()
    h, ok := handles[id]
    if ok { delete(handles, id) }
    handlesMu.Unlock()
    if ok { h.Delete() }
}
```

The `cgo.NewHandle` is the canonical Go-stdlib way to pin a Go value across the cgo boundary (the standard alternative, `runtime.SetFinalizer`, is less safe). The handle keeps the value alive (preventing GC) until `Delete()` is called.

A handle leaked (the Mochi side forgets to call `_close`) is recovered at process exit when the c-archive is torn down. During process lifetime, a leak accumulates one slot in the `handles` map per leak. The `[go.goroutine-bridge.max-handles]` soft limit (default 4096) is the back-pressure: when the pool size exceeds the limit, new acquires fail.

## Callback handle pattern

Go functions taking `func` parameters (`func(...) ...`) receive Mochi callbacks via the symmetric handle pattern:

```go
//export mochi_go_<module>_Walk
func mochi_go_filepath_Walk(root *C.char, fnHandle uint64) C.int {
    fn := resolveHandle(fnHandle).(func(string) error)
    err := filepath.Walk(C.GoString(root), func(path string, info fs.FileInfo, err error) error {
        return fn(path)
    })
    if err != nil { return cgoErrorCode(err) }
    return 0
}
```

The Mochi side does:

1. Construct a callback closure as a Mochi `fun` value.
2. Register it via `mochi_go_<module>_callback_register_<sig>(closurePtr)` which returns a handle.
3. Call `mochi_go_filepath_Walk(root, callbackHandle)`.
4. Inside Go, every `filepath.Walk` callback invocation dispatches back through `_call_<sig>(handle, args...)` which the Mochi side handles by calling the registered closure.
5. After the outer call returns, the Mochi side calls `mochi_go_<module>_callback_release(handle)` to clean up.

The bridge inserts the `register` and `release` calls into the synthesised shim file automatically; the user does not write any of this plumbing.

## `select` and multi-channel coordination

Go's `select` statement multiplexes over multiple channels. The bridge does not expose `select` directly to Mochi (it would require the Mochi side to express the multiplexing intent). Instead, the wrapper for any function internally using `select` is opaque: the function returns when its `select` completes, and the Mochi side sees only the function's result.

The Mochi side can do its own multiplexing via `stream<T>` selection (Mochi's streams have a `select(...)` combinator), but only over Mochi-side streams. Cross-language multiplexing (one Mochi stream and one Go channel) is not supported.

## Goroutine spawning

Mochi's `spawn <expr>` lowers to `go <expr-shim>()` in the wrapper when `<expr>` calls a Go-exported function. The Go runtime schedules the spawned goroutine; the Mochi side gets back a `Future<T>` that resolves when the goroutine completes.

The implementation: the wrapper exposes a `spawn_<fn>` variant for each exported `fn`:

```go
//export mochi_go_<module>_<fn>_spawn
func mochi_go_<module>_<fn>_spawn(args ...) uint64 {
    resultCh := make(chan resultType, 1)
    go func() {
        resultCh <- <fn>(args...)
    }()
    return acquireHandle(resultCh)
}

//export mochi_go_<module>_<fn>_await
func mochi_go_<module>_<fn>_await(handle uint64) resultType {
    ch := resolveHandle(handle).(chan resultType)
    r := <-ch
    releaseHandle(handle)
    return r
}
```

The Mochi side's `Future<T>` wraps the handle and exposes `.await()` which calls `_await`.

## Concurrent execution semantics

Multiple Mochi-side cgo calls into the same wrapper run concurrently inside the Go runtime's scheduler. Go's scheduler picks the right number of OS threads (controlled by `GOMAXPROCS`, default = `runtime.NumCPU()`). The bridge does not need to coordinate; the Go runtime handles it.

Two caveats:

- **GC stop-the-world events.** A Go GC pause (~ms scale on a typical wrapper module) blocks all cgo calls into the wrapper. The pause frequency depends on the wrapper's allocation pressure.
- **Goroutine leaks.** A goroutine spawned by a wrapper function that never returns leaks. The bridge documents the lifetime contract per spawned-goroutine variant.

## Cross-references

- [[02-design-philosophy]] §4 for why no runtime singleton.
- [[05-type-mapping]] for the `chan` and `func` mappings.
- [[09-abi-stability]] for the cgo boundary contract.
- [[12-risks-and-alternatives]] §R1 for the cgo cost risk.
- [The runtime/cgo package documentation](https://pkg.go.dev/runtime/cgo) for the handle pattern.
- [MEP-73 §08](/docs/research/0073/08-async-bridge) for the sister Rust-bridge tokio singleton.
