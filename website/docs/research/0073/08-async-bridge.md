---
title: "08. Async bridge"
sidebar_position: 9
sidebar_label: "08. Async bridge"
description: "The tokio runtime singleton, current-thread vs multi-thread flavour selection, block_on cost analysis, cancellation semantics across the FFI boundary, and the cross-runtime mismatch problem when an imported crate is written against async-std or smol instead of tokio."
---

# 08. Async bridge

This note documents how the bridge surfaces Rust `async fn` items into Mochi's synchronous call surface. Mochi v1 does not have a native async surface; an `async fn` on the Rust side translates to a synchronous Mochi `extern fn` whose wrapper blocks on a tokio runtime.

## The tokio runtime singleton

The wrapper crate owns a process-wide tokio runtime, created lazily:

```rust
use std::sync::OnceLock;
use tokio::runtime::{Runtime, Builder};

static RUNTIME: OnceLock<Runtime> = OnceLock::new();

pub fn runtime() -> &'static Runtime {
    RUNTIME.get_or_init(|| {
        Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("tokio runtime init")
    })
}
```

The `enable_all()` flag enables the IO driver, the time driver, and the signal driver. The wrapper invokes the runtime via `runtime().block_on(<future>)` from the synchronous extern fn entry point.

The singleton model is required because the Mochi side has no notion of "this call site is inside an async context". Every async-fn call from Mochi enters at the same blocking boundary. A per-call runtime would re-allocate the IO driver, the time driver, and the thread pool on every call, which is prohibitively expensive (tens to hundreds of milliseconds per call on a cold runtime).

## current-thread vs multi-thread

The default flavour is current-thread: a single-threaded executor that runs futures inline on the calling thread. The trade-offs:

| Property | current-thread | multi-thread |
|----------|----------------|--------------|
| Startup cost | ~50 microseconds | ~5 milliseconds (worker spawn) |
| Memory | ~1 KiB | ~32 KiB per worker x N workers |
| Parallelism | No (one task at a time) | Yes |
| `tokio::spawn` from inside the future | OK (runs on the same thread) | OK (distributed across workers) |
| `block_on` inside a spawned task | Deadlock risk | Deadlock risk (avoid) |
| Suitable for | Most IO-bound workloads | CPU-bound parallelism, multi-task fanout |

The user opts into multi-thread via:

```toml
[rust.runtime]
flavor = "multi-thread"
worker-threads = 4   # default: num_cpus
```

The wrapper then builds:

```rust
Builder::new_multi_thread()
    .worker_threads(4)
    .enable_all()
    .build()
```

For most imported-crate workflows (a few async-fn calls per request, IO-bound), current-thread is appropriate. The multi-thread mode pays its keep only when the imported crate internally spawns many concurrent tasks.

## block_on cost analysis

The per-call cost of `runtime().block_on(future)` after the runtime is warm:

- ~200 nanoseconds: the block_on machinery (park / unpark, future polling loop).
- IO costs are paid by the future itself, not the block_on.
- Time-driver wake-ups: ~50 nanoseconds per wake.

This is acceptable for any IO-bound call. For a CPU-bound async fn (rare in practice), the block_on cost is negligible against the body cost.

A pathological case is calling block_on in a tight loop from Mochi: each call re-enters the runtime, polls once, returns. The overhead amortises to ~200 ns/call, which is fine for a CLI-tool workload but not for a sub-millisecond hot path. The user should batch via a single-call API in such cases.

## Cancellation semantics

Mochi has no native cancellation primitive (no `Future::abort`, no `select!`, no `CancellationToken`). The wrapper therefore offers no cancellation: once `block_on` is entered, the call runs to completion or to a panic.

This is a deliberate v1 limitation. The alternatives considered:

- Surface tokio's `CancellationToken` as an opaque Mochi handle. Rejected: Mochi has no way to express "abort this in-flight call from another goroutine"; the typical use case for cancellation does not exist on the Mochi side yet.
- Time-bound each call with a `tokio::time::timeout` based on a manifest setting. Rejected: a per-call timeout is the wrong shape; it should be per-call-site.
- Surface a `mochi_<crate>_cancel(handle)` extern that signals cancellation. Rejected: requires a Mochi-side handle type for in-flight calls, which is itself a substantial design problem.

The user can wrap a cancellable call by hand-authoring an `extern fn` override that takes a tokio timeout duration:

```toml
[[rust.extern]]
item = "reqwest::Client::get"
signature = """
extern fn http_get(url: string, timeout_ms: int): string from rust "wrapper::http_get_with_timeout"
"""
```

The hand-authored `wrapper::http_get_with_timeout` then calls `tokio::time::timeout(Duration::from_millis(timeout_ms), client.get(url).send())`.

## Cross-runtime mismatch

A subset of Rust async crates ships against async-std or smol instead of tokio. Such crates cannot run on a tokio runtime: their IO drivers conflict, and a tokio reactor cannot drive an async-std future.

The bridge detects this at ingest time by scanning the crate's `Cargo.toml` for known async-runtime dependencies:

```
$ mochi pkg add rust async-std-only-crate
[1/3] Resolving versions ... 0.4.2
[2/3] Downloading .crate ... 23 KB
[3/3] Verifying Sigstore bundle ... OK
ERROR: async runtime mismatch
  Crate: async-std-only-crate@0.4.2
  Detected async-std dependency. MEP-73 only supports tokio-based crates.
  Resolution: use the tokio-bridge variant (if upstream offers one) or
              file an override at https://github.com/mochilang/mochi/issues/new.
```

For crates that conditionally support tokio via a feature flag (some crates expose `features = ["tokio-runtime"]`), the user opts in:

```toml
[rust-dependencies]
some-crate = { version = "1.0", features = ["tokio-runtime"], default-features = false }
```

The bridge does not attempt to auto-bridge between runtimes; the cross-runtime compatibility layer is too fragile to maintain.

## Streams and channels

Tokio `Stream<Item = T>` items (e.g., `tokio::sync::mpsc::Receiver<T>`) are not directly surfaced in v1. The wrapper layer can expose a manual extern that drains the stream into a Vec:

```rust
pub fn drain_to_vec(rx: &mut Receiver<i64>) -> Vec<i64> {
    runtime().block_on(async move {
        let mut out = Vec::new();
        while let Some(v) = rx.recv().await {
            out.push(v);
        }
        out
    })
}
```

Mochi sees `extern fn drain_to_vec(rx: Receiver): list<int>`. The Receiver is held as an opaque handle (see [[09-abi-stability]] §3 for handle lifetime).

A future sub-phase (post-v1) can introduce a Mochi async surface (`async fn` / `await`), at which point streams can be exposed natively. The wrapper layer is forward-compatible: the synchronous block_on entry can be replaced by an async-on-async pass-through.

## Spawn and JoinHandle

`tokio::spawn(future) -> JoinHandle<T>` does not translate to Mochi because the JoinHandle's `await` requires an async context Mochi lacks. The bridge skips the item with SkipFuture.

A user who needs spawn semantics can hand-author an extern that spawns into the runtime's task set and returns an opaque task ID:

```rust
static TASKS: OnceLock<Mutex<HashMap<u64, JoinHandle<i64>>>> = OnceLock::new();
static NEXT_ID: AtomicU64 = AtomicU64::new(0);

pub fn spawn_compute(input: i64) -> u64 {
    let id = NEXT_ID.fetch_add(1, Ordering::SeqCst);
    let handle = runtime().spawn(async move {
        // ... compute ...
        input * 2
    });
    TASKS.get_or_init(Default::default).lock().unwrap().insert(id, handle);
    id
}

pub fn await_compute(id: u64) -> i64 {
    let handle = TASKS.get().unwrap().lock().unwrap().remove(&id).unwrap();
    runtime().block_on(handle).unwrap()
}
```

This is a power-user pattern; the bridge does not generate it automatically.

## Tokio version compatibility

The wrapper crate pins tokio to a specific minor version:

```toml
# rust_wrap/<crate>/Cargo.toml
[dependencies]
tokio = { version = "=1.42", features = ["rt", "rt-multi-thread", "macros", "io-util", "sync", "time", "signal"] }
```

The pin is `=1.42` (exact match) to ensure all wrapper crates in the workspace use the same tokio version. Cargo's resolver collapses multiple `=1.42` requirements to one shared instance.

If the imported crate requires tokio `1.50+`, the bridge errors at lock time:

```
ERROR: tokio version mismatch
  Crate `imported-thing@0.3` requires tokio `>= 1.50`
  Bridge pins tokio `= 1.42`
  Resolution: upgrade the bridge to a version that pins a newer tokio.
```

The bridge ships a new minor version every 6 months tracking the tokio LTS line.

## Cross-references

- [[02-design-philosophy]] §4 for why tokio was chosen over async-std / smol.
- [[09-abi-stability]] §3 for opaque-handle storage across runtime boundaries.
- [[12-risks-and-alternatives]] §A9 for the rejected per-call runtime alternative.
- [MEP-73 §7](/docs/mep/mep-0073#7-async-bridge) for the normative runtime hook.
- [tokio runtime guide](https://docs.rs/tokio/latest/tokio/runtime/index.html) for the upstream model.
