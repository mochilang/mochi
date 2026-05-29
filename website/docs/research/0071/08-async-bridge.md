---
title: "08. Async bridge"
sidebar_position: 9
sidebar_label: "08. Async bridge"
description: "The asyncio event-loop model, the per-call `asyncio.run` default, the persistent event-loop opt-in, the cross-loop hazard, the `await` ceremony, cancellation and timeout semantics, the uvloop / trio incompatibility surface, mapping Mochi `async fn` to Python `async def` in both directions."
---

# 08. Async bridge

This note covers the async bridge between Mochi (which has `async fn` from MEP-13) and Python (which has `async def` plus `asyncio`). Async is the hardest cross-language concern in the bridge because both languages have first-class concurrency models that do not naturally compose.

## The asyncio model

Python's `asyncio` runs coroutines on an event loop:

```python
async def fetch(url):
    async with httpx.AsyncClient() as client:
        return await client.get(url)

asyncio.run(fetch("https://example.com"))
```

`asyncio.run` does the following:

1. Creates a new event loop (`asyncio.new_event_loop()`).
2. Sets it as the current loop (`asyncio.set_event_loop(loop)`).
3. Runs the coroutine to completion (`loop.run_until_complete(coro)`).
4. Cancels all remaining tasks, runs the loop until they finish.
5. Closes the loop (`loop.close()`).
6. Resets the current loop to None.

The cost is ~0.5-1ms on a warm interpreter; the cost on a cold interpreter (first call) is closer to 5-10ms because of `asyncio` module import.

The alternative is to keep a loop alive across calls and `run_coroutine_threadsafe` onto it from outside. This avoids the per-call setup cost but introduces shared mutable state (the running loop) with all the lifetime hazards that entails.

## The MEP-71 choice: per-call by default, persistent opt-in

The bridge defaults to per-call `asyncio.run`:

```mochi
import python "httpx" as httpx

async fn fetch(url: string): string {
    let client = httpx.AsyncClient()    // Mochi value, Python AsyncClient handle
    let resp = await client.get(url)     // Each await crosses the boundary
    return resp.text                     // Mochi string
}
```

Under the hood, the synthesised wrapper translates each `await client.get(url)` into:

```python
def _bridge_get(client_handle, url):
    coro = client_handle.get(url)
    return asyncio.run(coro)
```

The Mochi-side `await` is the Mochi runtime suspending until the wrapper returns; the Python side runs the coroutine to completion synchronously. Each Mochi `await` is one full asyncio.run cycle.

To enable persistent mode:

```toml
[python]
async-mode = "persistent"
```

In persistent mode, a singleton event loop is created on first use and kept alive on a dedicated Python thread. Mochi `await` translates to `asyncio.run_coroutine_threadsafe(coro, _PERSISTENT_LOOP).result()`. Subsequent calls reuse the loop; cost amortises.

## The cross-loop hazard

The persistent-mode hazard: Python objects bound to a specific event loop (Future, Task, AsyncIterator, AsyncContextManager) can only be awaited on that loop. If a Mochi function captures such an object and passes it to another Mochi function running on a different loop, behaviour is undefined.

Per-call mode avoids the hazard by construction: every loop is created and destroyed in the same call, so capturing a loop-bound object outside that call gives a stale reference that the next call's wrapper detects and rejects.

Persistent mode has the hazard. The bridge mitigates by:

1. **One singleton loop per Mochi process.** Persistent mode does not create multiple loops; the single loop is the only loop.
2. **Capturing loop-bound objects is explicit.** Returning a Future, Task, or AsyncIterator from a Python call requires the caller to hold the result as a `PyObject` handle; the wrapper does not auto-convert.
3. **Re-entrancy detection.** If a Mochi await is in flight on the persistent loop and the inner Python code tries to await a Mochi callback that itself awaits the persistent loop, the wrapper detects the cycle and refuses.

## Mochi callbacks into Python async

The opposite direction: a Mochi `async fn` is passed to Python as a callable, and Python awaits it.

```mochi
import python "asyncio" as asyncio

async fn worker(item: int): int {
    let result = compute(item)
    return result
}

async fn main() {
    let items = [1, 2, 3, 4, 5]
    let coros = items.map(worker)         // [Coroutine[int], ...] in Python
    let results = await asyncio.gather(*coros)
    return results.sum()
}
```

The Mochi `async fn worker` becomes a Python `async def` wrapper that awaits the Mochi runtime. The wrapper:

```python
async def _bridge_worker(item):
    return await _MOCHI_RUNTIME.await_async(worker_handle, item)
```

`_MOCHI_RUNTIME.await_async` is a Python coroutine that suspends until the Mochi runtime signals completion of the Mochi-side `worker`. The mechanism uses a `loop.create_future()` + `loop.call_soon_threadsafe` pattern: the Mochi runtime invokes the future's set_result from a Mochi thread, and asyncio resumes the Python coroutine.

This works under both per-call and persistent modes. Under per-call mode, the Python `asyncio.run` calls the wrapped Mochi callback, which suspends Python, runs Mochi work on a separate runtime, and resumes Python when done. Under persistent mode, the same pattern works on the singleton loop.

## Cancellation

asyncio cancellation propagates via `CancelledError`. When the Mochi side cancels an `await` (e.g., via `select` from MEP-13), the wrapper must propagate that cancellation into the Python coroutine.

Per-call mode: cancellation happens by signaling the running `asyncio.run`. The wrapper installs a cancel handler on the loop and triggers `task.cancel()` when Mochi cancels.

Persistent mode: cancellation is sent via `loop.call_soon_threadsafe(task.cancel)`. The Python coroutine receives `CancelledError`. The Mochi side waits for the wrapper to return (which it does after cancellation is processed) before treating the Mochi `await` as cancelled.

Edge case: a Python coroutine that catches `CancelledError` and continues running. The wrapper does not force-kill; cancellation is cooperative. Mochi's runtime times out after `[python].async-cancel-timeout` (default 30s) and treats the call as failed.

## Timeouts

Mochi's `await` accepts a timeout (MEP-13 §3.3). The bridge translates this to `asyncio.wait_for(coro, timeout=t)`. If the timeout fires, Python raises `asyncio.TimeoutError`, which the wrapper coerces to Mochi's `Error::Timeout`.

Persistent mode reuses the same `wait_for` pattern; the timeout is per-call, not per-loop.

## The uvloop / trio question

Python has alternative event-loop implementations:

- **uvloop**: a libuv-based replacement for asyncio's selector_events loop. 2-4x faster, drop-in compatible. Users opt in via `uvloop.install()`.
- **trio**: a structured-concurrency runtime with a different cancellation model. Not asyncio-compatible.
- **anyio**: a compatibility layer over asyncio + trio. Allows code to run on either.

MEP-71's stance:

- **uvloop**: works transparently. If the user's Python deps include `uvloop` and the user calls `uvloop.install()` before the bridge's first `asyncio.run`, the bridge picks up uvloop's loop policy automatically. No bridge-side configuration needed.
- **trio**: not supported. The bridge's `asyncio.run` does not work for trio coroutines. A Python dep that uses trio internally surfaces a `SkipReason::IncompatibleAsyncRuntime` at lock time (detected by walking the dep's `import trio` AST).
- **anyio**: works because anyio code is asyncio-compatible by default.

## The Mochi runtime hook

The Mochi runtime exposes an entry point at `runtime/python/mochi_runtime/async_bridge.py`:

```python
class AsyncBridge:
    def __init__(self, mode: str):
        self.mode = mode
        self._loop = None
        self._loop_thread = None
        if mode == "persistent":
            self._start_persistent_loop()

    def run_coro(self, coro):
        if self.mode == "per-call":
            return asyncio.run(coro)
        else:
            future = asyncio.run_coroutine_threadsafe(coro, self._loop)
            return future.result()

    def _start_persistent_loop(self):
        import threading
        self._loop = asyncio.new_event_loop()
        self._loop_thread = threading.Thread(
            target=self._loop.run_forever,
            daemon=True,
            name="mochi-asyncio-loop",
        )
        self._loop_thread.start()
```

The wrapper imports this and calls `_BRIDGE.run_coro(coro)` for every Mochi-to-Python await.

## Performance characteristics

| Mode | Cost per await | Loop count | Shared state |
|------|----------------|------------|--------------|
| Per-call (cold) | 5-10ms (first call) | 1 transient | None |
| Per-call (warm) | 0.5-1ms | 1 transient | None |
| Persistent (warm) | 50-100µs | 1 persistent | Yes |
| Native Python `asyncio.run` once | 0.5-1ms once + 50-100µs per inner await | 1 transient | None inside |

The persistent mode is ~10x faster per await. The trade-off is the shared-state hazard.

The default per-call mode is correct for the typical use case: each Mochi function does one Python async call and returns. The persistent mode is for hot loops (thousands of small async calls).

## Cross-references

- [[02-design-philosophy]] §4 for the per-call default rationale.
- [[10-gil-and-cextensions]] for GIL handling around the event loop.
- [[12-risks-and-alternatives]] §R8 for the persistent-mode hazard mitigation plan.
- [MEP-13](/docs/mep/mep-0013) for Mochi's async fn surface.
- [Python asyncio docs](https://docs.python.org/3/library/asyncio.html).
- [PEP 3156](https://peps.python.org/pep-3156/) for asyncio's original design.
