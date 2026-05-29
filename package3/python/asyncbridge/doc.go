// Package asyncbridge implements MEP-71 Phase 12: the async-fn -> sync-fn
// shim renderer that the wrapper synthesiser layers around every imported
// `async def` callable.
//
// The bridge resolves the Mochi / Python impedance mismatch between Mochi's
// immediate-evaluation `async` semantics (MEP-51 Phase 11 deferred the full
// async colour pass) and Python's coroutine + event-loop model. For every
// imported `async def f(...) -> T`, the bridge emits a synchronous
// `f_sync(...) -> T` shim that drives the coroutine to completion.
//
// Two modes are supported:
//
//   - Per-call (default): each call constructs a fresh event loop via
//     `asyncio.run(f(...))`. Loop construction + teardown costs ~200μs on
//     CPython 3.12 (Apple Silicon, dominated by selectors-init + signal
//     handler register); negligible for non-tight-loop workloads.
//
//   - Persistent: a process-global event loop is cached at first call via
//     `asyncio.new_event_loop()`. The shim drives the coroutine via
//     `loop.run_until_complete(f(...))`. Avoids the per-call teardown but
//     leaks the loop into the host process, which is why per-call is the
//     default.
//
// The mode is selected by the `[python] runtime = { event-loop = "per-call"
// | "persistent" }` knob in `mochi.toml`. The bridge does not pick a mode on
// the user's behalf.
//
// Both modes carry a cross-loop hazard guard: if the shim is invoked while
// a loop is already running on the current thread (asyncio.get_running_loop
// returns a loop), `asyncio.run` and `loop.run_until_complete` both raise
// `RuntimeError`. The generated shim catches that path early and raises a
// clear `MochiAsyncReentryError` so the user sees the nesting hazard
// instead of a vague asyncio traceback.
//
// Layout:
//
//   - mode.go: Mode enum + parser.
//   - fn.go: AsyncFn descriptor + Validate.
//   - render.go: RenderShim + RenderModule + the shared cross-loop helper
//     + the persistent-loop getter source.
//
// Free-threaded CPython 3.13t / 3.14t support is forward (Phase 17), not
// covered here. Cancellation + timeout propagation is sub-phase 12.3.
package asyncbridge
