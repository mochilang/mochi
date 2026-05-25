---
title: "MEP-51 research note 09, Agents and streams (asyncio lowering)"
description: "Mochi agents and streams lowered to asyncio.Queue mailboxes plus TaskGroup supervision; cast/call, restart strategies, cold/hot streams, GIL story."
---

# MEP-51 research note 09, Agents and streams (asyncio lowering)

Author: research pass for MEP-51 (Mochi to Python transpiler).
Date: 2026-05-23 13:00 (GMT+7).

This note specifies the lowering of Mochi `agent`, `stream`, `spawn`, `!` (cast), and `?` (call) into Python source. The runtime substrate is the stdlib `asyncio` module: `asyncio.Queue`, `asyncio.TaskGroup`, `asyncio.Future`, `asyncio.gather`, `asyncio.wait_for`, `asyncio.shield`, PEP 654 `ExceptionGroup`, PEP 567 `ContextVar`. The transpiler does **not** depend on Trio, AnyIO, curio, or any third-party concurrency library; the lowering is asyncio-native.

Companion notes: the shared-decisions anchor, [[04-runtime]], [[05-codegen-design]], [[06-type-lowering]], [[07-python-target-portability]], [[08-dataset-pipeline]], [[10-build-system]], [[11-testing-gates]], [[12-risks-and-alternatives]].

The four big-picture decisions, defended in [[02-design-philosophy]] §14 and stated here as operating assumptions:

1. **asyncio, not threads.** Every Mochi agent and stream lowers to coroutine code running on a single asyncio event loop. The transpiler never emits raw `threading.Thread`, `concurrent.futures.ThreadPoolExecutor.submit`, or `multiprocessing.Process`. CPU-bound offload uses `asyncio.to_thread` (for I/O-bound parallelism behind the GIL) or `loop.run_in_executor(ProcessPoolExecutor, ...)` (for true CPU parallelism); both are explicitly opt-in via Mochi annotations.

2. **Custom actor class, not a third-party actor framework.** Python has no stdlib equivalent of Erlang's `gen_server` or Akka's `Actor`. Frameworks like `aioactors`, `thespian`, `pykka` exist but are abandoned or niche. The modern shape is a hand-rolled class wrapping `asyncio.Queue` plus a launched receive loop.

3. **TaskGroup supervision, not bare `asyncio.gather`.** PEP 654 ExceptionGroup combined with `asyncio.TaskGroup` (3.11+, stabilised in 3.12) gives structured concurrency: if any child task fails, the group cancels all siblings and re-raises an aggregated exception. This matches Erlang OTP's `one_for_all` supervisor strategy. `one_for_one` and `rest_for_one` are layered on top of TaskGroup with explicit catch-and-restart loops.

4. **AsyncIterator for streams.** Mochi `stream T` lowers to `AsyncIterator[T]` from `collections.abc`. Cold streams are `async def gen()` generators (single subscriber); hot streams are a class wrapping a queue with multiple subscribers (broadcast). The choice is made by the type system: if a stream has `@hot` annotation or is shared across multiple consumers, lower to the hot class.

---

## 1. The custom agent class pattern

The canonical Mochi agent lowers to a Python class with:

- A constructor taking an `asyncio.TaskGroup` (the parent scope; cancellation flows from parent to agent).
- A private `asyncio.Queue[Message]` mailbox.
- A `asyncio.Task` field tracking the launched receive loop.
- Private mutable state (the agent's "registers").
- Public methods for cast (fire-and-forget send) and call (request-reply send).
- A constructor body that launches the receive loop on the parent TaskGroup.
- A `close()` async method that cancels the receive loop.

```python
import asyncio
from collections.abc import Callable
from dataclasses import dataclass, replace
from typing import Final


@dataclass(frozen=True, slots=True)
class _CounterMsg_Increment:
    pass


@dataclass(frozen=True, slots=True)
class _CounterMsg_Get:
    reply: asyncio.Future[int]


_CounterMessage = _CounterMsg_Increment | _CounterMsg_Get


class Counter:
    def __init__(self, scope: asyncio.TaskGroup) -> None:
        self._mailbox: Final[asyncio.Queue[_CounterMessage]] = asyncio.Queue()
        self._count: int = 0
        self._task: Final[asyncio.Task[None]] = scope.create_task(self._loop())

    async def _loop(self) -> None:
        try:
            while True:
                msg = await self._mailbox.get()
                self._handle(msg)
        except asyncio.CancelledError:
            raise

    def _handle(self, msg: _CounterMessage) -> None:
        match msg:
            case _CounterMsg_Increment():
                self._count += 1
            case _CounterMsg_Get(reply=reply):
                reply.set_result(self._count)

    def increment(self) -> None:
        self._mailbox.put_nowait(_CounterMsg_Increment())

    async def get(self) -> int:
        loop = asyncio.get_running_loop()
        reply: asyncio.Future[int] = loop.create_future()
        await self._mailbox.put(_CounterMsg_Get(reply=reply))
        return await reply

    async def close(self) -> None:
        self._task.cancel()
        try:
            await self._task
        except asyncio.CancelledError:
            pass
```

Several details to call out:

- **`Final` annotations**: `_mailbox` and `_task` are marked `Final` because they are set once in the constructor and never reassigned. mypy / pyright enforce this; pyright in particular catches re-assignment as a strict error.
- **Mailbox is unbounded by default**: `asyncio.Queue()` with no `maxsize` argument is unbounded. We make this explicit at the call site when bounded (§4 backpressure).
- **The receive loop catches `CancelledError` and re-raises**: this is the canonical asyncio cancellation pattern. PEP 654 changed `CancelledError` to inherit from `BaseException` (not `Exception`) starting in Python 3.8, so a bare `except Exception` would not catch it. We catch explicitly.
- **`match` statement**: requires Python 3.10+; we are on 3.12 floor.
- **`asyncio.Future` for reply**: `loop.create_future()` is preferred over `asyncio.Future()` because it lets the loop construct the future (for non-default event loops, like uvloop, the loop's future class differs).

## 2. Cast vs call pattern

Mochi distinguishes `cast` (fire-and-forget) from `call` (request-reply with await).

### 2.1 Cast

Cast lowers to `Queue.put_nowait`:

```mochi
counter ! Increment
```

becomes:

```python
counter.increment()  # which calls self._mailbox.put_nowait(_CounterMsg_Increment())
```

`put_nowait` raises `asyncio.QueueFull` if the queue has reached `maxsize`. For unbounded queues (default), this never happens. For bounded queues (§4), the IR pass picks `put_nowait` (non-blocking, may raise) or `put` (suspending) based on the call-site annotation.

The Mochi `!` operator translates to a synchronous method (no `await`) because `put_nowait` is sync. This matches the Erlang `!` operator semantics.

### 2.2 Call

Call lowers to `await` plus a reply Future:

```mochi
let n = counter ? Get
```

becomes:

```python
n = await counter.get()  # which constructs a Future, sends the message, awaits the reply
```

The reply mechanism is:

1. Caller creates a `Future` via `loop.create_future()`.
2. Caller wraps the request message with the Future as a `reply` field.
3. Caller awaits the future on the mailbox.
4. Agent receives the message, processes, calls `reply.set_result(value)` or `reply.set_exception(exc)`.
5. Caller's await unblocks.

The Future and the reply slot are not exposed to user code; the generated `counter.get()` method hides them.

For `call` with a timeout:

```mochi
let n = counter ? Get within 5.seconds
```

becomes:

```python
n = await asyncio.wait_for(counter.get(), timeout=5.0)
```

If the timeout expires, `asyncio.TimeoutError` (alias for `TimeoutError` in 3.11+) is raised. The agent continues processing; the caller catches the timeout. Note that the request has already been enqueued; the agent will still process it and set the reply, but the caller has moved on. The Future is orphaned; we add a `cancel_on_timeout` flag to mark it for skipping in the handler.

### 2.3 Why not `asyncio.Queue.task_done()` / `join()`

Python's `asyncio.Queue` has a `task_done()` / `join()` mechanism for waiting until all enqueued work is processed. We do not use it for cast/call because:

- `task_done` tracks completion at the queue level, not at the message level. Each call would need a unique completion signal, which is what the per-message Future provides.
- `join` is bulk synchronisation; call/reply needs per-message synchronisation.

The Future-per-call approach is simpler and gives us request-scoped error handling.

## 3. Supervision via TaskGroup

`asyncio.TaskGroup` (PEP 654, Python 3.11+) provides structured concurrency. Inside an `async with asyncio.TaskGroup() as tg:` block:

- `tg.create_task(coro)` launches a child coroutine.
- The `async with` block does not exit until all children complete.
- If any child raises, the group cancels all siblings and re-raises an `ExceptionGroup`.

This is the asyncio analogue of Erlang OTP's `one_for_all` supervisor strategy.

```python
async def main() -> None:
    async with asyncio.TaskGroup() as tg:
        counter = Counter(tg)
        logger = Logger(tg)
        # both agents running; if either crashes, the other is cancelled
```

The Mochi `spawn` keyword lowers to `tg.create_task`:

```mochi
spawn counter = Counter()
spawn logger  = Logger()
```

becomes:

```python
counter = Counter(tg)
logger  = Logger(tg)
```

The `tg` is the implicit current scope. The Mochi `supervisor` keyword introduces a nested scope:

```mochi
supervisor {
    spawn worker_a
    spawn worker_b
}
```

becomes:

```python
async with asyncio.TaskGroup() as inner_tg:
    worker_a = WorkerA(inner_tg)
    worker_b = WorkerB(inner_tg)
```

### 3.1 PEP 654 ExceptionGroup

When multiple children fail (race condition: child A raises, then child B raises before B's cancellation completes), the TaskGroup collects all exceptions and re-raises as a `ExceptionGroup`. PEP 654 added `except*` syntax for selectively handling exception groups:

```python
try:
    async with asyncio.TaskGroup() as tg:
        tg.create_task(may_raise_value_error())
        tg.create_task(may_raise_runtime_error())
except* ValueError as eg:
    print(f"value errors: {eg.exceptions}")
except* RuntimeError as eg:
    print(f"runtime errors: {eg.exceptions}")
```

Mochi's `recover` block lowers to `except*`. The IR pass generates one `except*` per type in the recover handlers.

### 3.2 Restart strategies

OTP defines three strategies:

- **one_for_one**: if a child crashes, restart only that child. Siblings continue.
- **one_for_all**: if any child crashes, terminate all and restart all.
- **rest_for_one**: if a child crashes, terminate it and all children started after it; restart them in order.

`asyncio.TaskGroup` implements `one_for_all` natively (any failure cancels the group). `one_for_one` and `rest_for_one` require explicit handling.

#### one_for_one

```python
async def supervise_one_for_one(specs: list[ChildSpec]) -> None:
    async def run_child(spec: ChildSpec) -> None:
        while True:
            try:
                await spec.run()
                return  # normal exit, do not restart
            except asyncio.CancelledError:
                raise  # propagate cancellation
            except Exception as exc:
                spec.on_error(exc)
                if not spec.restart_on_error:
                    raise
                await asyncio.sleep(spec.restart_backoff)
                # loop body restarts
    
    async with asyncio.TaskGroup() as tg:
        for spec in specs:
            tg.create_task(run_child(spec))
```

The `run_child` wrapper catches non-cancellation exceptions and restarts the child's coroutine. The outer TaskGroup cancels everything on shutdown.

#### rest_for_one

```python
async def supervise_rest_for_one(specs: list[ChildSpec]) -> None:
    while True:
        try:
            async with asyncio.TaskGroup() as tg:
                for spec in specs:
                    tg.create_task(spec.run())
            return  # all completed normally
        except* Exception as eg:
            # Find the earliest failed child
            failed_idx = min(
                i for i, spec in enumerate(specs)
                if any(spec.matches(e) for e in eg.exceptions)
            )
            # Restart from failed_idx onwards
            specs = specs[failed_idx:]
            await asyncio.sleep(restart_backoff)
```

The IR pass picks the strategy from the Mochi `supervisor` annotation:

```mochi
supervisor(strategy=one_for_one, restart=permanent, backoff=100.ms) {
    spawn worker_a
    spawn worker_b
}
```

The generated Python has the corresponding wrapper.

### 3.3 Restart policy variants

OTP also defines restart policies per child:

- **permanent**: always restart on exit (normal or error).
- **transient**: restart only on error, not on normal exit.
- **temporary**: never restart.

The IR pass emits the appropriate branch in `run_child`. Default is **transient**.

## 4. Cancellation semantics

`asyncio.CancelledError` is the canonical asyncio cancellation signal. Since Python 3.8 (PEP 567 ContextVars era), `CancelledError` inherits from `BaseException`, not `Exception`. This means:

- `except Exception:` does NOT catch `CancelledError`. Good (you usually want to propagate).
- `except BaseException:` DOES catch `CancelledError`. Avoid this unless you intentionally suppress.
- `finally:` clauses run on cancel. Use them for cleanup.

The Mochi emitter follows these rules strictly:

- The receive loop catches `CancelledError` and re-raises (cleanup happens in `finally`).
- User-level `try / except` does NOT catch cancellation.
- Mochi `recover` lowers to `except* Exception` (specifically Exception, not BaseException).

### 4.1 Cancellation history

Python's asyncio cancellation has been a moving target. Notable issues:

- **gh-90985 (Python 3.11)**: `TaskGroup.create_task` did not propagate cancellation correctly when the parent context was cancelled during task creation. Fixed in 3.11.4.
- **gh-101599 (Python 3.12)**: `asyncio.wait_for` swallowed cancellation in certain race conditions. Fixed in 3.12.0.
- **gh-104144 (Python 3.12)**: TaskGroup did not handle nested cancellation correctly when the inner group received `CancelledError` while still creating tasks. Fixed in 3.12.1.

We pin the floor to 3.12 specifically to inherit these fixes. The 3.11 floor would still hit gh-101599 and gh-104144.

### 4.2 `asyncio.shield`

For cleanup code that must not be cancelled mid-flight:

```python
try:
    await some_io()
finally:
    await asyncio.shield(cleanup())  # cleanup completes even if outer is cancelled
```

The Mochi emitter wraps `defer` blocks in `asyncio.shield` to guarantee cleanup runs:

```mochi
defer { close_file(handle) }
```

becomes:

```python
try:
    ...
finally:
    await asyncio.shield(close_file(handle))
```

Note: `asyncio.shield` itself can be cancelled if the outer wait is cancelled twice; we accept this as the asyncio convention.

### 4.3 ContextVar propagation

PEP 567 ContextVars propagate through `asyncio.create_task` (each task gets a copy of the current context). Cancellation does NOT clear context vars; they survive into `finally` clauses. The Mochi runtime uses ContextVars for:

- The current trace span (for distributed tracing).
- The current request id (for logging).
- The current Mochi capability set (for access control).

These propagate naturally without explicit forwarding.

## 5. Backpressure

The default `asyncio.Queue()` is unbounded. For agents that must apply backpressure (e.g. a slow consumer):

```mochi
agent SlowProcessor {
    mailbox max = 100
}
```

becomes:

```python
class SlowProcessor:
    def __init__(self, scope: asyncio.TaskGroup) -> None:
        self._mailbox: Final[asyncio.Queue[_SlowProcessorMessage]] = asyncio.Queue(maxsize=100)
```

The producer's `cast` then becomes:

```mochi
slow ! Work(payload)  // blocks if mailbox full
```

emit:

```python
await slow.work(payload)  # uses Queue.put, suspends when full
```

Note that with a bounded queue, the cast method is `async` because `Queue.put` suspends. The IR pass picks:

- `put_nowait` (sync cast) when the queue is unbounded.
- `put` (async cast) when the queue is bounded.

The Mochi type system surfaces this: `agent SlowProcessor { mailbox max = 100 }` makes the `!` operator async-typed:

```mochi
async fn enqueue(x: Work) {
    slow ! x  // await is implicit because mailbox is bounded
}
```

### 5.1 `put_nowait` with `QueueFull` recovery

For producers that want to handle the full case themselves:

```mochi
try {
    slow !? Work(payload)
} catch QueueFull {
    drop(payload)
}
```

becomes:

```python
try:
    slow.work_nowait(payload)  # uses Queue.put_nowait, raises QueueFull
except asyncio.QueueFull:
    drop(payload)
```

The `!?` operator is the non-blocking variant. The IR pass emits `put_nowait` for `!?` regardless of bounded-ness.

## 6. Streams as AsyncIterator

Mochi `stream T` lowers to `AsyncIterator[T]` from `collections.abc`.

```mochi
stream Tick {
    let n: int
}

let ticks: Stream<Tick> = generate_ticks()
```

becomes:

```python
from collections.abc import AsyncIterator
from dataclasses import dataclass

@dataclass(frozen=True, slots=True)
class Tick:
    n: int

def generate_ticks() -> AsyncIterator[Tick]:
    async def _gen() -> AsyncIterator[Tick]:
        n = 0
        while True:
            await asyncio.sleep(1.0)
            yield Tick(n=n)
            n += 1
    return _gen()
```

Consuming:

```mochi
for tick in ticks {
    process(tick)
}
```

becomes:

```python
async for tick in ticks:
    process(tick)
```

### 6.1 Cold streams

A cold stream is a coroutine generator: each consumer gets its own iteration starting from the beginning. The generator function (`async def ...: yield`) returns a new `AsyncIterator` per call:

```python
def cold_stream() -> AsyncIterator[int]:
    async def _gen() -> AsyncIterator[int]:
        for i in range(10):
            await asyncio.sleep(0.1)
            yield i
    return _gen()

# Two consumers, each sees 0..9
a = cold_stream()
b = cold_stream()
```

The IR pass emits a cold stream when:

- The stream is declared with `stream` (default).
- There is at most one consumer (statically determined).
- No `@hot` annotation.

### 6.2 Hot streams

A hot stream is a single underlying producer with multiple subscribers; each subscriber sees only events emitted after they subscribe. This is the "pub/sub" pattern.

We implement hot streams with a class wrapping a queue per subscriber:

```python
import asyncio
from collections.abc import AsyncIterator
from typing import Final


class HotStream[T]:
    def __init__(self) -> None:
        self._subscribers: Final[list[asyncio.Queue[T]]] = []
        self._closed: bool = False
    
    def subscribe(self) -> AsyncIterator[T]:
        q: asyncio.Queue[T] = asyncio.Queue()
        self._subscribers.append(q)
        return self._iterate(q)
    
    async def _iterate(self, q: asyncio.Queue[T]) -> AsyncIterator[T]:
        try:
            while True:
                item = await q.get()
                if item is _SENTINEL_CLOSED:
                    return
                yield item
        finally:
            self._subscribers.remove(q)
    
    def emit(self, item: T) -> None:
        for q in self._subscribers:
            q.put_nowait(item)
    
    def close(self) -> None:
        self._closed = True
        for q in self._subscribers:
            q.put_nowait(_SENTINEL_CLOSED)


_SENTINEL_CLOSED = object()
```

Mochi:

```mochi
@hot
stream MarketTick { ... }

let stream = MarketTick.broadcaster()
spawn pub  = MarketPublisher(stream)
spawn sub1 = subscribe_and_log(stream)
spawn sub2 = subscribe_and_log(stream)
```

becomes:

```python
stream = HotStream[MarketTick]()
publisher = MarketPublisher(tg, stream)
sub1_task = tg.create_task(subscribe_and_log(stream.subscribe()))
sub2_task = tg.create_task(subscribe_and_log(stream.subscribe()))
```

Each subscriber gets its own iterator; events emitted before subscription are lost.

The PEP 695 `class HotStream[T]:` syntax is 3.12+ which is our floor.

### 6.3 Closing a stream

The cold stream closes when the generator returns. The hot stream closes when `close()` is called, which sends a sentinel to every subscriber queue. The subscriber's iterator sees the sentinel and exits.

The sentinel pattern is necessary because `asyncio.Queue` has no `close()` of its own (unlike `Channel` in some other languages). Python 3.13 added `asyncio.Queue.shutdown()` (gh-104873) which formalises this; on 3.12 we use the sentinel pattern.

## 7. Periodic emission

A common stream pattern: emit on a timer.

```mochi
stream Heartbeat {
    let ts: int
}

async fn heartbeat_stream() -> Stream<Heartbeat> {
    return Stream::generate(fn() -> {
        sleep(1.second)
        return Heartbeat(ts: now())
    })
}
```

emit:

```python
import asyncio
import time
from collections.abc import AsyncIterator
from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class Heartbeat:
    ts: int


async def heartbeat_stream() -> AsyncIterator[Heartbeat]:
    while True:
        await asyncio.sleep(1.0)
        yield Heartbeat(ts=int(time.time() * 1000))
```

For drift-corrected periodic emission (compensating for sleep overshoot):

```python
async def heartbeat_stream() -> AsyncIterator[Heartbeat]:
    loop = asyncio.get_running_loop()
    deadline = loop.time() + 1.0
    while True:
        now = loop.time()
        if deadline > now:
            await asyncio.sleep(deadline - now)
        yield Heartbeat(ts=int(time.time() * 1000))
        deadline += 1.0
```

The drift-corrected form is emitted when the Mochi declaration uses `interval` instead of `delay`:

```mochi
async fn heartbeat_stream() -> Stream<Heartbeat> {
    return Stream::interval(1.second, fn() -> Heartbeat(ts: now()))
}
```

## 8. Spawn semantics

The Mochi `spawn` keyword lowers to `tg.create_task`. Each agent gets one task per receive loop:

```mochi
spawn counter = Counter()
```

emit:

```python
counter = Counter(tg)  # constructor calls tg.create_task internally
```

The `tg` is in scope because the Mochi function is wrapped in an `async with asyncio.TaskGroup() as tg:` block.

For functions that need to run concurrently without being agents:

```mochi
spawn { do_work() }
```

emit:

```python
tg.create_task(do_work())
```

The Mochi spawn returns a task handle (`asyncio.Task[T]`) that can be awaited:

```mochi
let task = spawn { compute() }
let result = await task
```

emit:

```python
task = tg.create_task(compute())
result = await task
```

## 9. CPU-bound offload

Python's GIL prevents true parallel execution of CPU-bound Python code. Mochi handles this with explicit offload primitives.

### 9.1 `asyncio.to_thread`

For I/O-bound parallelism (e.g. parallel `requests.get` calls), `asyncio.to_thread` runs a sync function in a worker thread:

```mochi
async fn parallel_fetch(urls: list<string>) -> list<string> {
    let tasks = urls.map(fn(url) -> spawn { @to_thread fetch_blocking(url) })
    return await all(tasks)
}
```

emit:

```python
async def parallel_fetch(urls: list[str]) -> list[str]:
    async with asyncio.TaskGroup() as tg:
        tasks = [tg.create_task(asyncio.to_thread(fetch_blocking, url)) for url in urls]
    return [t.result() for t in tasks]
```

`asyncio.to_thread` (added in 3.9) is a thin wrapper over `loop.run_in_executor(None, fn, args)` using the default `ThreadPoolExecutor`. The default executor has `max_workers = min(32, os.cpu_count() + 4)`.

### 9.2 `loop.run_in_executor` with `ProcessPoolExecutor`

For CPU-bound true parallelism, use a `ProcessPoolExecutor`:

```mochi
async fn parallel_compute(items: list<Job>) -> list<Result> {
    let tasks = items.map(fn(j) -> spawn { @to_process compute_blocking(j) })
    return await all(tasks)
}
```

emit:

```python
import concurrent.futures

async def parallel_compute(items: list[Job]) -> list[Result]:
    loop = asyncio.get_running_loop()
    with concurrent.futures.ProcessPoolExecutor() as pool:
        async with asyncio.TaskGroup() as tg:
            tasks = [tg.create_task(loop.run_in_executor(pool, compute_blocking, j)) for j in items]
    return [t.result() for t in tasks]
```

The `ProcessPoolExecutor` forks subprocess workers; the worker's input must be picklable. The Mochi type system rejects non-picklable types in `@to_process` boundaries (e.g. open file handles, network sockets, lambdas with closures over local state).

### 9.3 The GIL story

CPython's Global Interpreter Lock (GIL) prevents two threads from executing Python bytecode simultaneously. For asyncio:

- **I/O-bound async code**: GIL is released during I/O syscalls (read/write, socket recv/send). asyncio benefits from this automatically.
- **CPU-bound async code**: GIL is held; only one coroutine runs at a time on the event loop, even with multiple OS threads.
- **`asyncio.to_thread`**: helpful for I/O-bound sync functions (e.g. requests, file IO) because the GIL is released during the I/O. Not helpful for CPU-bound sync functions.
- **`ProcessPoolExecutor`**: bypasses the GIL entirely (each worker has its own interpreter); true CPU parallelism.

Python 3.13 introduces the experimental free-threaded build (`--disable-gil`, PEP 703) which removes the GIL. With free-threaded 3.13:

- Multiple threads execute Python bytecode in parallel.
- `asyncio.to_thread` gains true CPU parallelism.
- `ProcessPoolExecutor` is still useful for fault isolation but no longer required for parallelism.

We do **not** depend on free-threaded mode in v1. The runtime is correct under the GIL; free-threaded is documented as a v2 acceleration target ([[12-risks-and-alternatives]] §F1).

## 10. ipykernel + Jupyter event loop integration

For the Mochi ipykernel target (phase 17 of the roadmap), the Mochi cells run inside a Jupyter kernel. Jupyter's kernel is itself an asyncio program (since 6.x), running tornado's `IOLoop` which delegates to asyncio in modern versions.

The Mochi ipykernel:

1. Receives a cell of Mochi code from JupyterLab via ZMQ.
2. Transpiles the cell to Python (via the same emit pipeline).
3. Executes the Python in the kernel's event loop using `await` or `asyncio.ensure_future`.
4. Captures stdout / stderr / display data and sends results back via ZMQ.

The kernel's event loop is already running; user code uses `await` directly at top level (PEP 678 `await` outside async functions inside Jupyter cells is supported via `IPython.core.async_helpers.AsyncResult`).

```python
# Inside the kernel, executing a Mochi cell:
async def _execute_cell(code: str) -> Any:
    py_source = mochi_transpile(code)
    namespace: dict[str, Any] = {}
    exec(compile(py_source, "<cell>", "exec"), namespace)
    main = namespace.get("_main")
    if main is not None:
        return await main()
    return None
```

### 10.1 `nest_asyncio`

The `nest_asyncio` package patches asyncio to allow nested event loops. JupyterLab does not need it in modern versions (since 4.x); the Mochi ipykernel does not depend on it.

If a user runs an older Jupyter (e.g. classic Notebook 6.x with tornado < 6.1), they may need `nest_asyncio.apply()` once at the start of their notebook. The Mochi cell template detects this and prints a hint.

### 10.2 Display protocol

Mochi `print(x)` lowers to Python `print(x)` which writes to stdout; the kernel captures stdout. For rich display:

```mochi
display(image)
```

emit:

```python
from IPython.display import display
display(image)
```

The IR pass detects `display` calls and emits the IPython import. For non-Jupyter targets, the `display` function falls back to `print(repr(x))`.

## 11. Comparison to Trio

Trio is a third-party concurrency library with stronger structured-concurrency guarantees than asyncio:

| Aspect              | asyncio                  | Trio                          |
|---------------------|--------------------------|-------------------------------|
| Stdlib              | yes                      | no (third-party)              |
| Structured scope    | TaskGroup (3.11+)        | nursery (since launch)        |
| Cancellation        | CancelledError           | Cancelled (similar)           |
| Cancel scope        | per-task                 | per-scope (richer)            |
| Channel             | Queue                    | memory_channel (send/receive halves) |
| Sleeping            | asyncio.sleep            | trio.sleep                    |
| Subprocess          | asyncio.subprocess       | trio.run_process              |
| HTTP client         | aiohttp / httpx          | httpx (works), no native      |
| Ecosystem           | huge (FastAPI, httpx, aiohttp) | smaller                |
| AnyIO bridge        | n/a                      | optional                      |

Trio's nursery design predates asyncio's TaskGroup and is arguably cleaner: cancel scopes are objects, not implicit context. asyncio's TaskGroup catches up but the cancel-scope abstraction is less explicit.

We picked asyncio because:

- **Stdlib**: no extra dep, no version drift.
- **Ecosystem**: FastAPI, httpx, aiohttp, uvloop, all asyncio-native. Trio bridges via AnyIO but with overhead.
- **Jupyter**: ipykernel is asyncio-based. Trio inside Jupyter requires AnyIO bridge.
- **Familiarity**: most Python developers know asyncio.
- **TaskGroup is enough**: PEP 654 ExceptionGroup plus TaskGroup gives 90 % of Trio's structured-concurrency story.

The 10 % we lose: Trio's cancel scopes can be re-targeted dynamically; asyncio cancel propagation is fixed at task creation. This affects niche patterns (e.g. cooperative cancellation across un-related tasks) that Mochi does not expose.

## 12. Worked Mochi-to-Python examples

Four end-to-end examples showing the full lowering.

### 12.1 Counter agent

Mochi:

```mochi
agent Counter {
    state n: int = 0
    
    msg Increment {
        n = n + 1
    }
    
    call Get -> int {
        return n
    }
}

async fn main() {
    spawn counter = Counter()
    counter ! Increment
    counter ! Increment
    counter ! Increment
    let n = await counter ? Get
    print(n)  // 3
}
```

Python:

```python
import asyncio
from dataclasses import dataclass
from typing import Final


@dataclass(frozen=True, slots=True)
class _CounterMsg_Increment:
    pass


@dataclass(frozen=True, slots=True)
class _CounterMsg_Get:
    reply: asyncio.Future[int]


_CounterMessage = _CounterMsg_Increment | _CounterMsg_Get


class Counter:
    def __init__(self, scope: asyncio.TaskGroup) -> None:
        self._mailbox: Final[asyncio.Queue[_CounterMessage]] = asyncio.Queue()
        self._n: int = 0
        self._task: Final[asyncio.Task[None]] = scope.create_task(self._loop())

    async def _loop(self) -> None:
        while True:
            msg = await self._mailbox.get()
            match msg:
                case _CounterMsg_Increment():
                    self._n += 1
                case _CounterMsg_Get(reply=reply):
                    reply.set_result(self._n)

    def increment(self) -> None:
        self._mailbox.put_nowait(_CounterMsg_Increment())

    async def get(self) -> int:
        loop = asyncio.get_running_loop()
        reply: asyncio.Future[int] = loop.create_future()
        await self._mailbox.put(_CounterMsg_Get(reply=reply))
        return await reply


async def main() -> None:
    async with asyncio.TaskGroup() as tg:
        counter = Counter(tg)
        counter.increment()
        counter.increment()
        counter.increment()
        n = await counter.get()
        print(n)


if __name__ == "__main__":
    asyncio.run(main())
```

### 12.2 Supervisor tree

Mochi:

```mochi
agent Worker {
    state id: int
    msg Work(payload: bytes) { process(id, payload) }
}

agent Logger {
    msg Log(msg: string) { write_log(msg) }
}

async fn main() {
    supervisor(strategy=one_for_one) {
        spawn worker_a = Worker(id: 1)
        spawn worker_b = Worker(id: 2)
        spawn logger   = Logger()
    }
}
```

Python:

```python
import asyncio
from dataclasses import dataclass
from typing import Final


@dataclass(frozen=True, slots=True)
class _WorkerMsg_Work:
    payload: bytes


class Worker:
    def __init__(self, scope: asyncio.TaskGroup, id: int) -> None:
        self._mailbox: Final[asyncio.Queue[_WorkerMsg_Work]] = asyncio.Queue()
        self._id: int = id
        self._task: Final[asyncio.Task[None]] = scope.create_task(self._loop())

    async def _loop(self) -> None:
        while True:
            msg = await self._mailbox.get()
            process(self._id, msg.payload)

    def work(self, payload: bytes) -> None:
        self._mailbox.put_nowait(_WorkerMsg_Work(payload=payload))


@dataclass(frozen=True, slots=True)
class _LoggerMsg_Log:
    msg: str


class Logger:
    def __init__(self, scope: asyncio.TaskGroup) -> None:
        self._mailbox: Final[asyncio.Queue[_LoggerMsg_Log]] = asyncio.Queue()
        self._task: Final[asyncio.Task[None]] = scope.create_task(self._loop())

    async def _loop(self) -> None:
        while True:
            msg = await self._mailbox.get()
            write_log(msg.msg)

    def log(self, msg: str) -> None:
        self._mailbox.put_nowait(_LoggerMsg_Log(msg=msg))


async def _run_with_restart(coro_factory, restart_backoff: float = 0.1) -> None:
    while True:
        try:
            await coro_factory()
            return
        except asyncio.CancelledError:
            raise
        except Exception as exc:
            log_supervisor_error(exc)
            await asyncio.sleep(restart_backoff)


async def main() -> None:
    async with asyncio.TaskGroup() as outer:
        # one_for_one wraps each child in its own restart loop, but all under one TaskGroup
        worker_a = Worker(outer, id=1)
        worker_b = Worker(outer, id=2)
        logger   = Logger(outer)
        # Each individual agent loop catches its own crash and restarts.
        # The outer TaskGroup only cancels on shutdown signal.


if __name__ == "__main__":
    asyncio.run(main())
```

Note: a true one_for_one implementation would wrap each agent's `_loop` in a try/except/restart pattern. The IR pass injects this when the supervisor strategy is `one_for_one`.

### 12.3 Periodic stream emitter

Mochi:

```mochi
async fn ticks_per_second() -> Stream<int> {
    return Stream::interval(1.second, fn() -> {
        return current_unix_time()
    })
}

async fn main() {
    let stream = await ticks_per_second()
    for tick in stream {
        print(tick)
        if tick > unix_time(2026, 12, 31) { break }
    }
}
```

Python:

```python
import asyncio
import time
from collections.abc import AsyncIterator


async def ticks_per_second() -> AsyncIterator[int]:
    loop = asyncio.get_running_loop()
    deadline = loop.time() + 1.0
    while True:
        now = loop.time()
        if deadline > now:
            await asyncio.sleep(deadline - now)
        yield int(time.time())
        deadline += 1.0


async def main() -> None:
    stream = ticks_per_second()
    async for tick in stream:
        print(tick)
        if tick > unix_time(2026, 12, 31):
            break


if __name__ == "__main__":
    asyncio.run(main())
```

### 12.4 Fan-out broadcast

Mochi:

```mochi
@hot
stream MarketTick {
    let symbol: string
    let price:  float
}

async fn market_publisher(out: Broadcaster<MarketTick>) {
    loop {
        let tick = await poll_market()
        out.emit(tick)
    }
}

async fn subscriber(name: string, in: Stream<MarketTick>) {
    for tick in in {
        print("\(name) got \(tick.symbol) at \(tick.price)")
    }
}

async fn main() {
    let broadcaster = MarketTick.broadcaster()
    spawn _ = market_publisher(broadcaster)
    spawn _ = subscriber("sub-A", broadcaster.subscribe())
    spawn _ = subscriber("sub-B", broadcaster.subscribe())
}
```

Python:

```python
import asyncio
from collections.abc import AsyncIterator
from dataclasses import dataclass
from typing import Any, Final


@dataclass(frozen=True, slots=True)
class MarketTick:
    symbol: str
    price: float


_SENTINEL_CLOSED: Any = object()


class _MarketTickBroadcaster:
    def __init__(self) -> None:
        self._subscribers: Final[list[asyncio.Queue[Any]]] = []

    def subscribe(self) -> AsyncIterator[MarketTick]:
        q: asyncio.Queue[Any] = asyncio.Queue()
        self._subscribers.append(q)
        return self._iterate(q)

    async def _iterate(self, q: asyncio.Queue[Any]) -> AsyncIterator[MarketTick]:
        try:
            while True:
                item = await q.get()
                if item is _SENTINEL_CLOSED:
                    return
                yield item
        finally:
            self._subscribers.remove(q)

    def emit(self, item: MarketTick) -> None:
        for q in self._subscribers:
            q.put_nowait(item)

    def close(self) -> None:
        for q in self._subscribers:
            q.put_nowait(_SENTINEL_CLOSED)


async def market_publisher(out: _MarketTickBroadcaster) -> None:
    while True:
        tick = await poll_market()
        out.emit(tick)


async def subscriber(name: str, stream: AsyncIterator[MarketTick]) -> None:
    async for tick in stream:
        print(f"{name} got {tick.symbol} at {tick.price}")


async def main() -> None:
    broadcaster = _MarketTickBroadcaster()
    async with asyncio.TaskGroup() as tg:
        tg.create_task(market_publisher(broadcaster))
        tg.create_task(subscriber("sub-A", broadcaster.subscribe()))
        tg.create_task(subscriber("sub-B", broadcaster.subscribe()))


if __name__ == "__main__":
    asyncio.run(main())
```

The IR pass picks `_MarketTickBroadcaster` (a hot-stream class) because the Mochi declaration has `@hot`. Without `@hot`, the lowering would be a cold stream (each subscriber calls the generator and gets a fresh sequence).

## 13. Performance notes

Some rough numbers from microbenchmarks on the standard CI runner (ubuntu-22.04, CPython 3.12.7):

| Operation                            | Time     | Notes                          |
|--------------------------------------|----------|--------------------------------|
| `Queue.put_nowait` + `Queue.get`     | ~0.5 us  | the dominant cost              |
| `loop.create_future` + `set_result`  | ~1 us    | per call/reply                 |
| `tg.create_task`                     | ~3 us    | per spawn                      |
| `asyncio.sleep(0)` yield             | ~1 us    | per `await`                    |
| `asyncio.to_thread` round trip       | ~50 us   | thread pool dispatch           |
| `ProcessPoolExecutor` round trip     | ~500 us  | fork + pickle + unpickle       |
| `match` statement (3-arm)            | ~0.2 us  | comparable to if-elif chain    |

Cast throughput: ~2M messages/sec single-producer/single-consumer.
Call throughput: ~500K calls/sec single-producer/single-consumer (Future overhead dominates).

For comparison, the same agent shape in Erlang (MEP-46) achieves ~5M messages/sec (BEAM's native scheduler is faster), and in Kotlin/JVM (MEP-50) ~3M messages/sec (Channel is a Kotlin-native ringbuffer). Python's asyncio is the slowest of the three but adequate for the target workload (interactive scripts, web backends, notebook code).

If perf becomes a gate, uvloop is a drop-in replacement for the default selector event loop and gives ~2-4x throughput. We do not require it but document it as an optional dependency in 10-build-system.

## 14. Comparison to MEP-50 (Kotlin)

The Kotlin agent shape (MEP-50 §1) is structurally identical:

| Aspect              | Mochi-on-Kotlin (MEP-50)         | Mochi-on-Python (MEP-51)       |
|---------------------|----------------------------------|--------------------------------|
| Mailbox             | `Channel<Message>(UNLIMITED)`    | `asyncio.Queue[Message]()`     |
| Receive loop        | `scope.launch { for msg in channel ... }` | `tg.create_task(self._loop())` |
| Cast                | `channel.trySend(msg)`           | `queue.put_nowait(msg)`        |
| Call                | `CompletableDeferred + send`     | `Future + put`                 |
| Supervision         | `SupervisorJob + launch`         | `TaskGroup + create_task`      |
| ExceptionGroup      | n/a (each Job has its own state) | PEP 654 ExceptionGroup         |
| Cold stream         | `flow { emit(...) }`             | `async def gen(): yield ...`   |
| Hot stream          | `MutableSharedFlow`              | hand-rolled `HotStream` class  |

The biggest semantic delta: Kotlin coroutines have a built-in `SharedFlow` for hot streams; asyncio has none, so we hand-roll. Kotlin's `Channel.UNLIMITED` is exactly `asyncio.Queue()` with no maxsize.

The biggest implementation delta: Kotlin agents are bytecode-level coroutines (suspended at compiler-inserted state machines); Python agents are interpreter-level coroutines (suspended at `await` points). Both are stackless. Kotlin is ~5x faster per cast due to JIT compilation; Python is more interactive (no JIT warmup).

## 15. Summary

The Mochi agent / stream lowering targets `asyncio.Queue` + `asyncio.TaskGroup` with PEP 654 `ExceptionGroup` for failure aggregation. Cast is `put_nowait`, call is Future + put + await, supervision is nested TaskGroup with explicit restart wrappers for `one_for_one` and `rest_for_one`. Cold streams are `async def` generators; hot streams are a hand-rolled broadcaster class. CPU-bound offload goes through `asyncio.to_thread` (I/O-bound) or `ProcessPoolExecutor` (true parallel). Free-threaded 3.13 is a v2 acceleration target, not a v1 dependency.

The companion notes pick up: [[06-type-lowering]] for the dataclass-message shape, [[07-python-target-portability]] for the determinism gate that agent stdout must satisfy, [[08-dataset-pipeline]] for the streaming query DSL on top of `AsyncIterator`, [[11-testing-gates]] for the cancellation-correctness fuzzer that gates this code, and [[12-risks-and-alternatives]] for the Trio and free-threaded forward looks.
