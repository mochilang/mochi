---
title: "Phase 9. Agents"
sidebar_position: 14
sidebar_label: "Phase 9. Agents"
description: "MEP-51 Phase 9 -- Mochi agents lower to a custom class wrapping asyncio.Queue[Message] plus a TaskGroup-supervised receive loop; cast via put_nowait, call via Future, restart strategies layered on TaskGroup; 35 fixtures."
---

# Phase 9. Agents

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 9](/docs/mep/mep-0051#phase-plan) |
| Status         | NOT STARTED |
| Started        | -- |
| Landed         | -- |
| Tracking issue | -- |
| Tracking PR    | -- |

## Gate

`TestPhase9Agents`: 35 fixtures green on CPython 3.12.0 and 3.13.0 across x86_64-linux-gnu, aarch64-linux-gnu, aarch64-darwin, and x86_64-windows. Secondary gates: `mypy --strict --python-version=3.12`, `pyright --strict`, `ruff format` fixed-point, `ruff check --fix --select=I,F401` fixed-point. Tertiary gate: byte-equal stdout against vm3 for every fixture across at least 100 deterministic event-loop schedules (the runtime uses `asyncio.new_event_loop()` with a seeded `MochiClock` so the byte-equal gate is repeatable).

## Goal-alignment audit

Mochi's primary concurrency abstraction is the agent: a stateful entity with a mailbox, a receive loop, and cast/call message ports. Without Phase 9, no Mochi program that uses `agent`, `spawn`, `!`, or `?` reaches the Python target at all. Phase 9 is the gate that turns the Python transpiler from "scripting target" into "concurrent backend target", and it is the prerequisite for Phase 10 (streams, which share the queue substrate), Phase 11 (async coloring, which is seeded from agent-touching functions), Phase 13 (LLM, which is async), and Phase 14 (fetch, which is async). The user-facing payload is that a Mochi `agent Counter { ... }` becomes a Python class that any FastAPI handler or Jupyter cell can instantiate inside an `async with asyncio.TaskGroup() as tg:` block.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 9.0 | Agent class skeleton: `asyncio.Queue[Message]` mailbox, `_loop` receive coroutine, frozen-slots message dataclasses, PEP 695 union over variants | NOT STARTED | -- |
| 9.1 | Cast via `put_nowait` (sync method) and call via `asyncio.Future` request-reply (async method); per-call timeout via `asyncio.wait_for` | NOT STARTED | -- |
| 9.2 | TaskGroup supervision: `one_for_all` native via TaskGroup; `one_for_one` via try/except restart loop wrapper; `rest_for_one` via slice-and-restart | NOT STARTED | -- |
| 9.3 | ExceptionGroup unwrap to `MochiResult.Err`: `except*` syntax at the parent TaskGroup; aggregate inner exceptions into a list payload | NOT STARTED | -- |
| 9.4 | Named agents and registry via `mochi_runtime.agents.Registry`: name-to-instance lookup, weak references, lifecycle hooks | NOT STARTED | -- |

## Sub-phase 9.0 -- Agent class skeleton

### Goal-alignment audit (9.0)

The agent class skeleton is the load-bearing emit shape for the entire concurrency story. If the class layout is wrong, every subsequent sub-phase has to re-litigate constructor signatures, `Final` annotations, and the `_loop` cancellation pattern. Landing 9.0 first locks the shape; every later sub-phase extends it without breaking emit. Without 9.0 the rest of Phase 9 cannot compose, and the agent story slips by one sub-phase per blocked sibling.

### Decisions made (9.0)

Each Mochi `agent Foo { state ...; msg X ...; }` emits one Python module section:

```python
from __future__ import annotations

import asyncio
from dataclasses import dataclass
from typing import Final


@dataclass(frozen=True, slots=True)
class _CounterMsg_Increment:
    by: int


@dataclass(frozen=True, slots=True)
class _CounterMsg_Reset:
    pass


type _CounterMessage = _CounterMsg_Increment | _CounterMsg_Reset


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
            case _CounterMsg_Increment(by=n):
                self._count += n
            case _CounterMsg_Reset():
                self._count = 0
```

Decisions baked in:

- `_mailbox` and `_task` are `Final` so pyright catches accidental re-assignment in subclasses or in user-written extensions.
- The message union uses PEP 695 `type` alias (`type _CounterMessage = ...`), not `typing.Union` or `typing.TypeAlias`. mypy 1.13+ and pyright 1.1.380+ both narrow the alias correctly inside `match`.
- Message variants are emitted with a `_<AgentName>Msg_` prefix to avoid collision when two agents declare a `Reset` message.
- `_loop` catches `CancelledError` and re-raises explicitly. A bare `except Exception:` would not catch it (PEP 657 / Python 3.8 reparented `CancelledError` under `BaseException`). The explicit catch is the canonical asyncio cancellation pattern.
- State fields (here `_count: int`) are annotated with `Final` only when Mochi declares them `let` (immutable); mutable fields (`var n: int`) are annotated bare.
- The `_handle` dispatch is a sync method (no `await`); message handlers that need `await` lower to an `async def _handle` variant emitted only when at least one handler body contains an `await`. The colour pass (Phase 11.0) drives this choice.

## Sub-phase 9.1 -- Cast and call

### Goal-alignment audit (9.1)

Cast and call are the user-facing send primitives. Without 9.1 the agent class has no public message ports and is effectively unreachable. The cast/call split is what makes agents usable as a service: cast for fire-and-forget commands (logging, increment, mutation), call for request/reply queries (read state, perform query). Landing 9.1 turns the 9.0 skeleton into something Mochi programs can call.

### Decisions made (9.1)

Mochi `counter ! Increment(5)` lowers to a sync method call; Mochi `let n = counter ? Get` lowers to an `await` expression.

```python
from __future__ import annotations

import asyncio
from dataclasses import dataclass
from typing import Final


@dataclass(frozen=True, slots=True)
class _CounterMsg_Increment:
    by: int


@dataclass(frozen=True, slots=True)
class _CounterMsg_Get:
    reply: asyncio.Future[int]


type _CounterMessage = _CounterMsg_Increment | _CounterMsg_Get


class Counter:
    def __init__(self, scope: asyncio.TaskGroup) -> None:
        self._mailbox: Final[asyncio.Queue[_CounterMessage]] = asyncio.Queue()
        self._count: int = 0
        self._task: Final[asyncio.Task[None]] = scope.create_task(self._loop())

    async def _loop(self) -> None:
        try:
            while True:
                msg = await self._mailbox.get()
                match msg:
                    case _CounterMsg_Increment(by=n):
                        self._count += n
                    case _CounterMsg_Get(reply=fut):
                        if not fut.done():
                            fut.set_result(self._count)
        except asyncio.CancelledError:
            raise

    def increment(self, by: int) -> None:
        self._mailbox.put_nowait(_CounterMsg_Increment(by=by))

    async def get(self) -> int:
        loop = asyncio.get_running_loop()
        reply: asyncio.Future[int] = loop.create_future()
        await self._mailbox.put(_CounterMsg_Get(reply=reply))
        return await reply
```

Cast lowering:

- Unbounded mailbox (default): `put_nowait`; the method is `def`, returns `None`.
- Bounded mailbox (`mailbox max = N`): `await self._mailbox.put(...)`; the method becomes `async def`, returns `None`, and the cast call site becomes an `await`.

Call lowering:

- Always emits `async def`; the reply Future is constructed via `loop.create_future()` (not `asyncio.Future()`) so uvloop or any non-default loop produces the right Future subclass.
- The handler checks `if not fut.done():` before `set_result` so a cancelled or timed-out call does not raise `InvalidStateError` on the agent side.
- With timeout (`counter ? Get within 5.seconds`): the call site becomes `await asyncio.wait_for(counter.get(), timeout=5.0)`. The wait_for guard sets a flag on the request message so the handler skips replying to a known-orphaned Future.

The lowered method names follow Mochi-style snake_case: `cast IncrementBy(n: int)` becomes `def increment_by(self, n: int) -> None`. The IR pass strips the message dataclass construction at the call site; the user writes `counter ! IncrementBy(5)` and reads `counter.increment_by(5)`.

## Sub-phase 9.2 -- TaskGroup supervision

### Goal-alignment audit (9.2)

Mochi inherits Erlang OTP's supervision model: agents fail, supervisors restart. Without 9.2 the agent classes from 9.0 and 9.1 run unsupervised; the first unhandled exception in a handler propagates, cancels every sibling agent in the TaskGroup, and exits the entire `async with` scope. That is the `one_for_all` default and it is sometimes wanted, but the OTP `one_for_one` and `rest_for_one` strategies have no asyncio analogue and have to be hand-rolled. Landing 9.2 ships all three on top of TaskGroup; without it the Mochi `supervisor(strategy=one_for_one)` form has no Python target.

### Decisions made (9.2)

`one_for_all` is native to `asyncio.TaskGroup`: any child exception cancels all siblings and re-raises `ExceptionGroup` at the parent. Mochi `supervisor { ... }` with no annotation lowers directly:

```python
from __future__ import annotations

import asyncio


async def main() -> None:
    async with asyncio.TaskGroup() as tg:
        counter = Counter(tg)
        logger = Logger(tg)
        counter.increment(1)
        n = await counter.get()
        print(n)
```

`one_for_one` requires a wrapper coroutine that catches per-child exceptions and restarts:

```python
from __future__ import annotations

import asyncio
from collections.abc import Awaitable, Callable
from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class _ChildSpec:
    name: str
    factory: Callable[[asyncio.TaskGroup], Awaitable[None]]
    restart_on_error: bool
    backoff_seconds: float


async def _supervise_one_for_one(scope: asyncio.TaskGroup, specs: list[_ChildSpec]) -> None:
    async def _run_child(spec: _ChildSpec) -> None:
        while True:
            try:
                await spec.factory(scope)
                return
            except asyncio.CancelledError:
                raise
            except Exception:
                if not spec.restart_on_error:
                    raise
                await asyncio.sleep(spec.backoff_seconds)

    async with asyncio.TaskGroup() as inner_tg:
        for spec in specs:
            inner_tg.create_task(_run_child(spec))
```

The IR pass picks the wrapper based on the Mochi `strategy` annotation: `one_for_all` emits the bare `async with asyncio.TaskGroup()`, `one_for_one` emits the `_supervise_one_for_one` wrapper, `rest_for_one` emits a `_supervise_rest_for_one` wrapper that slices the spec list at the failed index and restarts the tail.

Default restart policy is `transient` (restart only on error, not on normal exit); `permanent` (always restart) and `temporary` (never restart) are alternate policies emitted from per-child annotations. Backoff defaults to 100ms linear; exponential backoff is a v2 deferral.

## Sub-phase 9.3 -- ExceptionGroup unwrap to MochiResult.Err

### Goal-alignment audit (9.3)

PEP 654 `ExceptionGroup` is what `asyncio.TaskGroup` re-raises when sibling tasks fail concurrently. Without 9.3 the user-facing Mochi error model (`Result<T, E>` with one `Err` carrying one value) cannot represent the multi-failure case at all; the Python target would either swallow all but the first exception or surface a `BaseExceptionGroup` to user code, neither of which round-trips against vm3. Landing 9.3 makes the Mochi `recover { ... }` form catch the group, unwrap inner exceptions, and lift them into a `MochiResult.Err[list[InnerError]]` payload that user code can match on.

### Decisions made (9.3)

Mochi `recover { ... } with err -> ...` lowers to PEP 654 `except*`:

```python
from __future__ import annotations

import asyncio
from mochi_runtime.result import Err, MochiResult, Ok


async def run_workers() -> MochiResult[int, list[Exception]]:
    try:
        async with asyncio.TaskGroup() as tg:
            tg.create_task(worker_a())
            tg.create_task(worker_b())
        return Ok(0)
    except* Exception as eg:
        return Err(list(eg.exceptions))
```

The IR pass groups all `except*` arms by exception type; arms catching a Mochi error variant (e.g. `recover { ... } with ParseError -> ...`) lower to `except* ParseError as eg`. Arms catching the universal `Exception` lower to `except* Exception as eg`. The runtime guarantees that `eg.exceptions` is a flat list (any nested `ExceptionGroup` is flattened by `eg.split()` before passing to user code).

`BaseException` (cancellation, system exit, keyboard interrupt) is never caught by `recover`. Mochi has no surface for catching cancellation; the user can only catch errors, not interrupts.

The Mochi panic form (`panic "msg"`) lowers to `raise RuntimeError(msg)`. Panics inside an agent handler propagate through `_loop`, get re-raised at the TaskGroup parent, and are caught by the enclosing `recover` block as part of the ExceptionGroup. The MochiResult wrapping is consistent: a successful TaskGroup returns `Ok`, a panic-on-any-child returns `Err`.

## Sub-phase 9.4 -- Named agents and registry

### Goal-alignment audit (9.4)

Mochi supports named agents (`register counter as "counters.main"`) for cross-module references and for the supervisor restart hook. Without 9.4 the named-agent form has no Python target and the Mochi `lookup("counters.main")` form does not lower. Landing 9.4 makes named agents reachable by name across module boundaries, which is the prerequisite for the Phase 10 stream registry and the Phase 13 LLM provider registry (both reuse the registry mechanism).

### Decisions made (9.4)

The registry lives at `mochi_runtime.agents.Registry`:

```python
from __future__ import annotations

import weakref
from collections.abc import Iterator
from dataclasses import dataclass
from typing import Final


@dataclass(frozen=True, slots=True)
class _RegistryEntry:
    name: str
    agent: object


class Registry:
    def __init__(self) -> None:
        self._entries: Final[dict[str, weakref.ref[object]]] = {}

    def register(self, name: str, agent: object) -> None:
        if name in self._entries:
            raise ValueError(f"agent name already registered: {name}")
        self._entries[name] = weakref.ref(agent, lambda _: self._entries.pop(name, None))

    def lookup(self, name: str) -> object | None:
        ref = self._entries.get(name)
        if ref is None:
            return None
        return ref()

    def names(self) -> Iterator[str]:
        return iter(self._entries)


_REGISTRY: Final[Registry] = Registry()


def register(name: str, agent: object) -> None:
    _REGISTRY.register(name, agent)


def lookup(name: str) -> object | None:
    return _REGISTRY.lookup(name)
```

`weakref.ref` ensures that registered agents do not pin themselves alive past the TaskGroup scope exit; when the parent scope cancels the agent's `_task`, the agent becomes unreachable and the registry entry is purged via the finalizer callback.

Mochi `register counter as "counters.main"` lowers to:

```python
from mochi_runtime.agents import register

register("counters.main", counter)
```

Mochi `let counter = lookup("counters.main") as Counter` lowers to:

```python
from mochi_runtime.agents import lookup

counter_raw = lookup("counters.main")
assert counter_raw is not None
assert isinstance(counter_raw, Counter)
counter: Counter = counter_raw
```

The `assert isinstance` guard is required for mypy and pyright to narrow `object | None` to `Counter`. The IR pass emits it; the user does not write it.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/agent.go` | Agent class generation: queue field, `_loop`, message dataclasses, PEP 695 union alias |
| `transpiler3/python/lower/agent_cast.go` | Cast lowering: `put_nowait` vs `await put` based on mailbox bound |
| `transpiler3/python/lower/agent_call.go` | Call lowering: Future construction + reply dataclass + await |
| `transpiler3/python/lower/supervisor.go` | TaskGroup wrappers for one_for_all / one_for_one / rest_for_one |
| `transpiler3/python/lower/recover.go` | `recover { ... } with ...` to `except* X as eg` |
| `runtime/python/mochi_runtime/agents/__init__.py` | Public surface: `Registry`, `register`, `lookup` |
| `runtime/python/mochi_runtime/agents/_supervisor.py` | `_ChildSpec`, `_supervise_one_for_one`, `_supervise_rest_for_one` |
| `runtime/python/mochi_runtime/result.py` | `Ok[T]`, `Err[E]`, `MochiResult[T, E]` frozen-slots dataclasses (cross-imported by Phase 11) |
| `transpiler3/python/build/phase09_test.go` | `TestPhase9Agents`: 35 fixtures + mypy/pyright/ruff gates |
| `tests/transpiler3/python/fixtures/phase09-agents/` | 35 fixture directories |

## Test set

- `TestPhase9Agents` -- 35 fixtures across the five sub-phases: counter, accumulator, balance, toggle, greeter (5 from 9.0); counter_get, balance_check, kv_store_get, name_query, ping_pong (5 from 9.1); supervisor_one_for_all, supervisor_one_for_one, supervisor_rest_for_one, restart_transient, restart_permanent, restart_temporary, backoff_linear (7 from 9.2); recover_single_err, recover_multi_err, recover_typed_err, panic_to_runtime_error, exception_group_flatten (5 from 9.3); registry_register, registry_lookup, registry_weakref_gc, registry_cross_module, named_supervisor (5 from 9.4); plus 8 mixed-fixture integration tests covering cast-then-call patterns, agent-spawns-agent, agent-publishes-to-stream, and the canonical "kv store with TTL eviction" worked example.

## Deferred work

- `asyncio.Queue.shutdown()` (Python 3.13 `gh-104873`): cleaner mailbox shutdown than the sentinel pattern. Deferred until the 3.13 floor is mandatory; 3.12 keeps the sentinel.
- Distributed agents (cross-process or cross-host messaging). Deferred to Phase 14+; would require a transport layer (gRPC, NATS, or ZeroMQ) and a serialization story for message dataclasses.
- Mailbox metrics (queue depth, dispatch latency) emitted via `mochi_runtime.metrics`. Deferred to a v1.5 observability MEP.
- Free-threaded 3.13 agent acceleration (multi-thread dispatch on `--disable-gil`). Deferred; v1 runs on the GIL and is correct under it.
