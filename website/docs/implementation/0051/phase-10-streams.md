---
title: "Phase 10. Streams"
sidebar_position: 15
sidebar_label: "Phase 10. Streams"
description: "MEP-51 Phase 10 -- Mochi streams lower to collections.abc.AsyncIterator; cold streams as async def generators, hot streams as broadcast classes, bounded streams via asyncio.Queue(maxsize=N), map/filter/take/zip operators in mochi_runtime.stream; 25 fixtures."
---

# Phase 10. Streams

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 10](/docs/mep/mep-0051#phase-plan) |
| Status         | NOT STARTED |
| Started        | -- |
| Landed         | -- |
| Tracking issue | -- |
| Tracking PR    | -- |

## Gate

`TestPhase10Streams`: 25 fixtures green on CPython 3.12.0 and 3.13.0 across x86_64-linux-gnu, aarch64-linux-gnu, aarch64-darwin, and x86_64-windows. Secondary gates: `mypy --strict --python-version=3.12`, `pyright --strict`, `ruff format` fixed-point, `ruff check --fix --select=I,F401` fixed-point. Tertiary gate: byte-equal stdout against vm3 for every fixture (the stream scheduler uses a seeded `MochiClock` so cold-stream timing is repeatable).

## Goal-alignment audit

Mochi streams are the composable data-flow abstraction; without Phase 10 no Mochi program that uses `stream`, `yield`, `subscribe`, `map`, `filter`, `take`, or `zip` reaches the Python target. Phase 10 lands the `collections.abc.AsyncIterator[T]` lowering, the broadcast pattern for hot streams, the bounded-queue backpressure path, and the stream operator library. It is the prerequisite for Phase 13 (LLM `generate` returns a token stream), Phase 14 (`fetch` streaming body responses), and the Phase 17 ipykernel cell-by-cell streaming output protocol. The user payload is that a Mochi `stream<Tick>` becomes an `AsyncIterator[Tick]` that any Python consumer (a Jupyter cell, a FastAPI WebSocket handler, a numpy ingestion pipeline) can `async for` over.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 10.0 | Cold stream generator: `async def` with `yield` returning `AsyncIterator[T]`; one consumer per call | NOT STARTED | -- |
| 10.1 | Consume via `async for item in stream:`; consumer site lowers from Mochi `for item in stream` after sendability check | NOT STARTED | -- |
| 10.2 | Bounded streams via `asyncio.Queue(maxsize=N)` for backpressure; producer side awaits `put` when full | NOT STARTED | -- |
| 10.3 | Stream operators (`map`, `filter`, `take`, `zip`, `flat_map`, `collect`) in `mochi_runtime.stream` | NOT STARTED | -- |

## Sub-phase 10.0 -- Async generator

### Goal-alignment audit (10.0)

The cold-stream `async def` with `yield` is the load-bearing shape; it composes with everything downstream. mypy and pyright both narrow async generators to `AsyncIterator[T]` only when the return annotation matches exactly and no `return value` appears in the body. Landing 10.0 first locks the emit shape so the operator library at 10.3 can rely on it; without 10.0 the operator return types would not type-check against the consumer site.

### Decisions made (10.0)

Mochi `stream<Tick>` declaration plus a producing function lower to:

```python
from __future__ import annotations

import asyncio
from collections.abc import AsyncIterator
from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class Tick:
    n: int


async def ticks(count: int) -> AsyncIterator[Tick]:
    for i in range(count):
        await asyncio.sleep(0.0)
        yield Tick(n=i)
```

Decisions:

- Return annotation is `AsyncIterator[T]` from `collections.abc`, never `typing.AsyncIterator`. PEP 585 stabilised the abc form; mypy 1.13 and pyright 1.1.380 both accept it under `from __future__ import annotations`.
- The body contains at least one `yield`; CPython's `async def` plus `yield` produces an async generator. The body cannot contain a `return value` (an async generator's return is implicit `StopAsyncIteration`); the IR pass rejects Mochi source that combines `yield` and a non-trivial return value in the same function.
- `await asyncio.sleep(0.0)` is the canonical "give the loop a chance" pattern; cold streams that produce eagerly without yielding control would starve other tasks. The IR pass inserts a zero-sleep before each `yield` only when the surrounding analysis cannot prove the body already awaits.
- The constructor `Tick(n=i)` is the frozen-slots dataclass from Phase 4 (records); streams of records compose with the Phase 7 query DSL via `async for` comprehensions in Phase 10.3.

For factory-style producers (Mochi `let ticks = generate_ticks()`), the lowering keeps the function returning `AsyncIterator[T]` and the call site assigns the returned iterator:

```python
async def main() -> None:
    stream: AsyncIterator[Tick] = ticks(10)
    async for tick in stream:
        print(tick.n)
```

## Sub-phase 10.1 -- Async for consumption

### Goal-alignment audit (10.1)

The producer side (10.0) is useless without the consumer side. Mochi `for item in stream` over a stream-typed source has to lower to `async for` rather than `for`, and the consumer function has to be `async def` rather than `def`. Phase 11 will make this colour-pass automatic; for Phase 10.1 the lowering is direct (the Mochi type checker has already coloured the consumer) and the gate is that the emitted `async for` produces byte-equal stdout against vm3 over both eager and lazy stream sources.

### Decisions made (10.1)

Mochi:

```mochi
async fun print_ticks(ticks: Stream<Tick>) {
    for tick in ticks {
        print(tick.n)
    }
}
```

Emit:

```python
from __future__ import annotations

from collections.abc import AsyncIterator


async def print_ticks(ticks: AsyncIterator[Tick]) -> None:
    async for tick in ticks:
        print(tick.n)
```

Decisions:

- The consumer parameter type is `AsyncIterator[Tick]`, never `AsyncIterable[Tick]`. The narrower type lets pyright catch consumers that try to re-iterate a one-shot generator.
- `async for` is the only consumption form emitted; the IR pass never emits `await stream.__anext__()` directly because the loop-level form composes better with `break`, `continue`, and `else`.
- `async for ... else:` lowers from Mochi `for ... else` and is permitted; the `else` runs when the stream exhausts without a `break`.
- Early termination via `break` lowers naturally; the producer's `finally` clause runs because `async for` calls `aclose()` on the iterator when the loop exits early.
- Re-iteration of a cold stream requires re-calling the factory; the IR pass detects multi-use of an `AsyncIterator` value and either (a) materialises via `[x async for x in stream]` if the stream is statically bounded, or (b) reports a compile error suggesting `@hot` if the stream is unbounded.

## Sub-phase 10.2 -- Bounded streams

### Goal-alignment audit (10.2)

Cold streams without backpressure are unbounded by default; a fast producer plus a slow consumer fills the queue indefinitely. Mochi exposes `stream<T> max = N` for explicit backpressure; without 10.2 the bound annotation has no Python target and the producer can overflow memory. Landing 10.2 makes backpressure correct: the producer awaits `put`, the consumer drains via `async for`, and the queue acts as a one-element rendezvous when `N == 0` (synchronous handoff) or a buffer when `N > 0`.

### Decisions made (10.2)

Mochi `stream<Tick> max = 8` lowers to a bounded `asyncio.Queue` with a producer task and a consumer iterator:

```python
from __future__ import annotations

import asyncio
from collections.abc import AsyncIterator
from typing import Final


class _BoundedTickStream:
    def __init__(self, scope: asyncio.TaskGroup, maxsize: int) -> None:
        self._queue: Final[asyncio.Queue[Tick | None]] = asyncio.Queue(maxsize=maxsize)
        self._producer_task: Final[asyncio.Task[None]] = scope.create_task(self._produce())

    async def _produce(self) -> None:
        try:
            for i in range(1000):
                await self._queue.put(Tick(n=i))
            await self._queue.put(None)
        except asyncio.CancelledError:
            raise

    async def __aiter__(self) -> AsyncIterator[Tick]:
        while True:
            item = await self._queue.get()
            if item is None:
                return
            yield item


def bounded_ticks(scope: asyncio.TaskGroup, maxsize: int = 8) -> AsyncIterator[Tick]:
    return _BoundedTickStream(scope, maxsize).__aiter__()
```

Decisions:

- The sentinel value is `None` (the queue is typed `Tick | None`); when the producer exhausts, it sends `None` to signal end-of-stream. Python 3.13 added `asyncio.Queue.shutdown()` which obviates the sentinel; the v1 floor is 3.12 so the sentinel stays.
- `maxsize=0` is rejected at the IR level (unbounded; use the unbounded path instead). `maxsize=1` is the synchronous handoff; the producer blocks until the consumer takes.
- The producer is a child task of the parent `TaskGroup`; cancellation propagates naturally. The producer's `finally` (not shown) closes any resources.
- Bounded streams must declare a `TaskGroup` scope at the call site (the constructor parameter); the IR pass injects this from the enclosing `async with asyncio.TaskGroup() as tg:` context.

For the `!?` non-blocking variant on cast streams (the producer accepts `QueueFull` rather than awaiting), the emit uses `put_nowait` and surrounds with `try / except asyncio.QueueFull`. This is rarely seen on streams (mainly on agent mailboxes) but the lowering is identical for both.

## Sub-phase 10.3 -- Stream operators

### Goal-alignment audit (10.3)

The operator library (`map`, `filter`, `take`, `zip`, `flat_map`, `collect`) is what makes streams composable. Without 10.3 user code has to hand-write the `async for` plus `yield` loop for every transformation, which (a) is verbose and (b) loses type narrowing across stages. Landing 10.3 ships a typed, tested operator set so the query DSL (Phase 7) and the LLM stream consumer (Phase 13) can compose on top. The user payload is `tick_stream.map(double).filter(positive).take(10)` lowering to a single typed pipeline.

### Decisions made (10.3)

The operators live in `runtime/python/mochi_runtime/stream/operators.py`:

```python
from __future__ import annotations

from collections.abc import AsyncIterator, Awaitable, Callable


async def map_async[T, R](
    source: AsyncIterator[T],
    fn: Callable[[T], Awaitable[R]],
) -> AsyncIterator[R]:
    async for item in source:
        yield await fn(item)


async def filter_async[T](
    source: AsyncIterator[T],
    pred: Callable[[T], Awaitable[bool]],
) -> AsyncIterator[T]:
    async for item in source:
        if await pred(item):
            yield item


async def take_async[T](source: AsyncIterator[T], n: int) -> AsyncIterator[T]:
    if n <= 0:
        return
    count = 0
    async for item in source:
        yield item
        count += 1
        if count >= n:
            return


async def zip_async[A, B](
    left: AsyncIterator[A],
    right: AsyncIterator[B],
) -> AsyncIterator[tuple[A, B]]:
    left_iter = left.__aiter__()
    right_iter = right.__aiter__()
    while True:
        try:
            a = await left_iter.__anext__()
            b = await right_iter.__anext__()
        except StopAsyncIteration:
            return
        yield (a, b)


async def flat_map_async[T, R](
    source: AsyncIterator[T],
    fn: Callable[[T], AsyncIterator[R]],
) -> AsyncIterator[R]:
    async for item in source:
        async for sub in fn(item):
            yield sub


async def collect_async[T](source: AsyncIterator[T]) -> list[T]:
    return [item async for item in source]
```

Decisions:

- All generics use PEP 695 bracket syntax (`[T]` on the function); no `TypeVar` import. mypy 1.13 and pyright 1.1.380 both narrow correctly.
- Sync predicates are accepted via an overload that auto-wraps in `asyncio.to_thread`. The IR pass picks the overload based on the Mochi predicate's colour (sync or async).
- Mochi `stream.map(f)` lowers to `map_async(stream, f)`; the IR pass does not emit method-chained syntax because Python's async-generator classes do not natively support method chaining on the return type.
- `zip_async` stops at the shorter stream (matching `itertools.zip_longest=False`); a `zip_longest_async` variant is part of the runtime but not emitted unless the Mochi source explicitly opts in.
- `collect_async` is the eager terminal; for infinite streams the IR pass rejects the call.

A worked composition:

```python
from __future__ import annotations

import asyncio
from collections.abc import AsyncIterator
from mochi_runtime.stream import collect_async, filter_async, map_async, take_async


async def double(n: int) -> int:
    return n * 2


async def positive(n: int) -> bool:
    return n > 0


async def main() -> None:
    async with asyncio.TaskGroup() as tg:
        source: AsyncIterator[int] = ticks_int(100)
        doubled = map_async(source, double)
        positives = filter_async(doubled, positive)
        first_ten = take_async(positives, 10)
        result: list[int] = await collect_async(first_ten)
        for n in result:
            print(n)
```

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/stream.go` | Stream declaration to `async def` returning `AsyncIterator[T]`; cold path |
| `transpiler3/python/lower/stream_hot.go` | Hot stream emission as a broadcast class with per-subscriber queue (cross-link from Phase 9.4 registry) |
| `transpiler3/python/lower/stream_bounded.go` | Bounded stream emission with TaskGroup producer + sentinel close |
| `transpiler3/python/lower/stream_consume.go` | Mochi `for x in stream` to `async for x in stream` after colour check |
| `transpiler3/python/lower/stream_ops.go` | Operator call lowering: `stream.map(f)` to `map_async(stream, f)` |
| `runtime/python/mochi_runtime/stream/__init__.py` | Public surface: operators, hot stream class |
| `runtime/python/mochi_runtime/stream/operators.py` | `map_async`, `filter_async`, `take_async`, `zip_async`, `flat_map_async`, `collect_async` |
| `runtime/python/mochi_runtime/stream/hot.py` | `HotStream[T]` broadcast class with subscriber queues |
| `transpiler3/python/build/phase10_test.go` | `TestPhase10Streams`: 25 fixtures + mypy/pyright/ruff gates |
| `tests/transpiler3/python/fixtures/phase10-streams/` | 25 fixture directories |

## Test set

- `TestPhase10Streams` -- 25 fixtures: cold_count, cold_strings, cold_records, cold_take_early, cold_empty, cold_factory_call (6 from 10.0); consume_print, consume_sum, consume_break, consume_else, consume_re_iter_error (5 from 10.1); bounded_one, bounded_eight, bounded_overflow_block, bounded_producer_cancel, bounded_consumer_cancel (5 from 10.2); op_map, op_filter, op_take, op_zip, op_flat_map, op_collect, op_compose_chain, op_sync_predicate, op_filter_then_take (9 from 10.3).

## Deferred work

- `asyncio.Queue.shutdown()` (Python 3.13) for cleaner close than the sentinel pattern. Deferred until the 3.13 floor is mandatory.
- Hot stream replay buffer (late subscribers see the last N events). Deferred to Phase 10.4; v1 hot streams drop events emitted before subscription.
- Stream operator fusion (`map(f).map(g)` fused into one pass). Deferred to v2 optimiser; v1 emits the two-pass form.
- `aiostream` interop (third-party operator library) bridge. Deferred pending adoption signal; v1 ships the runtime's own operators only.
