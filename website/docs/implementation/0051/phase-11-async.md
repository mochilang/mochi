---
title: "Phase 11. async coloring and MochiResult"
sidebar_position: 16
sidebar_label: "Phase 11. async + MochiResult"
description: "MEP-51 Phase 11 -- whole-program async coloring (fixed-point over the call graph), await/async for/async with lowering, MochiResult Ok/Err with throws E error model, ExceptionGroup unwrap, panic to RuntimeError; 30 fixtures."
---

# Phase 11. async coloring and MochiResult

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 11](/docs/mep/mep-0051#phase-plan) |
| Status         | NOT STARTED |
| Started        | -- |
| Landed         | -- |
| Tracking issue | -- |
| Tracking PR    | -- |

## Gate

`TestPhase11Async`: 30 fixtures green on CPython 3.12.0 and 3.13.0 across x86_64-linux-gnu, aarch64-linux-gnu, aarch64-darwin, and x86_64-windows. Secondary gates: `mypy --strict --python-version=3.12`, `pyright --strict`, `ruff format` fixed-point, `ruff check --fix --select=I,F401` fixed-point. Tertiary gates: byte-equal stdout against vm3 for every fixture; no `Any` leakage in any emitted module per `mypy --strict`; PEP 654 ExceptionGroup round-trip preserved through the MochiResult wrapping.

## Goal-alignment audit

Phase 9 (agents) and Phase 10 (streams) seeded the async story; Phase 11 completes it. The colour pass propagates "this function is async" through the call graph by fixed-point, so the user writes `fun` and the emitter picks `def` or `async def`. The MochiResult tagged union supplants Python exceptions for the Mochi `throws E` surface, which is what the rest of the runtime (LLM in Phase 13, fetch in Phase 14) consumes. PEP 654 `ExceptionGroup` is the bridge between asyncio's multi-failure semantics and Mochi's flat error model. Without Phase 11 the agent and stream phases ship but Mochi `await`, `async for`, `async with`, `throws`, `recover`, and `panic` have no Python target, and `mypy --strict` flags every emitted module on `Any` leakage. Landing 11 is what makes the Python target a real production-strength typed-async backend.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 11.0 | Async coloring pass: call-graph build, seed from `await` and agent `call`, fixed-point propagation, deterministic node ordering | NOT STARTED | -- |
| 11.1 | `await` lowering, `async for` over AsyncIterator, `async with` over async context managers | NOT STARTED | -- |
| 11.2 | `MochiResult[T, E]` with `Ok[T]` and `Err[E]` frozen-slots dataclasses in `mochi_runtime.result`; `throws E` lowering | NOT STARTED | -- |
| 11.3 | PEP 654 `ExceptionGroup` caught at TaskGroup parent, unwrapped to `MochiResult.Err` carrying list of inner errors | NOT STARTED | -- |
| 11.4 | `panic` lowered to `raise RuntimeError(msg)`; `panic` never goes through `MochiResult` and is not catchable by `recover` | NOT STARTED | -- |

## Sub-phase 11.0 -- Async coloring pass

### Goal-alignment audit (11.0)

Phase 9 manually coloured agent-touching functions; Phase 11.0 generalises to the whole program. Without the colour pass, every Mochi function would need an explicit `fun` vs `async fun` annotation, which is verbose and error-prone. The fixed-point pass walks the call graph from "must be async" seeds (await sites, agent calls, fetch, LLM) outwards; any function transitively reachable from a seed becomes `async def`. Landing 11.0 is what makes Mochi's "you don't write the colour, the compiler picks it" promise hold on the Python target.

### Decisions made (11.0)

The colour pass lives at `transpiler3/python/colour/colour.go`. It runs between `aotir` and `lower`. Algorithm:

1. Build the call graph over `aotir.Program`: nodes are functions, edges are calls (including method calls on agent classes and stream operators).
2. Seed the `Red` (async) set:
   - Any function containing an `AwaitExpr`.
   - Any function containing an agent `call` (Mochi `?`).
   - Any function containing a `FetchCallExpr` (Phase 14).
   - Any function containing an `LlmCallExpr` (Phase 13).
   - Any function containing an `async for` or `async with` source.
   - The `_loop` method of every emitted agent class.
   - Any function returning `AsyncIterator[T]`.
3. Fixed-point: for each `Blue` (sync) function, if it calls any `Red` function, recolour to `Red`. Repeat until no changes.
4. Produce `ColourMap: map[aotir.FuncID]Colour` consumed by `lower/`.

Worked example:

```mochi
fun double(n: int) -> int { return n * 2 }
async fun fetch_n() -> int { return await fetch("/n") }
fun main() {
    let n = await fetch_n()  // -> main becomes async
    print(double(n))
}
```

Emit after colour:

```python
from __future__ import annotations

import asyncio
from mochi_runtime.fetch import fetch as _runtime_fetch


def double(n: int) -> int:
    return n * 2


async def fetch_n() -> int:
    return await _runtime_fetch("/n")


async def main() -> None:
    n = await fetch_n()
    print(double(n))


def _entry() -> None:
    asyncio.run(main())


if __name__ == "__main__":
    _entry()
```

Decisions:

- `double` stays `Blue` (sync) because it never reaches a seed.
- `main` was `Blue` in the source; the `await fetch_n()` site re-colours it to `Red`.
- The pass is deterministic: nodes are processed in sorted-by-FuncID order so two runs produce identical output.
- The pass rejects programs where a sync function would need to call a Red function unconditionally; the diagnostic points at the call site and suggests adding `await`.
- The entry-point wrapper `_entry` is the only emitted sync function in a Red-Main program; `asyncio.run` is called exactly once per process.

The colour pass produces a JSON dump at `--target=python-source --emit-colour-map` for debugging; the dump is not part of the wheel.

## Sub-phase 11.1 -- await, async for, async with

### Goal-alignment audit (11.1)

The colour pass picks `async def`; sub-phase 11.1 is the corresponding expression-level lowering. Mochi `await`, `for ... in stream`, and `with` over async context managers all have to be rewritten to their async forms. Without 11.1 the colour pass produces `async def` functions whose bodies still contain sync `for` and sync `with` over async-typed sources, which type-checks but produces wrong runtime behaviour. Landing 11.1 closes that loop.

### Decisions made (11.1)

Mochi:

```mochi
async fun consume(stream: Stream<int>) {
    with file = open_log("./log.txt") {
        for n in stream {
            await write_line(file, n.to_string())
        }
    }
}
```

Emit:

```python
from __future__ import annotations

from collections.abc import AsyncIterator
from mochi_runtime.io import open_log, write_line


async def consume(stream: AsyncIterator[int]) -> None:
    async with open_log("./log.txt") as file:
        async for n in stream:
            await write_line(file, str(n))
```

Decisions:

- `await expr` lowers directly to Python `await expr`. The IR pass guarantees that the expression is awaitable (returns `Awaitable[T]` or `Coroutine[Any, Any, T]`); the type checker enforces this independently.
- Mochi `for x in stream` over a `Stream<T>` source lowers to `async for`; over a `List<T>` or `Iterator<T>` source it stays sync. The IR pass picks based on the source's static type.
- Mochi `with` over a `__aenter__` / `__aexit__` context manager lowers to `async with`; over a `__enter__` / `__exit__` context manager it stays sync. The IR pass picks based on the resource's protocol conformance (Mochi-level trait check).
- `async with` blocks always have an `as` binding when the Mochi source assigns to a name; bare `async with` (no binding) is allowed when the resource is only used for its setup/teardown side-effects.
- `await` inside a list comprehension lowers to an `async for` plus a temporary list rather than the awkward `[await x for x in ...]` (which CPython parses but pyright dislikes). The IR pass picks the rewritten form.

`asyncio.run(main())` is emitted exactly once at the module entry point; nested calls (the user-facing Mochi script that calls `asyncio.run` itself) are rejected at the IR level (one event loop per process).

## Sub-phase 11.2 -- MochiResult Ok/Err

### Goal-alignment audit (11.2)

Python exceptions are stringly-typed at the catch site (`except SomeError as e: ...`) and cannot be reasoned about by mypy or pyright as part of the function signature. Mochi `throws E` is part of the type, so the Python target needs a structured error carrier. `MochiResult[T, E]` (tagged union of `Ok[T]` and `Err[E]`) is the canonical answer; landing 11.2 makes the `throws` surface usable on the Python target. Without 11.2 the only error path on the Python side is Python exceptions, which means every Mochi `throws E` function would silently lose the error type.

### Decisions made (11.2)

The runtime types live at `runtime/python/mochi_runtime/result.py`:

```python
from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class Ok[T]:
    value: T


@dataclass(frozen=True, slots=True)
class Err[E]:
    error: E


type MochiResult[T, E] = Ok[T] | Err[E]
```

Mochi `fun parse(s: String) -> Ast throws ParseError` lowers to:

```python
from __future__ import annotations

from mochi_runtime.result import Err, MochiResult, Ok


def parse(s: str) -> MochiResult[Ast, ParseError]:
    if not s:
        return Err(ParseError(message="empty input"))
    return Ok(Ast(...))
```

Consumer site:

```python
match parse(s):
    case Ok(value=ast):
        print(ast)
    case Err(error=err):
        print(f"parse failed: {err.message}")
```

Decisions:

- `Ok[T]` and `Err[E]` are PEP 695 generic frozen-slots dataclasses. `Ok` carries `value: T`; `Err` carries `error: E`. Field names are intentionally distinct so positional `case Ok(v)` and `case Err(e)` both work via `__match_args__`.
- `MochiResult[T, E]` is a PEP 695 `type` alias over the union. mypy 1.13 and pyright 1.1.380 both narrow inside `match` correctly; the exhaustiveness check in `case _:` would fire if a third variant were ever added.
- Multiple Mochi error types lower to a union: `throws ParseError | IoError` becomes `MochiResult[Ast, ParseError | IoError]`. The `case Err(error=ParseError() as pe):` pattern narrows on the inner variant.
- The `?` operator (Mochi error-propagation) lowers to a `match` with early `return Err(...)` for the `Err` arm. The IR pass desugars `let x = expr?` into:

  ```python
  match expr:
      case Err(error=_err):
          return Err(_err)
      case Ok(value=x_inner):
          x = x_inner
  ```

- No `try / except` is emitted for Mochi `throws`; exceptions are reserved for `panic` (11.4) and for foreign FFI boundaries (Phase 12). This is the load-bearing choice that keeps the Python output statically checkable.

## Sub-phase 11.3 -- ExceptionGroup unwrap

### Goal-alignment audit (11.3)

PEP 654 `ExceptionGroup` is what `asyncio.TaskGroup` re-raises on multi-child failure. Mochi has no language surface for catching exception groups, but the runtime must not lose the multi-failure information; if it did, debugging a concurrent program that fails would only surface the first child exception. Landing 11.3 captures the group at the TaskGroup parent, lifts every inner exception into a Mochi-level error value, and bundles them into `MochiResult.Err[list[E]]`. The user code can then match on the list and either re-raise the first, log all, or aggregate.

### Decisions made (11.3)

Mochi:

```mochi
async fun run_workers() -> int throws list<WorkerError> {
    recover {
        spawn worker_a()
        spawn worker_b()
        return 0
    } with err -> {
        return err
    }
}
```

Emit:

```python
from __future__ import annotations

import asyncio
from mochi_runtime.result import Err, MochiResult, Ok


async def run_workers() -> MochiResult[int, list[WorkerError]]:
    try:
        async with asyncio.TaskGroup() as tg:
            tg.create_task(worker_a())
            tg.create_task(worker_b())
        return Ok(0)
    except* WorkerError as eg:
        return Err([e for e in eg.exceptions if isinstance(e, WorkerError)])
```

Decisions:

- The runtime guarantees `eg.exceptions` is a flat iterable of base exceptions. Nested `ExceptionGroup` instances are flattened by `eg.split()` before the comprehension; the IR pass injects the flatten when the catch arm is for a single concrete type.
- The list comprehension filter (`isinstance(e, WorkerError)`) narrows the typed list back to `list[WorkerError]`. Without it, mypy widens to `list[BaseException]`.
- `except* Exception` is the catch-all for typed error propagation; `except* BaseException` is never emitted (would catch `CancelledError`, which is wrong).
- Mochi `recover { ... } with err: list<E> -> ...` accepts the typed list; Mochi `recover { ... } with err: E -> ...` (single error) emits a `match` that errors at runtime if more than one exception arrived (or, optionally per Mochi declaration, returns the first).
- The runtime ships a helper `mochi_runtime.result.flatten_exception_group(eg, typ)` that does the filter-and-narrow for the IR pass; the emit calls into it when the error type is non-trivial.

## Sub-phase 11.4 -- panic to RuntimeError

### Goal-alignment audit (11.4)

Panics are Mochi's unrecoverable-error surface; they propagate through every scope and terminate the program with a stack trace. On Python, the natural carrier is `RuntimeError` (or a custom `MochiPanic` subclass). The split between recoverable errors (MochiResult.Err) and panics (raised exceptions) is what keeps the type system honest: a function signature says `throws E` for the first and stays silent on the second. Landing 11.4 closes the error model.

### Decisions made (11.4)

Mochi `panic "message"` lowers to `raise RuntimeError("message")`. Mochi `panic err` where `err: E` lowers to `raise RuntimeError(str(err))`. The IR pass injects `str(err)` so any Mochi value with a `to_string` method round-trips correctly.

```python
from __future__ import annotations


def must_have_key(d: dict[str, int], key: str) -> int:
    value = d.get(key)
    if value is None:
        raise RuntimeError(f"missing key: {key}")
    return value
```

Decisions:

- `RuntimeError` is the chosen Python type because it is in the `Exception` hierarchy (catchable by `except* Exception` if a `recover` block surrounds the panic site) but distinct from typed Mochi errors. The runtime does not provide a custom `MochiPanic` class in v1; if Mochi ever needs to distinguish, the IR pass can switch to `mochi_runtime.panic.MochiPanic` without breaking the catch behaviour because `MochiPanic` would also subclass `Exception`.
- A panic inside an agent handler propagates through `_loop`, gets re-raised at the TaskGroup parent, and arrives at the enclosing `recover` block as an `ExceptionGroup` member. Per 11.3, the IR pass wraps it back into `Err`. This is a special case: Mochi panics generally bypass `recover`, but when they happen inside a TaskGroup child they are caught at the group boundary because TaskGroup catches `Exception` (not just typed Mochi errors).
- Stack traces are preserved: the IR pass never strips `__cause__` or `__context__`, and CPython's PEP 657 fine-grained error locations surface in the Mochi-level panic message when `MOCHI_PANIC_LOCATIONS=1` is set.
- `assert` in Mochi lowers to `if not cond: raise RuntimeError(...)`; the Python `assert` statement is never emitted because `python -O` strips it and the byte-equal-stdout gate would diverge between optimised and non-optimised runs.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/colour/colour.go` | Async coloring driver: build call graph, seed Red set, fixed-point propagate |
| `transpiler3/python/colour/graph.go` | Call-graph construction from `aotir.Program` (agents, streams, fetch, LLM seeds) |
| `transpiler3/python/colour/fixpoint.go` | Deterministic fixed-point iteration with sorted node ordering |
| `transpiler3/python/lower/await.go` | `AwaitExpr` to `await`; `async for`; `async with` |
| `transpiler3/python/lower/result.go` | `Ok` / `Err` construction; `throws E` return-type to `MochiResult[T, E]` |
| `transpiler3/python/lower/propagate.go` | `?` operator desugar to match + early-return Err |
| `transpiler3/python/lower/except_group.go` | `recover { ... } with err -> ...` to `except* X as eg` + flatten |
| `transpiler3/python/lower/panic.go` | `panic` to `raise RuntimeError` |
| `runtime/python/mochi_runtime/result.py` | `Ok[T]`, `Err[E]`, `MochiResult[T, E]` plus `flatten_exception_group` helper |
| `transpiler3/python/build/phase11_test.go` | `TestPhase11Async`: 30 fixtures + mypy/pyright/ruff gates + colour-determinism gate |
| `tests/transpiler3/python/fixtures/phase11-async/` | 30 fixture directories |

## Test set

- `TestPhase11Async` -- 30 fixtures: colour_fixpoint_basic, colour_fixpoint_indirect, colour_red_main, colour_agent_seed, colour_stream_seed, colour_fetch_seed, colour_llm_seed, colour_no_promotion (8 from 11.0); await_int, await_string, async_for_stream, async_with_resource, async_with_no_binding, await_comprehension (6 from 11.1); result_ok, result_err, result_match, result_propagate_question, result_multi_err_union, result_throws_inferred (6 from 11.2); except_group_single, except_group_multi, except_group_flatten, except_group_typed_list, except_group_in_recover (5 from 11.3); panic_string, panic_typed, panic_in_agent, panic_assert_lowered, panic_stack_trace (5 from 11.4).

## Deferred work

- `asyncio.TaskGroup` improvements landing in CPython 3.13+ (per-task cancellation, `cancel_scope`). Deferred; v1 uses the 3.12 form.
- Colour-determinism property test (random-graph fuzzing). Deferred to a v1.5 hardening pass; v1 ships the sorted-order determinism gate only.
- Mochi `throws E` overload set (functions whose error type varies per overload). Deferred pending Mochi-side decision; v1 emits one error type per function.
- `MochiPanic` subclass of `RuntimeError` for fine-grained catching at FFI boundaries. Deferred to Phase 12; v1 emits bare `RuntimeError`.
