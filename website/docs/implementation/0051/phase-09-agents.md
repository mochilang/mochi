---
title: "Phase 9. Agents"
sidebar_position: 14
sidebar_label: "Phase 9. Agents"
description: "MEP-51 Phase 9 -- in-process Mochi agents lower to a plain Python class with mutable fields and intent methods; channels lower to collections.deque(maxlen=N); spawn / cross-task / async deferred to Phase 10."
---

# Phase 9. Agents

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 9](/docs/mep/mep-0051#phase-plan) |
| Status         | LANDED (9.0 only; 9.1-9.4 DEFERRED to Phase 10/11) |
| Started        | 2026-05-29 18:00 (GMT+7) |
| Landed         | 2026-05-29 19:07 (GMT+7) |
| Tracking issue | (filled at ship) |
| Tracking PR    | (filled at ship) |

## Gate

`TestPhase9Agents`: 10 fixtures green on CPython 3.12.7 in `transpiler3/python/build/phase09_test.go`. The corpus splits 5/5 between `agent_*` (in-process synchronous intents) and `chan_*` (bounded single-threaded FIFO channels). Each fixture rebuilds from `tests/transpiler3/python/fixtures/phase09-agents/*.mochi`, runs `python -m mochi_user_<name>`, and byte-compares stdout to the matching `.out` file. The full Phase 1-9 regression suite (`go test ./transpiler3/python/... -count=1`) finishes in 121s with zero regressions.

## Goal-alignment audit

Mochi's primary in-process concurrency abstraction is the agent: a stateful entity with mutable fields and intent methods that act as message handlers. Phase 9.0 is what turns "the Python target accepts agents at all" from false to true. Without 9.0 every Mochi program that declares `agent Counter { ... }` rejects at the Python target with "agent unsupported", which is the gate that blocks Phase 10 (streams reuse the channel substrate), Phase 11 (async coloring is seeded from agent-touching functions), and the Jupyter ipykernel (Phase 17, which embeds agents inside notebook cells). Landing 9.0 ships the load-bearing emit shape: every later sub-phase extends the same class layout without breaking emit.

The spawn / cast / call surface (9.1-9.4 in the original plan) is genuinely deferred, not punted: the Mochi C lower (`transpiler3/c/lower/lower.go` §Phase 9.3) currently rejects spawn-bound intent calls (`AgentIntentCallExpr.SpawnedRef = true` triggers a separate async lowering path) at the C target too, so the gap is upstream-shaped: the IR does not yet surface async-marked intent bodies. Phase 10's `Streams` work will introduce the async colour pass that drives both stream consumers and spawn-receiver dispatch; agents 9.1+ ride on top of that. Phase 9.0 standing alone is correct precisely because the synchronous in-process agent surface is what Mochi v1 fixtures exercise today (`tests/transpiler3/c/fixtures/agent/*` and `tests/transpiler3/c/fixtures/chan/*` are all single-thread, no-spawn).

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 9.0 | Agent class + intent methods (sync, no spawn); channel via `collections.deque(maxlen=N)` send/recv | LANDED 2026-05-29 | (filled at ship) |
| 9.1 | Cast (`put_nowait`) + call (`asyncio.Future`) over `asyncio.Queue`; spawn-bound intent calls | DEFERRED to Phase 10 | -- |
| 9.2 | TaskGroup supervision (one_for_all / one_for_one / rest_for_one) | DEFERRED to Phase 11 | -- |
| 9.3 | ExceptionGroup unwrap to `MochiResult.Err` (PEP 654 `except*`) | DEFERRED to Phase 11 | -- |
| 9.4 | Named agents and `mochi_runtime.agents.Registry` | DEFERRED to Phase 11 | -- |

## Sub-phase 9.0 -- Synchronous in-process agents and bounded channels

### Goal-alignment audit (9.0)

The synchronous agent surface is the load-bearing emit shape for the entire concurrency story. If the class layout is wrong, every subsequent sub-phase has to re-litigate constructor signatures, field-access rewriting, and intent method dispatch. Landing 9.0 first locks the shape: agent fields go on `self`, intent methods are bare `def name(self, ...) -> R`, and bodies rewrite VarRef `__self->field` to `self.field`. Phase 10 layers `_mailbox: asyncio.Queue` and a `_loop()` coroutine on top without touching the existing field/intent emit.

The channel surface (`collections.deque(maxlen=N)`) is exactly what the existing single-threaded fixture corpus exercises: every send is followed by a recv on the same execution path, no producer/consumer split, no blocking semantics. `deque` is in stdlib, is `O(1)` for `append`/`popleft`, and silently drops the leftmost element on overflow (matching the no-overflow invariant the fixtures rely on). Phase 10 will switch chan emit to `asyncio.Queue` when cross-task send/recv lands; the IR shape doesn't change.

### Decisions made (9.0)

**Class layout: plain class, not @dataclass.** A Mochi `agent Counter { var count: int = 0; intent inc(n: int) { count = count + 1 } }` emits:

```python
class Counter:
    count: int

    def __init__(self, count: int) -> None:
        self.count = count

    def inc(self, n: int) -> None:
        self.count = (self.count + 1)
```

A `@dataclass` would be ergonomic for the field plumbing but conflicts with the intent method body referencing `self.count`: `@dataclass` requires field defaults to be values, not method bodies, and rejects subclassing the field set. The plain class form matches what records already emit minus `frozen=True, slots=True`, so the codepath is one explicit constructor away from the existing record emit at `transpiler3/python/lower/record.go`.

**Field access rewrite via the `__self->` prefix.** The C lower bakes agent-field references into VarRef.Name as `__self->fieldname` (see `transpiler3/c/lower/lower.go:8577`, `emitName: "__self->" + f.Name`). The Python VarRef case in `transpiler3/python/lower/lower.go` checks `strings.CutPrefix(v.Name, "__self->")` and emits `pysrc.Attribute{Value: Name{"self"}, Attr: field}`. AssignStmt has the same prefix check and emits `pysrc.AttrAssignStmt` (`self.field = val`). This means the Python lower never needs to know which names are agent fields; it just trusts the C lower's mangled VarRef.Name as the source of truth. The original design carried a `selfFields map[string]bool` scoped via push/pop around intent bodies, but the C lower's mangling makes the map redundant, so it was removed before ship.

**Channel via `collections.deque(maxlen=N)`.** A Mochi `let c = make(chan<int>(8))` emits `c: deque[int] = deque(maxlen=8)`; `c <- 5` emits `c.append(5)`; `let v = <-c` emits `v = c.popleft()`. The `deque` is single-threaded, has no synchronization overhead, and matches the single-execution-path semantics of every existing chan fixture. The import `from collections import deque` is gated on `l.needsDeque` so non-chan programs do not import it.

**`AgentLit` lowering.** A Mochi `Counter { count: 0 }` lowers to `Counter(count=0)`; the lowerer walks `AgentLit.Fields` in declaration order and emits keyword arguments. Positional construction would also work but keyword form is robust to field-reorder refactors and matches what records already emit.

**Intent return type Unit -> None.** `intent inc(n: int) {}` has Mochi return type Unit; Python serializes that as `-> None`. The lower checks `intent.ReturnType == aotir.TypeUnit` and substitutes `pysrc.TypeNone`.

**Spawn rejection is explicit.** `lowerAgentIntentCallExpr` checks `e.SpawnedRef` and returns `fmt.Errorf("python/lower: spawned-agent calls not supported (Phase 9.1 deferred to async surface)")`. Same for the statement form. This gives a clear error to anyone who tries to compile a spawn-based program against the Python target today; Phase 10 lifts the gate.

### Fixture corpus (10 fixtures)

`tests/transpiler3/python/fixtures/phase09-agents/`:

| Fixture | Surface | Notes |
|---------|---------|-------|
| `agent_basic.mochi` | `var count: int`, single intent `inc()` | Mutates field; baseline shape |
| `agent_bool.mochi` | `var active: bool`, intent toggles via `not active` | Bool field round-trip |
| `agent_float.mochi` | `var amount: float`, intent adds float param | Float field with param |
| `agent_string.mochi` | `var name: string`, intent reassigns from param | String field |
| `agent_multi_intent.mochi` | Multiple intents reading and writing the same field | Intent dispatch shape |
| `chan_basic.mochi` | `make(chan<int>(4))`, single send/recv | Baseline deque shape |
| `chan_bool.mochi` | `chan<bool>(2)` | Bool element type |
| `chan_string.mochi` | `chan<string>(2)` | String element type |
| `chan_buffered.mochi` | Multiple sends before any recv | Bounded capacity exercise |
| `chan_fifo_order.mochi` | Sends `1, 2, 3` then recvs three times | FIFO order verification |

Each fixture has a matching `.out` file with the canonical vm3 stdout. `TestPhase9Agents` walks the directory, runs `runPythonFixture` (which builds the package, invokes `python -m mochi_user_<name>`, and diffs stdout byte-for-byte). All 10 fixtures pass on CPython 3.12.7.

### Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/agent.go` (new) | `lowerAgentDecl`, `lowerAgentLit`, `lowerAgentIntentCallExpr/Stmt`, `lowerChanMakeExpr`, `lowerChanSendStmt`, `lowerChanRecvExpr` |
| `transpiler3/python/lower/lower.go` | VarRef + AssignStmt rewrite agent self-field via `__self->` prefix; `needsDeque` flag + `from collections import deque` gating; dispatch cases for the new aotir nodes; `lowerLetStmt` handles TypeAgent and TypeChan annotations; Agents loop in `Lower()`; RawCStmt no-op skip |
| `transpiler3/python/pysrc/nodes.go` | Extend `ClassDef` with `Init *FunctionDef` and `Methods []*FunctionDef`; add `AttrAssignStmt{Target Expr; Attr string; Value Expr}` |
| `transpiler3/python/build/build.go` | Cache marker `mep51-phase08` -> `mep51-phase09` |
| `transpiler3/python/build/phase09_test.go` (new) | `TestPhase9Agents` walks `phase09-agents/` |
| `tests/transpiler3/python/fixtures/phase09-agents/` (new) | 10 `.mochi` + 10 `.out` files |

## Deferred work

- **9.1 spawn + async cast/call.** Requires Phase 10's async colour pass to mark intent bodies as `async def` when the receiver is `spawn`ed. Until then the Python lower errors on `SpawnedRef = true`. The C lower already supports the form so the IR side is unblocked; the gap is the Python emit.
- **9.2 supervision strategies.** `one_for_all` is native to `asyncio.TaskGroup` but `one_for_one` and `rest_for_one` need wrapper coroutines (`_supervise_one_for_one` in `mochi_runtime.agents`). Deferred to Phase 11 alongside the runtime supervisor module.
- **9.3 `recover { ... }` to `except* X`.** PEP 654 ExceptionGroup unwrap. Requires Phase 11's MochiResult runtime + the async colour pass to know which calls cross task boundaries.
- **9.4 named-agent registry.** `register agent_name as "key"` and `lookup("key")`. Requires Phase 11's runtime registry module (`mochi_runtime.agents.Registry`).
- **`asyncio.Queue.shutdown()` (Python 3.13 gh-104873).** Cleaner mailbox shutdown than the sentinel pattern. Phase 9.0 uses neither (single-threaded deque); Phase 10 will choose between the sentinel pattern and the 3.13 method based on the floor at that point.
- **Distributed agents (cross-process / cross-host).** Not on the v1 roadmap. Would require a transport layer (gRPC, NATS, ZeroMQ) and a serialization story for intent payloads.
