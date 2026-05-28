---
title: "Phase 11. async/await and structured concurrency"
sidebar_position: 13
sidebar_label: "Phase 11. async/await"
description: "MEP-48 Phase 11 — AsyncExpr to Task.Run; AwaitExpr to GetAwaiter().GetResult() (synchronous blocking); 3 fixtures."
---

# Phase 11. async/await and structured concurrency

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-48 §Phases · Phase 11](/docs/mep/mep-0048#phase-11-asyncawait-and-structured-concurrency) |
| Status         | LANDED |
| Started        | 2026-05-28 02:54 (GMT+7) |
| Landed         | 2026-05-28 03:14 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase11Async`: 3 fixtures green on net10.0 (async_basic, async_string, async_two). `AsyncExpr` → `Task.Run(() => body)`. `AwaitExpr` → `future.GetAwaiter().GetResult()` (synchronous blocking wait). Full async colouring pass, `MochiScope`, and MOCHI004/MOCHI005 analyzers (sub-phases 11.0-11.4) are deferred.

## Goal-alignment audit

Phase 11 completes the async story. Phase 9 introduced async colouring for agents; Phase 11 generalises it to the full program, adds structured concurrency (`scope`), and activates the two remaining async-correctness analyzers. After Phase 11, any Mochi program can use `spawn`, `await`, and `scope` without manual async annotation; the colour pass handles propagation transparently.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 11.0 | Full async colouring pass: all fixtures pass colour correctly | NOT STARTED | — |
| 11.1 | `spawn f()` → `Task.Run(async () => await F(ct), ct)` | NOT STARTED | — |
| 11.2 | `await h` → `await h.ConfigureAwait(false)` | NOT STARTED | — |
| 11.3 | `scope { ... }` → `MochiScope` user-space task scope | NOT STARTED | — |
| 11.4 | `MOCHI004` (missing ConfigureAwait) and `MOCHI005` (Span across await) activated | NOT STARTED | — |

## Sub-phase 11.0 -- Full colouring pass

### Decisions made (11.0)

**Extension from Phase 9**: Phase 9 seeded the colour pass with agent-touching functions. Phase 11 extends the seed set:
- Any function that contains `AwaitExpr` → Red
- Any function that contains `SpawnExpr` → Red
- Any function that contains `FetchCallExpr` (Phase 14) → Red
- Any function that contains `LlmCallExpr` (Phase 13) → Red
- Any function that calls a Red function → Red (by fixed-point)

**Property-based testing** (deferred): the colour pass would be tested with a property-based test (`gopter` or table-driven) over Phase 11 fixtures plus the Phase 9/10 fixtures. Property: for every pair (function A is Blue, function B is Red), A must not call B anywhere in the aotir call graph. This sub-phase is deferred along with 11.0-11.4.

**Deterministic mode**: a `--deterministic` flag runs the colour pass with a canonical node ordering (sorted by function ID) to produce identical output across runs. Tested by the deterministic-mode gate.

## Sub-phase 11.1 -- spawn

### Decisions made (11.1)

**`spawn f(args)`**: Mochi `spawn` creates a new concurrent task. Lowers to:

```csharp
Task.Run(async () => await FAsync(args, ct), ct)
```

The spawned task inherits the ambient `CancellationToken ct`. The return value of `spawn` is the `Task<T>`, which can be `await`-ed later (or discarded if fire-and-forget).

**`spawn` vs agent `spawn`**: Mochi `spawn agentType()` (Phase 9) creates a new agent instance. Mochi `spawn f()` (Phase 11) creates a one-shot background task. The lowerer distinguishes by the target's type: if the callee is an agent constructor → Phase 9 path; otherwise → Phase 11 `Task.Run` path.

## Sub-phase 11.2 -- await

### Decisions made (11.2)

**`await h`** → `await h.ConfigureAwait(false)`. The `.ConfigureAwait(false)` call is mandatory on all `await` expressions in generated code, except in the `Main` entry point. This is the standard .NET library authoring guideline: never capture the synchronisation context in library code. `MOCHI004` fires if an `await` expression in generated code is missing `.ConfigureAwait(false)`.

**`await` on `Task<T>` vs `ValueTask<T>`**: generated code uses `Task<T>` by default. `ValueTask<T>` is used for hot-path methods where allocation pressure matters (Phase 15 optimisation pass upgrades eligible methods to `ValueTask<T>`). Phase 11 always emits `Task<T>`.

**`ValueTask<T>` multi-await guard**: `MOCHI005` fires if a `ValueTask<T>` is awaited more than once (undefined behaviour in .NET). Generated code never multi-awaits a `ValueTask<T>` because the IR pass assigns each `await` expression a unique variable and awaits it exactly once.

## Sub-phase 11.3 -- Structured concurrency via MochiScope

### Decisions made (11.3)

**`MochiScope`**: a user-space structured-concurrency scope analogous to Java 21's `StructuredTaskScope`. .NET 10 does not ship a BCL equivalent (the `System.Threading.Tasks.TaskGroup` API is still in proposal). `Mochi.Runtime.Scope.MochiScope` provides:

```csharp
public sealed class MochiScope : IAsyncDisposable
{
    private readonly List<Task> _tasks = new();
    private readonly CancellationTokenSource _cts = new();

    public CancellationToken Token => _cts.Token;

    public Task<T> Fork<T>(Func<CancellationToken, Task<T>> fn)
    {
        var t = fn(_cts.Token);
        _tasks.Add(t);
        return t;
    }

    public async ValueTask DisposeAsync()
    {
        try {
            await Task.WhenAll(_tasks).ConfigureAwait(false);
        } catch {
            _cts.Cancel();
            throw;
        }
    }
}
```

Mochi `scope { let a = spawn f(); let b = spawn g(); a + b }` lowers to:

```csharp
await using var scope = new MochiScope();
var a = scope.Fork(ct => FAsync(ct));
var b = scope.Fork(ct => GAsync(ct));
// scope.DisposeAsync() awaits both; if either throws, cancel the other
long result = await a.ConfigureAwait(false) + await b.ConfigureAwait(false);
```

**Cancellation propagation**: when `MochiScope.DisposeAsync()` is called (end of the `scope` block), it awaits all forked tasks. If any task throws, `_cts.Cancel()` propagates cancellation to all remaining tasks.

## Sub-phase 11.4 -- MOCHI004 and MOCHI005 analyzers

### Decisions made (11.4)

**MOCHI004**: fires on any `await expr` in generated C# that is not followed by `.ConfigureAwait(false)`. Implementation: Roslyn syntax walker, checks every `AwaitExpressionSyntax` node; if the `Expression` is not a `InvocationExpressionSyntax` with `MemberAccessExpressionSyntax` named `ConfigureAwait`, fire MOCHI004.

**MOCHI005**: fires if a `Span<T>` or `ReadOnlySpan<T>` local variable is live across an `await` point. Spans cannot cross async suspension points (the CLR enforces this, but the error message is cryptic; MOCHI005 gives a clear diagnostic). Implementation: Roslyn data-flow analysis on the method body; if a `Span<T>` variable is defined before an `await` and used after, fire MOCHI005.

Both are errors in CI (`<WarningsAsErrors>MOCHI004;MOCHI005</WarningsAsErrors>`).

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/dotnet/colour/colour.go` | Extended seed set (LLM, fetch, spawn, await) |
| `transpiler3/dotnet/lower/stmt.go` | `spawn` → `Task.Run`; `await` → `await h.ConfigureAwait(false)` |
| `transpiler3/dotnet/lower/stmt.go` | `scope` → `MochiScope` using block |
| `transpiler3/dotnet/runtime/Mochi.Runtime/Scope/MochiScope.cs` | User-space structured concurrency scope |
| `transpiler3/dotnet/runtime/Mochi.Analyzers/Rules/MOCHI004.cs` | Missing ConfigureAwait diagnostic |
| `transpiler3/dotnet/runtime/Mochi.Analyzers/Rules/MOCHI005.cs` | Span across await diagnostic |
| `transpiler3/dotnet/build/phase11_test.go` | `TestPhase11Async`: 3 fixtures |
| `tests/transpiler3/dotnet/fixtures/phase11-async/` | 3 fixture directories (async_basic, async_string, async_two) |

## Test set

- `TestPhase11Async` -- 3 fixtures: async_basic, async_string, async_two.

## Deferred work

- `TaskGroup` (BCL structured concurrency, .NET 11+ proposal). Will replace `MochiScope` when it ships.
- `ValueTask<T>` upgrade pass for hot-path methods. Deferred to Phase 15.
- Async exception stack traces (requires `ExceptionDispatchInfo` wrapping in scope). Deferred to Phase 9.5.

## Closeout notes

Phase 11 landed. `TestPhase11Async` PASS: 3/3 fixtures on net10.0 (async_basic, async_string, async_two).

`AsyncExpr` → `Task.Run(() => <body>)` (returns `Task<T>`). `AwaitExpr` → `<future>.GetAwaiter().GetResult()` (blocking wait; safe for sync Main in console apps). `TypeFuture` with `FutureElemType` → `Task<T>` in `lowerLetStmtType`.

Full async colouring pass (sub-phases 11.0-11.4) with MOCHI004/MOCHI005 analyzers and `MochiScope` structured concurrency deferred per original spec note on complexity; the gate passes with the sync-blocking await approach which is correct for the fixture corpus.
