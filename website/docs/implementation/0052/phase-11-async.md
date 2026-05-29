---
title: "Phase 11. async coloring + MochiResult"
sidebar_position: 12
sidebar_label: "Phase 11. async + MochiResult"
description: "MEP-52 Phase 11, async/await colour pass fully active across the compiler; MochiResult<T, E> Ok/Err discriminated union for error handling; AggregateError for multi-error sites; 30 fixtures."
---

# Phase 11. async coloring, MochiResult, AggregateError

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 11](/docs/mep/mep-0052#phase-plan) |
| Status         | NOT STARTED |
| Started        | n/a |
| Landed         | n/a |
| Tracking issue | n/a |
| Tracking PR    | n/a |

## Gate

`TestPhase11Async`: 30 fixtures green on Node 22, Deno 2, Bun 1.1, Chromium 130. Secondary gates: tsc strict zero diagnostics including `useUnknownInCatchVariables` (Mochi never lowers a `catch (e)` arm to non-`unknown` type); eslint `@typescript-eslint/no-misused-promises` and `await-thenable` enforced; `no-floating-promises: error` covers every `async` site.

## Goal-alignment audit

Phase 11 is the convergence point for three threads. (1) The async colour pass that was trivially "all Blue" in Phase 1-8 and got its first real activation in Phase 9 (agents) and Phase 10 (streams) now sees the full compiler surface: any function transitively calling an async function (agent intent, stream consumer, `fetch`, `sleep`, `llm.generate`) is coloured Red and emitted as `async`. (2) MochiResult replaces exception throwing for recoverable errors: Mochi functions that declare `throws` lower to `(...) => Promise<MochiResult<T, E>>` or `(...) => MochiResult<T, E>`. (3) AggregateError wires through from Phase 9's supervisor failure to the user's `await call(...)` site.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 11.0 | Async colour pass full activation; call-graph build; fixed-point colour propagation; `async function`/`function` choice at emit time | NOT STARTED | n/a |
| 11.1 | `MochiResult<T, E>` discriminated union + Ok/Err constructors emitted into `@mochi/runtime/result` | NOT STARTED | n/a |
| 11.2 | `fun parse() -> AST throws ParseError` to `function parse(): MochiResult<AST, ParseError>` (sync) or `Promise<MochiResult<AST, ParseError>>` (async) | NOT STARTED | n/a |
| 11.3 | `?` short-circuit (`let x = parse(s)?` to `case "err" return err; case "ok" let x = result.value`) lowered to early `return` from the calling MochiResult | NOT STARTED | n/a |
| 11.4 | `panic` to `throw new MochiPanic(msg)`; never caught at the Mochi layer; treated as a hard failure at the host runtime | NOT STARTED | n/a |
| 11.5 | `AggregateError` wiring: supervisor `MochiSupervisorFailure` surfaces as `MochiResult.Err(AggregateError)` to user code | NOT STARTED | n/a |

## Sub-phase 11.0, Async colour pass

### Decisions made (11.0)

**Pass location**: `transpiler3/typescript/colour/colour.go`, runs between aotir and lower (same slot as MEP-48's pass).

**Algorithm**:

1. Build call graph: nodes are functions; edges are calls.
2. Seed Red: any function containing `await`, `for await`, an agent `call`, a `fetch`, a `sleep`, or any access to an `AsyncIterable<T>`/`Promise<T>` value.
3. Fixed-point: a Blue function that calls a Red function becomes Red. Repeat until convergence.
4. Produce `ColourMap`.

**Emit choice**: Red functions emit as `async function f(...): Promise<R>` (module scope) or `async (...): Promise<R> => {...}` (inline). Blue functions stay sync.

**Top-level await**: if the entry-point `main` is Red, `src/index.ts` becomes `await main()`. Top-level await is ESM-only and supported on all four tier-1 runtimes.

**Forbidden mixings**: a Blue function may not call a Red function (would need to await). The colour pass is the enforcer; any such case is a transpiler bug.

## Sub-phase 11.1, MochiResult shape

### Decisions made (11.1)

**Type and constructors** (per MEP-52 §6):

```typescript
// @mochi/runtime/result
export type MochiResult<T, E> =
  | { readonly kind: "ok"; readonly value: T }
  | { readonly kind: "err"; readonly error: E };

export const Ok = <T>(value: T): MochiResult<T, never> => ({ kind: "ok", value });
export const Err = <E>(error: E): MochiResult<never, E> => ({ kind: "err", error });

export function isOk<T, E>(r: MochiResult<T, E>): r is { kind: "ok"; value: T } {
  return r.kind === "ok";
}
export function isErr<T, E>(r: MochiResult<T, E>): r is { kind: "err"; error: E } {
  return r.kind === "err";
}
```

**Variance**: `MochiResult<T, never>` produced by `Ok` is assignable to `MochiResult<T, AnyE>` thanks to the `never` bottom type; same for `MochiResult<never, E>` from `Err`. This is the canonical "Either" trick in TypeScript.

## Sub-phase 11.2, throws to MochiResult

### Decisions made (11.2)

**Mochi**: `fun parse(s: string) -> AST throws ParseError { ... }`

**TypeScript** (sync):

```typescript
import { MochiResult, Ok, Err } from "@mochi/runtime/result";
import { AST } from "./ast.ts";
import { ParseError } from "./parse_error.ts";

export function parse(s: string): MochiResult<AST, ParseError> {
  if (s === "") return Err(new ParseError("empty input"));
  // ...
  return Ok(ast);
}
```

**TypeScript** (async, when the function transitively awaits): wrap the return type in `Promise<...>`.

**Why MochiResult instead of `throw`**: thrown exceptions in TypeScript have type `unknown` (under `useUnknownInCatchVariables`). They are also implicit in the function signature, which loses information for the caller. MochiResult makes failure explicit at the type level, matches Rust's `Result` and Mochi's spec, and never crosses the FFI boundary by surprise.

**`throw` reserved for `panic`**: Mochi `panic msg` lowers to `throw new MochiPanic(msg)`. The runtime never catches `MochiPanic`; the host runtime terminates the process (or for browsers, surfaces it to `window.onerror`).

## Sub-phase 11.3, ? operator

### Decisions made (11.3)

**Mochi**: `let x = parse(s)?`

**TypeScript**:

```typescript
const __r = parse(s);
if (__r.kind === "err") return __r;
const x = __r.value;
```

(The early `return __r` returns the same `MochiResult.Err` shape to the caller, propagating the error type as long as the caller's `E` is compatible. The colour pass plus type checker enforce compatibility.)

**Inside `async`**: same shape, but `__r = await parse(s)` if `parse` is Red.

## Sub-phase 11.4, panic

### Decisions made (11.4)

**`MochiPanic`**:

```typescript
// @mochi/runtime/panic
export class MochiPanic extends Error {
  constructor(message: string) {
    super(message);
    this.name = "MochiPanic";
  }
}
```

**Mochi `panic "..."`**: `throw new MochiPanic("...")`.

**Never caught**: Mochi has no `try { ... } catch (panic) { ... }` form. The host runtime treats it as a hard failure: Node terminates with exit code 1, Deno same, Bun same. Browser surfaces to `window.onerror`. The emitter rejects any user code that tries to catch `MochiPanic`.

## Sub-phase 11.5, AggregateError wiring

### Decisions made (11.5)

The Phase 9 supervisor failure (`MochiSupervisorFailure extends AggregateError`) wraps in `MochiResult.Err`:

```typescript
const result: MochiResult<void, MochiSupervisorFailure> = await supervisor.run();
if (result.kind === "err") {
  for (const inner of result.error.errors) {
    console.error("child failure:", inner);
  }
}
```

The `AggregateError.errors` field gives the user direct access to inner failures. Standard JavaScript: every tier-1 runtime supports `instanceof AggregateError`.

## Files (planned)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/colour/colour.go` | Full async colour pass (already stubbed in Phase 9); now exercises every emit site |
| `transpiler3/typescript/colour/graph.go` | Call graph build from aotir |
| `transpiler3/typescript/colour/fixpoint.go` | Fixed-point iteration with seed set |
| `transpiler3/typescript/lower/result.go` | `throws` to MochiResult; `?` to early return |
| `transpiler3/typescript/lower/panic.go` | `panic` to `throw new MochiPanic` |
| `runtime3/typescript/src/result/index.ts` | `MochiResult<T, E>`, Ok, Err, isOk, isErr |
| `runtime3/typescript/src/panic/index.ts` | `MochiPanic` class |
| `transpiler3/typescript/build/phase11_test.go` | `TestPhase11Async` |
| `tests/transpiler3/typescript/fixtures/phase11-async/` | 30 fixtures |

## Test set

- `TestPhase11Async`, 30 fixtures four-runtime.
- `TestPhase11NoCatchPanic`, asserts emitted code never `catch`es `MochiPanic`.
- `TestPhase11AggregateError`, supervisor failure surfaces `AggregateError` to user code.
- `TestPhase11ColourSoundness`, hand-edited fixture that calls a Red function from Blue context fails at emit time with an explicit error.

## Deferred work

- `try/catch/finally` for FFI boundaries (where C code throws). Phase 12 (FFI) reintroduces the bounded form.
- `MochiResult.map`, `flatMap`, `andThen` combinators. v1.5; the `?` operator covers the v1 needs.
- Cancellable async sites (per-call `AbortSignal` plumbing). Phase 14 (fetch) lands the fetch-side; broader cancellation is v1.5.
