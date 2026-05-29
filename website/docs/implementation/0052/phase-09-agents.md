---
title: "Phase 9. Agents"
sidebar_position: 10
sidebar_label: "Phase 9. Agents"
description: "MEP-52 Phase 9, Mochi agents as synchronous TypeScript classes (mutable fields + private constructor + static of() + method-dispatch per intent). 44 fixtures green on Node 22, Deno 2, Bun 1.1; the runtime engine planned in the spec is deferred."
---

# Phase 9. Agents

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 9](/docs/mep/mep-0052#phase-plan) |
| Status         | LANDED (Node + Deno + Bun) |
| Started        | 2026-05-29 23:30 (GMT+7) |
| Landed         | 2026-05-29 23:55 (GMT+7) |
| Tracking issue | [#22852](https://github.com/mochilang/mochi/issues/22852) |
| Tracking PR    | [#22853](https://github.com/mochilang/mochi/pull/22853) |

## Gate

`TestPhase9AgentsNode`, `TestPhase9AgentsDeno`, `TestPhase9AgentsBun`: 44 fixtures green on each of Node 22, Deno 2, Bun 1.1; the recorded `.out` is byte-equal across runtimes. Secondary gates: `TestPhase9EmitShape` checks the emit declares a `class NAME` with mutable fields, reads/writes via `this.X`, and dispatches via `receiver.intent(args)`. `TestPhase9NoAsyncRuntime` checks that no async-runtime engine tokens (`AsyncIterableQueue`, `AbortController`, `MochiAgent`, `MochiSupervisor`, `@mochi/runtime/agent`, `AggregateError`, ` await `, `async `) leak into the source.

## Goal-alignment audit

Agents are Mochi's primary concurrency abstraction across every backend. The MEP-52 spec proposed an async runtime engine (`AsyncIterableQueue<Message>` + `AbortController` + `Promise.withResolvers()` + `MochiAgent` base class + `MochiSupervisor` + `AggregateError` for sibling failures) under `@mochi/runtime/agent`, with a ~8 KB gzipped budget per the runtime-portability rubric.

The audit pushed back on shipping that path on TS for Phase 9 for the same reasons Phase 8 deferred its runtime Datalog engine:

1. **Every fixture in the Phase 9 corpus is a synchronous state machine.** No fixture uses `spawn`, mailbox-style messaging, supervision, or abort. Every intent body completes in one synchronous step. No fixture's stdout depends on async ordering.

2. **The C and Rust transpilers ship the synchronous path.** The aotir IR comment at `transpiler3/c/aotir/program.go:1690` is explicit: *"Phase 9.3: agent (synchronous dispatch, struct + functions)"*. The Rust transpiler's `lowerAgentDecl` at `transpiler3/rust/lower/lower.go:246` emits one struct plus per-intent free functions named `mochi_agent_NAME__INTENT(__self: &mut TypeName, ...)`. There is no actor runtime on Rust either.

3. **The async runtime engine would force the Phase 11 async colour onto every intent.** Phase 11's colour pass treats every async-typed function as Red; if every intent dispatches through a promise-replying mailbox, every intent is async, every call site is `await`, and every fixture's main body needs to be re-coloured. The corpus has no behaviour that justifies that re-colouring.

4. **The spec's 8 KB budget would land on every TS package even when no fixture uses messaging or supervision.** Phase 15's tree-shaking can't drop the engine because every `agent` decl pulls it (per the spec).

Phase 9 therefore lands the synchronous-class path. The runtime cost is zero bytes. The lowering is one new `AgentClassDecl` tstree node plus 175 lines of Go in `transpiler3/typescript/lower/phase09.go`. If a future phase introduces `spawn`, mailbox messaging, or cross-agent supervision, the runtime engine can be added without disturbing the synchronous path on closed programs.

## Sub-phases (as shipped)

| #   | Scope                                                                                       | Status   | Commit |
|-----|---------------------------------------------------------------------------------------------|----------|--------|
| 9.0 | `AgentDecl` lowering: TS class with mutable fields, private constructor, static of(), per-intent method declarations | LANDED   | (this PR) |
| 9.1 | `AgentLit` lowering: `Counter { count: 0 }` to `Counter.of({ count: 0 })`                  | LANDED   | (this PR) |
| 9.2 | Synchronous intent dispatch via `receiver.intent(args)` method-call shape                   | LANDED   | (this PR) |
| 9.3 | `__self->X` rewrite to `this.X` on read and write sites inside intent bodies                | LANDED   | (this PR) |
| 9.4 | `AsyncIterableQueue<T>` runtime class with `push` + `pushAwait` + `close` + `fail`         | DEFERRED | n/a    |
| 9.5 | `MochiAgent<Msg>` base class with mailbox + abort signal + loop driver                      | DEFERRED | n/a    |
| 9.6 | `cast` and `call` dispatch (fire-and-forget + request-reply via `Promise.withResolvers`)    | DEFERRED | n/a    |
| 9.7 | `MochiSupervisor` with `one_for_all` and `one_for_one` strategies                           | DEFERRED | n/a    |
| 9.8 | Sibling failure aggregation via `AggregateError` (ES2021)                                   | DEFERRED | n/a    |

Sub-phases 9.4 through 9.8 are deferred per the goal-alignment audit. None of them is reachable from the Phase 9 fixture corpus (no `spawn`, no mailbox-style messaging, no supervision tree). The async runtime engine becomes necessary when a future phase introduces `spawn`, an async intent surface, or cross-agent supervision; until then the synchronous path is the correct shape.

## As-shipped lowering

The compile-time pipeline is:

```
Mochi source             aotir IR                     TS IR
─────────────────────    ─────────────────────────    ─────────────────────────
agent Counter {       ─► AgentDecl{                   AgentClassDecl{
  var count: int = 0       Name:    "Counter",          Name:    "Counter",
  intent inc() {           Fields:  [{count, int, ""}], Fields:  [{count, number}],
    count = count + 1      Intents: [                   Methods: [
  }                          {                            {
  intent value(): int {        Name:       "inc",           Name:       "inc",
    return count               ReturnType: TypeUnit,        ReturnType: "void",
  }                            Body: Block{                 Body: [
}                                AssignStmt{                  MemberAssignStmt{
                                   Name: "__self->count",       Receiver: this,
                                   Value: BinaryExpr{           Member:   "count",
                                     Left: VarRef{              Value: BinaryExpr{
                                       Name:"__self->count"       Left: MemberAccessExpr{
                                     },                                Receiver: this,
                                     Op: BinAddI64,                    Member:   "count"
                                     Right: IntLit{1}              },
                                   }                              Op: "+",
                                 }                                Right: IntLit{1}
                               }                               }
                             }                              }
                           }, ...]                        }, ...]
                          }                              }

let c = Counter{      ─► LetStmt{                      LetDecl{
  count: 0                Name:    "c",                  Name:    "c",
}                         VarType: TypeAgent,            Type:    "Counter",
                          AgentName: "Counter",          Init: RecordOfCallExpr{
                          Init: AgentLit{                  RecordName: "Counter",
                            AgentName: "Counter",          Fields: {count: 0}
                            Fields: {count: 0}           }
                          }                            }
                        }

c.inc()               ─► AgentIntentCallStmt{          ExprStmt{
                          AgentName:  "Counter",         Expr: MemberCallExpr{
                          IntentName: "inc",                   Receiver: c,
                          Receiver:   VarRef{"c"},             Method:   "inc",
                          Args:       []                       Args:     []
                        }                                    }
                                                         }
```

Three things route across this boundary:

1. The aotir lowerer (`transpiler3/c/lower/lower.go`) walks `agent NAME { ... }` source and emits one `AgentDecl` per agent type, with intent bodies stamped using `__self->FieldName` for every field read or write. This mirrors what the C lowerer needs for its struct-plus-functions emit.

2. The TS lowerer's new `agentDecls()` (`transpiler3/typescript/lower/phase09.go`) walks `prog.Agents` in source order and emits one `AgentClassDecl` per agent. Each intent body lowers through the regular `lowerBlock` path; the `__self->` prefix is rewritten to `this.X` MemberAccess on read sites (VarRef case in `lowerExpr`) and `this.X = ...` MemberAssign on write sites (AssignStmt case in `lowerAssignStmt`).

3. `AgentLit`, `AgentIntentCallExpr`, and `AgentIntentCallStmt` are wired into `lowerExpr` and `lowerStmt`. The literal reuses the existing `RecordOfCallExpr` shape (factory call via `Counter.of({...})`); the intent call uses the existing `MemberCallExpr` shape (`receiver.intent(args)`).

The lowering: 

- one tstree node added (`AgentClassDecl` with mutable fields, private constructor, static of(), per-intent method declarations)
- one tstree statement added (`MemberAssignStmt` for `receiver.member = value;`)
- one new lower file (`phase09.go`, 175 lines)
- two short edits in `lower.go` (VarRef + AssignStmt prefix detection, three switch cases for the new aotir nodes, one prelude wiring)
- one short edit in `lowerLetStmt` to map `VarType==TypeAgent` to `s.AgentName` as the TS type

### Why synchronous class vs async runtime engine

| Concern                            | Async runtime engine                             | Synchronous class (shipped)        |
|------------------------------------|--------------------------------------------------|------------------------------------|
| Runtime size                       | ~8 KB gzipped                                    | 0 bytes                            |
| Tree-shakeability                  | none (engine pulled by any `agent` decl)         | n/a                                |
| Phase 11 async colour              | every intent becomes Red                         | every intent stays Blue            |
| Phase 16 byte-equal repro          | engine version skew shifts emit                  | emit is a plain class declaration  |
| Future `spawn` (Phase 9.5+)        | already in shape                                 | engine added at that time          |
| Cross-backend goal                 | mismatch (C, Rust ship synchronous)              | matches C, Rust (same algorithm)   |

### Example 1, increment counter

`tests/transpiler3/typescript/fixtures/phase09-agents/agent_basic.mochi`:

```
agent Counter {
    var count: int = 0
    intent increment() { count = count + 1 }
    intent value(): int { return count }
}
let c = Counter { count: 0 }
c.increment()
c.increment()
c.increment()
print(c.value())
```

Emits:

```ts
class Counter {
  count: number;
  private constructor(opts: { count: number; }) {
    this.count = opts.count;
  }
  static of(opts: { count: number; }): Counter {
    return new Counter(opts);
  }
  increment(): void {
    this.count = (this.count + 1);
  }
  value(): number {
    return this.count;
  }
}

function mochi_main(): void {
  const c: Counter = Counter.of({ count: 0 });
  c.increment();
  c.increment();
  c.increment();
  mochi_print_i64(c.value());
}
```

The three `c.increment()` calls mutate the instance's `count` field; the binding `c` is `const` because the binding itself never rebinds (the agent state mutates through field writes, not via reassignment).

### Example 2, multi-param intent

`tests/transpiler3/typescript/fixtures/phase09-agents/agent_intent_two_params.mochi`:

```
agent Calc {
    var total: int = 0
    intent muladd(a: int, b: int) { total = total + (a * b) }
    intent get(): int { return total }
}
let c = Calc { total: 0 }
c.muladd(3, 4)
c.muladd(5, 6)
print(c.get())
```

Emits:

```ts
class Calc {
  total: number;
  ...
  muladd(a: number, b: number): void {
    this.total = (this.total + (a * b));
  }
  get(): number {
    return this.total;
  }
}
```

The intent parameters become method parameters with the same names and types; the body reads/writes `this.total` directly.

### Example 3, two agents in one program

`tests/transpiler3/typescript/fixtures/phase09-agents/agent_two_agents.mochi`:

```
agent A {
    var n: int = 0
    intent inc() { n = n + 1 }
    intent get(): int { return n }
}
agent B {
    var n: int = 0
    intent inc() { n = n + 10 }
    intent get(): int { return n }
}
let a = A { n: 0 }
let b = B { n: 0 }
a.inc()
b.inc()
a.inc()
print(a.get())
print(b.get())
```

Emits two separate class declarations and two const bindings. The bindings carry independent state because TS class instances do not share mutable fields between instances.

## Files

| File                                                                                       | Purpose |
|---------------------------------------------------------------------------------------------|---------|
| `transpiler3/typescript/tstree/phase09.go`                                                  | `AgentClassDecl`, `MethodDecl`, `MemberAssignStmt` nodes |
| `transpiler3/typescript/lower/phase09.go`                                                   | `agentDecls`, `lowerAgentLit`, `lowerAgentIntentCallExpr`, `lowerAgentIntentCallStmt`, `stripSelfPrefix` |
| `transpiler3/typescript/lower/lower.go`                                                     | Wires `AgentLit` and `AgentIntentCallExpr` into `lowerExpr`; wires `AgentIntentCallStmt` into `lowerStmt`; rewrites `__self->X` in `VarRef`/`AssignStmt`; maps `LetStmt{VarType==TypeAgent}` to `s.AgentName` for the TS type slot; calls `agentDecls()` in the prelude |
| `transpiler3/typescript/build/phase09_test.go`                                              | `TestPhase9AgentsNode/Deno/Bun` + emit-shape + no-async-runtime assertions |
| `tests/transpiler3/typescript/fixtures/phase09-agents/`                                     | 44 fixtures mirroring the Rust Phase 9 corpus |

## Test set

| Test                                                                          | Status |
|--------------------------------------------------------------------------------|--------|
| `TestPhase9AgentsNode`, 44 fixtures byte-equal on Node 22                      | GREEN  |
| `TestPhase9AgentsDeno`, 44 fixtures byte-equal on Deno 2                       | GREEN  |
| `TestPhase9AgentsBun`, 44 fixtures byte-equal on Bun 1.1                       | GREEN  |
| `TestPhase9EmitShape`, agent class declaration + this. access + method call    | GREEN  |
| `TestPhase9NoAsyncRuntime`, no async-runtime engine tokens leak into emit      | GREEN  |

Fixture corpus (44, full Rust Phase 9 corpus):

- Counter / bumper shapes: `agent_basic`, `agent_decrement`, `agent_intent_calls_intent`, `agent_step`, `agent_value_intent`, `agent_zero_intent`, `agent_no_field_assign`
- Multi-intent: `agent_multi_intent`, `agent_intent_only_read`, `agent_intent_uses_field`, `agent_set_via_intent`
- Multi-param intents: `agent_intent_two_params`, `agent_intent_three_params`, `agent_intent_param_int`, `agent_intent_neg_param`, `agent_string_param`
- Return shapes: `agent_intent_return_bool`, `agent_intent_return_string`
- Conditional bodies: `agent_intent_if`, `agent_field_in_cond`, `agent_max_field`, `agent_intent_modulo`
- Bool fields: `agent_bool`, `agent_bool_toggle`, `agent_compare`
- Float fields: `agent_float`, `agent_float_div`, `agent_neg_float`, `agent_pi`, `agent_mul_div`, `agent_complex_arithmetic`
- String fields: `agent_string`, `agent_string_concat`, `agent_string_reset`, `agent_initial_string`
- Initial-state variations: `agent_initial_state`, `agent_negative_init`
- Loop interactions: `agent_in_for_loop`, `agent_in_while_loop`
- Multi-field shapes: `agent_two_fields`, `agent_three_fields`, `agent_mixed_fields`
- Multi-agent programs: `agent_two_agents`
- Print inside intent: `agent_print_in_intent`

## Deferred work

- **AsyncIterableQueue runtime** (`@mochi/runtime/agent`). The original spec budget. Lands when Mochi grows a `spawn` statement, an async intent surface, or supervision wiring (none of which is in the Phase 9 fixture corpus).
- **MochiAgent base class + cast/call dispatch**. Same precondition as above.
- **MochiSupervisor + one_for_all / one_for_one strategies**. Phase 9.7 in the deferred plan; requires `spawn` to land first.
- **AggregateError sibling-failure aggregation**. Phase 9.8; requires supervision to land first.
- **Per-call timeouts**. v1.5.
- **Distributed agents** (remote mailbox via WebSocket). Out of scope.
- **Persistent agents** (durable mailbox via IndexedDB or SQLite). Out of scope.
- **Hot reload (`agent_replace_state`)**. MEP-46 territory; not in MEP-52 v1.

The deferred areas share a common precondition: the agent dispatch is no longer fully synchronous and statically resolved at compile time. While Mochi's agent surface stays synchronous (no `spawn`, no mailbox messaging, no supervision tree), the synchronous-class path is the right shape.
