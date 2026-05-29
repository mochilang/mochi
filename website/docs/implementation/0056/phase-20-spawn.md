---
title: "Phase 20. Agent spawn (spawn AgentType())"
sidebar_position: 24
sidebar_label: "Phase 20. Agent spawn"
description: "MEP-56 Phase 20, spawn AgentType() lowered to AgentType.new with zero-valued fields."
---

# Phase 20. Agent spawn (spawn AgentType())

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | none |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | 2157007a50 |

## Gate

`TestPhase20AgentSpawn` in `transpiler3/ruby/build/phase20_test.go`: two subtests (`spawn_counter`, `spawn_string_state`). The first declares `agent Counter` with `var count: int = 0` plus `increment()` and `value()` intents, calls `spawn Counter()` three times to bump the counter, and asserts the final `value()` returns `3`. The second declares `agent Greeter { var prefix: string = "hi"; intent set_prefix; intent greet }`, spawns one, mutates state via the setter, and confirms the getter sees the new prefix. Both run under the resolved Ruby toolchain with `-I mochi-runtime/lib`. The two subtests together cover the int and string zero-value paths through `rubyZeroValue`.

## Lowering decisions

`spawn AgentType()` lowers to `AgentType.new(field1: zero, field2: zero, ...)` where each `zero` is computed from the agent's declared field type via `rubyZeroValue`. Routing intent calls (`c.increment()`, `g.greet("sam")`) reuses the same `AgentIntentCallExpr` lowering as the `AgentLit` form, so once `spawn` produces an instance the call path is unchanged (`transpiler3/ruby/lower/lower.go` lines 764 to 792):

- `aotir.AgentSpawnExpr` looks up `decl, ok := agentsByName[e.AgentName]` against the `agentsByName` cache built at the start of each `Lower()` pass (line 28). A miss returns `fmt.Errorf("ruby lower: spawn unknown agent %q", e.AgentName)` (lines 766 to 768) so a stale agent name fails the build instead of producing dangling Ruby.
- For each field in `decl.Fields` the lowerer renders `f.Name + ": " + rubyZeroValue(f.Type)` (lines 769 to 772) and joins them with `, ` to produce `Counter.new(count: 0)` or `Greeter.new(prefix: "")`. The `rubyZeroValue` helper (referenced from line 1402's `// the given Mochi scalar type` comment) maps `int -> 0`, `float -> 0.0`, `string -> ""`, `bool -> false`, so `spawn` always lands the agent in a clean zero state; user-supplied field defaults from the `agent { var x: T = expr }` syntax are read by the runtime via `attr_accessor` rather than baked into the constructor here.
- `aotir.AgentIntentCallExpr` lowers to `MethodCall{Receiver: recv, Method: e.IntentName, Args: ..., UseParens: true}` (lines 774 to 792), which renders as `c.increment()` or `g.greet("sam")`. This is shared with the `AgentLit` path, so `spawn`-built and literal-built agents behave identically post-construction.
- Phase 20 deliberately does not wrap the spawned instance in a `Thread.new`. The `Counter` and `Greeter` agents in the gate are passive (intent calls are synchronous method invocations on the instance); autonomous agents that need their own thread of control are covered by Phase 11 (`agent` declaration with a `run` intent) which uses `Thread.new` at agent-class scope. Keeping `spawn` itself thread-free lets a Mochi program spawn thousands of agents without paying for `pthread_create` per spawn.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | `AgentSpawnExpr` (lines 764 to 773) renders `AgentType.new(field: zero, ...)`; the `agentsByName` cache is populated at lines 28 and 31; intent dispatch shared via `AgentIntentCallExpr` at lines 774 to 792 |
| `transpiler3/ruby/build/phase20_test.go` | `TestPhase20AgentSpawn` with 2 subtests covering int and string zero-value initialisation |

## Test set

- `TestPhase20AgentSpawn/spawn_counter`, `spawn_string_state`.

## Closeout notes

Phase 20 landed on CRuby 4.0 (Homebrew). The key design pin is that `spawn` initialises each field to its type's Ruby zero (via `rubyZeroValue`) rather than re-evaluating the user-written field default expression. That keeps `spawn AgentType()` cheap (no thunk evaluation per call) and matches Mochi's documented `spawn` semantics, "construct in zero state, then run intents to drive state forward". Autonomous-thread agents stay a Phase 11 / Phase 12 concern; pushing `Thread.new` into `spawn` here would have made every short-lived agent allocate an OS thread.
