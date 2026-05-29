---
title: "Phase 9. Agents"
sidebar_position: 11
sidebar_label: "Phase 9. Agents"
description: "MEP-53 Phase 9, Mochi agents lowered to Rust structs plus impl blocks, no threads."
---

# Phase 9. Agents (Rust structs)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-53 §Phases](/docs/mep/mep-0053#phases) |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-29 07:35 (GMT+7) |
| Tracking issue | — (umbrella) |
| Tracking PR    | [#22499](https://github.com/mochilang/mochi/pull/22499) |
| Commit         | abf0aeeef0 |

## Gate

`TestPhase9Agents` walks `tests/transpiler3/rust/fixtures/phase09-agents/` (44 fixtures) and asserts byte-equal stdout. Coverage: agent declaration, agent spawn (`spawn AgentType()`), agent intent call, agent state mutation, multiple agents, agent with closures.

## Lowering decisions

```mochi
agent Counter {
    state count: int = 0
    on inc(by: int) { count = count + by }
    on get(): int { return count }
}
```

lowers to:

```rust
#[derive(Clone, Default, Debug)]
struct Counter {
    count: i64,
}

impl Counter {
    fn new() -> Self { Self::default() }
    fn inc(&mut self, by: i64) { self.count = self.count + by; }
    fn get(&self) -> i64 { self.count }
}
```

The key decision: agents are plain structs, not threads. `spawn AgentType()` lowers to `AgentType::new()` (immediate value construction); intent calls (`a.intent(arg)`) lower to direct method calls. There is no mailbox, no `send` / `receive` decoupling, no thread spawn. This matches MEP-53's single-thread runtime decision (§Abstract decision 2): users who need true concurrent agents can call into `std::thread` via FFI.

`&mut self` vs `&self` for intent methods is decided by the lower pass: if the intent body mutates any state field, `&mut self`; otherwise `&self`. The colour pass in phase 6 propagates the resulting borrow requirements through call sites.

Initial state values come from the `state field: T = expr` declarations and feed `Default::default()`. `expr` must be const-foldable; non-const initial values are rejected at lower time.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/rust/lower/agent.go` | Agent decl, spawn, intent lowering |
| `transpiler3/rust/build/phase09_test.go` | 44-fixture gate |
| `tests/transpiler3/rust/fixtures/phase09-agents/*.mochi` + `.out` | 44 fixtures |

## Test set

- `TestPhase9Agents/<fixture>` for each `.mochi` in the fixture directory (44 fixtures).

## Closeout notes

The 44-fixture set is the largest in phases 0-14 because agents touch every prior phase: scalar state, list state, sum-type state, closures-as-state. Each fixture exercises one composition. Two fixtures were dropped pre-merge: one needed `Rc<RefCell<Agent>>` to share an agent between two intent callers (not yet supported), one needed `Arc<Mutex<Agent>>` for cross-thread sharing (explicitly out of scope per the single-thread runtime decision).
