---
title: "Phase 9. Agents (final class, synchronous)"
sidebar_position: 13
sidebar_label: "Phase 9. Agents"
description: "MEP-49 Phase 9 — Mochi agent to Swift public final class; fields as public var properties; intents as public func methods; __self-> refs rewritten for Swift."
---

# Phase 9. Agents (final class, synchronous)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 9](/docs/mep/mep-0049#phase-9-agents) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase9Agents`: 10 fixtures green on Swift 6.0+, macOS 15. Gate builds each fixture and compares stdout to `.expected`.

## Goal-alignment audit

Mochi agents encapsulate mutable state with a set of named operations (intents). For the v1 Swift backend the lowering target is `public final class`: each field becomes a `public var` property, each intent becomes a `public func` method. This is simpler than an `actor`-based mailbox pattern, avoids async colouring until Phase 11, and maps cleanly onto the synchronous fixture suite inherited from other backends. The `actor`+`AsyncStream` mailbox pattern is deferred to a future sub-MEP.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 9.0 | `AgentDecl` → `public final class`; fields → `public var`; intents → `public func` | LANDED | mep/0049-phase-09 |
| 9.1 | `AgentLit` → `ClassName(field1: val1, ...)` initialiser call | LANDED | mep/0049-phase-09 |
| 9.2 | `AgentIntentCallExpr` → `receiver.method(args...)` | LANDED | mep/0049-phase-09 |
| 9.3 | Rewrite `__self->fieldName` refs in intent bodies to bare `fieldName` | LANDED | mep/0049-phase-09 |
| 9.4 | `actor` + `AsyncStream<Message>` mailbox; OTP-style supervision | DEFERRED | — |
| 9.5 | `@ui` annotation → `@MainActor` isolation | DEFERRED | — |

## Sub-phase 9.0 -- Final class declaration

### Decisions made (9.0)

**`public final class`**: a Mochi `agent Counter { var n: int = 0; ... }` lowers to:

```swift
public final class Counter {
    public var n: Int64

    public init(n: Int64 = Int64(0)) {
        self.n = n
    }

    public func inc() {
        n = (n + Int64(1))
    }

    public func value() -> Int64 {
        return n
    }
}
```

**Why `final class` instead of `actor`**: the v1 gate fixtures are synchronous (no cross-agent calls, no concurrent access). Using `final class` avoids `async`/`await` requirements at every call site and keeps the generated code readable. The `actor`-based pattern is preserved as future work in 9.4.

**Field types**: each `aotir.AgentDecl.Field` is lowered to a Swift type via the same `lowerFieldTypeName` helper used for records.

**Init signature**: the generated `public init` takes one parameter per field. Default values are not yet emitted (fields with literal initialisers in the Mochi source do not yet propagate defaults to the Swift init).

## Sub-phase 9.1 -- Agent literal

### Decisions made (9.1)

**`AgentLit` → labeled call**: `Counter { n: 0 }` in Mochi → `Counter(n: Int64(0))` in Swift via `RawSwiftExpr` with labeled argument list.

## Sub-phase 9.2 -- Intent call

### Decisions made (9.2)

**`AgentIntentCallExpr`**: `counter.inc()` and `counter.value()` in Mochi lower to identical `receiver.method(args...)` calls in Swift. The lowerer emits a `MethodCallExpr` node.

**SpawnedRef**: the `SpawnedRef` flag (used for Erlang actor spawning in the BEAM backend) is not supported in the Swift backend and returns an error if encountered.

## Sub-phase 9.3 -- Self reference rewriting

### Decisions made (9.3)

**`__self->fieldName` pattern**: the aotir IR encodes field accesses inside intent bodies as `__self->fieldName` (C-style, matching the C backend convention). Swift class methods access `self.field` implicitly, so the lowerer runs `rewriteAgentSelfRefs` before lowering the intent body. This pass walks the aotir block and replaces every `VarExpr` whose name matches `__self->X` with just `X`.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/lower.go` | `lowerAgentDecl`, `lowerAgentLit`, `lowerAgentIntentCallExpr`, `rewriteAgentSelfRefs` |
| `transpiler3/swift/sxtree/nodes.go` | `ClassDecl`, `ClassProp` node types |
| `transpiler3/swift/build/phase09_test.go` | `TestPhase9Agents`: 10 fixtures |
| `tests/transpiler3/swift/fixtures/phase09-agents/` | 10 fixture directories |

## Test set

- `TestPhase9Agents` -- 10 fixtures: `agent_bool`, `agent_chain`, `agent_cond`, `agent_counter`, `agent_float`, `agent_multi`, `agent_nested_call`, `agent_params`, `agent_string`, `agent_two`.

## Deferred work

- `actor` + `AsyncStream<Message>` mailbox pattern (request-reply via `withCheckedContinuation`, fire-and-forget via `nonisolated` continuation). Deferred to Phase 9.4.
- OTP-style supervision (`MochiRuntime.Supervisor` actor, restart strategies). Deferred to Phase 9.4.
- `@ui` annotation → `@MainActor`-isolated class conforming to `ObservableObject`. Deferred to Phase 9.5.
- Distributed actors (Swift Distributed Actors framework). Deferred.
