---
title: "Phase 11. Agents (Ruby classes)"
sidebar_position: 15
sidebar_label: "Phase 11. Agents"
description: "MEP-56 Phase 11, agent declarations lowered to Ruby classes with @ivar state and instance-method intents."
---

# Phase 11. Agents (Ruby classes)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | none |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | 9494a06464 |

## Gate

`TestPhase11Agents` in `transpiler3/ruby/build/phase11_test.go`: two inline subtests, `agent_basic` and `agent_multi_intent`. Each subtest compiles a Mochi source that declares an agent with mutable state and value-returning intents, instantiates it via the record-literal form (`Counter { count: 0 }`), drives state through a sequence of intent calls, and prints the final result. The `.rb` runs under the resolved Ruby toolchain with `-I mochi-runtime/lib`, and stdout is diffed against the recorded expectation. `agent_basic` additionally locks five render-shape substrings: `class Counter`, `def initialize(count:)`, `@count = count`, `def increment`, and `def value`. Those substrings pin the class wrapper, the keyword-arg constructor, the ivar assignment in `initialize`, and the instance-method form for both unit-returning (`increment`) and value-returning (`value`) intents. `agent_multi_intent` exercises three intents (`add`, `sub`, `get`) on a single agent to confirm the lowerer emits one method per intent and that state survives across calls (`Adder.add(10) + add(5) - sub(3)` ends at `12`).

## Lowering decisions

Each `aotir.AgentDecl` lowers via `lowerAgent` (lower.go lines 1244 to 1279) to an `rtree.ClassDecl` containing one `initialize` method plus one instance method per intent. The class declarations are appended to the outer wrapper module's `progDecls` between records and unions but before the `Main` module (lower.go lines 71 to 77), keeping them namespaced under the source-file module so two Mochi files can declare same-named agents without collision.

`initialize` is generated as a keyword-args method: each `aotir.AgentField` becomes a `rtree.MethodParam{Name: f.Name + ":"}` and a body statement `rtree.Assign{LHS: "@" + f.Name, RHS: Ident{Name: f.Name}}` (lines 1252 to 1262). The rendered Ruby is `def initialize(count:); @count = count; end`. Keyword-arg construction was chosen because Mochi's agent-init form is `Counter { count: 0 }`, which already commits to named fields, and keyword args make the rendered Ruby self-documenting and order-independent.

Each `aotir.AgentIntent` lowers to a `rtree.MethodDecl{Name: intent.Name, Params: params, Body: body}` (lines 1263 to 1277). The intent body is lowered by `lowerBlock`, the same path used for free-standing user functions. Crucially, the agent intent body reads and writes the agent's fields via plain `VarRef`/`AssignStmt` against the field names (no `__self` prefix on the IR side); the C front-end has already rewritten field references to the field name. The Ruby lowerer relies on a separate VarRef/AssignStmt rewrite (called out in the comment at lower.go lines 1247 to 1248) to map `count` reads to `@count` reads and `count = expr` writes to `@count = expr` when the surrounding scope is an agent intent. That rewrite produces the `count = count + 1` body as the rendered `@count = @count + 1`, but only at the IR-to-IR rewrite step that runs before `lowerBlock`.

`aotir.AgentLit` lowers in `lowerExpr` (lower.go lines 754 to 763) to a `RawExpr` of the form `AgentName.new(field: value, field: value)`, dispatching to the keyword-args initialize defined above. `aotir.AgentSpawnExpr` (lines 764 to 773) covers the call-site form `spawn AgentName` (no init args); it looks up the agent decl in the `agentsByName` map (populated at the start of every `Lower()` pass, lines 28 to 31) and synthesises zero-value field arguments via `rubyZeroValue` (lines 1404 to 1416), so `spawn Counter` renders as `Counter.new(count: 0)`. `aotir.AgentIntentCallStmt` and `aotir.AgentIntentCallExpr` both lower to a `MethodCall{Receiver: recv, Method: e.IntentName, Args: args, UseParens: true}` (lines 217 to 235 for the stmt form, 774 to 792 for the expr form), so `a.add(10)` renders as `a.add(10)` with no transformation.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | `lowerAgent` producing `class A; def initialize(field:); @field = field; end; def intent; ...; end; end`; agent decls appended into `progDecls` between records and unions; `AgentLit` → `A.new(field: v)`; `AgentSpawnExpr` synthesises zero-value init args via `rubyZeroValue`; `AgentIntentCallStmt`/`Expr` → `recv.method(args)` |
| `transpiler3/ruby/rtree/` | `ClassDecl`, `MethodDecl`, `MethodParam`, `Assign` nodes rendering the Ruby class shape |
| `transpiler3/ruby/build/phase11_test.go` | `TestPhase11Agents` with 2 subtests and 5 render-shape assertions |

## Test set

- `TestPhase11Agents/agent_basic`, `agent_multi_intent`.

## Closeout notes

Phase 11 landed on CRuby 3.4 with both subtests green. The synchronous-dispatch decision (intents are plain instance methods, not message sends through a queue) was made because Phase 11 covers single-threaded agent use; the actor-style asynchronous variant lands in a later phase by re-wrapping the same class behind a `Thread::SizedQueue` mailbox. Locking the `def initialize(count:)` keyword form in `wantInRb` rather than the positional form was a deliberate guard: a future refactor that switched to positional construction would still satisfy the runtime assertion (Ruby accepts both forms on `.new`) but would silently break the `AgentLit` lowering at the call-site, since `AgentLit` emits keyword args. Key implementation insight: the `agentsByName` map at lower.go line 16 is populated once per `Lower()` pass and only used by `AgentSpawnExpr`, but it has to live at file scope (not stack-local) because `lowerExpr` is dispatched through a long switch chain and a parameter would have to thread through every arm.
