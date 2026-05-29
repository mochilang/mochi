---
title: "Agents and streams: Phase 9 agents, Phase 10 streams, Phase 11 async coloring"
description: "Phase 9 agents (mutable PHP class, FIFO channel), Phase 10 streams (MochiStream/MochiSub fan-out, backpressure), Phase 11 async coloring (all-Blue, sync wrappers, no Amp/Revolt)."
sidebar_position: 9
---

# Agents and streams: Phase 9 agents, Phase 10 streams, Phase 11 async coloring

Author: research pass for MEP-55 (Mochi-to-PHP 8.4 transpiler).
Date: 2026-05-29 15:00 (GMT+7).
Sources: `transpiler3/php/lower/lower.go` (lines 83-95 agent loop,
256-377 stream/async runtimeDecls, 500-517 AgentIntentCallStmt,
1151-1198 lowerAgent, 1200-1221 VarRef __self->),
`transpiler3/php/colour/colour.go`,
`transpiler3/php/build/phase09_test.go`,
`transpiler3/php/build/phase10_test.go`,
`tests/transpiler3/php/fixtures/phase09-agents/`,
`tests/transpiler3/php/fixtures/phase10-streams/`,
`tests/transpiler3/php/fixtures/phase11-async/`.

## 1. Phase 9: Agents

### 1.1 Design motivation

Mochi agents are stateful concurrent entities that communicate via
message passing. In targets with real concurrency (BEAM MEP-46, Loom
MEP-47) agents map to lightweight processes or virtual threads. PHP has
no preemptive scheduler in the standard library. The PHP target lowers
agents to a simpler model: a mutable PHP class with intent methods.

The choice sacrifices true concurrency but gains full compatibility with
every PHP deployment environment (shared hosting, PHP-FPM, CLI), zero
runtime dependencies, and simple reasoning: the program is sequential
and deterministic.

### 1.2 Agent class shape

Each `agent` declaration lowers to a `final class` (no `readonly`, since
intent bodies mutate fields). The `lowerAgent` function (lower.go lines
1156-1198) produces a `ClassDecl` with `Mutable: true`.

Fields become promoted public constructor parameters without the
`readonly` modifier. This is the key difference from record classes
(which use `final readonly class`): agent fields must be mutable.

From `TestPhase9EmitFragments` `agent_counter.mochi`:
```php
/**
 * Mochi agent `Counter`. Generated; do not edit by hand.
 */
final class Counter
{
    public function __construct(
        public int $count,
    ) {}

    public function increment(): void
    {
        $this->count = ($this->count + 1);
    }

    public function value(): int
    {
        return $this->count;
    }
}
```

### 1.3 The `__self->FIELD` sentinel rewrite

The aotir lowerer (MEP-45) encodes intent-body field access using the
sentinel prefix `__self->FIELD`, which maps to the C backend's
`self->field` pointer dereference. The PHP lowerer rewrites this in two
places:

**Reads** (`lowerExpr`, lower.go lines 1214-1221):
```go
if field, ok := strings.CutPrefix(v.Name, "__self->"); ok {
    return &ptree.PropAccessExpr{
        Receiver: &ptree.VarExpr{Name: "this"},
        Field:    field,
    }, nil
}
```
`__self->count` becomes `$this->count`.

**Writes** (`lowerAssignStmt`, lower.go lines 630-637):
```go
if field, ok := strings.CutPrefix(s.Name, "__self->"); ok {
    return []ptree.Stmt{&ptree.PropAssignStmt{
        Receiver: &ptree.VarExpr{Name: "this"},
        Field:    field,
        Value:    v,
    }}, nil
}
```
`__self->count = ...` becomes `$this->count = ...`.

### 1.4 PHP reserved name collision

`phpClassName(name)` (lower.go lines 965-972) suffixes `_` when the
agent name is a PHP reserved word. The canonical example is
`agent Switch { ... }` which emits `final class Switch_` because `switch`
is a reserved keyword in PHP. The fragment test `agent_bool.mochi`
pins this: it asserts `final class Switch_` appears in the output and
`$s = new Switch_(active: false);`.

### 1.5 `spawn AgentType()` lowering

`spawn AgentType()` constructs a new agent with zero-value fields. The
PHP lowerer calls `lookupAgentDecl` (lower.go lines 978-985) to find
the agent declaration, then calls `phpZeroLit` (lines 992-1004) for
each field to synthesise the default value:
- `TypeInt` → `IntLit{0}`
- `TypeFloat` → `FloatLit{0}`
- `TypeBool` → `BoolLit{false}`
- `TypeString` → `StringLit{""}`

The result is a `NewExpr` with named args: `new Counter(count: 0)`.
This is shape-equal to the `AgentLit` form (`Counter { count: 0 }`).

From `agent_spawn.mochi` fragment test:
```php
$c = new Counter(count: 0);
$c->increment();
$v = $c->value();
```

### 1.6 Intent call dispatch

`AgentIntentCallStmt` (lower.go lines 500-517) lowers to a
`MethodCallExpr`:
- `c.increment()` → `$c->increment();`
- `v = c.value()` → `$v = $c->value();`

The receiver is lowered via `lowerExpr`; arguments are lowered via the
normal expression path.

## 2. Phase 10: Streams

### 2.1 Design

Mochi streams are broadcast pub/sub channels. Multiple subscribers can
attach; each emit fans out to all attached subscribers. On PHP, there is
no native channel or event loop. The PHP target uses a synchronous
array-backed model: `MochiStream` holds a per-subscriber message queue
(array of arrays), and `MochiSub` holds a reference back to the stream
plus a subscriber index.

All Phase 10 fixtures use emit-before-recv patterns (emit everything
first, then receive everything). This is a deliberate constraint that
fits the synchronous model: there is no way for a subscriber to block
and wait for the next emit in a sequential PHP execution.

### 2.2 Inline runtime classes

`runtimeDecls` (lower.go lines 256-336) emits the stream classes when
`l.runtime.streams == true`:

```php
final class MochiStream
{
    /** @var array<int, array<int, mixed>> Per-subscriber message queues. */
    public array $subs = [];

    /** @var array<int, int> Per-subscriber drop threshold; 0 = unlimited. */
    public array $limits = [];

    public function __construct(public int $cap) {}
}

final class MochiSub
{
    public function __construct(
        public MochiStream $stream,
        public int $idx,
    ) {}
}
```

Note that `MochiStream` is `final class` (not readonly), because
`$subs` and `$limits` are mutable.

### 2.3 Stream helpers

Five inline helper functions are emitted:

- `mochi_stream_make(int $cap): MochiStream` — constructs a new stream
  with the given capacity.
- `mochi_sub_make(MochiStream $s): MochiSub` — creates a subscriber,
  initialising `$s->subs[$idx] = []` and `$s->limits[$idx] = 0`.
- `mochi_stream_emit(MochiStream $s, $v): void` — fans out `$v` to all
  subscriber queues, respecting per-subscriber drop limits (lower.go
  lines 316-327).
- `mochi_sub_recv(MochiSub $sub): mixed` — shifts the head of the
  subscriber's queue: `array_shift($sub->stream->subs[$sub->idx])`.
- `mochi_sub_make_limit(MochiStream $s, int $limit): MochiSub` —
  creates a subscriber with a drop threshold (Phase 10.2 backpressure).

The fan-out loop in `mochi_stream_emit`:
```php
foreach (array_keys($s->subs) as $k) {
    if ($s->limits[$k] > 0 && count($s->subs[$k]) >= $s->limits[$k]) { continue; }
    $s->subs[$k][] = $v;
}
```
The `continue` drops the message silently when the subscriber's queue is
full (limit > 0 and queue length >= limit).

### 2.4 Backpressure (Phase 10.2)

`mochi_sub_make_limit($s, 2)` creates a subscriber that drops messages
when its queue already holds 2 items. The `stream_backpressure` fixture
pins both the `subscribe_limit` path and the drop branch. From
`TestPhase10EmitFragments`:
```
function mochi_sub_make_limit(MochiStream $s, int $limit): MochiSub
$s->limits[$idx] = $limit;
if ($s->limits[$k] > 0 && count($s->subs[$k]) >= $s->limits[$k]) { continue; }
$sub = mochi_sub_make_limit($s, 2);
```

### 2.5 `StreamEmitStmt` lowering

`StreamEmitStmt` (lower.go lines 483-499) lowers `emit(s, v)`:
```go
l.runtime.streams = true
// ...
return []ptree.Stmt{&ptree.ExprStmt{
    Expr: &ptree.CallExpr{
        Callee: &ptree.IdentExpr{Name: "mochi_stream_emit"},
        Args:   []ptree.Expr{s, val},
    },
}}, nil
```
Setting `l.runtime.streams = true` triggers the class and helper
injection in `runtimeDecls`.

## 3. Phase 11: Async coloring

### 3.1 The colour pass

`colour.Compute(prog)` (colour/colour.go lines 39-45) returns a
`ColourMap` where every function is assigned `Blue`:
```go
func Compute(prog *aotir.Program) ColourMap {
    m := make(ColourMap, len(prog.Functions))
    for _, fn := range prog.Functions {
        m[fn.Name] = Blue
    }
    return m
}
```

The `Red` colour constant is defined but never produced. The package
comment explains: "Phase 11 shipped async/await as a synchronous value
wrapper rather than the originally-planned Amphp/Fiber dispatch, so no
PHP function ever needs an `Amp\Future<T>` return type."

The `ColourMap` is passed to `lower.Lower` but the parameter is named
`_` (lower.go line 52): the lowerer ignores it entirely. The pass exists
for API symmetry with the other transpiler3 targets.

### 3.2 `MochiFuture` sync wrappers

When `l.runtime.async == true`, `runtimeDecls` emits (lower.go lines
338-377):

```php
final class MochiFuture
{
    public function __construct(public mixed $value) {}
}

function mochi_future_make($v): MochiFuture
{
    return new MochiFuture(value: $v);
}

function mochi_future_await(MochiFuture $f): mixed
{
    return $f->value;
}

function mochi_future_await_all(array $fs): array
{
    return array_map(fn(MochiFuture $f) => $f->value, $fs);
}
```

`mochi_future_make` wraps an already-computed value. `mochi_future_await`
unwraps it immediately. There is no deferred execution, no event loop,
no suspension point. Phase 11 fixtures all produce results that are
available synchronously.

### 3.3 Why not Amp or Revolt

Two PHP async libraries were considered:
- `amphp/amp` v3: green-thread-style coroutines with an event loop.
- `revolt/event-loop`: Amp's event-loop extracted as a standalone.

Both require adding Composer runtime dependencies. Amp changes function
return types (`Future<T>` instead of `T`) and requires `\Amp\async()`
wrappers at every call site. Revolt requires an event loop to be
installed and started.

The MEP-55 audit round 1 removed `amphp/revolt` from `require-dev`
(it was briefly listed) after Phase 11 confirmed that all Phase 11
fixtures work correctly with sync wrappers. No fixture required true
concurrent execution.

The sync wrapper approach produces zero-dependency code that runs under
CLI, PHP-FPM, FrankenPHP worker mode, and RoadRunner without any event
loop configuration.

See [[02-design-philosophy]] for the full rationale and
[[12-risks-and-alternatives]] for the scheduling risk discussion.
