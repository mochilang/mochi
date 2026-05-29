---
title: "01. Language surface"
sidebar_position: 2
sidebar_label: "01. Language surface"
description: "Mochi features mapped onto Go 1.21+ lowering obligations for MEP-54."
---

# 01. Language surface

This note enumerates the Mochi surface forms the Go transpiler must accept and the Go shape each lowers to. The exhaustive table is in [MEP-54 §3](/docs/mep/mep-0054#3-surface-syntax-lowering); this note frames the categories and calls out the cases that drove specific Go-target decisions.

## Scope

Mochi's surface is roughly: scalars, control flow, records, sum types, closures, lists / maps / sets, query DSL, Datalog, agents, channels, streams, async, try / catch / panic, FFI, fetch, JSON, LLM generate. Each maps to a Go idiom, but several mappings have non-trivial trade-offs.

## Scalars and control flow

`int` is `int64` (never bare `int`). `float` is `float64`. `bool` is `bool`. `string` is Go's `string` (UTF-8 byte sequence). Arithmetic uses native ops; integer division and modulo route through `mochiruntime.DivI64` / `ModI64` so that vm3's "panic code 5 on zero divisor" semantics are preserved exactly (Go's built-in `/` and `%` already panic on zero for ints, but the panic message format differs from vm3's; routing through the runtime gives uniform panic codes).

`for i in lo..hi` lowers to a C-style `for i := lo; i < hi; i++` loop with `i` retyped to `int64` so the loop variable matches Mochi's int pin. `for x in xs` lowers to `for _, x := range xs`. The integer-pin to `int64` choice (rather than `int`) avoids platform-dependent overflow behaviour on 32-bit targets; see [[type-lowering]] for the reasoning.

## Records and sum types

`record` and anonymous `type X = { ... }` both lower to:

```go
type Foo struct {
    A int64
    B string
}
```

The fields are exported (uppercase) so reflection-based helpers (`fmt.Sprintf("%+v", foo)`, `json.Marshal`) work without per-field tags. Equality is field-by-field via Go's built-in `==` operator when all fields are comparable; for record types with slice or map fields the lowerer emits a generated `Equal(other Foo) bool` method.

`type T = A | B` lowers to a discriminated interface with one final struct per variant:

```go
type T interface{ isT() }
type A struct { X int64 }
func (*A) isT() {}
type B struct { Y string }
func (*B) isT() {}
```

The marker method (`isT()`) prevents external types from satisfying the interface. `match e { A(x) => arm }` lowers to a type switch: `switch v := e.(type) { case *A: x := v.X; arm; ... }`. The match-to-decision-tree pass (Maranget 2008) is reused from the C target via clower.

Self-referential variants (`type Tree = Leaf | Node(Tree, Tree)`) wrap the recursive position in an interface field, which gives free indirection without an explicit `Box<...>` (unlike Rust).

## Closures

Go closures are first-class: `func(x int64) int64 { return x + 1 }` works directly. Captures default to by-reference, which is wrong for Mochi's by-value capture semantics when the same closure runs in multiple goroutines reading the same captured variable. The lowerer emits an explicit `ClosureEnvStmt` that snapshots captured variables into a heap-allocated env struct at the point of closure creation; the closure body reads from `env.X` rather than directly from the outer `x`.

Function types lower to Go function types: `fun(int) int` becomes `func(int64) int64`. There is no `Box<dyn Fn>` overhead like Rust; Go function values are a (code pointer, env pointer) pair.

Recursive closures use the lifted-env pattern: the env struct holds a `func(int64) int64` field that is filled in after the env is allocated.

## Collections

`list<T>` is `[]T`. `map<K, V>` is `map[K]V`. `set<T>` is `map[T]struct{}` (idiomatic Go set). `omap<K, V>` is a small generic helper `mochiruntime.OMap[K, V]` that pairs a `map[K]V` with an insertion-order `[]K` for deterministic iteration.

List builtins (`append`, `len`, slice syntax `xs[lo:hi]`) lower to Go's native operators with no helper indirection. Higher-order helpers (`map`, `filter`, `reduce`) lower to runtime helpers that use Go 1.18+ generics: `func Map[T, U any](xs []T, f func(T) U) []U`.

## Channels and streams

`chan<T>` is Go's native `chan T`. `make_chan(N)` is `make(chan T, N)`. `send(ch, v)` is `ch <- v`. `recv(ch)` is `<-ch`. The runtime helper layer is thin to nonexistent here: Go's channel semantics are exactly Mochi's bounded blocking channel.

Streams (`make_stream`, `subscribe`, `emit`, `recv_sub`) lower to a runtime struct holding `[]chan T` subscriber slots, each with bounded capacity for backpressure. `subscribe_limit(s, N)` controls the per-subscriber capacity.

## Agents

`agent A { state x: int = 0; on tick { x = x + 1 } }` lowers to:

```go
type AAgent struct {
    in chan AMsg
    X  int64
}

func NewA() *AAgent {
    a := &AAgent{in: make(chan AMsg, 64)}
    go a.run()
    return a
}

func (a *AAgent) run() {
    for m := range a.in {
        switch m := m.(type) {
        case *ATickMsg:
            a.X = a.X + 1
            _ = m
        }
    }
}
```

`spawn AgentType()` calls `NewAgentType()`; the goroutine spawn happens inside the constructor. `a.intent(arg)` lowers to a send on `a.in` of the corresponding message struct.

## Try / catch / panic

`try { body } catch e { handler }` lowers to an IIFE with `defer` + `recover`:

```go
func() {
    defer func() {
        if r := recover(); r != nil {
            e := r.(int64)
            _ = e
            handler
        }
    }()
    body
}()
```

`panic(code)` lowers to `panic(int64(code))`. Go's recover is per-goroutine, which means an unrecovered panic in an agent goroutine terminates the program; Phase 10 documents this and provides an opt-in supervisor wrapper.

## Query DSL and Datalog

`from x in xs where p select e` desugars at clower time into `aotir.QueryExpr`; the Go lowerer emits a straight-line `for _, x := range xs { if p { result = append(result, e) } }` loop. Joins, group-by, and `order_by` use the patterns in [[dataset-pipeline]].

Datalog (`fact parent(...)`, `rule ancestor(...) := ...`, `query parent(_, Y)`) is evaluated at compile-time via semi-naive fixpoint in `transpiler3/go/lower/datalog.go` and emitted as a frozen `[]Tuple` literal.

## FFI

`import "C"` is Go's stable cgo mechanism. Mochi's `extern` declarations lower to a cgo block:

```go
/*
#include <stdio.h>
*/
import "C"

func printHello() {
    C.printf(C.CString("hello\n"))
}
```

cgo is unavailable on `GOOS=wasip1`; Phase 12 documents the skip.

## Builtins

`print` / `println` / `printf` lower to `fmt.Println` / `fmt.Printf`. File I/O wraps `os.ReadFile` and `os.WriteFile`. CSV uses `encoding/csv`. JSON uses `encoding/json`. HTTP uses `net/http`. The runtime module wraps these so that error semantics match vm3 (typically "return empty value on error" rather than "return error code").
