---
title: "06. Type-system lowering"
sidebar_position: 7
sidebar_label: "06. Type lowering"
description: "Mochi types onto int64 / float64 / string / []T / map[K]V / map[T]struct{} / struct / discriminated interface + variant structs / func(...)"
---

# 06. Type-system lowering

This note details how each Mochi type maps onto a Go type. The table is in [MEP-54 §3](/docs/mep/mep-0054#3-surface-syntax-lowering); this note explains the choices.

## Scalars

| Mochi | Go | Why |
|-------|----|----|
| `int` | `int64` | Pin to 64-bit so semantics are platform-independent. Bare `int` is 32-bit on 32-bit Go targets (e.g., GOARCH=386, GOARCH=arm) which would silently change overflow semantics. |
| `float` | `float64` | IEEE-754 binary64. Mochi's float pin is 64-bit; Go's `float64` matches. |
| `bool` | `bool` | Direct. |
| `string` | `string` | UTF-8 byte sequence. Mochi strings are byte-addressable; Go strings are too. Rune-aware operations (`Substring`, `RuneAt`) live in `mochiruntime/stringz`. |

The `int64` pin is load-bearing. We considered:

- **Bare `int`.** Rejected: 32-bit on 32-bit Go targets; overflow semantics change silently.
- **`int32`.** Rejected: Mochi's source-language int is 64-bit; lowering to int32 would lose range.
- **`*big.Int`.** Rejected: Mochi's int is fixed-width 64-bit, not arbitrary precision. Using big.Int would change semantics.

The `float64` pin similarly excludes `float32` and `*big.Float`.

## Strings

Go's `string` is a byte sequence with no encoding guarantee, but the Go standard library universally treats them as UTF-8. Mochi's source language treats strings as UTF-8 with rune-indexed addressing for `Substring`, `index`, `len` (rune count), `reverse`, etc.

The runtime helpers in `mochiruntime/stringz` use `utf8.RuneCountInString`, `utf8.DecodeRuneInString`, and the `for i, r := range s` rune-iteration pattern to convert between byte indices and rune indices. This means a 1000-char ASCII string is treated identically by Go and the runtime helper, but a string with multi-byte characters (CJK, emoji) goes through O(N) rune-counting per length / index operation. Phase 7.6 documents this; users for whom this is a hot path can opt into byte indexing via `byte_len(s)` / `byte_at(s, i)`.

## Lists, maps, sets

| Mochi | Go |
|-------|----|
| `list<T>` | `[]T` |
| `map<K, V>` | `map[K]V` |
| `set<T>` | `map[T]struct{}` (idiomatic Go set) |
| `omap<K, V>` | `mochiruntime.OMap[K, V]` (Go generic) |

`set<T>` as `map[T]struct{}` is the canonical Go pattern; `struct{}` takes zero bytes so the set holds only the key. Set operations:

- `add(s, x)` → `s[x] = struct{}{}`
- `has(s, x)` → `_, ok := s[x]; _ = ok` (or inline `_, ok := s[x]; if ok { ... }`)
- `remove(s, x)` → `delete(s, x)`
- iterate → `for x := range s { ... }`

`omap<K, V>` cannot be a plain `map[K]V` because Mochi's omap is insertion-ordered. The runtime helper `OMap[K, V]` is a small struct pairing the map with an insertion-order key slice. Phase 7.11 wires this.

## Records (anonymous and named)

Both `record User { id: int }` and anonymous `type Pair = { a: int, b: int }` lower to:

```go
type User struct {
    Id int64
}
```

Fields are exported (uppercase) so reflection-based helpers (`fmt.Sprintf("%+v", u)`, `encoding/json` if the user opts in) work without per-field tags. Equality:

- All comparable field types (no slices, no maps, no funcs) → use Go's built-in `==`.
- Mixed → emit a generated `Equal(other User) bool` method.

The lowerer decides at type-lower time which path to take per record type.

## Sum types

`type Shape = Circle(int) | Square(int) | Triangle(int, int, int)` lowers to a discriminated interface plus one final struct per variant:

```go
type Shape interface{ isShape() }

type Circle struct{ V0 int64 }
type Square struct{ V0 int64 }
type Triangle struct{ V0, V1, V2 int64 }

func (*Circle) isShape()   {}
func (*Square) isShape()   {}
func (*Triangle) isShape() {}
```

The marker method (`isShape()`) prevents external types from satisfying the interface — this gives a closed sum, matching Mochi's source-language semantics. Variants are passed by pointer (`*Circle`) so:

- The interface fits in two words regardless of variant size.
- Type assertions and type switches dispatch on the pointer type, which is a constant-time tag check.
- Zero allocation for the interface-wrap (the struct already lives on the heap from the constructor).

`match e { Circle(r) => ... }` lowers to:

```go
switch v := e.(type) {
case *Circle:
    r := v.V0
    // ...
}
```

Self-referential variants (`type Tree = Leaf | Node(Tree, Tree)`) use the interface for the recursive position; no `*Tree` box needed because the interface is already pointer-sized.

## Function types and closures

`fun(int, string): bool` lowers to `func(int64, string) bool`. Go function values are a (code, env) pair so closure capture is free.

The lowerer wraps closures that capture variables in a `ClosureEnvStmt` (see [[codegen-design]]) so by-value capture semantics are preserved. Closures that capture nothing skip the env-lift.

## Stream and channel types

`chan<T>` → `chan T`. `stream<T>` → a runtime struct (`mochiruntime.Stream[T]`) holding `[]chan T` subscriber slots. `subscribe(s)` returns a `chan T` typed as `<-chan T` (receive-only) to enforce the subscriber-cannot-emit invariant at the type level.

## Agent types

`agent A { ... }` lowers to two types: the agent struct (`AAgent`) and the message interface (`AMsg`). Each `on Foo` handler becomes a struct (`AFooMsg`) implementing the marker method. `spawn A()` calls `NewA()` which spawns the goroutine and returns `*AAgent`.

## Pointers and references

Mochi has no explicit pointer syntax. The lowerer chooses pointer-vs-value at type-lower time:

- Records lower to value-typed structs by default (`User`, not `*User`). The `Equal` method takes a value receiver.
- Sum-type variants lower to pointer-typed structs (`*Circle`, not `Circle`) for the interface-fit-in-two-words reason.
- Agents lower to pointer-typed structs (`*AAgent`) because the goroutine mutates the agent state and the constructor returns the pointer.

This is a lowering choice, not a source-language choice. From the Mochi source, `let u = User{id: 5}; u.id` reads identically regardless of whether `User` is a value or pointer type in Go.

## Generic helpers

The runtime helpers use Go generics (1.18+). `mochiruntime.Map[T, U]`, `Filter[T]`, `Reduce[T, A]`, `Sort[T cmp.Ordered]`, `MapKeys[K, V]`, `MapValues[K, V]`. Generic methods are not supported in Go 1.21 so all generic helpers are free functions.

## Type aliases

Mochi's `type Foo = Bar` lowers to Go's `type Foo = Bar` (type alias, not new type). This means `Foo` and `Bar` are interchangeable, matching Mochi's source-language semantics.

## Reflection escape hatch

The runtime exports `mochiruntime.AnyEqual(a, b any) bool` and `AnyHash(v any) uint64` for the rare cases where the lowerer needs heterogeneous comparison (e.g., `==` between two `any`-typed values). These use `reflect.DeepEqual` and a hash combining the type name with the field bytes. Phase 7.5 uses these for aggregations over union types.
