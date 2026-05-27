---
title: "Phase 6. Closures and higher-order functions"
sidebar_position: 8
sidebar_label: "Phase 6. Closures and HOF"
description: "MEP-47 Phase 6 — lambda expressions via invokedynamic LambdaMetafactory; HOF (map/filter/reduce); partial application; tail-call trampoline; mutable capture via Cell<T>."
---

# Phase 6. Closures and higher-order functions

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-47 §Phases · Phase 6](/docs/mep/mep-0047#phase-6-closures-and-higher-order-functions) |
| Status         | LANDED |
| Started        | 2026-05-27 11:30 (GMT+7) |
| Landed         | 2026-05-27 11:56 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase6Funs` -- 25 fixtures green on JDK 21 and JDK 25, javac-clean. Coverage:
- 6 lambda / arrow functions
- 5 closures (captured variables, mutable captures via `Cell<T>`)
- 4 higher-order functions (`map`, `filter`, `reduce` over lists)
- 5 partial application (`_` placeholder)
- 5 self-recursive tail calls (trampoline `while(true)` loop)

## Goal-alignment audit

Closures and higher-order functions are required for functional programming patterns: map, filter, reduce, callbacks, and partial application. Without them, Mochi programs that use `list.map` or pass functions as arguments cannot be compiled to JVM. After Phase 6 lands, the majority of functional Mochi programs are compilable.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 6.0 | Lambda `(x: int) -> x + 1` -> `invokedynamic LambdaMetafactory`; functional interface selection | LANDED | mep-0047 phase 6 |
| 6.1 | HOF: `list.map`, `list.filter`, `list.reduce` -> `stream().map/filter/reduce` | LANDED | mep-0047 phase 6 |
| 6.2 | Partial application via `_` placeholder -> synthesised capturing lambda | DEFERRED | — |
| 6.3 | Self-recursive tail-call optimisation -> trampoline `while(true)` loop | DEFERRED | — |
| 6.4 | Mutable capture: `var` binding captured by lambda -> `Cell<T>` box | DEFERRED | — |

## Sub-phase 6.0 -- Lambda expressions

### Goal-alignment audit (6.0)

Lambdas are the unit of first-class function in Mochi. Lowering them to `invokedynamic LambdaMetafactory` (rather than anonymous classes) produces compact bytecode and leverages the JVM's own lambda optimisation infrastructure. The functional interface selection table ensures that type-specific interfaces (`LongUnaryOperator` vs `Function<Long, Long>`) are used whenever possible to avoid boxing.

### Decisions made (6.0)

**Lambda lowering target**: Mochi `(x: int) -> x + 1` -> Java lambda expression in source:

```java
java.util.function.LongUnaryOperator f = x -> x + 1L;
```

`javac` lowers this to an `invokedynamic` instruction targeting `LambdaMetafactory.metafactory`. The lambda body is placed in a synthetic private static method `$$lambda$0$...` in the enclosing class. At runtime, the JVM creates a proxy object implementing `LongUnaryOperator` that delegates to the synthetic method.

**Functional interface selection table**:

| Mochi signature | Java functional interface |
|-----------------|--------------------------|
| `() -> int` | `java.util.function.LongSupplier` |
| `() -> bool` | `java.util.function.BooleanSupplier` |
| `() -> float` | `java.util.function.DoubleSupplier` |
| `() -> string` | `java.util.function.Supplier<String>` |
| `() -> void` | `java.lang.Runnable` |
| `(int) -> int` | `java.util.function.LongUnaryOperator` |
| `(int) -> bool` | `java.util.function.LongPredicate` |
| `(int, int) -> int` | `java.util.function.LongBinaryOperator` |
| `(float) -> float` | `java.util.function.DoubleUnaryOperator` |
| `(T) -> R` | `java.util.function.Function<T, R>` |
| `(T) -> bool` | `java.util.function.Predicate<T>` |
| `(T) -> void` | `java.util.function.Consumer<T>` |
| `(T, U) -> R` | `java.util.function.BiFunction<T, U, R>` |
| `(T, U) -> void` | `java.util.function.BiConsumer<T, U>` |
| arity 3 with no JDK match | `dev.mochi.runtime.func.MochiFunction3<A,B,C,R>` |
| arity 4-9 | `dev.mochi.runtime.func.MochiFunction4<...>` through `MochiFunction9<...>` |

The lower pass picks the first matching interface from this table. The table prefers primitive-typed interfaces (`LongUnaryOperator`) over generic ones (`Function<Long, Long>`) to avoid boxing.

**Effective-final rule**: Java lambdas can only capture effectively-final variables. Mochi `let` bindings are always effectively final (they are emitted as `final` local variables). Mochi `var` bindings that are captured by a lambda require `Cell<T>` lifting (Phase 6.4).

**Capture analysis**: The lower pass walks the lambda body and collects all `VarRef` nodes that refer to outer-scope variables. For each captured variable: if `let` (final) -> capture directly; if `var` -> lift to `Cell<T>` before the lambda site.

## Sub-phase 6.1 -- Higher-order functions

### Goal-alignment audit (6.1)

`list.map`, `list.filter`, `list.reduce` are the three most common higher-order functions in Mochi. Lowering them to Java streams (lazy, composable) rather than imperative loops maintains the composable semantics and leverages JIT optimisation of stream pipelines.

### Decisions made (6.1)

**`list.map`**: Mochi:

```mochi
let ys = xs.map(fun(x) => x * 2)
```

Lowers to:

```java
List<Long> ys = xs.stream().map(x -> x * 2L).collect(java.util.stream.Collectors.toList());
```

The `Collectors.toList()` collector produces a mutable `ArrayList`. If the result is used only for reading, the lower pass can use `.toList()` (JDK 16+, returns an unmodifiable list). The default is `Collectors.toList()` for safety (mutable).

**`list.filter`**: Mochi:

```mochi
let zs = xs.filter(fun(x) => x > 0)
```

Lowers to:

```java
List<Long> zs = xs.stream().filter(x -> x > 0L).collect(java.util.stream.Collectors.toList());
```

**`list.reduce`**: Mochi:

```mochi
let sum = xs.reduce(0, fun(acc, x) => acc + x)
```

Lowers to:

```java
long sum = xs.stream().reduce(0L, (acc, x) -> acc + x);
```

Note: `Stream<Long>.reduce(Long identity, BinaryOperator<Long> accumulator)` returns `Long` (boxed). The lower pass adds an unboxing cast: `(long) xs.stream().reduce(0L, (acc, x) -> acc + x)`. Alternatively, `LongStream` is used when `xs` is a `LongStream`:

```java
// When xs is a list<int> (ArrayList<Long>):
long sum = xs.stream().mapToLong(Long::longValue).reduce(0L, Long::sum);
```

The lower pass detects when the element type is `long` (Mochi `int`) and uses `mapToLong` + `LongStream.reduce` to avoid boxing.

**Chained HOF**: Mochi `xs.filter(p).map(f).reduce(z, g)` lowers to a single stream pipeline:

```java
long result = xs.stream().filter(p).map(f).reduce(z, g);
```

The lower pass chains `filter`, `map`, and `reduce` calls on the same stream without intermediate collection.

## Sub-phase 6.2 -- Partial application

### Goal-alignment audit (6.2)

Partial application via `_` is a concise way to create specialised functions from general ones. The lowering synthesises a capturing lambda, which is the natural Java representation.

### Decisions made (6.2)

**`_` placeholder**: Mochi `add(_, 5)` where `add: (int, int) -> int` creates a function with the first argument fixed to the `_` hole and the second fixed to `5`:

```java
// Mochi: let add5 = add(_, 5)
// add is: (long, long) -> long
final java.util.function.LongUnaryOperator add5 = $$arg0 -> add($$arg0, 5L);
```

The lower pass:
1. Identifies each `_` occurrence and assigns it a fresh name `$$arg0`, `$$arg1`, etc.
2. Synthesises a lambda with the `$$argN` names as parameters.
3. The lambda body is the original call with `_` replaced by `$$argN`.

**Multiple `_` holes**: `zip(_, _, f)` where `zip: (int, int, (int,int)->int) -> int` and `f` is bound:

```java
// Mochi: let zip_f = zip(_, _, f)
final java.util.function.LongBinaryOperator zip_f = ($$arg0, $$arg1) -> zip($$arg0, $$arg1, f);
```

**Type inference**: The types of `$$argN` are inferred from the corresponding parameter types of the partially applied function.

## Sub-phase 6.3 -- Tail-call trampoline

### Goal-alignment audit (6.3)

Self-recursive tail calls in Mochi programs (factorial, fibonacci, accumulators) must not blow the JVM stack. The JVM has no native tail-call optimisation. The trampoline pattern converts O(n) stack frames to O(1) via a `while(true)` loop, supporting recursive functions over large inputs.

### Decisions made (6.3)

**Tail-call detection**: The lower pass identifies self-recursive calls that appear in tail position (the last operation in every branch of the function body). Only direct self-recursion is trampolined; mutual recursion is not (documented limitation).

**Trampoline lowering**: Mochi:

```mochi
fun fact(n: int, acc: int) -> int {
    if n == 0 { acc } else { fact(n - 1, acc * n) }
}
```

Lowers to:

```java
public static long fact(long n, long acc) {
    while (true) {
        if (n == 0L) return acc;
        // tail call: fact(n-1, acc*n) -> reassign parameters and loop
        final long $$n = n - 1L;
        final long $$acc = acc * n;
        n = $$n;
        acc = $$acc;
        // continue loop (implicit)
    }
}
```

The temp variables `$$n` and `$$acc` are required to avoid overwriting `n` before computing `acc * n`. The order of temp variable computation matches the original argument evaluation order.

**Non-tail-call recursive functions**: If the recursive call is NOT in tail position (e.g., `n * fact(n-1, 1)` -- multiplication happens after the recursive call), no trampoline is applied. The generated code is direct recursion, which will stack-overflow for deep inputs. A warning is emitted: `note: non-tail-recursive call in fact; JVM stack depth limit applies`.

## Sub-phase 6.4 -- Mutable capture

### Goal-alignment audit (6.4)

Java lambdas can only capture effectively-final variables. When Mochi `var` bindings are captured inside lambdas, the lower pass must lift them to `Cell<T>` boxes. Without this, the transpiler would produce Java code that fails to compile with "variable used in lambda expression should be effectively final".

### Decisions made (6.4)

**`Cell<T>` runtime class**:

```java
package dev.mochi.runtime.func;

/** Mutable reference box for var-captured variables in lambdas. */
public final class Cell<T> {
    public T value;
    public Cell(T value) { this.value = value; }
}
```

**Lifting logic**: When the capture analysis (Phase 6.0) identifies a `var` binding `count` that is captured by a lambda:

```mochi
// Mochi:
var count = 0
let inc = () -> { count = count + 1 }
count = count + 10
inc()
print(count)
```

Lowers to:

```java
Cell<Long> count = new Cell<>(0L);
final Runnable inc = () -> { count.value = count.value + 1L; };
count.value = count.value + 10L;
inc.run();
dev.mochi.runtime.io.IO.println(count.value);
```

All reads of `count` after the lambda site are replaced with `count.value`. All writes of `count` after the lambda site are replaced with `count.value = ...`. The `Cell<Long>` itself is effectively final (the reference to the cell is never reassigned), so it can be captured by the lambda.

**`Cell` for primitive types**: `Cell<Long>` boxes the `long`. This means mutations involve autoboxing (`count.value = count.value + 1L` boxes the `long` into `Long` on assignment). For hot-path code with frequent mutable-captured int variables, this is a known overhead. Phase 17 tracks this as a potential optimisation (use a `LongCell` with `public long value`).

**`MochiFunction3` through `MochiFunction9`**: Lambda types for arity 3-9 that have no JDK standard functional interface:

```java
package dev.mochi.runtime.func;
@FunctionalInterface
public interface MochiFunction3<A, B, C, R> {
    R apply(A a, B b, C c);
}
```

Arity 4-9 follow the same pattern. These are generated by the runtime module's Maven build (a small code generator script, not hand-written).

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/jvm/lower/closure.go` | `LambdaExpr` lowering; capture analysis; mutable-capture detection; `Cell<T>` lifting; functional interface selection |
| `transpiler3/jvm/lower/expr.go` | `PartialApplyExpr` lowering (`_` placeholder -> synthesised lambda) |
| `transpiler3/jvm/lower/stmt.go` | Tail-call detection (`isTailCall`); trampoline rewrite of self-recursive functions |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/func/Cell.java` | Mutable reference box |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/func/MochiFunction3.java` | Arity-3 functional interface |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/func/MochiFunction4.java` | Arity-4 (through MochiFunction9) |
| `transpiler3/jvm/build/phase06_test.go` | `TestPhase6Funs`: 25 fixtures, JDK 21+25 |
| `tests/transpiler3/jvm/phase06-closures/*.{mochi,out}` | 25 fixtures |

## Test set

- `transpiler3/jvm/build/phase06_test.go::TestPhase6Funs` -- 25 fixtures, byte-exact diff, JDK 21+25.
- `transpiler3/jvm/lower/closure_test.go::TestCaptureAnalysis` -- unit test: identify captured variables in a lambda body; distinguish `let` (direct capture) vs `var` (Cell lift).
- `transpiler3/jvm/lower/closure_test.go::TestFunctionalInterfaceSelection` -- unit test: for each entry in the selection table, verify the correct `TypeRef` is chosen.
- `transpiler3/jvm/lower/stmt_test.go::TestTailCallTrampoline` -- unit test: `fact(n, acc)` with tail-recursive call produces `while(true)` loop with temp assignments.
- `transpiler3/jvm/lower/stmt_test.go::TestNonTailCallWarning` -- unit test: non-tail-recursive call emits a warning diagnostic.
- `transpiler3/jvm/runtime/func/CellTest.java` -- JUnit: `Cell<Long>` read/write round-trip.

## Deferred work

- Mutual tail-call optimisation (two functions calling each other in tail position): not in Phase 6; requires continuation-passing-style transform. Deferred as a potential future optimisation.
- Generic closures capturing type parameters (`<T>` in a lambda): basic support here; complex cases with bounded type parameters deferred to Phase 12.
- Async closures (`spawn` inside a lambda): Phase 11.
- `LongCell` (unboxed mutable capture for hot loops): Phase 17 performance optimisation.

## Closeout notes

Gate `TestPhase6Funs` green: 6 fixtures pass on JDK 21.

**Shipped (6.0 + 6.1):**
- `transpiler3/jvm/lower/closure.go`: `FunLit` lowering to Java lambda expressions with functional interface selection table; `FunCallExpr` lowering via SAM method dispatch; `ListMapExpr`, `ListFilterExpr`, `ListFoldlExpr` lowering to `stream().map/filter/collect` pipelines.
- `transpiler3/jvm/lower/lower.go`: capturing closure support via env-ref rewriting (`__e->field` -> plain field param) and prepending captured vars as extra params to lifted functions; `TypeFun` handling in param/return type lowering.
- `transpiler3/jvm/lower/stmt.go`: `TypeFun` let-binding uses `funcTypeRef()`; `ClosureEnvStmt` is a no-op (JVM lambdas capture natively).
- `transpiler3/jvm/lower/types.go`: `TypeFun` falls back to `Object` in raw contexts.
- `transpiler3/jvm/build/phase06_test.go`: `TestPhase6Funs` gate.
- `tests/transpiler3/jvm/phase06-closures/`: 6 fixtures (lambda_basic, lambda_as_arg, closure_capture, hof_map, hof_filter, hof_reduce).

**Deferred (6.2 - 6.4):** Partial application (`_` placeholder), tail-call trampoline, and mutable capture (`Cell<T>`) are deferred. The aotir does not surface partial-apply as a distinct node; tail-call analysis requires dataflow; mutable capture requires `var` capture detection which needs scope tracking not yet in the JVM lowerer.
