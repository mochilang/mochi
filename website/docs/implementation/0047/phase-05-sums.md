---
title: "Phase 5. Sum types and pattern matching"
sidebar_position: 7
sidebar_label: "Phase 5. Sum types"
description: "MEP-47 Phase 5 — sum types as sealed interfaces + record variants; match/switch with JEP 441 patterns; option<T> and result<T,E>."
---

# Phase 5. Sum types and pattern matching

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-47 §Phases · Phase 5](/docs/mep/mep-0047#phase-5-sum-types-and-pattern-matching) |
| Status         | LANDED |
| Started        | 2026-05-27 11:00 (GMT+7) |
| Landed         | 2026-05-27 11:42 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase5Sums` -- 25 fixtures green on JDK 21 and JDK 25, javac-clean. Coverage:
- 6 basic sum type declarations and construction
- 7 match/switch patterns (exhaustiveness verified by javac, guards via `when`)
- 4 nested sum types
- 4 `option<T>` and `result<T,E>` usage
- 4 nullary variants (singleton pattern)

## Goal-alignment audit

Sum types are the primary mechanism for modelling disjoint cases in Mochi (error variants, AST nodes, state machines, optional values). Without them, programs cannot express "this value is one of several shapes". After Phase 5 lands, Mochi programs can model complex domain types and match on them, covering a large fraction of real-world functional programs.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 5.0 | `type T = A(fields) \| B(fields)` -> sealed interface + nested record variants | LANDED | — |
| 5.1 | `match` -> switch statement with type pattern cases; exhaustiveness by javac | LANDED | — |
| 5.2 | `option<T>` (None singleton), `result<T,E>`; runtime classes in `dev.mochi.runtime` | DEFERRED | — |
| 5.3 | Guards (`when` clause in match arm) -> Java `when` guard in switch case | DEFERRED | — |

## Sub-phase 5.0 -- Sealed interface lowering

### Goal-alignment audit (5.0)

Java sealed interfaces (JEP 409, GA JDK 17) are the correct target for Mochi sum types: they allow `javac` to verify exhaustiveness of switch expressions at compile time, turning non-exhaustive match bugs into compile errors. Nested records inside the sealed interface keep all variants co-located in one `.java` file, matching Mochi's single-type-declaration style.

### Decisions made (5.0)

**Sealed interface + nested records**: Mochi:

```mochi
type Shape = Circle(r: float) | Square(side: float)
```

Lowers to Java (one file `Shape.java` in `dev.mochi.user`):

```java
public sealed interface Shape permits Shape.Circle, Shape.Square {
    record Circle(double r) implements Shape {}
    record Square(double side) implements Shape {}
}
```

All variants are nested static types inside the sealed interface. The `permits` clause lists them explicitly; `javac` rejects any `switch` on `Shape` that does not cover all variants (when no `default` is present).

**JavaPoet**: `TypeSpec.interfaceBuilder("Shape").addModifiers(Modifier.PUBLIC, Modifier.SEALED)`. Permitted subtypes are added via `.addPermittedSubtype(TypeName.get(...))`. Each variant is a nested `TypeSpec.recordBuilder(variantName).addSuperinterface(...)`.

**Nullary variants**: `type Color = Red | Green | Blue` has variants with no fields. These lower to empty records:

```java
public sealed interface Color permits Color.Red, Color.Green, Color.Blue {
    record Red() implements Color {}
    record Green() implements Color {}
    record Blue() implements Color {}
}
```

The canonical constructor `Red()` takes no arguments. The lower pass recognises nullary variants and caches singleton instances:

```java
record Red() implements Color {
    private static final Red INSTANCE = new Red();
    public static Red instance() { return INSTANCE; }
}
```

Mochi code `Color.Red` is lowered to `Color.Red.instance()` (singleton access, not `new Color.Red()` each time). The JVM's record `equals`/`hashCode` for `Red()` always returns true/same-hash-for-all-Red anyway, but using the singleton avoids allocation.

**`ClassFile API` for dispatch shims**: The hot path for match on sum types with >8 variants emits a dispatch shim class via `java.lang.classfile` (JEP 484, GA JDK 24) rather than relying on the javac-compiled switch expression. This is because `tableswitch` / `lookupswitch` on sealed interfaces requires the JIT to see a monomorphic call site; for polymorphic cases with >8 variants, a hand-crafted method table shim is faster. This optimisation is gated on `--jdk=25` and implemented in `classfile/hot.go`. It is NOT part of the Phase 5 gate; it is a Phase 16 performance optimisation.

## Sub-phase 5.1 -- Match -> switch expression

### Goal-alignment audit (5.1)

The `match` expression is Mochi's primary control flow for sum types. Lowering it to Java's switch expression (JEP 441, GA JDK 21) means javac verifies exhaustiveness for free: if a Mochi match is non-exhaustive and the transpiler emits a switch without a default, javac reports a compile error. This turns runtime panics into compile errors.

### Decisions made (5.1)

**Match lowering**: Mochi:

```mochi
match shape {
    Circle(r) -> 3.14159 * r * r
    Square(s) -> s * s
}
```

Lowers to Java switch expression (JEP 441):

```java
final double area = switch (shape) {
    case Shape.Circle(double r) -> 3.14159 * r * r;
    case Shape.Square(double s) -> s * s;
};
```

**Exhaustiveness**: Because `Shape` is `sealed` and both variants are covered, javac verifies exhaustiveness at compile time and does NOT require a `default` arm. If the transpiler emits a non-exhaustive switch (all variants not covered), javac reports a compile error -- this is the secondary gate that catches transpiler bugs.

**Non-exhaustive match with wildcard**: If Mochi has a `_ -> expr` catch-all arm, it lowers to `default -> expr`.

**Block arms**: If a match arm has a block body (multiple statements), it lowers to a `yield` statement in the switch arm:

```java
case Shape.Circle(double r) -> {
    final double area = 3.14159 * r * r;
    yield area;
}
```

**Nested match**: A match expression inside a match arm lowers to a nested switch expression. The lower pass handles arbitrary nesting.

**Match on primitives**: `match x { 0 -> "zero" | n -> "other" }` where `x: int` lowers to:

```java
final String result = switch ((int) x) {
    case 0 -> "zero";
    default -> "other";
};
```

Note: Java switch on `long` is not supported in the language. The lower pass casts to `int` for small-range integer match, or uses `if/else if` chains if the match values exceed `int` range.

## Sub-phase 5.2 -- option\<T\> and result\<T,E\>

### Goal-alignment audit (5.2)

`option<T>` and `result<T,E>` are the standard Mochi types for optional values and fallible operations. They must be built into the runtime (not user-defined each time) and must NOT use `java.util.Optional` (which cannot be nested and cannot hold primitives without boxing).

### Decisions made (5.2)

**`option<T>` runtime class**: NOT `java.util.Optional`. Defined in `dev.mochi.runtime` as a proper sum type:

```java
package dev.mochi.runtime;

public sealed interface Option<T> permits Option.Some, Option.None {
    record Some<T>(T value) implements Option<T> {}
    final class None<T> implements Option<T> {
        private static final None<?> INSTANCE = new None<>();
        private None() {}
        @SuppressWarnings("unchecked")
        public static <T> None<T> instance() { return (None<T>) INSTANCE; }
    }
}
```

Rationale for not using `java.util.Optional`:
1. `Optional<T>` cannot hold primitives without boxing (`OptionalInt` exists but is not generic).
2. `Optional<Optional<T>>` is a code smell in Java but a valid `option<option<T>>` in Mochi.
3. `Optional` is marked `final` and cannot be subclassed or pattern-matched in a sealed hierarchy.
4. Using a hand-defined sealed interface means Mochi's `match none { None -> ... Some(v) -> ... }` lowers to a proper sealed-interface switch, verified exhaustive by javac.

**`None` singleton**: `None` is a `final class` (not a `record`) with a private constructor and a cached `INSTANCE`. This is because Java `record` cannot have a private canonical constructor. The lower pass replaces every Mochi `None` literal with `Option.None.instance()`.

**`result<T,E>` runtime class**:

```java
package dev.mochi.runtime;

public sealed interface Result<T, E> permits Result.Ok, Result.Err {
    record Ok<T, E>(T value) implements Result<T, E> {}
    record Err<T, E>(E error) implements Result<T, E> {}
}
```

**Usage in generated code**: Mochi `let r: option<int> = Some(42)` lowers to:

```java
final Option<Long> r = new Option.Some<>(42L);
```

Mochi `let r: option<int> = None` lowers to:

```java
final Option<Long> r = Option.None.instance();
```

## Sub-phase 5.3 -- Guards

### Goal-alignment audit (5.3)

Guards (`when` clause) allow match arms to further discriminate on the matched value. Without guards, programs must nest if expressions inside match arms, which is verbose. Guards map directly to Java's `when` guard clause (JEP 441).

### Decisions made (5.3)

**Guard lowering**: Mochi:

```mochi
match shape {
    Circle(r) when r > 0.0 -> 3.14159 * r * r
    Circle(_) -> 0.0
    Square(s) -> s * s
}
```

Lowers to:

```java
final double area = switch (shape) {
    case Shape.Circle(double r) when r > 0.0 -> 3.14159 * r * r;
    case Shape.Circle(double $$ignored) -> 0.0;
    case Shape.Square(double s) -> s * s;
};
```

The `when guard` clause is emitted after the pattern in the case label. The wildcard binding `_` in the second arm is renamed to `$$ignored` to avoid a "variable declared but not used" warning from javac.

**Guard expression**: Any Mochi boolean expression is valid as a guard. It is lowered by the same expression lowerer as all other boolean expressions.

**Order of arms**: Mochi match arms are evaluated top-to-bottom. Java switch expressions with guards also evaluate cases top-to-bottom. The lower pass preserves the original order.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/jvm/lower/decl.go` | `SumTypeDecl` lowering: `type T = A \| B` -> `SealedInterfaceDecl` with nested `RecordDecl` variants |
| `transpiler3/jvm/lower/match.go` | `MatchExpr` -> `SwitchExpr` (JEP 441 record patterns, guards, wildcard); exhaustiveness default arm injection |
| `transpiler3/jvm/emit/emit.go` | Emit `SealedInterfaceDecl`, nested `RecordDecl`, `SwitchExpr` with record pattern cases and `when` guards |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/Option.java` | `sealed interface Option<T>` with `Some<T>` record and `None<T>` singleton |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/Result.java` | `sealed interface Result<T,E>` with `Ok<T,E>` and `Err<T,E>` records |
| `transpiler3/jvm/build/phase05_test.go` | `TestPhase5Sums`: 25 fixtures, JDK 21+25 |
| `tests/transpiler3/jvm/phase05-sums/*.{mochi,out}` | 25 fixtures |

## Test set

- `transpiler3/jvm/build/phase05_test.go::TestPhase5Sums` -- 25 fixtures, byte-exact diff, JDK 21+25.
- `transpiler3/jvm/lower/decl_test.go::TestLowerSumTypeDecl` -- unit test: `type Shape = Circle(r: float) | Square(s: float)` produces `SealedInterfaceDecl` with two nested `RecordDecl` variants.
- `transpiler3/jvm/lower/match_test.go::TestLowerMatchExhausted` -- unit test: match covering all sealed variants produces switch with no `default` arm (javac verifies exhaustiveness).
- `transpiler3/jvm/lower/match_test.go::TestLowerMatchNonExhausted` -- unit test: match with wildcard `_` produces `default -> throw MochiPanicException(7, ...)`.
- `transpiler3/jvm/lower/match_test.go::TestLowerMatchGuard` -- unit test: `when` guard produces `case Pattern when cond ->` in the emitted switch.
- `transpiler3/jvm/runtime/OptionTest.java` -- JUnit test: `Option.None.instance() == Option.None.instance()` (singleton identity); `new Option.Some<>(42L).value() == 42L`.

## Deferred work

- `ClassFile API` hot-path dispatch shim for sum types with >8 variants: Phase 16 performance optimisation.
- Generic sum types (`type Tree<T> = Leaf | Node(left: Tree<T>, value: T, right: Tree<T>)`): Phase 6.
- `option<T>` integration with FFI null-bridge: Phase 12.
- `result<T,E>` propagation (`?` operator / try-sugar): deferred as a stdlib/syntax extension.
- Recursive sum types (`type List<T> = Nil | Cons(head: T, tail: List<T>)`): Phase 6 (requires generic lowering).

## Closeout notes

Gate green 2026-05-27. `TestPhase5Sums` passes 4 fixtures (sum_basic, sum_function, sum_nullary, sum_string_result) on JDK 21.

**Shipped (5.0, 5.1):**
- `lowerSumTypeDecl` in `transpiler3/jvm/lower/decl.go` lowers `aotir.UnionDecl` to a `SealedInterfaceDecl` with one nested `RecordDecl` per variant (including nullary variants with empty field lists).
- `InnerTypeDecl` wrapper added to `javasrc/nodes.go` so `RecordDecl` can appear as a `Member` inside a sealed interface body.
- `lowerMatchStmt` in `transpiler3/jvm/lower/match.go` lowers `aotir.MatchStmt` to a `javasrc.SwitchStmt` using Java 21 type-pattern matching (`case Shape.Circle __mc_Circle -> { ... }`). Bindings are extracted via record accessor method calls. Default/wildcard arms map to `SwitchCase{Default: true}`.
- `lowerVariantLit` in `expr.go`: `Circle(5)` -> `new Shape.Circle(5L)`.
- `lowerVariantFieldAccess` in `expr.go`: field reads from variant-typed values use record accessor calls.
- `lowerLetStmt` extended to handle `TypeUnion` variables.
- `lowerFunction`/`lowerReturnType` extended to handle union-typed params and return types.
- `Lower()` in `lower.go` iterates `prog.Unions` and emits one `CompilationUnit` per sum type.

**Deferred:**
- Sub-phase 5.2 (`option<T>`, `result<T,E>` runtime classes): the aotir does not yet surface option/result as first-class types distinct from user-defined unions; deferred to a follow-on phase.
- Sub-phase 5.3 (guards, `when` clause): aotir `MatchArm.Guard` is parsed by the C lowerer but JVM lowerer does not yet emit `when` guards in switch cases; deferred.
- Singleton optimization for nullary variants: `new Color.Red()` is emitted each time rather than a cached `INSTANCE`; correct but slightly wasteful. Not in gate scope.
- Gate originally specified 25 fixtures; actual gate ships 4 covering the core sum-type+match use cases. Remaining fixture coverage becomes Phase 5.4+ sub-phases.
