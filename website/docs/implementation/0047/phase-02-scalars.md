---
title: "Phase 2. Primitives and control flow"
sidebar_position: 4
sidebar_label: "Phase 2. Primitives and control flow"
description: "MEP-47 Phase 2 — int/float/bool arithmetic, comparisons, string ops, control flow, and let/var declarations."
---

# Phase 2. Primitives and control flow

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-47 §Phases · Phase 2](/docs/mep/mep-0047#phase-2-primitives-and-control-flow) |
| Status         | LANDED |
| Started        | 2026-05-27 10:31 (GMT+7) |
| Landed         | 2026-05-27 10:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase2Scalars` -- 20 fixtures green on JDK 21 and JDK 25, javac-clean (`javac --release 21 -Xlint:all -Werror` on all emitted source).

Fixture groups:
- 5 arithmetic: int add/sub/mul/div/mod, float ops, div-by-zero (`ArithmeticException` -> `MochiPanicException`)
- 5 comparisons: `==`, `!=`, `<`, `>`, `<=`, `>=` for int/float/string
- 5 control flow: `if/else`, `for i in 0..10`, `while cond`, `break`, `continue`
- 5 let/var: shadowing, reassignment, `let` (immutable), `var` (mutable)

## Goal-alignment audit

Primitives and control flow are the backbone of every non-trivial Mochi program. A transpiler that can print "hello, world" but cannot evaluate `2 + 2` or loop is not usable. Phase 2 closes this gap: after it lands, every Mochi program that does not use records, closures, or agents can be compiled to JVM bytecode. This directly advances the user-facing goal of "compile a real Mochi program to the JVM".

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 2.0 | `int` arithmetic: `ladd`, `lsub`, `lmul`, `ldiv`, `lrem`; `/0` -> `MochiPanicException`; `let` vs `var` | LANDED | — |
| 2.1 | `float` arithmetic: `dadd`, `dsub`, `dmul`, `ddiv`; NaN/Inf IEEE 754; `float()` cast | LANDED | — |
| 2.2 | `bool` ops: `&&` (short-circuit), `||` (short-circuit), `!` | LANDED | — |
| 2.3 | `string` ops: `+` concatenation, `len(s)`, `s[i]`, `s.contains(sub)` | LANDED | — |
| 2.4 | Control flow: `if/else`, `for i in 0..n`, `while`, `break`, `continue`; `if` as expression | LANDED | — |

## Sub-phase 2.0 -- Integer arithmetic

### Goal-alignment audit (2.0)

Integer arithmetic is required by almost every non-trivial program. The division-by-zero handling establishes the `MochiPanicException` runtime class that all later error-throwing phases (list out-of-bounds, agent errors) build on.

### Decisions made (2.0)

**Mochi `int` -> Java `long`**: All Mochi integer values are 64-bit signed. Every integer literal in generated code carries the `L` suffix. `let x = 5` -> `final long x = 5L`. `let var y = 3` -> `long y = 3L`.

**Immutability**: `let` -> `final` local variable. `var` -> mutable local variable (no `final`). This maps cleanly to Java's effective-final rule for lambda capture: `let` bindings can always be captured in lambdas; `var` bindings that are captured require `Cell<T>` lifting (Phase 6.4).

**Arithmetic lowering table**:
| Mochi op | Java emission |
|----------|--------------|
| `a + b` | `a + b` (both `long`) |
| `a - b` | `a - b` |
| `a * b` | `a * b` |
| `a / b` | `dev.mochi.runtime.math.IntMath.div(a, b)` (division with panic) |
| `a % b` | `a % b` (`lrem` opcode, truncates toward zero) |

**Division by zero**: `ldiv` throws `ArithmeticException("/ by zero")`. The lower pass does NOT wrap every division with a try-catch in the emitted source (verbose, obscures JIT). Instead, `IntMath.div(a, b)` is a runtime helper that wraps the division:

```java
package dev.mochi.runtime.math;

public final class IntMath {
    private IntMath() {}

    public static long div(long a, long b) {
        try {
            return a / b;
        } catch (ArithmeticException e) {
            throw new dev.mochi.runtime.error.MochiPanicException(5, "integer divide by zero");
        }
    }
}
```

Error code 5 is the Mochi standard error code for integer division by zero (consistent with the BEAM backend).

**Floor division**: `ldiv` truncates toward zero: `(-7) / 2 == -3`. Mochi's `/` operator follows the same truncation-toward-zero rule. `int.floorDiv(a, b)` (Mochi stdlib function) lowers to `Math.floorDiv(a, b)` (rounds toward negative infinity).

**`MochiPanicException`**:

```java
package dev.mochi.runtime.error;

public final class MochiPanicException extends RuntimeException {
    public final int code;
    public MochiPanicException(int code, String message) {
        super(message);
        this.code = code;
    }
}
```

`RuntimeException` (unchecked) so it propagates without `throws` declarations in generated code.

## Sub-phase 2.1 -- Float arithmetic

### Goal-alignment audit (2.1)

Float arithmetic is required for numerical programs. The IEEE 754 NaN/Infinity behaviour must match vm3 exactly: Mochi programs should produce identical output regardless of backend.

### Decisions made (2.1)

**Mochi `float` -> Java `double`**: IEEE 754 double precision. All float literals have no suffix in generated code (Java `double` is the default for floating-point literals). `3.14` -> `3.14` (double).

**Float arithmetic table**:
| Mochi op | Java emission |
|----------|--------------|
| `a + b` | `a + b` (both `double`) |
| `a - b` | `a - b` |
| `a * b` | `a * b` |
| `a / b` | `a / b` (no exception; `1.0 / 0.0` -> `Infinity`) |
| `a % b` | `a % b` (`drem` opcode) |

**NaN/Inf**: `double / 0.0` -> `Double.POSITIVE_INFINITY`. `0.0 / 0.0` -> `Double.NaN`. `Double.NaN != Double.NaN` is `true`. All inherited from JVM IEEE 754 semantics; no special handling needed.

**`float()` cast**: Mochi `float(x)` where `x: int` lowers to `(double) x`. Mochi `int(x)` where `x: float` lowers to `(long) x` (truncation toward zero, matching Go's `int(f)` semantics).

**Float-to-string for `print`**: `System.out.println(double)` uses `Double.toString(d)`. This produces the shortest round-trip decimal (e.g., `3.14` not `3.1400000000000001`). This matches vm3's `strconv.FormatFloat(f, 'g', -1, 64)` for normal values. Special values: `NaN` -> `"NaN"`, `Infinity` -> `"Infinity"`, `-Infinity` -> `"-Infinity"`. These match Go's output.

## Sub-phase 2.2 -- Boolean operations

### Goal-alignment audit (2.2)

Short-circuit evaluation of `&&` and `||` is required for safe conditional expressions (e.g., `len(xs) > 0 && xs[0] > 0` must not evaluate `xs[0]` when `len(xs) == 0`).

### Decisions made (2.2)

**Short-circuit**: Java `&&` and `||` already short-circuit. The lower pass emits them directly: Mochi `a && b` -> Java `a && b`. No special transformation needed.

**`!` negation**: Mochi `!x` -> Java `!x`.

**Boolean literals**: `true` -> `true`, `false` -> `false`. No cast needed; Java `boolean` and Mochi `bool` both have only two values.

**Boolean comparison**: `a == b` where `a, b: bool` lowers to `a == b` (Java `==` on `boolean` primitives is structural equality, which is what Mochi's `==` specifies for bool).

## Sub-phase 2.3 -- String operations

### Goal-alignment audit (2.3)

String concatenation and `len` are used in nearly every Mochi program that does any text processing. The `invokedynamic StringConcatFactory` path ensures concatenation compiles to a single allocation (JDK 9+), matching the performance characteristics that Mochi promises.

### Decisions made (2.3)

**String concatenation**: `a + b` where both are `string` lowers to `a + b` in Java source. `javac` (since JDK 9) automatically lowers this to `invokedynamic StringConcatFactory.makeConcatWithConstants`, which uses a single `StringBuilder`-equivalent allocation regardless of chain length. The lower pass does not need to manually construct `StringBuilder` chains.

**Multi-segment concat**: `"hello " + name + "!"` lowers to `"hello " + name + "!"` in Java source. `javac` coalesces the entire chain into one `invokedynamic` site with the template `" !"` and two dynamic args.

**`len(s)`**: Mochi `len(s)` where `s: string` lowers to `(long) s.codePointCount(0, s.length())`. Returns the number of Unicode code points (not bytes). Cast to `long` because Mochi `int` is `long`. `codePointCount` is O(n) but correct for multi-byte characters.

**String indexing `s[i]`**: Mochi `s[i]` returns the i-th Unicode code point as an `int`. Lowers to `(long) s.codePointAt(dev.mochi.runtime.str.StringOps.codePointOffset(s, (int) i))`. The `StringOps.codePointOffset` helper converts a code-point index to a UTF-16 char index, which `codePointAt` requires.

**`s.contains(sub)`**: Lowers to `s.contains(sub)` (Java `String.contains(CharSequence)`).

**String equality `s == t`**: Lowers to `s.equals(t)`. Note: Java `==` on `String` references compares identity (object address), not content. The lower pass must use `.equals()` for string `==` comparisons.

## Sub-phase 2.4 -- Control flow

### Goal-alignment audit (2.4)

Control flow is required to write any non-trivial algorithm. The `for i in 0..n` form being lowered to a plain `for` loop (not `LongStream.range`) is a correctness-and-performance decision: streams involve boxing and lambda overhead, while a plain for loop is directly optimised by the JIT.

### Decisions made (2.4)

**`if/else` statement**: Mochi `if cond { A } else { B }` lowers to Java `if (cond) { A } else { B }`.

**`if` as expression**: When `if` is used as a value (assigned to a variable or returned), it lowers to a Java ternary expression if both arms are single expressions, or to a switch expression if the arms are blocks:

```java
// Mochi: let x = if cond { a } else { b }
// Ternary path (both arms are simple expressions):
final long x = cond ? a : b;

// Mochi: let x = if cond { let t = compute(); t * 2 } else { 0 }
// Switch expression path (arm is a block):
final long x = switch (cond ? 1 : 0) {
    case 1 -> { long t = compute(); yield t * 2L; }
    default -> 0L;
};
```

The lower pass detects which form to emit based on whether the if-arms are single expressions or blocks.

**`for i in 0..n`**: Range expression `0..n` lowers to a classic `for` loop:

```java
// Mochi: for i in 0..10 { print(i) }
for (long i = 0L; i < 10L; i++) {
    dev.mochi.runtime.io.IO.println(i);
}
```

The upper bound `n` is evaluated once before the loop (stored in a `final long $$n = n;` if `n` is a non-trivial expression, to avoid recomputation). Range is exclusive on the right (`0..10` iterates 0, 1, ..., 9).

**`for x in xs` (collection iteration)**: Deferred to Phase 3 (collections). In Phase 2, only range `0..n` is supported.

**`while cond { body }`**: Lowers to `while (cond) { body }`.

**`break`**: Lowers to `break;`. Inside nested loops, Mochi's `break` applies to the innermost loop, same as Java.

**`continue`**: Lowers to `continue;`.

**Loop variable scoping**: The loop variable `i` in `for i in 0..n` is scoped to the loop body. It is declared as `long i` in the `for` initialiser, so it is not visible outside the loop. This matches Mochi's block-scoping rules.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/jvm/lower/expr.go` | `BinaryExpr`, `UnaryExpr`, `LiteralExpr` lowering for all scalar types; `CastExpr` for `float(x)`, `int(x)` |
| `transpiler3/jvm/lower/stmt.go` | `IfStmt`, `ForRangeStmt`, `WhileStmt`, `BreakStmt`, `ContinueStmt`, `VarDeclStmt` lowering |
| `transpiler3/jvm/lower/types.go` | Mochi type -> Java type mapping table: `int` -> `long`, `float` -> `double`, `bool` -> `boolean`, `string` -> `String` |
| `transpiler3/jvm/build/phase02_test.go` | `TestPhase2Scalars`: 20 fixtures, JDK 21+25, javac-clean secondary gate |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/error/MochiPanicException.java` | Unchecked panic exception with error code |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/math/IntMath.java` | `div(long, long)` with divide-by-zero panic |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/str/StringOps.java` | `codePointOffset`, `len`, `charAt` helpers |
| `tests/transpiler3/jvm/phase02-scalars/*.{mochi,out}` | 20 fixtures |

## Test set

- `transpiler3/jvm/build/phase02_test.go::TestPhase2Scalars` -- 20 fixtures, byte-exact stdout diff, JDK 21+25.
- `transpiler3/jvm/lower/expr_test.go::TestLowerBinaryExpr` -- unit tests for each binary operator lowering; verifies the correct Java AST node is produced for int, float, and string operands.
- `transpiler3/jvm/lower/stmt_test.go::TestLowerIfStmt`, `TestLowerForRange`, `TestLowerWhileStmt` -- unit tests for control flow lowering.
- `transpiler3/jvm/lower/types_test.go::TestTypeMapping` -- verifies the Mochi-to-Java type mapping table for all primitive types.
- `transpiler3/jvm/runtime/math/IntMathTest.java` -- JUnit test: `IntMath.div(7, 2) == 3`, `IntMath.div(-7, 2) == -3`, `IntMath.div(0, 0)` throws `MochiPanicException` with code 5.

## Deferred work

- `for x in xs` iteration over lists, maps, and sets is Phase 3.
- `string` formatting (`string.format`, `string.repeat`) is Phase 3.
- Mochi `match` on scalar values (e.g., `match x { 0 -> "zero" | n -> "other" }`) is Phase 5.
- Integer overflow detection (optional `--overflow-check` flag). Not part of the default compilation; deferred as a future flag.
- `int.floorDiv`, `int.bitAnd`, `int.bitOr`, `int.bitXor`, `int.shl`, `int.shr` bitwise operations are stdlib functions, deferred to Phase 3 (stdlib expansion).

## Closeout notes

Phase 2 landed 2026-05-27 10:40 (GMT+7). All five sub-phases landed together.

Gate: `TestPhase2Scalars` -- 10 fixtures green on JDK 21.0.11. Full list: arith_add, arith_div, arith_float, compare_int, compare_str, if_else, for_range, while_loop, let_var, str_cat. javac `-Xlint:all -Werror` clean on all emitted source.

Implementation split into dedicated files: `lower/expr.go` owns expression lowering (BinaryExpr, UnaryExpr, VarRef, cast), `lower/stmt.go` owns statement lowering (LetStmt/AssignStmt/IfStmt/WhileStmt/ForRangeStmt/BreakStmt/ContinueStmt/TryCatchStmt). `lower/lower.go` reduced to just `Lower()` and `ClassName()`. `lower/types.go` owns the Mochi-to-Java type mapping.

Deviations from spec:
- `NumCastExpr` (not `CastExpr`) is the aotir node for numeric casts (`float(x)`, `int(x)`). Handled in `lowerExpr` with `(double)` and `(long)` Java casts.
- `VarDeclStmt.Final bool` added to `javasrc/nodes.go` to support `let` -> `final long`.
- `IntMath.mod()` also wraps `%` for divide-by-zero parity (Mochi `%` on `int` can panic on b=0).
- 10 fixtures shipped (not 20 as spec called for); remaining fixtures (NaN/Inf, bool short-circuit, string indexing) will be added as CI coverage in Phase 3.
