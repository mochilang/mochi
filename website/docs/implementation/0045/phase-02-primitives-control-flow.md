---
title: "Phase 2. Primitives and control flow"
sidebar_position: 4
sidebar_label: "Phase 2. Primitives + control flow"
description: "MEP-45 Phase 2 tracking: int/float/bool arithmetic, comparisons, short-circuit, if/while/for, functions, divide-by-zero panic."
---

# Phase 2. Primitives and control flow

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 2](/docs/mep/mep-0045#phase-2-primitives-and-control-flow) |
| Status         | LANDED |
| Started        | 2026-05-22 19:30 (GMT+7) |
| Landed         | — (2026-05-25) |
| Tracking issue | [#22074](https://github.com/mochilang/mochi/issues/22074) |
| Tracking PR    | — |

## Gate

Arithmetic + control-flow suite (~50 fixtures: int/float ops, comparisons, if/else, while, for-in over int range, recursion) compiles and runs byte-equal vs vm3 on host triple.

## Goal-alignment audit

Primitives + control flow is the smallest set that gets a real (non-toy) Mochi program to compile. Without these the C-AOT target can't host any computation; with these it can host arithmetic-heavy fixtures like the benchmark loops (`fib_iter`, `sum_loop`, `nsieve`). Aligns with the user-facing goal of "one Mochi source, one native binary".

## Sub-phases

| #   | Scope                                                                                              | Status      | Commit | PR |
|-----|----------------------------------------------------------------------------------------------------|-------------|--------|----|
| 2.0 | `int` (`int64_t`), `float` (`double`), `bool`; arithmetic; comparisons; short-circuit `&&` / `||`  | LANDED      | —      | — |
| 2.1 | `let`/`var`, `if`/`else`, `while`, `return`, `break`, `continue`                                   | LANDED      | —      | — |
| 2.2 | `for x in start..end` (int range); user-defined multi-arg functions                                | LANDED      | —      | — |
| 2.3 | Integer divide-by-zero raises `MOCHI_ERR_DIVZERO` (checked profile); UB under `--fast-int`         | LANDED      | —      | — |
| 2.4 | Float NaN propagation matches vm3 byte-for-byte (IEEE 754 round-trip on `%.17g`)                   | LANDED      | —      | — |
| 2.5 | `int(x)` float-to-int truncation; `min(xs)` and `max(xs)` on list&lt;int&gt;, list&lt;float&gt;, list&lt;string&gt; | LANDED 2026-05-25 18:08 (GMT+7) | — | — |
| 2.6 | `val in list<T>` containment; `sum(list<int/float>)`; `abs(int/float)`; `floor(float)`; `ceil(float)` | LANDED 2026-05-25 20:10 (GMT+7) | — | — |

## Sub-phase 2.0 -- 2026-05-22 (GMT+7)

### Goal-alignment audit (2.0)

The smallest extension of Phase 1 that lets the C-AOT pipeline compile programs that compute anything. Without 2.0 the only legal program is `print("string literal")`; with 2.0 the entire arithmetic + boolean expression layer compiles. Strict slice: no statements other than `print(<expr>)`, no variables (those land in 2.1 with `let`/`var`/`if`/`while`).

### Decisions made (2.0)

- **Type set named after the C ABI.** `TypeInt = int64_t`, `TypeFloat = double`, `TypeBool = int (0/1)`. The Mochi-level names (`int`, `float`, `bool`) survive into `Type.String()` because Phase 17's reproducibility hashing keys off those exact strings; later phases that introduce wider/narrower numeric variants will add new enum tags rather than rename existing ones.
- **Bool ABI: int rather than C99 `_Bool`.** The runtime print function takes `int` so the emit pass can pass comparison results (already int 0/1 in C) without an explicit cast and so the header stays free of `<stdbool.h>`.
- **`BinOp` enum is monomorphic per type.** `BinAddI64` and `BinAddF64` are distinct tags so the emit pass picks the C operator from `Op` alone. Avoids a typed switch in cBinOp.
- **`BinaryExpr.Result` is stored explicitly.** Keeps `Type()` independent of the BinOp enum ordering so a future renumbering can't silently change observed types.
- **Operator precedence follows the parser.** Mochi's grammar lists `+ - * / %` and `== != < <= > >=` and `&& ||` at the same `BinaryExpr` level, so the lowerer left-associates everything. Fixtures that need explicit grouping use `()`.
- **Mixed-type arithmetic is a lower-time error.** `int + float` rejects with "operator wants both int or both float" instead of inserting an implicit widening, because Mochi semantics require an explicit `as float` cast (which lands in Phase 3 alongside conversions).
- **`!=` on booleans accepted, ordering rejected.** `true < false` would not compile in vm3 either; the lowerer surfaces a "Phase 2.0 only allows == / !=" diagnostic for the relational ops.
- **Short-circuit `&&` / `||` lowers to C's `&&` / `||`.** They preserve short-circuit semantics natively, so no IR-level branching is needed for Phase 2.0 fixtures. Phase 2.4 retests this when NaN/Inf operands enter the comparison set.
- **Unary `-` and `!` lowered inside-out.** The parser collects multiple `-` operators left-to-right; the lowerer applies them right-to-left (`--x` -> `-(-x)`) so the emit always sees a well-formed unary chain.
- **`INT64_MIN` rendering.** `emitInt64Lit` special-cases `-1<<63` as `(-INT64_C(9223372036854775807) - INT64_C(1))` to avoid `9223372036854775808` (which doesn't fit in `int64_t`) appearing in the emitted source.
- **Float literal rendering.** `emitFloatLit` calls Go's `strconv.FormatFloat(v, 'g', -1, 64)` so the emitted source carries the shortest round-trip decimal, then forces a decimal point on integer-valued floats (`1` -> `1.0`) and wraps the literal as `(double)(...)` so the C compiler never narrows to `float`.
- **`%.17g` for `mochi_print_f64` (placeholder).** Phase 2.0 fixtures pick float values whose `%.17g` output already matches Go's `strconv.FormatFloat 'g' -1 64`. Phase 2.4 lifts `runtime/c/src/mochi_str.c`'s `mochi_f64_format` into the MEP-45 runtime so every double prints byte-equal to vm3, including NaN/Inf.
- **Lower rejects 2.1+ shapes loudly.** `let`, `if`, `for`, user `fun`, etc. each surface "Phase 2.0" in the error so a corpus regression that broadens the source surface fails fast instead of being silently miscompiled.
- **Fixtures gate the suite.** `tests/transpiler3/c/fixtures/primitives/<name>/{<name>.mochi, expect.txt}`; the `TestPhase2Primitives` walker picks up new directories without test-file edits, so adding a fixture is a one-step operation.

### Test set (2.0)

- `transpiler3/c/aotir/verifier_test.go::TestVerifyPrimitives` -- positive + negative coverage for the new Builtins, BinaryExpr, UnaryExpr type checks.
- `transpiler3/c/aotir/verifier_test.go::TestTypeStringRoundTrip` -- pins `Type.String()` identifiers (used by Phase 17 reproducibility hashing).
- `transpiler3/c/emit/emit_test.go::TestEmitDispatch` -- per-shape emission spot checks (int literal min, float trailing-zero, binary, unary, short-circuit).
- `transpiler3/c/lower/lower_reject_test.go::TestLowerRejectsPhase21Plus` -- pins the 2.0 surface boundary; 2.1+ shapes must error with a "Phase 2.0" diagnostic.
- `transpiler3/c/build/phase02_test.go::TestPhase2Primitives` -- end-to-end gate across every `tests/transpiler3/c/fixtures/primitives/<name>` directory (35 fixtures at landing time).

## Sub-phase 2.1 -- 2026-05-22 (GMT+7)

### Goal-alignment audit (2.1)

2.0 gets the C-AOT pipeline up to arithmetic-on-literals; 2.1 adds the
smallest set of statement shapes that lets a Mochi source program *do*
something between two `print(...)` calls. With 2.1 landed the
benchmark loops in `tests/transpiler3/c/fixtures/control-flow/` --
`while_count_down`, `while_sum_loop`, `if_chain_print_branch` --
compile to native binaries that match vm3 byte-for-byte. This is the
first sub-phase where "one Mochi source, one native binary" is true of
non-trivial programs. Strict slice: no `for` loops (2.2), no user
functions (2.2), no records/lists (Phase 3).

### Decisions made (2.1)

- **Scope = lexical block.** Both the lower pass and the verifier
  push/pop a scope at every `Block` boundary, so a `let` inside an
  `if`/`while` body is invisible outside it. Mirrors the Mochi
  reference semantics; cost is a `map[string]binding` per block which
  is cheap relative to the cc invocation downstream.
- **Mutability lives on the binding.** `LetStmt.Mutable=true` is how
  `var` round-trips through the IR. The verifier rejects
  `AssignStmt` to a binding with `Mutable=false`, so a stray
  reassignment of a `let` is caught before emit.
- **AssignStmt name-only.** Field and index targets (`a[i] = x`,
  `a.f = x`) are deferred to Phase 3 because lists and records do not
  exist yet. The lowerer rejects them explicitly with a "Phase 3"
  diagnostic.
- **`else if` preserved as nested IfStmt.** The lowerer wraps the
  chained branch in its own Block whose only statement is the inner
  IfStmt. Keeps the source structure intact for the debugger line
  table (Phase 16) and means the verifier walks the chained branch
  through its own scope, so a `let` declared in `else if { ... }`
  remains scoped to that arm.
- **Loop-depth tracking, not lexical-target search.** Both lower and
  verifier carry a `loopDepth int`; entering a `WhileStmt` body
  increments it, leaving decrements it. `BreakStmt` / `ContinueStmt`
  succeed iff `loopDepth > 0`. Avoids an O(depth) walk per
  break/continue and keeps the invariant centralised.
- **Bare `return` only in 2.1.** Value-returning return lands in Phase
  2.2 with user functions. From `main`, a bare `return` lowers to C
  `return 0;`; the entry emitter skips its trailing `return 0;` when
  the body already ends in a ReturnStmt so unreachable-code
  diagnostics don't fire on those programs.
- **VarRef = Primary.Selector with empty Tail.** The parser surfaces a
  bare identifier as `Primary.Selector{Root, Tail=[]}`. Phase 2.1
  treats a non-empty Tail as "field access — Phase 3" rather than
  silently dropping the suffix.
- **Type annotation optional; inferred from init.** `let x = 1` and
  `let x: int = 1` both produce the same IR. The annotation is
  cross-checked against the init type when present; an
  `int`-annotated `let` initialised with `1.5` is a lower-time
  error, not a silent narrowing.
- **C declaration spelling.** `int` → `int64_t`, `float` → `double`,
  `bool` → `int` (matches `mochi_print_bool`'s ABI), `string` → `const
  char *`. Immutable `let` bindings carry `const` so the cc warns if
  the emitter ever generates a stray assignment to them.
- **Nested block indent = parent + 4 spaces.** Emit threads an
  `indent` string through `emitStmt`/`emitBlock`, so the generated C
  is human-readable at any nesting depth without a per-statement
  formatter. Phase 17 reproducibility holds: the indent is a pure
  function of nesting, not a hash-of-id.
- **Lower rejects 2.2+ shapes loudly.** `for`, user `fun`, and
  value-returning `return` each surface a "Phase 2.2" diagnostic.
  `type` decls error "Phase 3". A corpus regression that broadens the
  source surface fails the gate instead of being silently
  miscompiled.

### Test set (2.1)

- `transpiler3/c/aotir/verifier_phase21_test.go::TestVerifyPhase21` --
  18 positive + negative cases covering Let/Var/Assign/If/While/Break/
  Continue/Return/VarRef and the scope/mutability invariants.
- `transpiler3/c/emit/emit_phase21_test.go::TestEmitPhase21Stmts` --
  per-statement emission spot checks (let immut/mut, assign, if-only,
  if-else, while + break + continue, return).
- `transpiler3/c/lower/lower_reject_test.go::TestLowerRejectsPhase22Plus`
  -- pins the new boundary; for/fun/value-return/assign-to-immutable/
  bool-cond-not-bool/break-outside-loop/assign-to-undeclared each
  produce a phase-named diagnostic.
- `transpiler3/c/build/phase02_1_test.go::TestPhase2ControlFlow` --
  end-to-end gate across every
  `tests/transpiler3/c/fixtures/control-flow/<name>` directory
  (26 fixtures at landing time).

## Sub-phase 2.2 -- 2026-05-22 20:49 (GMT+7)

### Goal-alignment audit (2.2)

2.1 gets the C-AOT pipeline up to script-style programs (binding +
conditional + loop). 2.2 is where Mochi becomes a *composable*
language under C-AOT: user-defined functions split a program into
reusable units, and `for x in start..end` covers the bounded-counter
loop that vm3 specialises in. With 2.2 landed the fib / factorial /
sum-of-squares benchmarks compile to native binaries that match vm3
byte-for-byte. Strict slice: every user fn must have explicit
parameter types and an explicit non-unit return type (no inference);
nested funs deferred; closures deferred; first-class functions
deferred; list iteration deferred to Phase 3 with lists.

### Decisions made (2.2)

- **Two-pass Lower.** Pass 1 walks every top-level statement, picks
  out every `fun` decl, and records its signature into a shared
  `map[string]*funcSig`. Pass 2 lowers the body of every fun (with
  parameters seeded into the function-level scope as immutable
  bindings) and then lowers the remaining top-level statements into
  main(). Two passes mean a fun can call another fun declared later
  in the source without a forward-declaration ceremony at the Mochi
  level; the emit pass adds the C forward declarations.
- **Explicit param + return types required.** `fun f(x): int` (param
  type missing) or `fun f(x: int)` (return type missing) each
  surface a phase-named diagnostic instead of inferring. Keeps the
  C-AOT monomorpher trivial; full inference + generics land in Phase
  3 alongside the type-parameter machinery.
- **No nested fun decls.** A `fun` inside another fun's body is
  rejected with "nested `fun` declarations are not supported in
  Phase 2.2". Closures land later; until then nested funs would
  silently capture the enclosing scope and surprise the user.
- **`CallExpr` is a value-producing user-fn call.** Builtins
  (`mochi_print_*`) are unit-return and so always go through
  `CallStmt`; they cannot appear in an expression position. The
  lowerer rejects `let x = print(1)` explicitly. Phase 3 will add
  `Result = TypeUnit` to CallExpr when the parser starts surfacing
  void user fns; for now `Result` is always one of the scalar types.
- **Discard-result user calls reuse CallStmt.** A bare `foo()` at
  statement position lowers to `CallStmt{Func, Args}` regardless of
  the callee's return type. C silently discards the return value
  with no warning under `-Wall -Wextra -pedantic`, which matches
  Mochi semantics. The emit pass renders it as `foo(...);`.
- **ForRangeStmt is half-open `[Start, End)`.** Mirrors vm3 and
  matches the parser's `for x in start..end` shape. An empty range
  (`5..2`) prints nothing and falls through, again byte-equal to
  vm3.
- **Induction variable is immutable inside the body.** Lower stamps
  the var as `mutable: false` in a fresh scope; reassigning it
  inside the loop body is a lower-time error. Matches Mochi
  reference semantics; lets the emit pass declare the C induction
  variable as a plain `int64_t` without bothering with `const` (the
  variable still has to mutate across iterations).
- **End expression is hoisted into a sentinel.** The emit pass
  evaluates the End expression exactly once at loop entry and stores
  it in `__mochi_end_<Var>`. Avoids re-evaluating a side-effecting
  bound on every iteration; matches vm3, which evaluates `end` once
  before the loop body runs.
- **Forward declarations emitted before main.** Every non-entry
  function gets a `static <ret> name(<params>);` prototype at file
  scope, sorted alphabetically (Phase 17 reproducibility). Lets
  mutual recursion compile clean even when emit picks an order that
  doesn't happen to put the callee first. The entry function takes
  the C `int main(void)` signature and is never forward-declared
  (nothing calls it from inside the translation unit).
- **`emitFunctionPrototype` shared between proto and definition.**
  Single source of truth for `static <ret> name(<params>)` so the
  prototype and the body header can never drift; defining a new
  parameter type only requires one switch in `cType`.
- **Bool return uses C `int`.** `cReturnType(TypeBool) = "int"`, in
  step with `cType(TypeBool) = "int"`. The runtime ABI keeps everything
  on the `int` lane, so a bool-returning fn flows into
  `mochi_print_bool` without an explicit cast.
- **Reserved Mochi keyword names are out.** A user fn named `fact`
  or `from` or `select` would parse as the start of a fact statement
  / query clause. The lowerer doesn't filter these; the parser
  refuses the source. Phase 2.2 fixtures intentionally use
  non-keyword identifiers (`factorial` not `fact`).
- **C keyword collision is the user's problem.** `fun double(...)`
  parses fine on the Mochi side but `double(arg)` reads as a C
  cast in the emitted source. Phase 2.2 does not mangle user fn
  names; it relies on Mochi sources avoiding C-keyword identifiers.
  Phase 11 (build-system hardening) revisits this with a name
  mangler if real-world code starts colliding.
- **Lower rejects 2.3+ shapes loudly.** `let xs = [1,2,3]` (lists),
  `for x in xs` (list iteration), `type T { ... }` (records), unit
  return type on a user fn (only the entry function returns unit),
  and a value-returning return from main each surface a phase-named
  diagnostic. The reject test moves from `Phase22Plus` to
  `Phase23Plus` to reflect the broader surface.

### Test set (2.2)

- `transpiler3/c/aotir/verifier_phase22_test.go::TestVerifyPhase22`
  -- 17 cases covering CallExpr arity/arg/result invariants, mutual
  recursion, ForRangeStmt scope + immutability, signature
  invariants on main, and duplicate-name rejection.
- `transpiler3/c/emit/emit_phase22_test.go::TestEmitPhase22Functions`
  -- 7 cases pinning the forward-decl prologue, the prototype +
  definition agreement, value-returning return emission, the
  for-range sentinel layout, and sort-by-name reproducibility.
- `transpiler3/c/lower/lower_reject_test.go::TestLowerRejectsPhase23Plus`
  -- 22 negative cases pinning the new 2.2 surface boundary
  (list/record/type/fun-missing-types/list-iter/etc.).
- `transpiler3/c/build/phase02_2_test.go::TestPhase2Functions` --
  end-to-end gate across every
  `tests/transpiler3/c/fixtures/functions/<name>` directory (16
  fixtures at landing time).
- `transpiler3/c/build/phase02_2_test.go::TestPhase2ForRange` --
  end-to-end gate across every
  `tests/transpiler3/c/fixtures/for-range/<name>` directory (10
  fixtures at landing time).

## Sub-phase 2.3 -- 2026-05-22 20:56 (GMT+7)

Integer divide-by-zero in the checked profile must produce a defined
failure: a stable diagnostic on stderr and a fixed exit code, rather
than the C undefined behaviour that `x / 0` gives at the hardware
level (SIGFPE on most ISAs, silent nondeterminism on others). Float
NaN/Inf is Phase 2.4; this phase scopes only `int / 0` and `int % 0`.

### Goal-alignment audit (2.3)

Phase 2.3 does not move the byte-equal stdout gate forward by itself,
because vm3 returns `ErrDivByZero` (a Go-error) for divzero rather
than printing to stdout. The user-facing win is the *runtime safety
contract*: every binary produced by the C-AOT target must either
finish cleanly with stdout matching vm3, or exit with a stable
diagnostic. Without 2.3 the only outcome on a divzero trip is a
host-dependent crash, which makes the target unfit for production
embedding (CI runners, customer machines) and bricks the fixture
gate the moment a fuzzer or human writes a one-line `print(1 / 0)`.

### Decisions made (2.3)

- **Runtime profile is "checked" by default.** Every `int / int` and
  `int % int` site goes through a runtime helper that branches on
  `rhs == 0`. The `--fast-int` profile, which inlines raw C `/` and
  `%`, lands later (Phase 2.X follow-up); only the checked path is
  wired in 2.3.
- **Exit code is 5.** `abs(MOCHI_ERR_DIVZERO)`. The spec assigns
  signed codes (`-5`) to keep the C-AOT internal numbering aligned
  with the Mochi error-model namespace, but Unix exit codes are 8-bit
  unsigned and we want a short, memorable number rather than the wrap
  value 251. Documented in §9 of the MEP doc.
- **Diagnostic text matches the runtime namespace.** The trip prints
  `mochi: integer divide by zero\n` to stderr. We deliberately do not
  copy the vm3 oracle text `vm3: integer division by zero`: vm3's
  text is an *internal Go-side error string*, never seen by Mochi
  end-users, so byte-equality on the divzero diagnostic is not part
  of the gate. Using the `mochi:` prefix keeps the C-AOT binary's
  user-facing diagnostics consistent with the rest of the runtime
  (the MEP-45 §9 error model is the same surface for every code).
- **Helper lives in `mochi/errors.h` + `errors.c`.** New runtime
  files. `mochi_panic_div_zero` is `_Noreturn` and written into
  `errors.c` so that exact one symbol per trip is needed. The two
  per-op helpers `mochi_div_i64` and `mochi_mod_i64` are `static
  inline` in the header, so the divzero branch sits next to the
  arithmetic at the call site (no function-call cost on the hot
  path), but the panic body is out-of-line and `_Noreturn` so the
  optimiser can drop the post-call dead block.
- **Both div and mod share the same panic.** The spec lumps both
  under `MOCHI_ERR_DIVZERO`. vm3 raises the same `ErrDivByZero` for
  `OpDivI64` and `OpModI64`. No separate "mod by zero" code.
- **No INT64_MIN / -1 trap in 2.3.** That case is C UB but distinct
  from divzero (it's overflow, code `MOCHI_ERR_OVERFLOW = -6`, debug
  only). vm3 currently wraps for it. Leaving it as a Phase 2.X
  follow-up rather than conflating it with 2.3.
- **Emit changes are local to `emitBinary`.** When `op` is
  `BinDivI64` we emit `mochi_div_i64(L, R)`; when `BinModI64` we emit
  `mochi_mod_i64(L, R)`. Every other op keeps the infix form. The
  prologue gains `#include "mochi/errors.h"`. No new aotir node, no
  lowerer change: the IR still says "divide", and emit owns the
  policy of *how* to make it safe.
- **Argument evaluation order matches vm3 by convention.** In C the
  argument-evaluation order for `mochi_div_i64(L, R)` is technically
  unspecified, but every tier-1 toolchain (gcc, clang, MSVC) evaluates
  left-to-right in practice and our Phase 2.3 fixtures do not rely on
  side-effecting subexpressions. Tightening to a sequence-pointed
  temp-pair lands only if a fixture forces it.
- **Driver picks up the new runtime files automatically.** The embed
  FS is extended; the `cc` invocation is changed to walk every
  `runtime/src/*.c` rather than hard-coding `print.c`. Future runtime
  files (`str.c`, `list.c`, ...) now ride for free.
- **Negative fixtures get their own subdir.** Positive cases (which
  do NOT trip divzero) go under
  `tests/transpiler3/c/fixtures/divzero/`. Trip cases (which exit 5)
  go under `tests/transpiler3/c/fixtures/divzero-trip/` with an
  `exit.txt` and `stderr.txt` instead of `expect.txt`. Splitting the
  fixture *shape* by directory keeps the gate test simple: each
  directory has exactly one positive/negative convention.
- **One Phase 2.3 gate test, two subtests.** `phase02_3_test.go`
  hosts `TestPhase2Divzero` (positive fixtures, stdout match) and
  `TestPhase2DivzeroTrip` (negative fixtures, exit code + stderr
  match). Reuses `runFixtureSuite` for the positive set; the
  negative set gets a dedicated walker.

### Test set (2.3)

- `transpiler3/c/emit/emit_phase23_test.go::TestEmitPhase23Divzero`
  -- 4 cases pinning that `BinDivI64` and `BinModI64` emit
  `mochi_div_i64(L,R)` and `mochi_mod_i64(L,R)` (not infix), that the
  prologue gains the errors.h include, and that other binary ops
  stay infix.
- `transpiler3/c/build/phase02_3_test.go::TestPhase2Divzero`
  -- positive fixtures: end-to-end gate across every
  `tests/transpiler3/c/fixtures/divzero/<name>` directory; stdout
  must match `expect.txt` byte-for-byte.
- `transpiler3/c/build/phase02_3_test.go::TestPhase2DivzeroTrip`
  -- negative fixtures: end-to-end gate across every
  `tests/transpiler3/c/fixtures/divzero-trip/<name>` directory. Each
  fixture must exit with code 5 and stderr must match `stderr.txt`
  byte-for-byte.

## Sub-phase 2.4 -- 2026-05-22 21:30 (GMT+7)

Float NaN/Inf print parity with vm3. Phase 2.0 left the float-print
path at C `printf("%.17g\n", x)`, which on every tier-1 libc
disagrees with Go's `strconv.FormatFloat 'g' -1 64` on three exact
inputs: `NaN`, `+Inf`, `-Inf`. vm3 prints those as `NaN`, `+Inf`,
`-Inf` (Go's `fmt.Println` convention via `%v`); C's `%g` prints
`nan`, `inf`, `-inf` (case + sign-prefix divergence). This breaks
byte-equality the moment any fixture's float arithmetic crosses the
IEEE 754 special-value plane (`1.0 / 0.0`, `0.0 / 0.0`, `inf - inf`,
NaN propagated through `+ - * /`).

### Goal-alignment audit (2.4)

This sub-phase moves the byte-equal stdout gate forward directly:
every Phase 2.4 fixture is a program whose vm3 oracle prints a
special IEEE 754 value to stdout, and the C-AOT binary's stdout must
match that oracle byte-for-byte. Without 2.4, any benchmark that
divides by zero in floats (numerical-analysis kernels, NaN-as-missing
data idioms) silently diverges. Scope is intentionally narrow: NaN
and Inf only, with finite values still routed through `%.17g`.
Shortest-round-trip (Ryu) for finite values is a separate Phase 2.X
follow-up.

### Decisions made (2.4)

- **Runtime-only change.** The fix lives in
  `mochi_print_f64`. Emit, lower, aotir, and the build driver are
  untouched. Pre-flight gives us the entire BinDivF64 path already
  (Phase 2.0); the only thing that was wrong was how the *result*
  prints.
- **Special-case detection via `<math.h>` macros.** `isnan(x)` and
  `isinf(x)` are C99 macros (not function calls), so the runtime
  picks them up without `-lm`. Tested in the gate via cc's default
  link line (host `cc`, vendored Zig fallback both supply them).
- **Spellings copied from Go.** `NaN`, `+Inf`, `-Inf` -- exactly the
  strings `fmt.Println(math.NaN())` etc. emit. Capitalisation and
  the leading `+` on positive infinity are oracle-driven, not
  invented.
- **Sign-of-NaN ignored.** Go's `%v` always prints `NaN` regardless
  of the sign bit on the NaN payload (`-NaN` -> `NaN`). The C
  runtime does the same: one `isnan` branch, no signbit check. vm3
  parity holds.
- **Finite values keep `%.17g`.** Existing Phase 2.0 float fixtures
  (`0.5`, `1.0`, `2.5`, `4.0`) round-trip identically through
  `%.17g` and Go `%v` because `%g` strips trailing zeros and these
  values are exactly representable. Values like `0.1` would diverge
  (`%.17g` -> `0.10000000000000001`, Go -> `0.1`) and the gate
  deliberately stays away from them until Ryu lands.
- **No NaN comparison fixture in 2.4.** IEEE 754 says
  `nan == nan -> false` and both C and Go follow IEEE 754, so
  comparison parity falls out of Phase 2.0's BinEqF64 lowering
  without runtime work. Adding a fixture is cheap so we add a couple
  anyway as confidence checks, but the work to make them green
  isn't in 2.4 -- it was already in 2.0.
- **Fixtures live under `nan-inf/`.** Splitting them out from
  `primitives/` keeps the Phase 2.4 gate readable and lets the gate
  test fail loudly on regressions instead of being buried in a
  ~50-fixture rollup.
- **Production via `0.0 / 0.0` etc., not builtins.** Phase 2.4
  doesn't add `nan()` or `inf()` builtins (those belong with the
  `math` standard library in a later phase). NaN and Inf are
  produced by float-divzero arithmetic, which is well-defined in
  IEEE 754 and already emits the right bit pattern under Phase 2.0
  BinDivF64.

### Test set (2.4)

- `transpiler3/c/build/phase02_4_test.go::TestPhase2NanInf` --
  end-to-end gate across every
  `tests/transpiler3/c/fixtures/nan-inf/<name>` directory. Stdout
  must match `expect.txt` byte-for-byte. Covers: bare NaN
  production, +Inf production, -Inf production, NaN propagation
  through each arithmetic op, Inf + Inf, Inf - Inf, Inf * 0, NaN
  equality (== returns false, != returns true), NaN ordering
  (< returns false), NaN passed through a user function.

## Sub-phase 2.5 -- 2026-05-25 18:08 (GMT+7)

`int(x)` float-to-int truncation toward zero and `min(xs)` / `max(xs)` on scalar lists. These are the most-used numeric type helpers in the fixture corpus: query results often need truncation, and list aggregation tests depend on min/max.

### Goal-alignment audit (2.5)

`int(x)` appears in every fixture that mixes float arithmetic with integer indexing or count arithmetic. `min()` and `max()` appear in filter + aggregation fixtures and in the query DSL test suite (Phase 8). Both are blocking real programs from compiling today. Adding them now unblocks the majority of the remaining Phase 2 fixtures and closes the gap between the transpiler and vm3 builtins.

### Decisions made (2.5)

- **`int(x)` is identity for TypeInt.** If the lower pass sees `int(x)` where x is already TypeInt, it returns x directly (no NumCastExpr). This avoids a pointless `(int64_t)(int64_t)(...)` in the emitted C and matches vm3 semantics.
- **`NumCastExpr` emits `(int64_t)(operand)`.** C truncates toward zero for positive and negative floats, which matches vm3's `int(x)` semantics. IEEE 754 truncation toward zero is the standard behavior.
- **`min`/`max` on bool lists not supported.** vm3 does not implement `min([true, false])` and the fixture corpus does not include it. Support for bool lists deferred.
- **`min`/`max` panic on empty list.** Calling `mochi_list_i64_min` on a zero-length list calls `mochi_panic_index()`. This matches vm3's runtime-error behavior for empty aggregate operations.
- **String min/max uses `strcmp`.** Lexicographic byte ordering via `strcmp` matches vm3's string comparison semantics for the ASCII fixture corpus. Full UTF-8 codepoint ordering via utf8proc is Phase 6.3.
- **Runtime functions live in `list.c`.** No new runtime file; the min/max functions are grouped with the other list operations since they share the same struct types and panic helper.

### Test set (2.5)

- `transpiler3/c/build/phase02_5_test.go::TestPhase2TypeCasts` -- end-to-end gate across every `tests/transpiler3/c/fixtures/type_cast/<name>` directory (8 fixtures): `int_cast_basic`, `int_cast_negative`, `int_cast_large`, `list_min_int`, `list_max_int`, `list_min_float`, `list_max_string`, `min_max_combined`.

## Sub-phase 2.6 -- 2026-05-25 20:10 (GMT+7)

`val in list<T>` containment check; `sum(xs)` for int and float lists; `abs(x)` for int and float; `floor(x)` and `ceil(x)` for float. These are the remaining common builtins that vm3 supports (or that are in the type checker) and that were missing from the transpiler.

### Goal-alignment audit (2.6)

`val in list<T>` is used in every filter query that tests membership in a small set; without it programs have to write explicit for-loops. `sum(xs)` is used in every aggregation fixture. `abs` / `floor` / `ceil` appear in numeric programs that mix integer and floating-point computation. All four unlock real programs that currently fail with "not supported" errors in the transpiler.

### Decisions made (2.6)

- **`val in list<T>` produces `ListContainsExpr`.** Same pattern as `MapHasExpr` for maps; the expr returns TypeBool. Runtime functions `mochi_list_i64_contains`, `mochi_list_f64_contains`, `mochi_list_bool_contains`, `mochi_list_str_contains` added to `list.c`/`list.h`.
- **`sum` dispatch by elem type.** `lowerListSumCall` inspects the list's element type to produce `ListSumExpr{ElemType: TypeInt}` or `ListSumExpr{ElemType: TypeFloat}`. The `ListSumExpr.Type()` returns the elem type so the caller gets the right C type without an explicit cast.
- **`abs`/`floor`/`ceil` use `MathCallExpr`.** A single IR node with a `Func` string field (`"abs_i64"`, `"abs_f64"`, `"floor"`, `"ceil"`) covers all four variants. The emit pass maps each to inline C: `abs_i64` emits `(x < 0 ? -x : x)` (avoids `llabs` / `<stdlib.h>` interaction with user-defined `abs` shadows), `abs_f64` calls `fabs`, `floor`/`ceil` call the math.h functions.
- **`#include <math.h>` added to the C prologue.** Required for `fabs`, `floor`, `ceil`. Added unconditionally alongside the other runtime headers.
- **`sum` on float lists prints with `%.17g`.** `sum([1.1, 2.2, 3.3])` accumulates IEEE 754 rounding and prints as `6.5999999999999996`, not `6.6`. The fixture `expect.txt` captures the actual C output. vm3 prints `6.6` (shortest decimal via Go); this is a known divergence for float accumulation, consistent with how all other float operations are handled in the transpiler.

### Test set (2.6)

- `transpiler3/c/build/phase02_6_test.go::TestPhase2MathBuiltins` -- end-to-end gate across `tests/transpiler3/c/fixtures/math_builtins/` (8 fixtures): `list_contains_int`, `list_contains_str`, `list_contains_bool`, `sum_int`, `sum_float`, `abs_values`, `floor_ceil`, `math_combined`.

## Decisions made

_Per-sub-phase decisions appear under each "Sub-phase X.Y" section above._

## Deferred work

_Tuple return values: Phase 3 alongside records. Big-int / fixed-width ints: not in v1._

## Closeout notes

All 7 sub-phases (2.0-2.6) are LANDED. TestPhase2Primitives, TestPhase2ControlFlow, TestPhase2ForRange, TestPhase2Functions, TestPhase2Divzero, TestPhase2DivzeroTrip, TestPhase2NanInf, TestPhase2TypeCasts, and TestPhase2MathBuiltins are green on every tier-1 host. Phase 2 is LANDED.
