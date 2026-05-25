---
title: "Codegen design: aotir to TypeScript AST via Mochi-side syntax tree printer, monomorphisation, closure conversion, match-to-switch-tag, source maps"
description: "How transpiler3/typescript/lower/ walks the aotir IR, builds a TypeScript syntax tree on the Mochi side (no tsc dependency at build time), pretty-prints it, runs prettier 3.x then tsc --noEmit, emits .ts.map source maps, lowers Mochi match expressions to switch on a literal kind discriminator with _exhaustive: never tails, performs monomorphisation per integer producer, and closure-converts to plain TS arrow functions plus an environment record."
sidebar_position: 5
---

# Codegen design for MEP-52 (Mochi to TypeScript)

**Author**: Mochi compiler team, internal note.
**Date**: 2026-05-23 17:05 (GMT+7).
**Method**: lift the structure of [[mep-0051]] note 05, adapt to the TypeScript target. Cross-reference [[04-runtime]] for the runtime library shape and [[06-type-lowering]] for per-Mochi-type lowering rules. The aotir IR is shared with MEP-45 through MEP-51; only the lower and emit passes change here.

This note specifies the **codegen pipeline** that turns a fully-checked aotir program (the same IR MEP-45 lowers to C, MEP-50 lowers to Kotlin, MEP-51 lowers to Python) into a directory of `.ts` files that pass `tsc --strict --noUncheckedIndexedAccess --exactOptionalPropertyTypes --noEmit` and run byte-equal to vm3 on Node 22, Deno 2, Bun 1.1, and a Playwright-driven browser.

The pipeline has **eleven phases**. Each phase is a separate Go package under `transpiler3/typescript/` so unit tests can target it in isolation.

1. **A**. `aotir.Program` -> Mochi-side type pre-pass (monomorphisation seeds, escape-analysis seeds, capture-set computation).
2. **B**. Type lowering pass: every aotir type gets a deterministic TypeScript type spelling (see [[06-type-lowering]]).
3. **C**. Closure conversion: every Mochi lambda that captures one or more outer locals becomes an arrow function plus a `const env_<n> = { ... }` record.
4. **D**. Match-to-switch-tag lowering: every Mochi match-expression becomes a `switch (x.kind)` over a literal discriminator, with a `default` branch that does `const _exhaustive: never = x;`.
5. **E**. Monomorphisation: each integer-producer site is tagged `bigint` or `number`; each generic function is duplicated per concrete instantiation when type inference cannot prove a single instantiation suffices.
6. **F**. Mochi-side TypeScript syntax tree construction: walk the closure-converted, match-lowered, monomorphised aotir; build `ts.File`, `ts.ImportDecl`, `ts.FuncDecl`, `ts.ClassDecl`, `ts.SwitchStmt`, etc. as plain Go structs.
7. **G**. Pretty-printer: walk the `ts.File`, emit a `[]byte` in canonical TypeScript syntax. Indent two spaces, line width 100, trailing commas in multi-line array/object literals.
8. **H**. `prettier --write` post-format: run the official prettier 3.x CLI over the emitted file. Diff against the pretty-printer output. If the diff is non-empty the pretty-printer has a bug; CI gate fails.
9. **I**. `tsc --noEmit`: typecheck the emitted file with the exact compiler options the runtime ships with. Zero diagnostics required.
10. **J**. Source map emission: `.ts` files carry an inline `//# sourceMappingURL=foo.ts.map` plus a sibling `foo.ts.map` file. Mochi source positions thread through every aotir node so the source map can be built without re-parsing.
11. **K**. Build manifest: produce a `mochi.lock.json` recording the SHA256 of every emitted `.ts`, the prettier version, the tsc version, the Mochi compiler version, and the input `.mochi` file SHA256. Reproducibility is gated against this file across two CI hosts.

The rest of this note works through each phase in detail.

## 1. Aotir IR refresher

The aotir IR is the same monomorphic, fully-typed, A-normal-form IR every MEP-45+ backend consumes. Key node kinds:

- `aotir.Module` (top-level): list of `aotir.Decl`.
- `aotir.Decl`: `FuncDecl`, `ConstDecl`, `TypeDecl`, `ImportDecl`, `AgentDecl`, `StreamDecl`.
- `aotir.Stmt`: `Let`, `Assign`, `If`, `Switch`, `While`, `For`, `Return`, `Break`, `Continue`, `Expr` (statement-expression), `Block`.
- `aotir.Expr`: `Lit`, `Var`, `Call`, `Match`, `Unary`, `Binary`, `Index`, `Field`, `Record`, `List`, `Map`, `Set`, `Lambda`, `Cast`, `Coerce`, `Try`.
- `aotir.Type`: `Int`, `Float`, `Bool`, `String`, `Bytes`, `List<T>`, `Map<K, V>`, `Set<T>`, `Record{...}`, `Sum{...}`, `Option<T>`, `Result<T, E>`, `Func{...}`, `Agent{...}`, `Stream<T>`.

Each node carries:

- `Pos` (source position: file, byte offset, line, column).
- `Type` (always set after the type checker runs).
- `Effect` (pure / io / async / blocking).
- `Capture` (for `Lambda`: which outer locals are captured).

The TypeScript backend reads aotir as a Go struct tree, never re-parses Mochi source. The Mochi parser ran in phase 0 (front-end) and stamped every `Pos`.

## 2. Phase A: pre-pass

The pre-pass is read-only over aotir. It walks the program twice:

1. **Walk 1 (escape analysis)**: every `Lambda` node records which locals from enclosing functions it captures. Result attached as `Lambda.Capture: []LocalID`.
2. **Walk 2 (integer fit analysis)**: every `aotir.Int` producer (literal, arithmetic op, function call returning `int`, etc.) records whether its value can be statically proven to fit in `[-(2^53-1), 2^53-1]`. The proof is by interval analysis: each `Int` node carries a range `[lo, hi]` propagated through `+ - * / %`, clamped at the IR-declared type bounds. If both `lo` and `hi` fit, the producer is tagged `IntFit = .Number`; else `IntFit = .BigInt`.

The pre-pass output is a sidecar `analysisMap` keyed by `aotir.NodeID`. It does **not** mutate aotir.

```go
// transpiler3/typescript/lower/prepass.go (sketch)
type Analysis struct {
    Captures map[aotir.NodeID][]aotir.LocalID
    IntFit   map[aotir.NodeID]IntFitTag
}

type IntFitTag uint8

const (
    IntFitBigInt IntFitTag = iota // default: must be bigint
    IntFitNumber                  // proven to fit in i53
)

func PrePass(p *aotir.Program) *Analysis { /* ... */ }
```

The pre-pass also pre-allocates the local-name pool: every aotir local gets a guaranteed-unique TypeScript identifier `v_<aotir.LocalID>`. The codegen does not auto-rename later because the source map must reference these names by position.

## 3. Phase B: type lowering

Type lowering is a pure function from `aotir.Type` to a Mochi-side TypeScript-AST `ts.Type` node. See [[06-type-lowering]] for the full table; here we list only the entry-point logic.

```go
// transpiler3/typescript/lower/type.go
func lowerType(t aotir.Type, ctx *Ctx) ts.Type {
    switch t := t.(type) {
    case aotir.Int:
        if ctx.IntFit[t.NodeID] == IntFitNumber {
            return ts.Number{}
        }
        return ts.BigInt{}
    case aotir.Float:
        return ts.Number{}
    case aotir.Bool:
        return ts.Boolean{}
    case aotir.String:
        return ts.String{}
    case aotir.Bytes:
        return ts.Named{Name: "Uint8Array"}
    case aotir.List:
        elem := lowerType(t.Elem, ctx)
        if t.Mutated {
            return ts.Array{Elem: elem}
        }
        return ts.ReadonlyArray{Elem: elem}
    case aotir.Map:
        return ts.Generic{Name: "Map", Args: []ts.Type{
            lowerType(t.Key, ctx),
            lowerType(t.Val, ctx),
        }}
    case aotir.Set:
        return ts.Generic{Name: "Set", Args: []ts.Type{lowerType(t.Elem, ctx)}}
    case aotir.Record:
        return ts.Named{Name: ctx.RecordName(t)}
    case aotir.Sum:
        return ts.Named{Name: ctx.SumName(t)}
    case aotir.Option:
        return ts.Union{Members: []ts.Type{
            lowerType(t.Inner, ctx),
            ts.Null{},
        }}
    case aotir.Result:
        return ts.Generic{Name: "MochiResult", Args: []ts.Type{
            lowerType(t.Ok, ctx),
            lowerType(t.Err, ctx),
        }}
    case aotir.Func:
        params := make([]ts.Param, len(t.Params))
        for i, p := range t.Params {
            params[i] = ts.Param{Name: fmt.Sprintf("a%d", i), Type: lowerType(p, ctx)}
        }
        ret := lowerType(t.Ret, ctx)
        if t.Async {
            ret = ts.Generic{Name: "Promise", Args: []ts.Type{ret}}
        }
        return ts.Arrow{Params: params, Ret: ret}
    case aotir.Agent:
        return ts.Named{Name: ctx.AgentName(t)}
    case aotir.Stream:
        return ts.Generic{Name: "AsyncIterable", Args: []ts.Type{lowerType(t.Elem, ctx)}}
    }
    panic("unknown aotir type")
}
```

The `ctx.RecordName` / `ctx.SumName` / `ctx.AgentName` helpers are deterministic: for nominal types they reuse the Mochi declaration name (mangled if it collides with a JS reserved word, see §17); for anonymous tuples they synthesise `Tuple_<sha8>`.

Lists are **invariant** by default in TypeScript (`T[]`); the `readonly T[]` form is **covariant**. The escape analysis from phase A also tracks "mutated" / "not-mutated" per list value; the type lowering picks `readonly` when not mutated. The runtime data is the same JS array; the difference is purely in the type spelling.

## 4. Phase C: closure conversion

Mochi lambdas can capture outer locals freely. JavaScript closures do too, but TypeScript inference does not know which captures are mutable and which are not, and a lambda that captures a `let`-bound counter will not narrow `T | null` correctly inside the lambda body.

Mochi closure-conversion turns

```mochi
fun makeAdder(n: int) -> (int) -> int {
  return |x| x + n
}
```

into

```typescript
function makeAdder(n: bigint): (x: bigint) => bigint {
  const env = { n };
  return (x: bigint): bigint => x + env.n;
}
```

That is, the captured locals become fields of a `const env = { ... }` record, and the lambda body references `env.<name>` instead of `<name>` directly. Two reasons:

1. **Source-map clarity**: the captured-variable record is named after its enclosing function so debuggers can show it as a "scope" frame.
2. **Mutation semantics**: when a closure captures a Mochi `var` (mutable) the env record uses `let`-bound fields by reference indirection: the env record holds a `box: { value: T }` for that field, and writes go through `env.box.value = ...`. TS narrowing then never sees the captured slot as a plain local and avoids the unsound "narrowed to non-null then re-read inside async callback" trap.

Closure conversion is **not** required for lambdas with zero captures; those become plain arrow functions with no env record.

```go
// transpiler3/typescript/lower/closure.go
func closureConvert(fn *aotir.FuncDecl, an *Analysis) *aotir.FuncDecl {
    // walk fn.Body, find every Lambda with non-empty Capture
    // for each: synthesise an EnvRecord, rewrite the body to read env.<name>
    // hoist the EnvRecord declaration just before the lambda
    // ...
}
```

The Mochi convention is that `var`-captured fields go through a box wrapper, and `let`-captured fields are copied by value into the env record at closure-creation time. This matches MEP-50 (Kotlin) and MEP-51 (Python).

### 4.1 Why not just use TS native closures?

We could emit

```typescript
function makeAdder(n: bigint): (x: bigint) => bigint {
  return (x: bigint): bigint => x + n;
}
```

and TS handles the capture natively. We pay a small cost (the env-record indirection) to get:

- **Predictable mutation**: `let` rebinding in the parent does **not** affect the captured value, matching Mochi semantics where `let` is single-assignment and `var` is explicit.
- **Stack-frame transparency**: the env record makes the captured set visible in `JSON.stringify(env)` for debug-print purposes.
- **Source-map names**: the env's field names match the Mochi source identifiers, even after mangling at the outer scope.

For zero-capture lambdas there is no overhead; for non-zero-capture the cost is one allocation per closure creation, which is negligible compared to JS engine closure-overhead anyway.

## 5. Phase D: match-to-switch-tag

Mochi `match` over a sum type:

```mochi
type Shape = Circle{r: float} | Square{side: float} | Triangle{a: float, b: float, c: float}

fun area(s: Shape) -> float {
  match s {
    Circle{r}      => 3.14159 * r * r,
    Square{side}   => side * side,
    Triangle{a, b, c} => {
      let s_ = (a + b + c) / 2
      sqrt(s_ * (s_ - a) * (s_ - b) * (s_ - c))
    }
  }
}
```

lowers to:

```typescript
type Shape =
  | { kind: "Circle"; r: number }
  | { kind: "Square"; side: number }
  | { kind: "Triangle"; a: number; b: number; c: number };

function area(s: Shape): number {
  switch (s.kind) {
    case "Circle": {
      const r = s.r;
      return 3.14159 * r * r;
    }
    case "Square": {
      const side = s.side;
      return side * side;
    }
    case "Triangle": {
      const a = s.a;
      const b = s.b;
      const c = s.c;
      const s_ = (a + b + c) / 2;
      return Math.sqrt(s_ * (s_ - a) * (s_ - b) * (s_ - c));
    }
    default: {
      const _exhaustive: never = s;
      throw new Error("non-exhaustive match: " + JSON.stringify(_exhaustive));
    }
  }
}
```

Two notes:

1. **Discriminator field**: every Mochi sum type variant gets a `kind: "<VariantName>"` field at lowering time. The literal type is the variant name verbatim. TS's exhaustiveness checker uses this to prove the `default` is unreachable.
2. **`_exhaustive: never` tail**: if a new variant is added later and a `match` is not updated, `tsc` flags the assignment `const _exhaustive: never = s;` because `s`'s type at that point would no longer narrow to `never`. This is the canonical TS exhaustiveness idiom.

### 5.1 Match with guards

Mochi allows guarded match arms:

```mochi
match x {
  n if n > 0 => "positive",
  n if n < 0 => "negative",
  _ => "zero"
}
```

Guards do not map cleanly to `switch` since `case` labels must be constant. We lower this to an `if`/`else if` ladder:

```typescript
let result: string;
if (true) {
  if (x > 0n) {
    result = "positive";
  } else if (x < 0n) {
    result = "negative";
  } else {
    result = "zero";
  }
}
```

The outer `if (true)` block ensures every emitted arm is in a fresh scope so let-bindings inside one arm cannot leak to the next. The Mochi `_` (wildcard) is the final `else`.

### 5.2 Match on tuples and nested records

A Mochi match like

```mochi
match (a, b) {
  (Some(x), Some(y)) => x + y,
  (None, _) => 0,
  (_, None) => 0
}
```

lowers to a nested switch ladder:

```typescript
let result: bigint;
if (a !== null && b !== null) {
  const x = a;
  const y = b;
  result = x + y;
} else if (a === null) {
  result = 0n;
} else {
  result = 0n;
}
```

The compiler chooses between switch and if-ladder based on whether all arms can be expressed as constant case labels.

### 5.3 Match as expression vs statement

Mochi `match` is an expression. TypeScript `switch` is a statement. We bridge by emitting a `let result: T;` declaration above the switch and assigning in each branch. Where the surrounding context can accept an IIFE we instead emit

```typescript
const r: T = (() => {
  switch (x.kind) {
    case "A": return ...;
    case "B": return ...;
    default: { const _: never = x; throw new Error("..."); }
  }
})();
```

The choice between let-result-assign and IIFE is driven by readability heuristics: at top level in a function body we prefer let-result-assign; inside a complex expression we prefer the IIFE.

## 6. Phase E: monomorphisation

The aotir IR is already monomorphic at the type level (generics are instantiated during type-checking). However, the **integer-representation** dimension is still polymorphic at IR time. Each integer producer can be `bigint` or `number`, and the choice is per-node, driven by the IntFit tag from phase A.

Two cases:

1. **Single instantiation suffices**: all consumers of a producer agree on the representation. Just lower the producer to the chosen TS type. No code duplication.
2. **Multiple instantiations needed**: a producer flows to consumers with different representations. Mochi cannot mix `bigint` and `number` in arithmetic, so the IR is **already invalid** if this happens, and the type checker has rejected it. The monomorphisation pass is a check, not a duplication: it asserts that the IntFit tag is consistent across the producer-consumer chain.

Function-level monomorphisation is needed when a generic function is instantiated at both `int` (-> bigint) and `int` (-> number) representations from different call sites. The IR represents these as two distinct `FuncDecl` nodes (post-type-checking) so no duplication is needed at codegen time; we just emit both.

### 6.1 Number-bigint conversion sites

Coercions between `bigint` and `number` are explicit in Mochi (via `as i64` / `as f64` etc.). They lower to:

```typescript
// bigint -> number (lossy if out of i53 range)
const m: number = Number(b); // Mochi: `b as i32` after fit-check

// number -> bigint
const b: bigint = BigInt(Math.trunc(m)); // Mochi: `m as i64`
```

The cast is a no-op at runtime for the common case where the IntFit pre-pass picked the same representation on both sides; the emitter elides it.

## 7. Phase F: Mochi-side TS syntax tree

We do **not** depend on the TypeScript Compiler API. Pulling `tsc` into the Go build chain would require Node at compile time, which is unacceptable for the standalone Mochi compiler binary. Instead, we ship a tiny Go package `transpiler3/typescript/ts/` that defines the TypeScript AST as plain Go structs.

```go
// transpiler3/typescript/ts/nodes.go
package ts

type File struct {
    Path    string
    Imports []ImportDecl
    Decls   []Decl
}

type ImportDecl struct {
    Specifier string   // e.g. "@mochi/runtime/collections"
    Items     []string // named imports
    TypeOnly  bool
}

type Decl interface{ declNode() }

type FuncDecl struct {
    Name    string
    Params  []Param
    Ret     Type
    Body    []Stmt
    Async   bool
    Exported bool
    JSDoc   string
}
func (*FuncDecl) declNode() {}

type ClassDecl struct {
    Name    string
    Fields  []Field
    Methods []FuncDecl
    Ctor    *FuncDecl
    Exported bool
    JSDoc   string
}
func (*ClassDecl) declNode() {}

type TypeAlias struct {
    Name    string
    Generic []string // type parameters
    Body    Type
    Exported bool
    JSDoc   string
}
func (*TypeAlias) declNode() {}

type Stmt interface{ stmtNode() }
type LetStmt struct { Name string; Type Type; Init Expr; Const bool }
type IfStmt struct { Cond Expr; Then []Stmt; Else []Stmt }
type SwitchStmt struct { Expr Expr; Cases []SwitchCase; Default []Stmt }
type SwitchCase struct { Label Expr; Body []Stmt }
type ReturnStmt struct { Value Expr }
type ExprStmt struct { Expr Expr }
type ForStmt struct { Init, Cond, Post Stmt; Body []Stmt }
type ForOfStmt struct { LHS string; Iter Expr; Body []Stmt; Await bool }
type ThrowStmt struct { Value Expr }
type TryStmt struct { Body []Stmt; CatchVar string; Catch []Stmt; Finally []Stmt }
func (*LetStmt) stmtNode() {}
// ... etc

type Expr interface{ exprNode() }
type LitExpr struct { Kind string; Value any } // "string","number","bigint","boolean","null","undefined"
type VarExpr struct { Name string }
type CallExpr struct { Callee Expr; Args []Expr; TypeArgs []Type }
type BinaryExpr struct { Op string; L, R Expr }
type UnaryExpr struct { Op string; Operand Expr; Prefix bool }
type IndexExpr struct { Obj Expr; Index Expr }
type FieldExpr struct { Obj Expr; Name string }
type ObjectExpr struct { Fields []ObjectField }
type ArrayExpr struct { Items []Expr }
type ArrowExpr struct { Params []Param; Body []Stmt; Ret Type; Async bool }
type NewExpr struct { Class string; Args []Expr; TypeArgs []Type }
type AsExpr struct { Inner Expr; Type Type }
type AwaitExpr struct { Inner Expr }
type YieldExpr struct { Inner Expr; Delegate bool }
type TemplateExpr struct { Strings []string; Exprs []Expr }
func (*LitExpr) exprNode() {}
// ... etc

type Type interface{ typeNode() }
type Named struct { Name string }
type Generic struct { Name string; Args []Type }
type Array struct { Elem Type }
type ReadonlyArray struct { Elem Type }
type Union struct { Members []Type }
type Intersection struct { Members []Type }
type Arrow struct { Params []Param; Ret Type }
type ObjectType struct { Fields []ObjectTypeField }
type Literal struct { Kind string; Value any } // "string","number","boolean"
type Number struct{}; type String struct{}; type Boolean struct{}; type BigInt struct{}
type Null struct{}; type Undefined struct{}; type Void struct{}; type Never struct{}; type Unknown struct{}; type Any struct{}
func (Number) typeNode() {}
// ... etc

type Param struct { Name string; Type Type; Default Expr; Optional bool }
type Field struct { Name string; Type Type; Readonly bool; Static bool; Private bool; Init Expr }
type ObjectField struct { Key string; Value Expr; Computed bool }
type ObjectTypeField struct { Key string; Type Type; Readonly bool; Optional bool }
```

This package is **read-only at codegen time**: the lower pass builds an immutable tree; the printer walks it. There is no AST mutation API by design (every node is constructed once).

### 7.1 Position threading

Every `ts.Stmt` and `ts.Expr` carries an embedded `Pos` field referencing the originating Mochi source byte offset. The pretty-printer threads positions into the source map sidecar.

```go
type Pos struct {
    File   string
    Offset int
    Line   int
    Col    int
}

// Every node embeds Pos:
type LetStmt struct {
    Pos
    Name string
    // ...
}
```

The lower pass copies the Pos from the originating aotir node.

## 8. Phase G: pretty-printer

The printer walks the `ts.File` and emits bytes. Its rules:

- Two-space indent.
- Line width 100; long expressions wrap on operator boundaries.
- Object literals: `{ k: v, ... }` on one line if it fits, else
  ```
  {
    k: v,
    ...
  }
  ```
- Function params: same rule as object literals.
- Trailing commas on multi-line array, object, param, and import lists.
- Strings: double-quote by default; switch to backtick if the string contains a `"` or a `${`.
- BigInt literals: `42n`.
- Imports sorted: first standard runtimes (`@mochi/runtime/*` alphabetical), then relative imports (alphabetical).
- JSDoc above declarations.

Example pretty-printer output for the `area` function above:

```typescript
import { Mochi } from "@mochi/runtime";

export type Shape =
  | { kind: "Circle"; r: number }
  | { kind: "Square"; side: number }
  | { kind: "Triangle"; a: number; b: number; c: number };

/** Compute area of a shape. */
export function area(s: Shape): number {
  switch (s.kind) {
    case "Circle": {
      const r = s.r;
      return 3.14159 * r * r;
    }
    case "Square": {
      const side = s.side;
      return side * side;
    }
    case "Triangle": {
      const a = s.a;
      const b = s.b;
      const c = s.c;
      const s_ = (a + b + c) / 2;
      return Math.sqrt(s_ * (s_ - a) * (s_ - b) * (s_ - c));
    }
    default: {
      const _exhaustive: never = s;
      throw new Error("non-exhaustive match: " + JSON.stringify(_exhaustive));
    }
  }
}
```

The printer is **idempotent**: feeding its own output back into prettier 3.x (phase H) produces zero diff.

### 8.1 Printer implementation

The printer is a recursive `Visit` over the `ts.File`. Each method writes to a `*bytes.Buffer`. Indentation is tracked by an `int` field; line-width by tracking the current column.

```go
// transpiler3/typescript/print/printer.go
type Printer struct {
    buf    bytes.Buffer
    indent int
    col    int
    sm     *SourceMapBuilder // phase J
}

func (p *Printer) writeStr(s string) {
    p.buf.WriteString(s)
    p.col += len(s)
}

func (p *Printer) newline() {
    p.buf.WriteByte('\n')
    for i := 0; i < p.indent*2; i++ {
        p.buf.WriteByte(' ')
    }
    p.col = p.indent * 2
}

func (p *Printer) printStmt(s ts.Stmt) {
    p.sm.MarkOutput(p.col, p.buf.Len())
    p.sm.MarkInput(s.SrcPos())
    switch s := s.(type) {
    case *ts.LetStmt:
        if s.Const { p.writeStr("const ") } else { p.writeStr("let ") }
        p.writeStr(s.Name)
        if s.Type != nil {
            p.writeStr(": ")
            p.printType(s.Type)
        }
        if s.Init != nil {
            p.writeStr(" = ")
            p.printExpr(s.Init)
        }
        p.writeStr(";")
    case *ts.SwitchStmt:
        // ...
    }
}
```

The printer is single-file and exhaustively unit-tested: every node kind has at least three test cases covering single-line, multi-line, and edge-of-line-width formats.

### 8.2 Line-wrapping heuristics

When an object literal `{ k1: v1, k2: v2, ... }` does not fit on the current line:

1. Start a new line, increase indent.
2. Each field on its own line followed by a comma.
3. Close brace on a new line at the original indent.

When a function call has too many arguments:

1. Open paren on the same line.
2. Each argument on its own line, increase indent.
3. Close paren on a new line at the original indent.

When a long binary expression `a + b + c + ...` does not fit:

1. Break at the lowest-precedence operator.
2. Each operand on its own line, operator at the start of the continuation line.

These heuristics mirror prettier 3.x defaults so the post-format diff is empty.

## 9. Phase H: prettier post-format

After phase G writes a file `foo.ts`, the codegen invokes:

```bash
npx prettier@3.4 --write foo.ts
```

(or, in CI, `pnpm dlx prettier@3.4 --write foo.ts`.) The output should be byte-identical to phase G's output. If `diff` is non-empty, the printer has a bug. CI fails.

Why bother with prettier if the printer is already a fixed point? Two reasons:

1. **Defence in depth**: our printer might have a bug we have not unit-tested. Prettier catches it.
2. **Format upgrades**: when prettier 3.5 lands with a new default rule, we can re-run prettier without re-running the printer; if the diff is non-empty we update the printer rules to match.

The prettier config is checked into `tooling/prettier.config.cjs`:

```javascript
// tooling/prettier.config.cjs
module.exports = {
  printWidth: 100,
  tabWidth: 2,
  useTabs: false,
  semi: true,
  singleQuote: false,
  quoteProps: "as-needed",
  trailingComma: "all",
  bracketSpacing: true,
  arrowParens: "always",
  endOfLine: "lf",
  embeddedLanguageFormatting: "off",
};
```

This file ships in the Mochi distribution at `templates/typescript/prettier.config.cjs` and is copied into every generated project.

## 10. Phase I: tsc --noEmit gate

After prettier, the codegen runs:

```bash
tsc --noEmit --project tsconfig.base.json
```

with the base config:

```json
{
  "compilerOptions": {
    "target": "ES2024",
    "module": "ESNext",
    "moduleResolution": "Bundler",
    "lib": ["ES2024", "DOM"],
    "strict": true,
    "noUncheckedIndexedAccess": true,
    "exactOptionalPropertyTypes": true,
    "noImplicitOverride": true,
    "noFallthroughCasesInSwitch": true,
    "noPropertyAccessFromIndexSignature": true,
    "noUncheckedSideEffectImports": true,
    "rewriteRelativeImportExtensions": true,
    "isolatedModules": true,
    "verbatimModuleSyntax": true,
    "esModuleInterop": false,
    "allowSyntheticDefaultImports": false,
    "skipLibCheck": false,
    "resolveJsonModule": true,
    "declaration": true,
    "declarationMap": true,
    "sourceMap": true,
    "removeComments": false
  }
}
```

Zero diagnostics required. Any diagnostic kills the build. The phase G printer is designed so this never happens for valid aotir input.

### 10.1 Why so many strict flags

Each flag closes a hole that would otherwise let unsound TypeScript escape into the runtime:

- `strict`: enables all the base strict-mode flags (`strictNullChecks`, `strictFunctionTypes`, etc.).
- `noUncheckedIndexedAccess`: `arr[i]` has type `T | undefined` (matches Mochi bounds-check semantics).
- `exactOptionalPropertyTypes`: `{x?: T}` cannot hold `undefined` explicitly; only "missing" or `T`.
- `noImplicitOverride`: subclass methods must use the `override` keyword.
- `noFallthroughCasesInSwitch`: every `case` must end with `return`, `throw`, `break`, or `continue`.
- `noPropertyAccessFromIndexSignature`: `obj.foo` is illegal if `obj` has only an index signature; must use `obj["foo"]`.
- `noUncheckedSideEffectImports`: `import "foo"` without a binding must resolve to a known module (TS 5.6).
- `rewriteRelativeImportExtensions`: lets us write `import "./bar.ts"` and have tsc rewrite to `./bar.js` in dist (TS 5.6).
- `isolatedModules`: every file must be independently compileable, no cross-file type-only declarations.
- `verbatimModuleSyntax`: `import type` and `export type` are not erased silently; emitted as written.

### 10.2 Module resolution

`moduleResolution: "Bundler"` is the Node 22 + Deno 2 + Bun 1.1 + esbuild common subset. It does not require file extensions in import paths (we add them anyway for clarity), supports `package.json` `exports` field, and supports conditional exports (`node`, `deno`, `bun`, `browser`).

## 11. Phase J: source map emission

Every emitted `.ts` file carries a sibling `.ts.map` file in the standard Source Map V3 format. The map links every output character (byte offset, line, column) back to the originating Mochi source position.

The pretty-printer (phase G) maintains a `SourceMapBuilder` that records:

- For each `ts.Stmt` and `ts.Expr` it prints: the output line and column at the start of the node.
- The input position from the node's `Pos` field.

At end of file, the builder serialises to JSON:

```json
{
  "version": 3,
  "file": "foo.ts",
  "sourceRoot": "",
  "sources": ["../src/foo.mochi"],
  "names": ["area", "Shape", "Circle", "Square", "Triangle"],
  "mappings": "AAAA,SAAS,MAAM,GAAG,CAAC,CAAS,IAAI..."
}
```

VLQ-encoded mappings, the same format Babel and tsc produce.

### 11.1 Stack-trace symbolication

When a Node 22 / Deno 2 / Bun 1.1 process throws and prints a stack trace, the trace points into the `.js` (post-tsc) file. Combining the `.ts.map` (Mochi -> TS) with the `.js.map` (TS -> JS, produced by tsc) gives Mochi -> JS. The runtime helper `@mochi/runtime/dev/symbolicate` reads both maps and rewrites traces:

```
Error: not implemented
    at area (/dist/foo.js:42:13)
        <- /src/foo.ts:18:5
            <- /src/foo.mochi:7:3
```

The `<- /src/foo.mochi:7:3` line is added by the symbolicator.

In production this helper is tree-shaken out; only debug builds (`mochi build --debug`) include it.

### 11.2 Source-map fan-out

When the codegen monomorphises a generic Mochi function into two TS functions (e.g. `lookup_bigint` and `lookup_number`), both TS functions point to the same Mochi source range. The source map handles this: multiple output ranges can map to the same input range. Reverse lookup (Mochi -> TS) returns a list, not a single position.

## 12. Phase K: build manifest

After phases A through J succeed, the codegen writes `mochi.lock.json`:

```json
{
  "mochi_version": "0.52.0",
  "compiler_version": "1.0.0",
  "tsc_version": "5.6.3",
  "prettier_version": "3.4.2",
  "node_version": "22.11.0",
  "target": "typescript",
  "inputs": {
    "src/main.mochi": "sha256:abcdef0123...",
    "src/util.mochi": "sha256:fedcba9876..."
  },
  "outputs": {
    "src/generated/main.ts": "sha256:1111...",
    "src/generated/main.ts.map": "sha256:2222...",
    "src/generated/util.ts": "sha256:3333...",
    "src/generated/util.ts.map": "sha256:4444..."
  },
  "tsconfig": {
    "sha256": "5555..."
  }
}
```

The lock file is the input to the reproducibility check (see [[11-testing-gates]] §7). Two CI runs on different hosts must produce byte-identical `mochi.lock.json` (SHA256 over the file itself).

### 12.1 Reproducibility constraints

To guarantee byte-equal output across hosts:

- No timestamps in emitted files.
- No filesystem-order iteration (Mochi-internal package iteration is sorted).
- No Go-map iteration (`map[K]V` is replaced with sorted slices at emit time).
- No PRNG without a fixed seed.
- The Mochi compiler binary is built with `-trimpath -buildvcs=false`.
- `SOURCE_DATE_EPOCH` is honoured for any tar/zip artefact.

## 13. The lower pass in detail

This section walks the lower pass file by file. It is the bulk of the codegen complexity.

### 13.1 Files

```
transpiler3/typescript/
  lower/
    lower.go        # entry: aotir.Program -> ts.File
    decl.go         # FuncDecl, ConstDecl, TypeDecl, AgentDecl
    stmt.go         # Let, Assign, If, Switch, For, While, Return, ...
    expr.go         # Lit, Var, Call, Match, Binary, Unary, ...
    type.go         # aotir.Type -> ts.Type
    closure.go      # phase C
    monomorph.go    # phase E (assertion-only, no duplication)
    record.go       # record class generation
    sum.go          # sum-type discriminated union generation
    agent.go        # agent class generation
    stream.go       # async generator generation
    name.go         # identifier mangling, reserved word handling
    runtime.go      # @mochi/runtime import management
    ctx.go          # lowering context
  ts/
    nodes.go        # AST node definitions
    builder.go      # convenience constructors
  print/
    printer.go      # phase G pretty-printer
    sourcemap.go    # phase J source map builder
  emit/
    emit.go         # phase H/I/J/K orchestration
    prettier.go     # prettier invocation
    tsc.go          # tsc invocation
    lockfile.go     # mochi.lock.json builder
  test/
    fixtures/       # golden file fixtures
    lower_test.go
    print_test.go
    integration_test.go
```

### 13.2 Lowering context

The `Ctx` struct is passed through every lower function:

```go
// transpiler3/typescript/lower/ctx.go
type Ctx struct {
    Program  *aotir.Program
    Analysis *Analysis              // from phase A
    Imports  map[string]map[string]bool // module -> name -> needed
    Records  map[aotir.TypeID]string // record TypeID -> ts class name
    Sums     map[aotir.TypeID]string // sum TypeID -> ts type alias name
    Agents   map[aotir.TypeID]string // agent TypeID -> ts class name
    Stack    []FrameInfo            // function-frame stack (for closure conversion)
    Decls    []ts.Decl              // accumulated decls in current file
    Used     map[string]bool        // identifiers used in this file
}

type FrameInfo struct {
    FuncName string
    Locals   map[aotir.LocalID]string // aotir local -> ts name
    Env      *EnvRecord               // current closure env, if any
}
```

`Ctx.Imports` records every `@mochi/runtime/<module>` symbol the file needs. At emit time these are sorted and written as a single import list at the top of the file.

### 13.3 Lowering a function

```go
// transpiler3/typescript/lower/decl.go
func lowerFunc(f *aotir.FuncDecl, ctx *Ctx) *ts.FuncDecl {
    ctx.PushFrame(f.Name)
    defer ctx.PopFrame()

    params := make([]ts.Param, len(f.Params))
    for i, p := range f.Params {
        name := ctx.LocalName(p.LocalID)
        params[i] = ts.Param{
            Name: name,
            Type: lowerType(p.Type, ctx),
        }
    }

    ret := lowerType(f.Ret, ctx)
    if f.Async {
        ret = ts.Generic{Name: "Promise", Args: []ts.Type{ret}}
    }

    body := make([]ts.Stmt, 0, len(f.Body))
    for _, s := range f.Body {
        body = append(body, lowerStmt(s, ctx))
    }

    return &ts.FuncDecl{
        Name:     mangleIdent(f.Name),
        Params:   params,
        Ret:      ret,
        Body:     body,
        Async:    f.Async,
        Exported: f.Exported,
        JSDoc:    formatJSDoc(f.Doc),
    }
}
```

### 13.4 Lowering a statement

```go
// transpiler3/typescript/lower/stmt.go
func lowerStmt(s aotir.Stmt, ctx *Ctx) ts.Stmt {
    switch s := s.(type) {
    case *aotir.Let:
        return &ts.LetStmt{
            Name:  ctx.LocalName(s.LocalID),
            Type:  lowerType(s.Type, ctx),
            Init:  lowerExpr(s.Value, ctx),
            Const: !s.Mutable,
        }
    case *aotir.Assign:
        return &ts.ExprStmt{
            Expr: &ts.BinaryExpr{
                Op: "=",
                L:  lowerLValue(s.LHS, ctx),
                R:  lowerExpr(s.RHS, ctx),
            },
        }
    case *aotir.If:
        return &ts.IfStmt{
            Cond: lowerExpr(s.Cond, ctx),
            Then: lowerBlock(s.Then, ctx),
            Else: lowerBlock(s.Else, ctx),
        }
    case *aotir.Switch:
        return lowerSwitch(s, ctx)
    case *aotir.While:
        return &ts.WhileStmt{
            Cond: lowerExpr(s.Cond, ctx),
            Body: lowerBlock(s.Body, ctx),
        }
    case *aotir.For:
        return lowerFor(s, ctx)
    case *aotir.Return:
        return &ts.ReturnStmt{Value: lowerExpr(s.Value, ctx)}
    case *aotir.Expr:
        return &ts.ExprStmt{Expr: lowerExpr(s.Expr, ctx)}
    case *aotir.Block:
        return lowerBlock(s.Body, ctx)
    case *aotir.Break:
        return &ts.BreakStmt{}
    case *aotir.Continue:
        return &ts.ContinueStmt{}
    }
    panic("unknown stmt kind")
}
```

### 13.5 Lowering an expression

The expression lowerer is the biggest single file. Highlights:

```go
// transpiler3/typescript/lower/expr.go
func lowerExpr(e aotir.Expr, ctx *Ctx) ts.Expr {
    switch e := e.(type) {
    case *aotir.IntLit:
        if ctx.Analysis.IntFit[e.NodeID] == IntFitNumber {
            return &ts.LitExpr{Kind: "number", Value: e.Value}
        }
        return &ts.LitExpr{Kind: "bigint", Value: e.Value}
    case *aotir.FloatLit:
        return &ts.LitExpr{Kind: "number", Value: e.Value}
    case *aotir.StringLit:
        return &ts.LitExpr{Kind: "string", Value: e.Value}
    case *aotir.BoolLit:
        return &ts.LitExpr{Kind: "boolean", Value: e.Value}
    case *aotir.NullLit:
        return &ts.LitExpr{Kind: "null"}
    case *aotir.Var:
        return lowerVar(e, ctx)
    case *aotir.Call:
        return lowerCall(e, ctx)
    case *aotir.Match:
        return lowerMatch(e, ctx)
    case *aotir.Binary:
        return lowerBinary(e, ctx)
    case *aotir.Unary:
        return lowerUnary(e, ctx)
    case *aotir.Index:
        return lowerIndex(e, ctx)
    case *aotir.Field:
        return &ts.FieldExpr{
            Obj:  lowerExpr(e.Obj, ctx),
            Name: e.Field,
        }
    case *aotir.Record:
        return lowerRecord(e, ctx)
    case *aotir.List:
        items := make([]ts.Expr, len(e.Items))
        for i, it := range e.Items {
            items[i] = lowerExpr(it, ctx)
        }
        return &ts.ArrayExpr{Items: items}
    case *aotir.Map:
        return lowerMapLit(e, ctx)
    case *aotir.Set:
        return lowerSetLit(e, ctx)
    case *aotir.Lambda:
        return lowerLambda(e, ctx)
    case *aotir.Cast:
        return lowerCast(e, ctx)
    case *aotir.Try:
        return lowerTry(e, ctx)
    }
    panic("unknown expr kind")
}
```

### 13.6 Lowering arithmetic

Mochi `+ - * / %` over `int` lower to `+ - * / %` over `bigint` or `number` depending on the IntFit tag. Two caveats:

1. **Integer division**: `a / b` over `bigint` is truncated-toward-zero by default (`5n / 2n === 2n`). Mochi `int` division is also truncated-toward-zero, so this matches.
2. **Number division**: `a / b` over `number` is float division (`5 / 2 === 2.5`). Mochi distinguishes `int / int` (truncated) from `float / float` (real), so when both operands are tagged `number`-as-int we emit `Math.trunc(a / b)`.

```go
// transpiler3/typescript/lower/expr.go
func lowerBinary(e *aotir.Binary, ctx *Ctx) ts.Expr {
    l := lowerExpr(e.L, ctx)
    r := lowerExpr(e.R, ctx)
    op := e.Op
    if e.Op == "/" && e.L.Type() == aotir.Int && ctx.Analysis.IntFit[e.NodeID] == IntFitNumber {
        // truncated division on number
        return &ts.CallExpr{
            Callee: &ts.FieldExpr{Obj: &ts.VarExpr{Name: "Math"}, Name: "trunc"},
            Args:   []ts.Expr{&ts.BinaryExpr{Op: "/", L: l, R: r}},
        }
    }
    return &ts.BinaryExpr{Op: op, L: l, R: r}
}
```

For overflow checking on `bigint`, Mochi `int` is arbitrary-precision by default, so no overflow check is needed. For fixed-width integer types (`i32`, `i64`) we emit explicit masking:

```typescript
// (a + b) as i32 -> wrap to int32 range
const r = Number(BigInt.asIntN(32, BigInt(a) + BigInt(b)));
```

This matches MEP-45's `-fwrapv` semantics.

### 13.7 Lowering string operations

Mochi strings are sequences of Unicode code points. JavaScript strings are sequences of UTF-16 code units. The mismatch matters for `len`, indexing, slicing, and `for c in s`.

| Mochi op | TS lowering |
|----------|-------------|
| `len(s)` | `[...s].length` (or runtime helper `mochiStrLen(s)`) |
| `s[i]` | `[...s][Number(i)]` (or `mochiStrAt(s, i)`) |
| `s[a..b]` | `[...s].slice(Number(a), Number(b)).join("")` (or `mochiStrSlice(s, a, b)`) |
| `for c in s` | `for (const c of s)` (this one is correct natively because `for-of` iterates code points) |
| `s + t` | `s + t` |
| `s == t` | `s === t` |
| `contains(s, t)` | `s.includes(t)` |

The runtime helpers `mochiStrLen`, `mochiStrAt`, `mochiStrSlice` are emitted from `@mochi/runtime/strings` (see [[04-runtime]] §5). The codegen prefers inline `[...s].length` for short strings but switches to the helper for `len()` calls inside loops to avoid O(n) cost per iteration.

### 13.8 Lowering list operations

| Mochi op | TS lowering |
|----------|-------------|
| `[1, 2, 3]` | `[1n, 2n, 3n]` (or `[1, 2, 3]` if IntFit picks number) |
| `xs[i]` | `xs[Number(i)]!` (the `!` non-null assertion because of `noUncheckedIndexedAccess`) plus a runtime bounds check |
| `len(xs)` | `BigInt(xs.length)` (or `xs.length` if integer rep is number) |
| `xs + ys` | `[...xs, ...ys]` |
| `append(xs, y)` | `[...xs, y]` (immutable view) or `xs.push(y)` (mutated view) |
| `xs[i..j]` | `xs.slice(Number(i), Number(j))` |
| `for x in xs` | `for (const x of xs)` |

The `xs[i]!` is unsafe in TypeScript's view. To keep the type checker happy under `noUncheckedIndexedAccess`, the codegen wraps each indexed access in a helper:

```typescript
// @mochi/runtime/collections
export function listGet<T>(xs: readonly T[], i: bigint | number): T {
  const idx = typeof i === "bigint" ? Number(i) : i;
  if (idx < 0 || idx >= xs.length) {
    throw new RangeError(`list index out of bounds: ${idx} not in [0, ${xs.length})`);
  }
  return xs[idx]!;
}
```

The codegen emits `listGet(xs, i)` instead of `xs[i]!`. This gets the bounds check, the bigint/number bridge, and the type safety in one helper.

### 13.9 Lowering map operations

| Mochi op | TS lowering |
|----------|-------------|
| `{"a": 1, "b": 2}` | `new Map([["a", 1n], ["b", 2n]])` |
| `m[k]` | `mapGet(m, k)` (runtime helper) |
| `m[k] = v` | `m.set(k, v)` |
| `len(m)` | `BigInt(m.size)` |
| `keys(m)` | `[...m.keys()]` |
| `values(m)` | `[...m.values()]` |
| `for (k, v) in m` | `for (const [k, v] of m)` |
| `has(m, k)` | `m.has(k)` |
| `delete(m, k)` | `m.delete(k)` |

The `mapGet` helper throws on missing key (matching Mochi semantics where `m[k]` aborts on missing); the variant `mapGetOpt(m, k)` returns `T | null` for the `m?.[k]` Mochi syntax.

### 13.10 Lowering set operations

| Mochi op | TS lowering |
|----------|-------------|
| `{1, 2, 3}` (set literal) | `new Set([1n, 2n, 3n])` |
| `has(s, x)` | `s.has(x)` |
| `add(s, x)` | `s.add(x)` |
| `remove(s, x)` | `s.delete(x)` |
| `a | b` (union) | `a.union(b)` (ES2024) |
| `a & b` (intersection) | `a.intersection(b)` (ES2024) |
| `a - b` (difference) | `a.difference(b)` (ES2024) |
| `a <= b` (subset) | `a.isSubsetOf(b)` (ES2024) |
| `len(s)` | `BigInt(s.size)` |
| `for x in s` | `for (const x of s)` |

ES2024 set methods (`union`, `intersection`, `difference`, `isSubsetOf`, `isSupersetOf`, `isDisjointFrom`, `symmetricDifference`) ship in Node 22+, Deno 1.42+, Bun 1.1+, Safari 17, Firefox 127, Chrome 122. The compiler's emit target ES2024 makes them legal at type-check time.

For older browsers, the runtime polyfills via `@mochi/runtime/collections/set-polyfill` (see [[04-runtime]]).

### 13.11 Lowering record construction

A Mochi record:

```mochi
type Point = {x: float, y: float}

let p = Point{x: 1.0, y: 2.0}
```

lowers to a TypeScript class with a private constructor and a static factory:

```typescript
export class Point {
  readonly x: number;
  readonly y: number;
  private constructor(x: number, y: number) {
    this.x = x;
    this.y = y;
  }
  static make(args: { x: number; y: number }): Point {
    return new Point(args.x, args.y);
  }
}

const p = Point.make({ x: 1.0, y: 2.0 });
```

Two reasons for class-with-static-factory:

1. **Identity discrimination**: a class instance has a unique prototype, so `instanceof` works for runtime type tests.
2. **Immutability**: `readonly` fields plus private constructor prevent mutation; only `make` (or `with`, see below) constructs new instances.

For records with optional fields (`type Foo = {a: int, b: int?}`), `b` is typed `bigint | null` and the factory default-fills `null` if not provided.

### 13.12 Record with-syntax

Mochi `p with {x: 5.0}` (functional update) lowers to:

```typescript
const p2 = Point.with(p, { x: 5.0 });
```

with the static method:

```typescript
static with(prev: Point, args: Partial<{ x: number; y: number }>): Point {
  return new Point(args.x ?? prev.x, args.y ?? prev.y);
}
```

The `Partial<...>` makes every field optional in the override args; missing fields fall back to the previous instance.

### 13.13 Lowering sum types

```mochi
type Result<T, E> = Ok{value: T} | Err{error: E}
```

becomes:

```typescript
export type Result<T, E> =
  | { kind: "Ok"; value: T }
  | { kind: "Err"; error: E };

export const Result = {
  Ok<T, E>(value: T): Result<T, E> { return { kind: "Ok", value }; },
  Err<T, E>(error: E): Result<T, E> { return { kind: "Err", error }; },
};
```

The discriminator key `kind` is hardcoded across all Mochi sum types. The variant tag is the variant name verbatim, single-quoted as a string literal type.

For sum types with type parameters, the variant factories are generic; the type-checker propagates inference.

### 13.14 Lowering agents

A Mochi agent:

```mochi
agent Counter {
  state: int = 0
  on inc(n: int) -> int {
    state = state + n
    return state
  }
}
```

lowers to:

```typescript
import { AgentBase, type Message } from "@mochi/runtime/agent";

interface CounterMsg_inc {
  kind: "inc";
  n: bigint;
  reply: (v: bigint) => void;
}

type CounterMsg = CounterMsg_inc;

export class Counter extends AgentBase<CounterMsg> {
  private state: bigint = 0n;

  constructor(signal: AbortSignal) {
    super(signal);
  }

  async inc(n: bigint): Promise<bigint> {
    const { promise, resolve } = Promise.withResolvers<bigint>();
    this.cast({ kind: "inc", n, reply: resolve });
    return promise;
  }

  protected override handle(msg: CounterMsg): void {
    switch (msg.kind) {
      case "inc": {
        this.state = this.state + msg.n;
        msg.reply(this.state);
        return;
      }
      default: {
        const _exhaustive: never = msg.kind;
        throw new Error("unreachable: " + String(_exhaustive));
      }
    }
  }
}
```

The `AgentBase<T>` class is in `@mochi/runtime/agent` (see [[04-runtime]] §6). It owns the `AsyncIterableQueue<T>` mailbox and the loop.

### 13.15 Lowering streams

```mochi
stream nats() -> stream<int> {
  var i = 0
  loop {
    yield i
    i = i + 1
  }
}
```

lowers to:

```typescript
export async function* nats(): AsyncGenerator<bigint, void, undefined> {
  let i: bigint = 0n;
  while (true) {
    yield i;
    i = i + 1n;
  }
}
```

Mochi `yield` becomes TS `yield`; `stream<T>` becomes `AsyncGenerator<T, void, undefined>` (or the wider `AsyncIterable<T>` at the type-spelling level). The codegen prefers `AsyncGenerator` at the function-return position so TS can infer `yield` types but uses `AsyncIterable<T>` at parameter positions so callers can pass any async iterator.

## 14. Identifier mangling

JavaScript reserved words must be mangled. The mangle rule: append a trailing underscore.

| Mochi | TS |
|-------|----|
| `class` | `class_` |
| `function` | `function_` |
| `new` | `new_` |
| `delete` | `delete_` |
| `void` | `void_` |
| `import` | `import_` |
| `export` | `export_` |
| `default` | `default_` |
| `await` | `await_` |
| `async` | `async_` |
| `yield` | `yield_` |

Also mangled: strict-mode reserved words (`implements`, `interface`, `let`, `package`, `private`, `protected`, `public`, `static`, `enum`), future reserved words (`as`, `of`, `from`, `get`, `set`).

The mangle is one-way: Mochi `class` becomes TS `class_`. The Mochi-source identifier is preserved in a JSDoc tag for source-map reverse lookup:

```typescript
/** @mochi class */
const class_ = 42;
```

The leading-underscore identifier `_class` is **not** used because Mochi already uses leading-underscore for "unused-on-purpose" idioms.

## 15. Import management

Every `@mochi/runtime` symbol referenced from generated TS code is recorded in `ctx.Imports`. At file finalisation, `ctx.Imports` is sorted and emitted:

```typescript
// (auto-generated, do not edit)
import { Counter } from "@mochi/runtime/agent";
import { listGet, mapGet } from "@mochi/runtime/collections";
import { mochiStrLen } from "@mochi/runtime/strings";
import type { Result } from "@mochi/runtime/result";
```

Type-only imports use `import type` (see `verbatimModuleSyntax` tsconfig flag).

Cross-module imports (between user-generated files) use the explicit `.ts` extension:

```typescript
import { area } from "./shapes.ts";
```

The `rewriteRelativeImportExtensions` tsconfig flag rewrites these to `.js` in dist.

## 16. JSDoc generation

Every Mochi doc-comment (lines starting with `///`) is preserved as a JSDoc block above the corresponding declaration:

```mochi
/// Compute the area of a shape.
/// Works for circles, squares, and triangles.
fun area(s: Shape) -> float {
  ...
}
```

becomes:

```typescript
/**
 * Compute the area of a shape.
 * Works for circles, squares, and triangles.
 */
export function area(s: Shape): number {
  // ...
}
```

JSDoc tags (`@param`, `@returns`, `@throws`, `@deprecated`) propagate from Mochi-side equivalent annotations.

## 17. Top-level orchestration

The entry point is `mochi.Compile(args)`:

```go
// cmd/mochi/build/typescript.go
func BuildTypescript(args BuildArgs) error {
    pkg, err := loader.LoadPackage(args.Source)
    if err != nil { return err }
    prog, err := typecheck.Check(pkg)
    if err != nil { return err }
    ir, err := aotir.Lower(prog)
    if err != nil { return err }

    an := lower.PrePass(ir)
    files := lower.Lower(ir, an)

    if err := emit.WriteFiles(args.OutDir, files); err != nil { return err }
    if err := emit.RunPrettier(args.OutDir); err != nil { return err }
    if err := emit.RunTsc(args.OutDir); err != nil { return err }
    if err := emit.WriteSourceMaps(args.OutDir, files); err != nil { return err }
    if err := emit.WriteLockfile(args.OutDir, files); err != nil { return err }
    return nil
}
```

Each emit step is fail-fast: any error aborts the build. The intermediate files are kept on disk for debugging (`mochi build --keep-temps`).

## 18. Lowering examples

### 18.1 Hello world

Mochi:

```mochi
fun main() {
  print("hello")
}
```

TS output:

```typescript
// generated/main.ts
import { print } from "@mochi/runtime/io";

export function main(): void {
  print("hello");
}

main();
```

`tsc --noEmit` accepts; `prettier --check` accepts; `node dist/main.js` prints `hello`.

### 18.2 Recursive factorial

Mochi:

```mochi
fun fact(n: int) -> int {
  if n <= 1 {
    return 1
  }
  return n * fact(n - 1)
}
```

TS output (bigint variant):

```typescript
export function fact(n: bigint): bigint {
  if (n <= 1n) {
    return 1n;
  }
  return n * fact(n - 1n);
}
```

TS output (number variant, used when caller proves `n` fits in i53):

```typescript
export function fact_n(n: number): number {
  if (n <= 1) {
    return 1;
  }
  return n * fact_n(n - 1);
}
```

The codegen emits both if both are needed; otherwise just one.

### 18.3 Match on Option

Mochi:

```mochi
fun unwrap_or(x: int?, default: int) -> int {
  match x {
    Some(v) => v,
    None => default,
  }
}
```

TS output:

```typescript
export function unwrap_or(x: bigint | null, default_: bigint): bigint {
  if (x !== null) {
    return x;
  }
  return default_;
}
```

Note: Mochi `Option<T>` is special-cased to lower to `T | null`, not to a discriminated union. The match lowers to a `null` check.

### 18.4 List comprehension

Mochi:

```mochi
fun squares(n: int) -> list<int> {
  return [i * i for i in range(0, n)]
}
```

TS output:

```typescript
import { range } from "@mochi/runtime/collections";

export function squares(n: bigint): bigint[] {
  const result: bigint[] = [];
  for (const i of range(0n, n)) {
    result.push(i * i);
  }
  return result;
}
```

The codegen lowers the comprehension to an explicit accumulator loop. JS does not have native list comprehensions; the helper-free imperative form is fastest at runtime.

### 18.5 Query DSL

Mochi:

```mochi
let result = from u in users
             join o in orders on o.user_id == u.id
             where u.age >= 18
             select {name: u.name, total: o.total}
```

TS output:

```typescript
import { hashJoin } from "@mochi/runtime/query";

const joined = hashJoin(users, orders, (u) => u.id, (o) => o.user_id);
const result: { name: string; total: number }[] = [];
for (const [u, o] of joined) {
  if (u.age >= 18n) {
    result.push({ name: u.name, total: o.total });
  }
}
```

Or, when the IR can prove iteration helpers fit better:

```typescript
const result = Iterator.from(users)
  .flatMap((u) =>
    Iterator.from(orders)
      .filter((o) => o.user_id === u.id)
      .map((o) => [u, o] as const)
  )
  .filter(([u, _o]) => u.age >= 18n)
  .map(([u, o]) => ({ name: u.name, total: o.total }))
  .toArray();
```

Iterator helpers are ES2024 stage-4 and ship in Node 22, Deno 2, Bun 1.1, Chrome 122+, Firefox 131+, Safari 18. The codegen prefers them when readability is preserved and the query is short.

### 18.6 Agent ping-pong

Mochi:

```mochi
agent Pinger {
  count: int = 0
  on ping() -> string {
    count = count + 1
    return "pong " + str(count)
  }
}

async fun main() {
  let p = spawn Pinger()
  print(await p.ping())  // "pong 1"
  print(await p.ping())  // "pong 2"
}
```

TS output:

```typescript
import { AgentBase } from "@mochi/runtime/agent";

interface PingerMsg_ping {
  kind: "ping";
  reply: (v: string) => void;
}
type PingerMsg = PingerMsg_ping;

export class Pinger extends AgentBase<PingerMsg> {
  private count: bigint = 0n;
  constructor(signal: AbortSignal) { super(signal); }
  async ping(): Promise<string> {
    const { promise, resolve } = Promise.withResolvers<string>();
    this.cast({ kind: "ping", reply: resolve });
    return promise;
  }
  protected override handle(msg: PingerMsg): void {
    switch (msg.kind) {
      case "ping": {
        this.count = this.count + 1n;
        msg.reply("pong " + this.count.toString());
        return;
      }
      default: {
        const _exhaustive: never = msg.kind;
        throw new Error("unreachable: " + String(_exhaustive));
      }
    }
  }
}

import { print } from "@mochi/runtime/io";

export async function main(): Promise<void> {
  const controller = new AbortController();
  try {
    const p = new Pinger(controller.signal);
    print(await p.ping());
    print(await p.ping());
  } finally {
    controller.abort();
  }
}

await main();
```

The `try` / `finally` wraps every agent-spawning scope to guarantee abort on exit.

### 18.7 Async stream

Mochi:

```mochi
stream nats(start: int) -> stream<int> {
  var i = start
  loop {
    yield i
    i = i + 1
  }
}

async fun first_n(s: stream<int>, n: int) -> list<int> {
  var result = []
  var count = 0
  for await x in s {
    if count >= n { break }
    append(result, x)
    count = count + 1
  }
  return result
}
```

TS output:

```typescript
export async function* nats(start: bigint): AsyncGenerator<bigint, void, undefined> {
  let i: bigint = start;
  while (true) {
    yield i;
    i = i + 1n;
  }
}

export async function first_n(
  s: AsyncIterable<bigint>,
  n: bigint
): Promise<bigint[]> {
  const result: bigint[] = [];
  let count: bigint = 0n;
  for await (const x of s) {
    if (count >= n) break;
    result.push(x);
    count = count + 1n;
  }
  return result;
}
```

## 19. The emit pass

Phases H, I, J, K are the "emit pass". They are orchestrated by `emit.go`:

```go
// transpiler3/typescript/emit/emit.go
func WriteFiles(outDir string, files []*ts.File) error {
    for _, f := range files {
        path := filepath.Join(outDir, f.Path)
        if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
            return err
        }
        p := print.NewPrinter()
        p.Print(f)
        if err := os.WriteFile(path, p.Bytes(), 0o644); err != nil {
            return err
        }
        if err := os.WriteFile(path+".map", p.SourceMap().JSON(), 0o644); err != nil {
            return err
        }
    }
    return nil
}

func RunPrettier(outDir string) error {
    cmd := exec.Command("npx", "prettier@3.4", "--write", outDir)
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    return cmd.Run()
}

func RunTsc(outDir string) error {
    cmd := exec.Command("npx", "tsc@5.6", "--noEmit", "--project", filepath.Join(outDir, "tsconfig.base.json"))
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    return cmd.Run()
}
```

In CI we use `pnpm dlx` instead of `npx` for caching speed. The exact prettier and tsc versions are pinned in the project's `package.json` `devDependencies`, but the compiler binary uses `npx` to allow standalone invocation outside a project context.

### 19.1 Caching

The emit pass caches per-file: if `mochi.lock.json` records an input SHA256 matching the current input, the existing output is kept and prettier/tsc skipped.

### 19.2 Parallel emit

For large projects, the lower pass produces N files in parallel (one Go routine per top-level Mochi package). The emit pass also parallelises prettier/tsc invocations: one prettier per N files, one tsc over the whole project.

## 20. Failure modes and diagnostics

When something goes wrong, the codegen reports back to Mochi-source positions.

### 20.1 Pre-pass failure: integer fit ambiguity

If a producer's IntFit is ambiguous (some consumers expect bigint, some expect number), the type checker should have caught it earlier. If it slips through to the lower pass, we emit:

```
mochi: build error at src/foo.mochi:42:5
  integer producer flows to both bigint and number consumers:
    bigint use at src/foo.mochi:45:3
    number use at src/foo.mochi:47:3
  add an explicit `as i32` or `as i64` cast to disambiguate.
```

### 20.2 Lower-pass failure: unsupported feature

If aotir contains a feature the TS backend does not yet support (e.g. a hypothetical "shared-memory atomics" extension), we emit:

```
mochi: build error at src/foo.mochi:42:5
  shared-memory atomics are not yet supported in the TypeScript backend;
  use the C backend (mochi build --target=c) for this feature.
```

### 20.3 Prettier failure: printer bug

If our printer's output differs from prettier's reformatting, we emit:

```
mochi: codegen invariant violation
  pretty-printer output differed from prettier reformatting at:
    src/generated/foo.ts:42:5
  expected (printer):
    {a: 1, b: 2}
  got (prettier):
    {
      a: 1,
      b: 2,
    }
  please file a bug at github.com/mochilang/mochi
```

This is a hard failure: the printer must match prettier exactly to maintain the byte-equal reproducibility guarantee.

### 20.4 Tsc failure: typecheck error

If `tsc --noEmit` reports a diagnostic, we wrap it in a Mochi-pointing report:

```
mochi: typecheck error
  src/generated/foo.ts:42:5 -- error TS2322: Type 'bigint' is not assignable to type 'number'
  this originates from src/foo.mochi:18:3 (binary expression)
  the IntFit pre-pass tagged this site as bigint but a consumer expected number;
  please file a bug at github.com/mochilang/mochi
```

The Mochi source position is recovered from the source map.

## 21. Performance characteristics

Targets for an average 5kLoC Mochi project on a modern laptop (M3 Pro, 11 cores, 16GB):

- Lower pass: under 100ms.
- Pretty-printer: under 50ms.
- Prettier: 500ms to 2s (NPX overhead dominates; with persistent daemon, 100ms).
- Tsc: 1s to 5s (most of the wall time).
- Total cold build: under 10s.
- Total incremental build (one file changed): under 2s (tsc incremental cache).

For huge projects (50kLoC), we rely on `tsc --build --incremental` which caches type info per file.

## 22. Testing strategy

Three layers:

1. **Unit tests on lower/print**: Go tests for individual node lowerings and individual printer cases. Golden files in `transpiler3/typescript/test/fixtures/<feature>.{mochi,ts}`.
2. **Integration tests**: Mochi -> TS -> run on Node, Deno, Bun, browser. Byte-equal stdout against vm3. Gated in `transpiler3/typescript/test/integration_test.go`.
3. **Phase-gate tests** (see [[11-testing-gates]]): each MEP-52 phase has a fixed list of fixtures that must pass on all four runtimes.

Golden file refresh: `go test -update` regenerates the `.ts` fixture from the current lower implementation. Diffing the regenerated file against the committed one reveals regressions.

## 23. Comparison to MEP-51 (Python target)

MEP-51 lowers aotir -> Python AST (`ast.Module`); MEP-52 lowers aotir -> Mochi-side `ts.File`. Both use a pretty-printer (Python's `ast.unparse` vs our custom printer) then a post-format (black vs prettier) then a typechecker (mypy `--strict` vs tsc `--strict`).

Differences:

- MEP-51 has no pre-pass equivalent to IntFit because Python `int` is always arbitrary-precision; no bigint/number split.
- MEP-51 has no closure-conversion phase (Python closures are uniform).
- MEP-51 has no monomorphisation (Python is dynamic at runtime; mypy is structural).
- MEP-51 has no source-map step (Python tracebacks already reference source positions via `__file__` + line number).
- MEP-51 uses Python's `ast` module directly; MEP-52 ships its own `ts/nodes.go` because the TypeScript Compiler API is too heavy a dependency.
- MEP-52 has a richer type lowering because TS has variance and bigint/number; MEP-51 lowers everything to `int | str | float | bool | list | dict | set | None | dataclass`.

Otherwise the structure (lower -> emit -> format -> typecheck -> source-map -> lockfile) is identical.

## 24. Future extensions

The codegen is designed for these future phases (see [[01-language-surface]]):

- **Effect tracking** (post-phase-18): aotir effects (pure/io/async/blocking) propagate to TS so async functions get `async` keyword and pure functions get `const` const-binding optimisation.
- **Linear types** (research): would influence `readonly T[]` vs `T[]` choice (linear lists get `T[]` for in-place updates).
- **Refinement types** (research): would tighten IntFit (e.g. `int<0, 100>` always fits in number, never needs bigint).
- **WebAssembly target**: a sibling backend; the lower-pass module structure (one Go package per phase) makes the wasm backend a parallel emit pipeline.

## 25. Summary

The MEP-52 codegen is structured as eleven phases (A through K) that turn aotir into a directory of typecheck-clean, prettier-formatted `.ts` files plus source maps plus a reproducibility lockfile. Key design decisions:

- **No tsc dependency at compile time**: ship a tiny Go `ts/` AST package and our own pretty-printer.
- **Run prettier 3.x then tsc --noEmit** as defence-in-depth gates after our printer emits.
- **Source maps via threaded Pos fields**: every aotir node's source position propagates through every transformation, so the final `.ts.map` is built without re-parsing.
- **Match-to-switch-tag with `_exhaustive: never` tails**: gives TS the full exhaustiveness check.
- **Closure-conversion with env records**: gives predictable mutation semantics and source-map clarity.
- **IntFit monomorphisation**: choose bigint or number per integer producer based on static interval analysis.
- **Reproducible lockfile**: byte-equal output across hosts is gated via `mochi.lock.json`.

Cross-references: [[04-runtime]] for the runtime library being targeted; [[06-type-lowering]] for the per-Mochi-type lowering rules invoked from phase B; [[10-build-system]] for how the emitted output flows into npm/JSR/Deno/Bun publishing; [[11-testing-gates]] for the per-phase test gates that exercise this pipeline.
