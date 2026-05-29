---
title: "05. Codegen design"
sidebar_position: 6
sidebar_label: "05. Codegen design"
description: "aotir-to-Go lowering via the structural gotree AST, rendering through go/format, closure-environment lifting, monomorphisation, stable item ordering."
---

# 05. Codegen design

This note describes the lowering pipeline from `aotir.Program` to a rendered `.go` file. The pipeline has three stages: lowering (aotir → gotree), rendering (gotree → bytes), formatting (bytes → canonical bytes).

## Stage 1: lowering

`transpiler3/go/lower/Lower(prog *aotir.Program, fileBase, packageName string) (*gotree.SourceFile, error)` walks the aotir program and builds a `gotree.SourceFile`. The lowerer is a struct (`*lowerer`) holding per-emit state:

```go
type lowerer struct {
    pkg       string
    runtime   runtimeFlags     // which runtime helpers to import
    closures  []*closureLift   // closures needing env lifting
    omap      bool             // did we use OMap?
    bigSorts  bool             // did we use heap-backed top-K?
    // ...
}
```

The `runtimeFlags` struct tracks which runtime packages the emit needs. The renderer reads it at the end and emits the corresponding `import "dev.mochilang/runtime/go/..."` lines. This avoids the dead-import problem (importing `mochiruntime/collections` when the program does not use `Map`/`Filter`/`Reduce`).

The walk is recursive: `lowerStmt` dispatches on `aotir.Stmt` variants; `lowerExpr` dispatches on `aotir.Expr` variants; `lowerType` dispatches on `aotir.Type` variants. Each lower function returns a `gotree.Node` plus an error. Errors are surfaced eagerly — the lowerer does not attempt to recover and produce partial output.

## Stage 2: rendering

`transpiler3/go/emit/Render(sf *gotree.SourceFile) ([]byte, error)` walks the gotree and produces a byte sequence. Rendering uses a `*bytes.Buffer` and writes tokens directly (`fmt.Fprintf` style); indentation is tracked by an `indent int` parameter passed down the walk.

Rendering is not concerned with canonical Go style: it produces correct-but-ugly output (extra blank lines, suboptimal grouping). The output is fed into Stage 3 for canonicalisation.

## Stage 3: formatting

The rendered bytes are piped through `go/format.Source(buf) ([]byte, error)` which is the same function `gofmt` calls. This:

1. Re-parses the bytes into a `go/ast.File`.
2. Re-prints via `go/printer` with canonical spacing.
3. Reorders import groups (stdlib first, then third-party, then local) per the `goimports` convention.

If `go/format.Source` fails to parse, that is a lowerer bug — the structural gotree should never produce invalid Go. The driver surfaces parse errors with the offending bytes so the lowerer can be fixed.

The two-pass design (render then format) is simpler than producing canonical output in one pass. We pay a small CPU cost (one extra parse-and-print) in exchange for a renderer that does not need to understand Go style.

## Closure-environment lifting

Mochi's closures capture by value. Go's closures capture by reference. The lowerer bridges with an explicit env-lifting pass: every closure that captures variables produces a `ClosureEnvStmt` that allocates an env struct on the heap and snapshots the captured variables into it. The closure body reads from the env struct fields rather than directly from the outer scope.

Example:

```mochi
let x = 5
let f = fun(): int => x
```

lowers to:

```go
x := int64(5)
env := struct{ X int64 }{X: x}
f := func() int64 { return env.X }
```

When the closure outlives the function scope (e.g., returned from the function, stored in a struct field, sent to a goroutine), the env is heap-allocated and survives. When the closure stays in scope, Go's escape analysis keeps the env on the stack.

For recursive closures, the env struct holds a `func(...)` field that is filled in after the env is allocated:

```mochi
let fact = fun(n: int): int => if n == 0 { 1 } else { n * fact(n - 1) }
```

lowers to:

```go
env := &struct{ Fact func(int64) int64 }{}
env.Fact = func(n int64) int64 {
    if n == 0 { return 1 }
    return n * env.Fact(n-1)
}
fact := env.Fact
```

This pattern is from MEP-53 (Rust target) adapted for Go. It works because Go function values are nilable references; we allocate the env first, then assign the function value pointing to itself.

## Monomorphisation

aotir's type substitutions are resolved at clower time. The Go lowerer sees concrete types only. Generic functions (e.g., `fun map<T, U>(xs: list<T>, f: fun(T): U): list<U>`) are not lowered to Go generics — they are monomorphised to one Go function per (T, U) instantiation. Exception: the runtime helpers (`mochiruntime.Map[T, U]`) are Go generics so they handle arbitrary (T, U) without per-instantiation copies.

The monomorphisation lives in clower (shared with C / Rust / etc.); the Go lowerer just emits the monomorphic functions clower produced.

## Match to type-switch

Sum-type `match` lowers to Go's type switch on the discriminated interface. The match-to-decision-tree pass (Maranget 2008) runs in clower and produces a sequence of test-and-bind operations; the Go lowerer emits these as a type switch followed by inner `switch`/`if` chains for nested patterns.

```mochi
match shape {
    Circle(r) => area_circle(r)
    Square(s) => s * s
}
```

lowers to:

```go
switch v := shape.(type) {
case *Circle:
    r := v.R
    return area_circle(r)
case *Square:
    s := v.S
    return s * s
}
```

Exhaustiveness is checked at clower time. The Go lowerer trusts the check; it does not emit a default panic branch unless clower flagged the match as inexhaustive.

## Stable item ordering

The gotree's `SourceFile.Decls` field is a `[]Decl` slice. The lowerer appends declarations in a stable order:

1. `package` declaration.
2. `import` block (sorted: stdlib, then `dev.mochilang/runtime/go/...`).
3. Type declarations (records, sums, agent structs), in source order.
4. Variable declarations.
5. Function declarations, in source order.
6. `func main()` last.

Stable ordering means two builds from the same Mochi source produce byte-identical Go source. This is a prerequisite for the reproducibility gate (Phase 16).

## Colour pass

A small colour pass annotates which `go` statements need to wrap calls (the async-colouring lowering) and which struct types need a `Clone()` method (the by-value capture lowering). The pass is single-pass over the gotree post-lower; it adds metadata to nodes rather than mutating them.

The colour pass is intentionally simple. Future sub-phases may extend it to do escape-analysis-driven box / unbox decisions (for boxed closures stored in struct fields), but the initial implementation lets Go's escape analysis handle that.

## Identifier mangling

Mochi identifiers that collide with Go keywords (`func`, `var`, `type`, `range`, `chan`, `select`, `defer`, `go`, `package`, `import`, `return`, `break`, `continue`, `for`, `if`, `else`, `switch`, `case`, `default`, `struct`, `interface`, `map`, `nil`, `true`, `false`) get a trailing underscore: `func_`, `var_`, etc.

Identifiers that collide with Go predeclared names (`int`, `string`, `bool`, `len`, `cap`, `make`, `new`, `append`, `copy`, `delete`, `panic`, `recover`, `print`, `println`) do not get mangled — they shadow the predeclared name within the lowered scope, which is legal Go and matches what a human Go programmer would write.

## Renderer error handling

The two stages that can produce errors are lowering (which surfaces type mismatches the clower missed, or unhandled aotir variants the Go lowerer does not yet implement) and formatting (`go/format.Source` parse errors). Both errors are wrapped with the offending node or byte offset and surfaced to the driver, which writes them to the test output for the gate to catch.
