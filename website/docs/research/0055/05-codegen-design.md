---
title: "Codegen design: aotir → ptree → emit pipeline"
description: "The aotir→ptree→emit pipeline for MEP-55: ptree node types, runtimeFlags struct, mochi__ prefix, emit pass, and Driver.Build wiring."
sidebar_position: 5
---

# Codegen design: aotir → ptree → emit pipeline

Author: research pass for MEP-55 (Mochi-to-PHP 8.4 transpiler).
Date: 2026-05-29 15:00 (GMT+7).
Sources: `transpiler3/php/ptree/nodes.go`,
`transpiler3/php/lower/lower.go`,
`transpiler3/php/emit/emit.go`,
`transpiler3/php/build/build.go`,
`transpiler3/php/colour/colour.go`.

This note walks the full MEP-55 pipeline from Mochi source to PHP 8.4
source file. It is the "how" companion to [[02-design-philosophy]]'s
"why" and [[01-language-surface]]'s "what". See [[06-type-lowering]] for
per-type details and [[10-build-system]] for the Driver.Build entry point.

## 1. Pipeline overview

```
.mochi source
  → parser.Parse              (shared)
  → types.Check               (shared)
  → clower.Lower              (MEP-45 aotir, shared)
  → colour.Compute            (PHP colour pass, all-Blue)
  → lower.Lower               (PHP-specific lowerer → ptree.PhpFile)
  → emit.Emit                 (ptree → main.php on disk)
  → [optional] php main.php   (TargetPhpRun)
```

The pipeline is wired in `Driver.Build` (build.go lines 64-123). Each
step is sequential and deterministic.

## 2. The ptree package

`transpiler3/php/ptree/nodes.go` defines the PHP-specific intermediate
representation. It is not a general PHP AST but a minimal set of node
types sufficient to represent the PHP programs that MEP-55 generates.

### 2.1 Top-level structure: PhpFile

`PhpFile` (nodes.go lines 17-35) has:
- `Namespace string`: PSR-4 namespace (empty for global-namespace files
  in Phase 0-14; non-empty from Phase 15 onward).
- `Uses []string`: `use Foo\Bar;` import statements, sorted and
  de-duplicated by `sortStringsUnique` (nodes.go lines 72-91).
- `Decls []Decl`: ordered list of top-level declarations.
- `TrailingExec []Stmt`: statements after all declarations (used for the
  `mochi_main();` trailing call).

`PhpFile.PhpSource()` (nodes.go lines 38-70) produces the complete PHP
source string. It always opens with:
```php
<?php

declare(strict_types=1);
```

### 2.2 Decl interface

The `Decl` interface has two implementations that appear in emitted code:

- `FuncDecl` (nodes.go lines 113-172): a top-level PHP function with
  optional PHPDoc, name, parameters, return type, and body statements.
- `ClassDecl` (nodes.go lines 706-793): a PHP class. The `Abstract`,
  `Mutable`, and `Extends` flags control whether the emitter writes
  `abstract readonly class`, `final class`, or `final readonly class`.
- `RawDecl` (nodes.go lines 103-110): verbatim PHP source spliced into
  the output. Used for the `MochiStream`, `MochiSub`, and `MochiFuture`
  inline runtime classes whose shape is fixed.

### 2.3 Stmt interface

Key statement nodes:

| Node | PHP rendered form | Used for |
|------|-------------------|----------|
| `ExprStmt` | `<expr>;` | Function calls, method calls |
| `ReturnStmt` | `return <expr>;` | Function return |
| `AssignStmt` | `$name = <value>;` | `let` and `var` bindings |
| `IfStmt` | `if (c) { } else { }` | `if/else` |
| `WhileStmt` | `while (c) { }` | `while` |
| `ForRangeStmt` | `for ($x = s; $x < e; $x++) { }` | `for x in start..end` |
| `ForEachStmt` | `foreach ($s as $x) { }` | `for x in collection` |
| `BreakStmt` | `break;` | `break` |
| `ContinueStmt` | `continue;` | `continue` |
| `ChainedIfStmt` | `if (c1) {} elseif (c2) {} else {}` | `match` arms |
| `IndexAssignStmt` | `$name[$key] = $value;` | Map put |
| `PropAssignStmt` | `$recv->field = $value;` | Agent field mutation |
| `RawStmt` | verbatim text | Inline runtime helper bodies |

`RawStmt.PhpString` (nodes.go lines 351-362) re-indents each line of
`Text` by the requested level, so inline runtime bodies written as
flat strings are indented correctly in the final output.

### 2.4 Expr interface

Key expression nodes:

| Node | PHP rendered form | Used for |
|------|-------------------|----------|
| `CallExpr` | `callee(args)` | Function calls |
| `StaticCallExpr` | `Class::method(args)` | Static method calls |
| `MethodCallExpr` | `$recv->method(args)` | Agent intent calls |
| `IdentExpr` | bare name | Function names, class names, constants |
| `VarExpr` | `$name` | Variable references |
| `StringLit` | `"..."` | String literals (with `$` escaping) |
| `IntLit` | integer | Integer literals |
| `FloatLit` | Go 'g' format | Float literals |
| `BoolLit` | `true` / `false` | Boolean literals |
| `NullLit` | `null` | Null seed for uninitialised bindings |
| `RawExpr` | verbatim text | Edge cases |
| `BinaryExpr` | `(left op right)` or `op(left, right)` | All binary ops |
| `UnaryExpr` | `(op operand)` | Negation, logical not |
| `CastExpr` | `(type) expr` | `int()` cast |
| `ArrayLit` | `[a, b]` or `[k => v]` | List, map, set literals |
| `ArrayAppendExpr` | `[...$inner, $tail]` | `append(xs, v)` |
| `IndexExpr` | `$recv[$idx]` | List/map/string index access |
| `NewExpr` | `new ClassName(field: val)` | Record construction |
| `ClosureExpr` | `fn(params): ret => body` | Closure literals |
| `PropAccessExpr` | `$recv->field` | Record field / agent field read |
| `InstanceOfExpr` | `($recv instanceof Class)` | Match arm discriminator |

All `BinaryExpr` and `UnaryExpr` nodes wrap the result in parentheses
unconditionally (nodes.go lines 545-549, 561-563), so operator
precedence in the source program is preserved without the lowerer needing
to track PHP's precedence table.

## 3. The runtimeFlags struct

`runtimeFlags` (lower.go lines 25-37) is a boolean struct embedded in the
`lowerer`. Each flag is set to `true` the first time the lowerer encounters
a construct that needs the corresponding inline runtime helper:

```go
type runtimeFlags struct {
    printStr    bool
    printInt    bool
    printBool   bool
    printF64    bool
    strContains bool
    setMake     bool
    setAdd      bool
    listSortAsc bool
    streams     bool
    async       bool
    llm         bool
}
```

The `runtimeDecls()` method (lower.go lines 147-428) checks each flag and
appends a `FuncDecl` or `RawDecl` to the output only for the helpers that
are actually used. This means a program that uses only integers and strings
does not emit the `mochi_set_make`, `mochi_stream_make`, or
`mochi_llm_generate` helpers. The output is minimal.

## 4. The `mochi__` prefix for user functions

User-defined Mochi functions are emitted with a `mochi__` (double
underscore) prefix. The inline runtime helpers use a `mochi_` (single
underscore) prefix. This ensures that a user function named `llm_generate`
emits as `mochi__llm_generate`, which does not collide with the runtime
helper `mochi_llm_generate`.

PHP's global function namespace is flat; there are no modules or
namespaces until Phase 15 adds PSR-4. The prefix convention is the
only collision-avoidance mechanism in Phase 0-14.

## 5. The lowerer: entry point and ordering

`lower.Lower(prog, colours)` (lower.go lines 52-143) is the entry point.
It processes declarations in this order:

1. **Records** (`prog.Records`): one `final readonly class` per record.
2. **Sum types** (`prog.Unions`): one `abstract readonly class` base plus
   `final readonly class` variants per union.
3. **Agents** (`prog.Agents`): one mutable `final class` per agent.
4. **Non-main user functions** (`prog.Functions` excluding `prog.Main`):
   in source order for Phase 16 reproducibility.
5. **Runtime declarations** (`runtimeDecls()`): inline helpers used by
   the program.
6. **Main function** (`prog.Functions[prog.Main]`): lowered as
   `mochi_main(): void`.
7. **Trailing call**: `mochi_main();` appended to `TrailingExec`.

Source order is preserved for categories 1-4 because Go slices preserve
insertion order and the aotir lowerer (MEP-45) produces deterministic
slice orderings. Phase 16 relies on this.

## 6. The emit pass

`emit.Emit(file, workDir, name)` (emit/emit.go) is intentionally trivial:
it calls `file.PhpSource()` to get the complete PHP source string and
writes it to `<workDir>/<name>.php`. The entire rendering logic lives in
the ptree nodes' `PhpString` methods.

The emit pass is a one-line filesystem operation layered on top of the
ptree's self-describing rendering. This keeps the emit package small
(36 lines) and ensures there is only one place where the PHP text is
generated: the ptree node methods.

## 7. The colour pass

`colour.Compute(prog)` (colour/colour.go lines 39-45) assigns every
function the `Blue` colour. The `ColourMap` is passed to `lower.Lower`
but currently ignored (the signature parameter is named `_`). The
comment in colour.go explains the design: Phase 11 shipped async as
synchronous value wrappers, so no function ever needs the `Red` (async)
treatment. The pass exists for symmetry with the other transpiler3
targets and to provide a single flip-point for a future async revival.

## 8. Emit shapes for key constructs

### 8.1 Function declaration

```php
/**
 * Generated Mochi entry point. Do not edit by hand.
 */
function mochi_main(): void
{
    // body
}
```

The PHPDoc is optional; the lowerer only adds it for `mochi_main`. User
functions (`mochi__foo`) do not get a docblock in Phase 0-14.

### 8.2 Record class declaration

```php
/**
 * Mochi record `Point`. Generated; do not edit by hand.
 */
final readonly class Point
{
    public function __construct(
        public int $x,
        public int $y,
    ) {}
}
```

Constructor promotion with `public TYPE $field` for each record field.

### 8.3 Sum-type hierarchy

```php
/**
 * Mochi sum type `Shape` base class. Generated; do not edit by hand.
 */
abstract readonly class Shape
{
}

/**
 * Variant `Circle` of `Shape`.
 */
final readonly class Shape_Circle extends Shape
{
    public function __construct(
        public float $radius,
    ) {}
}
```

### 8.4 Match statement

```php
$__mochi_match_1 = $shape;
if (($__mochi_match_1 instanceof Shape_Circle)) {
    $r = $__mochi_match_1->radius;
    // arm body
} elseif (($__mochi_match_1 instanceof Shape_Rect)) {
    $w = $__mochi_match_1->width;
    $h = $__mochi_match_1->height;
    // arm body
} else {
    // wildcard body
}
```

The temp variable `$__mochi_match_N` is minted by the `matchSeq` counter
(lower.go line 45), which is monotonic per lowerer instance, ensuring
nested match statements get distinct temps.

### 8.5 While loop

```php
while ($i < 10) {
    // body
    $i = ($i + 1);
}
```

### 8.6 For-range loop

```php
for ($i = 0; $i < 10; $i++) {
    // body
}
```
