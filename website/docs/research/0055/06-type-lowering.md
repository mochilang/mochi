---
title: "Type lowering: per-Mochi-type PHP 8.4 lowering rules"
description: "Complete per-type lowering rules for every Mochi type onto PHP 8.4: scalars, collections, records, sum types, closures, function types, Result, panic. Grounded in lower.go."
sidebar_position: 6
---

# Type lowering: per-Mochi-type PHP 8.4 lowering rules

Author: research pass for MEP-55 (Mochi-to-PHP 8.4 transpiler).
Date: 2026-05-29 15:00 (GMT+7).
Sources: `transpiler3/php/lower/lower.go` (`phpScalarType`,
`phpParamType`, `lowerBinaryExpr`, `runtimeDecls`, `lowerRecord`,
`lowerUnion`, `lowerFunLit`), `transpiler3/php/ptree/nodes.go`.

This note gives the complete lowering rule for every Mochi type. Each
entry has: the PHP 8.4 spelling, the rationale, and the generated code
shape. Grounded in `lower.go`.

## 1. `int` → PHP `int`

**Spelling**: `int` (parameter and return type declaration, `phpScalarType`
lower.go lines 868-870).

**Why**: PHP guarantees 64-bit signed integers on all supported 64-bit
platforms. No boxing, no overflow wrapper needed for the common case.
Integer overflow (> PHP_INT_MAX = 2^63-1) is a known risk for DJB2
cassette keys; that specific path uses GMP. See [[12-risks-and-alternatives]].

**Integer division**: `intdiv($a, $b)` via `BinaryExpr{IsCall: true, Op:
"intdiv"}` (nodes.go `BinaryExpr.PhpString` lines 545-548). PHP's `/`
operator produces a float when the result is not an integer, which would
break Mochi's truncating semantics.

**Modulo**: `%` operator. `BinaryExpr{Op: "%"}`.

**Comparison**: all comparisons use `===` (strict). `BinaryExpr{Op:
"==="}`. This applies to int, float, bool, and string equally.

**Literals**: `IntLit.PhpString()` calls `strconv.FormatInt(e.Value, 10)`
(nodes.go lines 480-482).

## 2. `float` → PHP `float`

**Spelling**: `float`.

**Why**: PHP `float` is IEEE 754 binary64, matching Mochi's `float`.

**Float comparison**: uses `===`. PHP's `==` applies coercion (`0.0 ==
0` is true under `==`, false in Mochi).

**Float literals**: `FloatLit.PhpString()` uses `strconv.FormatFloat(e.Value,
'g', -1, 64)` (nodes.go lines 491-494). This is the same format as Go's
default float printing, ensuring round-trip fidelity.

**Special values**: `mochi_print_f64` handles NaN, +Inf, -Inf (lower.go
lines 174-184):
```php
if (is_nan($value)) { echo "NaN\n"; return; }
if (is_infinite($value)) { echo $value < 0 ? "-Inf\n" : "+Inf\n"; return; }
if ((float) (int) $value === $value && abs($value) < 1.0e15) { echo (int) $value, "\n"; return; }
echo $value, "\n";
```

**Infinity literals in source**: `+Inf` lowers to `fdiv(1, 0)`, `-Inf`
to `fdiv(-1, 0)`, `NaN` to `fdiv(0, 0)` (these are compile-time constant
expressions in PHP).

## 3. `bool` → PHP `bool`

**Spelling**: `bool`.

**Print contract**: `mochi_print_bool` emits `"true"` or `"false"` (lower-
case), not PHP's empty-string-for-false (lower.go lines 185-195):
```php
echo $value ? "true\n" : "false\n";
```

**Literals**: `BoolLit{Value: true}` → `true`, `BoolLit{Value: false}` →
`false` (nodes.go lines 503-508).

## 4. `string` → PHP `string`

**Spelling**: `string`. PHP strings are byte sequences; no built-in UTF-8
awareness. `ext-mbstring` provides UTF-8 functions when needed.

**`str_contains`**: the `mochi_str_contains` helper (lower.go lines
196-206) short-circuits on empty needle:
```php
return $needle === "" || str_contains($haystack, $needle);
```
This matches Mochi's semantics (empty string is contained in any string)
without relying on PHP's `str_contains` returning true for empty needles
(which it does, but the explicit check is more readable).

**`str.index`**: `IndexExpr{Receiver: $s, Index: $i}` → `$s[$i]`.

**`len(s)`**: `strlen($s)`. PHP's `strlen` counts bytes.

**Concatenation**: `BinaryExpr{Op: "."}` → `($a . $b)`.

**String literals**: `StringLit.PhpString()` calls `strconv.Quote` then
replaces `$` with `\$` (nodes.go lines 451-471) to prevent PHP variable
interpolation inside double-quoted strings.

## 5. `list<T>` → PHP `array` (0-indexed)

**Spelling**: `array` in type declarations.

**Literals**: `ArrayLit{Elems: [a, b, c]}` → `[a, b, c]`.

**`append(xs, v)`**: `ArrayAppendExpr{Inner: $xs, Tail: $v}` → `[...$xs, $v]`
(nodes.go lines 617-625). PHP's spread operator (7.4) makes this a
single-expression non-mutating append.

**`for x in list`**: `ForEachStmt{Var: "x", Source: $list}` →
`foreach ($list as $x)`.

**Index access**: `IndexExpr` → `$xs[$i]`.

**`len(list)`**: `count($list)`.

## 6. `map<K, V>` → PHP `array` (string-keyed)

**Spelling**: `array`.

**Literals**: `ArrayLit{Keys: [k1, k2], Values: [v1, v2]}` →
`[k1 => v1, k2 => v2]`. PHP preserves insertion order for associative
arrays.

**Map put**: `MapPutStmt{Name: "m", Key: $k, Value: $v}` →
`IndexAssignStmt` → `$m[$k] = $v;` (lower.go lines 519-527).

**Map get**: `IndexExpr{Receiver: $m, Index: $k}` → `$m[$k]`.

**`for k, v in map`**: The aotir lowers this to `ForEachStmt` over the
map value; key-value pairs use the `foreach ($m as $k => $v)` form.

## 7. `set<T>` → PHP `array` of `value => true`

**Spelling**: `array`.

**Representation**: a PHP associative array where each element is the key
and the value is `true`. This preserves insertion order (PHP preserves
array insertion order), allows O(1) membership tests (`isset($s[$e])`),
and survives PHP serialization round-trips.

**`mochi_set_make([1, 2, 1])`** → `[1 => true, 2 => true]` (lower.go
lines 207-223). Duplicates dropped on first occurrence.

**`mochi_set_add($s, 4)`** → returns copy with `$s[4] = true` (lower.go
lines 224-237). PHP copy-on-write makes this cheap.

## 8. `record` → `final readonly class` with constructor promotion

**Spelling**: `final readonly class NAME` (or `abstract readonly class`
for sum-type bases, see §9 below).

**Construction**: `NewExpr{Class: "Point", Args: [{Name: "x", Value: 1}]}`
→ `new Point(x: 1, y: 2)`. Named arguments (PHP 8.0+) let the lowerer
pass fields in any order.

**Field access**: `PropAccessExpr{Receiver: $p, Field: "x"}` → `$p->x`.

**Lower path**: `lowerRecord(r)` (lower.go lines 1135-1149) maps each
`aotir.RecordField` to a `ptree.ClassField{TypeName, Name}` via
`phpParamType`. The `ClassDecl` emitter writes `public readonly TYPE $NAME`
via constructor promotion.

## 9. Sum types → `abstract readonly class` + `final readonly class` variants

**Base class**: `ClassDecl{Abstract: true}` → `abstract readonly class
NAME {}` with empty body. The PHPDoc note is
`"Mochi sum type \`NAME\` base class. Generated; do not edit by hand."`.

**Variant class**: `ClassDecl{Extends: "NAME"}` → `final readonly class
NAME_VARIANT extends NAME` with constructor-promoted fields.

**Variant class name**: `variantClassName(union, variant)` → `union + "_" +
variant` (lower.go line 934). So `Shape` / `Circle` → `Shape_Circle`.

**PHP reserved name collision**: `phpClassName(name)` suffixes `_` for any
name in `phpReservedClassNames` (lower.go lines 939-972). For example,
`agent Switch` emits `final class Switch_`, as confirmed by the
`agent_bool.mochi` fragment test.

**Pattern matching**: `lowerMatchStmt` (lower.go lines 1083-1133) emits
`ChainedIfStmt` with `InstanceOfExpr` per arm:
```php
$__mochi_match_1 = $shape;
if (($__mochi_match_1 instanceof Shape_Circle)) {
    $r = $__mochi_match_1->radius;
    // body
} elseif (($__mochi_match_1 instanceof Shape_Rect)) {
    // body
}
```
Field bindings become `AssignStmt` assignments at the top of each arm body.
Guards (Phase 5.1 feature) are explicitly rejected with an error message.

## 10. Closures → PHP arrow functions (`fn(...)`)

**Spelling**: `Closure` in type declarations (phpParamType, lower.go line
921).

**Arrow function form**: `ClosureExpr{Params, ReturnType, Body}` →
`fn(int $p0): int => callee($p0)` (nodes.go lines 900-918).

**Capture semantics**: PHP arrow functions capture variables from the
enclosing scope by value automatically. No `use ($x)` clause needed.
This matches Mochi's by-value capture semantics.

**Lifting**: the aotir lowerer lifts anonymous functions to top-level
definitions. The PHP lowerer translates a `FunLit` to a `ClosureExpr`
whose body is a call to the lifted function name, with capture variables
forwarded as leading arguments (`lowerFunLit`, lower.go lines 1041-1072).

**env-ref rewriting**: The C-target aotir lowerer injects `__e->field`
for capture access. `rewriteEnvRefs` (lower.go lines 750-862) renames
these to plain variable names so the PHP arrow function captures them
from the enclosing scope by name.

## 11. Function types → `Closure` in declarations

PHP cannot express a parameterised callable signature at the declaration
site. `callable` accepts strings, arrays, and closures indiscriminately.
The lowerer maps `aotir.TypeFun` to `Closure` (phpParamType lower.go line
921), which accepts only real closure objects.

Precise signatures are recovered by PHPStan and Psalm from `@param
Closure(int, string): bool $f` PHPDoc annotations added in Phase 15.

## 12. `Result<T, E>` → `final readonly class Ok` / `final readonly class Err`

Result types lower via the sum-type path: the union `Result` has variants
`Ok` (carrying a value field) and `Err` (carrying an error field). Both
become `final readonly class` extending `abstract readonly class Result`.
No PHP exception is thrown; error handling is explicit discriminated-union
matching.

## 13. `panic` → `throw new \RuntimeException`

Mochi `panic(msg)` lowers to `throw new \RuntimeException($msg)`.
`\RuntimeException` is in the global namespace and requires no import.
The leading `\` prevents ambiguity with any user-defined `RuntimeException`
in a namespace (though in Phase 0-14 the global namespace is used
throughout).

## 14. `MochiStream` (Phase 10) → custom final class

**Type spelling**: `MochiStream` for stream parameters, `MochiSub` for
subscriber parameters (phpParamType lower.go lines 905-910).

**Runtime classes**: emitted as `RawDecl` fragments by `runtimeDecls`
when `l.runtime.streams == true`. No ptree class hierarchy for this
one; the shape is fixed and written verbatim.

## 15. `MochiFuture` (Phase 11) → custom final class

**Type spelling**: `MochiFuture` (phpParamType lower.go line 913).

**Runtime class**: `final class MochiFuture { public function __construct(
public mixed $value) {} }`, emitted as `RawDecl` when
`l.runtime.async == true`.

## 16. Type mapping summary

| Mochi type | PHP 8.4 declaration type | Notes |
|------------|--------------------------|-------|
| `int` | `int` | `intdiv` for `/` |
| `float` | `float` | `===` comparison, special print |
| `bool` | `bool` | lowercase literal print |
| `string` | `string` | byte sequence, `mochi_str_contains` |
| `list<T>` | `array` | packed 0-indexed |
| `map<K,V>` | `array` | string-keyed assoc |
| `set<T>` | `array` | `value => true` |
| record | `final readonly class` | constructor promotion |
| sum type base | `abstract readonly class` | empty body, PHP 8.4 |
| sum type variant | `final readonly class extends BASE` | named-arg ctor |
| closure / fun type | `Closure` | arrow fn, PHPDoc for signature |
| `Result<T,E>` | sum type Ok/Err | no exception path |
| `panic` | `throw new \RuntimeException` | no import needed |
| stream | `MochiStream` | inline runtime class |
| subscriber | `MochiSub` | inline runtime class |
| future | `MochiFuture` | inline runtime class |
