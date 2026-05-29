---
title: "Language surface: Mochi features mapped to PHP 8.4 lowering obligations"
description: "Every Mochi language feature and the PHP 8.4 construct it maps to, grounded in the phase00-skeleton through phase17-packaging fixture corpus."
sidebar_position: 1
---

# Language surface: Mochi features mapped to PHP 8.4 lowering obligations

Author: research pass for MEP-55 (Mochi-to-PHP 8.4 transpiler).
Date: 2026-05-29 15:00 (GMT+7).
Sources: `transpiler3/php/lower/lower.go`, `transpiler3/php/ptree/nodes.go`,
`tests/transpiler3/php/fixtures/phase01-hello` through
`tests/transpiler3/php/fixtures/phase17-packaging`,
`transpiler3/php/build/phase*_test.go`.

This note documents every Mochi language surface that MEP-55 lowers to
PHP 8.4 source, and the specific PHP construct chosen for each one. The
"what" companion to [[02-design-philosophy]]'s "why" and
[[05-codegen-design]]'s pipeline walkthrough.

## 1. Scalar types

### 1.1 `int`

Mochi `int` maps to PHP `int`. PHP guarantees 64-bit signed integers on
all supported platforms (PHP 8.4 requires LP64 or LLP64; the 32-bit
PHP era ended with 7.x). The lowerer (`phpScalarType` in lower.go) maps
`aotir.TypeInt` to the string `"int"` for parameter and return type
declarations.

Integer division uses `intdiv($a, $b)` rather than `/` to preserve
Mochi's truncating-toward-zero semantics. The `BinaryExpr` node in
ptree sets `IsCall = true` for the `intdiv` operator so the emit pass
renders `intdiv(left, right)` instead of an infix form (nodes.go
`BinaryExpr.PhpString`).

### 1.2 `float`

Mochi `float` maps to PHP `float` (IEEE 754 binary64). All comparisons
emit `===` (strict), never `==`. The PHP `==` operator applies type
coercion that makes `0 == "foo"` true; Mochi semantics require strict
identity, so the lowerer always emits the `===` form via `BinaryExpr`
with Op `"==="`.

Float printing uses a `mochi_print_f64` inline helper (defined in
`runtimeDecls`, lower.go lines 171-184) that mirrors Go's
`strconv.FormatFloat('g', -1, 64)` contract: whole-number values
print without a decimal point (`4.0` becomes `"4"`), NaN prints as
`"NaN"`, and infinities print as `"+Inf"` / `"-Inf"`. The same logic
appears in `Mochi\Runtime\IO::printFloat` in the Phase 15 Composer
package.

### 1.3 `bool`

Mochi `bool` maps to PHP `bool`. The `mochi_print_bool` helper emits
`"true"` or `"false"` (lowercase), not PHP's native empty-string-for-
false convention, matching the vm3 reference output.

### 1.4 `string`

Mochi `string` maps to PHP `string` (a byte sequence; PHP strings are
not UTF-8-aware by default). The `str_contains` builtin lowers to a
`mochi_str_contains` helper (lower.go lines 196-206) that short-circuits
on an empty needle with `return $needle === "" || str_contains(...)`
rather than delegating unconditionally to PHP's built-in, which would
return `true` for empty needles anyway but at a slight cost for the
common-case non-empty check.

String index access (`s[i]`) lowers to `$s[$i]` (PHP string indexing
returns a one-character substring). String length (`len(s)`) lowers
to `strlen($s)`. String concatenation uses PHP's `.` operator via
`BinaryExpr` with Op `"."`.

`StringLit` in ptree uses `strconv.Quote` then replaces `$` with `\$`
to suppress PHP's double-quoted string variable interpolation
(nodes.go `StringLit.PhpString`).

## 2. Collection types

### 2.1 `list<T>`

Mochi lists lower to PHP `array` (0-indexed, packed). The `ArrayLit`
ptree node renders as `[a, b, c]`. The functional `append(xs, v)`
lowers to `ArrayAppendExpr`: `[...$xs, $v]` (spread operator available
since PHP 7.4). The `for-each` loop over a list lowers to
`ForEachStmt`: `foreach ($list as $elem) { ... }`.

### 2.2 `map<K, V>`

Mochi maps lower to PHP associative `array`. `MapPutStmt` in lower.go
emits an `IndexAssignStmt`: `$name[$key] = $value;`. Map literal
construction uses `ArrayLit` with `Keys` and `Values` populated
(nodes.go `ArrayLit.PhpString`).

### 2.3 `set<T>`

Mochi sets lower to PHP `array` of `value => true` pairs. The
`mochi_set_make` helper (lower.go lines 207-223) iterates the input
list and sets `$out[$e] = true`, dropping duplicates. The
`mochi_set_add` helper (lines 224-237) returns a copy with the element
added; PHP's copy-on-write semantics make this cheap.

## 3. Records

Records lower to `final readonly class` with constructor promotion
(PHP 8.0+). Each field becomes a `public TYPE $name` promoted
constructor parameter. The lowerer uses `ClassDecl` in ptree with
`Abstract = false` and `Mutable = false` (the default), which causes
the emit pass to write `final readonly class NAME` (nodes.go
`ClassDecl.PhpString` lines 734-739).

Construction uses PHP 8.0 named arguments: `new Point(x: 1, y: 2)`,
emitted via `NewExpr`. This lets the lowerer pass fields in any order
and keeps the emitted source readable. The `phase04-records` fixture
corpus exercises single-field, multi-field, nested-record, and
record-returning-function cases.

## 4. Sum types (algebraic data types)

Sum types lower to a two-level class hierarchy:

1. An `abstract readonly class NAME` (the base) with an empty body.
   PHP 8.4 requires that a `final readonly` subclass extends an
   `abstract readonly` base; extending a plain (non-readonly) abstract
   class would prevent the subclass from being `readonly`. This
   constraint was discovered during Phase 5 CI and fixed by setting
   `Abstract = true` on the base `ClassDecl`.

2. One `final readonly class NAME_VARIANT extends NAME` per variant,
   with constructor-promoted fields. The variant class name is computed
   by `variantClassName(union, variant)` (lower.go line 934), which
   joins with `_`. For example, union `Shape` with variant `Circle`
   becomes `Shape_Circle`.

Pattern matching (`match`) lowers to a `ChainedIfStmt` using
`instanceof` checks (lower.go `lowerMatchStmt` lines 1083-1133). Each
arm opens with `if ($tmp instanceof Shape_Circle) { ... }`, field
bindings are extracted as `$fieldName = $tmp->fieldName;`, and the
wildcard arm becomes the trailing `else { ... }`.

## 5. Closures and function types

### 5.1 Closures

Mochi closures lower to PHP arrow functions (`fn(...) => ...`),
represented as `ClosureExpr` in ptree (nodes.go lines 888-919). Arrow
functions capture variables from the enclosing scope by value
automatically, which matches Mochi's by-value capture semantics.

The aotir lowerer lifts anonymous functions to top-level definitions
(closure conversion). The PHP lowerer translates a `FunLit` (an
in-scope reference to a lifted closure) into a `ClosureExpr` whose body
is a call to the lifted function name, with capture variables forwarded
as the leading arguments (`lowerFunLit`, lower.go lines 1041-1072).

The `__e->FIELD` sentinel references that the C-target aotir lowerer
injects for capture access are rewritten to plain variable names via
`rewriteEnvRefs` (lower.go lines 750-862).

### 5.2 Function types

PHP cannot express a parameterised callable type at the declaration
site. The `callable` pseudo-type accepts strings, arrays, and closures
alike. The lowerer maps `aotir.TypeFun` to the PHP type `Closure`
(phpParamType, lower.go lines 917-924), which accepts only real closure
values, which is narrower and more correct. PHPStan and Psalm recover
the precise signature from `@param Closure(int): string $f`-style
PHPDoc added in Phase 15.

## 6. Control flow

### 6.1 `if / else`

`IfStmt` in ptree renders as `if (cond) { then } else { else }` with
the else block omitted when nil (nodes.go lines 239-270). This is a
direct 1:1 mapping; no PHP-specific complications.

### 6.2 `while`

`WhileStmt` in ptree renders as `while (cond) { body }`. Mochi's
`while` is direct-mapped (nodes.go lines 272-294).

### 6.3 `for x in start..end`

Range loops lower to `ForRangeStmt`: `for ($x = start; $x < end; $x++)`
(nodes.go lines 296-325). The `<` boundary matches Mochi's exclusive
upper bound.

### 6.4 `for x in collection`

Collection iteration lowers to `ForEachStmt`: `foreach ($coll as $x)`
(nodes.go lines 640-665). Used for list, map (over values), and set
(over keys).

### 6.5 `break` / `continue`

Direct 1:1 mappings: `BreakStmt` and `ContinueStmt` in ptree
(nodes.go lines 328-339).

## 7. User functions

User functions lower to top-level PHP functions prefixed with `mochi__`.
This prefix prevents collisions with the inline runtime helpers
(`mochi_print_i64`, `mochi_str_contains`, `mochi_llm_generate`, and so
on) which all use a single-underscore `mochi_` prefix. The distinction
is deliberate and documented in the MEP-55 spec.

The `lowerFunction` path (lower.go lines 699-744) handles parameter
types including scalars, records, sum-type bases, lists, maps, closures,
and futures. Lifted closures get their capture variables prepended as
leading parameters before the declared parameter list.

Function return types go through `phpParamType` for the same type
vocabulary.

## 8. Query DSL

The query DSL (`from X in list where ... select ...`) is desugared by
the shared aotir lowerer into `LetStmt + ForEachStmt + ArrayAppendExpr`
(imperative gather) plus optional `ListSortAscExpr` and
`ListSliceExpr`. The PHP lowerer drops the C-specific
`QueryScopeStmt` arena wrapper (`lowerStmt` case `*aotir.QueryScopeStmt`
at lower.go lines 473-475) and inlines the body directly, since PHP
relies on reference-counting and cycle collection rather than an arena
allocator.

Query temp variables use the name `$__query1`, `$__query2`, etc.,
minted by the shared aotir desugaring pass.

Order-by lowers to the `mochi_list_sort_asc` helper, which uses PHP's
spaceship operator (`<=>`) for type-agnostic natural ordering. Skip and
take lower to `array_slice`. See [[08-dataset-pipeline]] for details.

## 9. Agents

Phase 9 agents lower to `final class` (no `readonly`, since intent
bodies mutate fields). Each agent field becomes a promoted public
constructor parameter; each `intent` becomes a public instance method.
The aotir `__self->FIELD` sentinel is rewritten to `$this->FIELD` in
both reads (`lowerExpr` VarRef case) and writes (`lowerAssignStmt`).

`spawn AgentType()` constructs a new agent instance with zero-value
fields synthesised from the `AgentDecl` (lower.go `lookupAgentDecl`,
`phpZeroLit`). Agent class names that collide with PHP reserved words
are suffixed with `_` by `phpClassName` (lower.go lines 965-972).

## 10. Streams

Phase 10 streams lower to the `MochiStream` / `MochiSub` inline runtime
classes. `MochiStream` holds a `$subs` array of per-subscriber queues and
a `$limits` array of per-subscriber drop thresholds. `MochiSub` holds a
reference to the parent stream and a subscriber index. See
[[09-agent-streams]] for the full stream design.

## 11. Async (`async` / `await`)

Phase 11 async functions lower to synchronous wrappers. `mochi_future_make`
wraps a value in a `MochiFuture` object; `mochi_future_await` unwraps
it; `mochi_future_await_all` maps over a list of futures. No PHP fibers
or Amp/Revolt are involved. See [[09-agent-streams]] and
[[02-design-philosophy]] for the choice rationale.

## 12. LLM generation

Phase 13 LLM calls lower to `mochi_llm_generate(provider, model, prompt)`.
The inline runtime performs DJB2 hashing via GMP and reads a cassette
file from `MOCHI_LLM_CASSETTE_DIR`. See [[08-dataset-pipeline]] for the
Datalog side and [[12-risks-and-alternatives]] for risks.

## 13. File I/O

Phase 12 `writeFile(path, content)` lowers to `file_put_contents($path,
$content)` (creates or truncates). `appendFile(path, content)` lowers to
`file_put_contents($path, $content, FILE_APPEND)`. These are direct
mappings to PHP built-in functions.

## 14. `panic`

Mochi `panic` lowers to `throw new \RuntimeException($message)`, a
direct PHP exception throw. PHP's `\RuntimeException` is in the global
namespace and requires no import.

## Phase corpus at a glance

| Phase | Fixture directory | Core feature added |
|-------|-------------------|--------------------|
| 00 | `phase00-skeleton` | Empty `mochi_main()` stub + trailing call |
| 01 | `phase01-hello` | `mochi_print_str`, `mochi_print_i64`, `mochi_print_f64`, `mochi_print_bool` |
| 02 | `phase02-scalars` | Literals, let/var, binary/unary ops, `if`, `while`, `for`-range, `break`, `continue` |
| 03 | `phase03-collections` | `list`, `map`, `set`, `for-each`, `append`, index access |
| 04 | `phase04-records` | `final readonly class`, constructor promotion, named args |
| 05 | `phase05-sums` | `abstract readonly class` base, `final readonly class` variants, `match` / `instanceof` |
| 06 | `phase06-closures` | Arrow functions, lifted closures, `mochi__` prefix, function types |
| 07 | `phase07-query` | Query DSL desugaring, `mochi_list_sort_asc`, `array_slice` |
| 08 | `phase08-datalog` | Compile-time Datalog semi-naive evaluation, static array literal |
| 09 | `phase09-agents` | Mutable agent classes, intent methods, `spawn` zero-init |
| 10 | `phase10-streams` | `MochiStream` / `MochiSub`, fan-out, backpressure |
| 11 | `phase11-async` | `MochiFuture` sync wrappers, all-Blue colour pass |
| 12 | `phase12-ffi` | FFI extension, `file_put_contents`, `appendFile` |
| 13 | `phase13-llm` | DJB2 cassette dispatch, GMP hash, `MOCHI_LLM_CASSETTE_DIR` |
| 14 | `phase14-fetch` | `curl_exec` HTTP fetch |
| 15 | `phase15-composer` | PSR-4 autoload, Composer sandbox staging |
| 16 | `phase16-repro` | Byte-equal reproducible builds, `Driver.Deterministic` |
| 17 | `phase17-packaging` | Phar stager, FrankenPHP Caddyfile+Dockerfile, RoadRunner `.rr.yaml`+`worker.php` |
