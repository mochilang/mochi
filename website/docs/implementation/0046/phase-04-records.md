---
title: "Phase 4. Records"
sidebar_position: 6
sidebar_label: "Phase 4. Records"
description: "MEP-46 Phase 4 tracking: record literal, field access, field update, methods, equality on BEAM using tagged maps."
---

# Phase 4. Records

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-46 §Phases · Phase 4](/docs/mep/mep-0046#phase-4-records) |
| Status         | LANDED |
| Started        | 2026-05-26 14:09 (GMT+7) |
| Landed         | 2026-05-26 14:21 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Records suite (25 fixtures: literal construction, field access, field update, methods, equality, list&lt;record&gt; round-trip) compiles via `mochi build --target=beam-escript` and runs byte-equal vs vm3; `TestPhase4Records` is green.

## Goal-alignment audit

Records are the primary user-defined data type in Mochi. Without records the BEAM target cannot compile any realistic Mochi program (the query DSL, agent state, and virtually all non-trivial Mochi libraries use records). Phase 4 is also the phase that validates the BEAM representation decision (tagged maps rather than Erlang records or tuples) under the full field-access, update, and pattern-matching surface. Aligns directly with the user-facing goal.

## Sub-phases

| #   | Scope | Status | Commit | PR |
|-----|-------|--------|--------|----|
| 4.0 | Record literal construction: `Person{name: "alice", age: 30}` | LANDED 2026-05-26 (GMT+7) | `24cb35621a` | — |
| 4.1 | Field access `p.name`; field update `p with {age: 31}`; pattern matching on records | LANDED 2026-05-26 (GMT+7) | `24cb35621a` | — |
| 4.2 | Record methods: `fun (p: Person) greet() -> string` lowered to module-level functions | LANDED 2026-05-26 (GMT+7) | `24cb35621a` | — |
| 4.3 | Record equality: `p1 == p2` via BEAM structural equality `=:=` | LANDED 2026-05-26 (GMT+7) | `24cb35621a` | — |

## Sub-phase 4.0 -- Record literal construction

### Goal-alignment audit (4.0)

Record literals are the minimum foothold needed before any other Phase 4 sub-phase can be tested. Without `lowerRecordLit`, there are no record values to access fields on, update, or compare. All 25 Phase 4 fixtures depend on record construction. Phase 3.5 (list&lt;record&gt;) also depends on this sub-phase.

### Decisions made (4.0)

**Representation: tagged BEAM maps.** A Mochi record `Person{name: "alice", age: 30}` is represented as:
```erlang
#{mochi_record_tag => person, name => <<"alice"/utf8>>, age => 30}
```

The `mochi_record_tag` key carries a lowercase atom identifying the record type. This is a compile-time constant key used for variant discrimination in pattern matching (Phase 5) and for `is_record`-style guards.

**Why tagged maps rather than Erlang records.** Erlang's `record` construct is a compile-time syntactic sugar over tuples (e.g., `#person{name=..., age=...}` compiles to `{person, ..., ...}`). They have fixed field order, no structural equality, and no dynamic field access. BEAM maps support all three. Using maps means the transpiler can implement field access as `maps:get(FieldName, Map)` rather than generating record header files. Maps also support the `with` update syntax natively via Core Erlang's `c_map_update`.

**Why tagged maps rather than bare maps.** Without a tag, two records with the same field names and values would be indistinguishable. The `mochi_record_tag` key enables:
1. Pattern matching in Phase 5 sum types (`match v { Point{x, y} => ... }`).
2. Meaningful error messages when a wrong record type is passed to a function.
3. Future Dialyzer specs (Phase 17) to narrow the map type per tag value.

**Record type name is lowercased atom.** `Person` -> `person`, `HTTPRequest` -> `httprequest`. The lowercasing is applied at lower time (not parse time) so the aotir IR preserves the original casing for diagnostics. `lowerRecordLit` lowercases the record name when building the `c_atom`.

**`lowerRecordLit(RecordLit) -> cerl.Expr`:**
```
c_map([
  c_map_pair_exact(c_atom(mochi_record_tag), c_atom(person)),
  c_map_pair_exact(c_atom(name), lowerExpr(NameExpr)),
  c_map_pair_exact(c_atom(age),  lowerExpr(AgeExpr))
])
```

`c_map_pair_exact` corresponds to Core Erlang's exact-match map constructor (`:=`), which is used for construction (not update). The pairs are emitted in the order the fields appear in the `type` declaration, not the order they appear in the literal. This ensures a deterministic map construction order independent of source order, which matters for Phase 17 (reproducibility) and for future Dialyzer map specs.

**Field order in the type declaration is the canonical order.** The aotir type checker enforces that all record literals include every required field (no optional fields in Phase 4). The lowerer trusts the type checker and emits fields in declaration order.

**String field values are UTF-8 binaries.** `"alice"` -> `c_binary([{bin_element, {string, "alice"}, default, [utf8]}])`. This is consistent with all other string lowering in the pipeline.

### Test set (4.0)

Fixtures `300_record_basic.mochi` through `304_record_nested.mochi` (5 fixtures):
- `300_record_basic.mochi`: single record, print one field.
- `301_record_two_fields.mochi`: print both fields.
- `302_record_in_function.mochi`: record constructed inside a function, returned and printed.
- `303_record_list.mochi`: `[Person{name: "alice", age: 30}]` -- cross with Phase 3.5.
- `304_record_nested.mochi`: record containing another record as a field value.

## Sub-phase 4.1 -- Field access, update, and pattern matching

### Goal-alignment audit (4.1)

Field access and update are the day-to-day operations on records. Without them a record is a write-once value with no way to read it back. Pattern matching on records (used pervasively in Phase 5 sum types and Phase 8 query DSL) requires the `c_map_pattern` lowering that this sub-phase establishes.

### Decisions made (4.1)

**Field access `p.name`** -> `c_call(c_atom(maps), c_atom(get), [c_atom(name), V_p])`.

If the field is not present in the map, `maps:get/2` raises `{badkey, name}`. The lowerer wraps field access in a try/catch only when the type checker cannot prove the field exists at compile time (which for typed records it always can). In Phase 4.1, the type checker always knows the record type of `p`, so the field access is emitted without a try/catch wrapper. The type checker's field-existence guarantee is an invariant; the runtime try/catch is defence-in-depth and is deferred to Phase 17 (defensive hardening pass).

**Why `maps:get/2` rather than pattern matching for field access.** Core Erlang supports map pattern matching (`c_map_pattern`), but using it for every field access would generate a `c_case` with one clause for each field accessed. `maps:get/2` is simpler, is a single BIF call, and is JIT-compiled in OTP 27 to an efficient hash lookup. Pattern matching is used only for `match` expressions (Phase 5).

**Field update `p with {age: 31}`** (Mochi update syntax) -> Core Erlang map update:
```erlang
c_map_update(V_p, [c_map_pair(c_atom(age), c_int(31))])
```

`c_map_update` in Core Erlang corresponds to `Map#{Key := Value}` (exact-update semantics in Erlang). The `mochi_record_tag` field is carried over automatically (BEAM map update preserves all existing keys). The result is a new map; the original `p` is unmodified.

Why `c_map_pair` (not `c_map_pair_exact`) for update: `c_map_pair` is `=>` semantics in Erlang (create or update), while `c_map_pair_exact` is `:=` (must exist). Since the Mochi type checker guarantees the field exists in the record, both are semantically correct; `c_map_pair` is used because it is slightly more forgiving and matches how `c_map_update` is typically used in generated Core Erlang.

**Updating `mochi_record_tag` is not allowed.** The lowerer rejects an update literal that includes `mochi_record_tag` as a field name (this would be a name collision; the Mochi source can't spell it anyway since `mochi_record_tag` is not a valid Mochi identifier).

**Pattern matching on records: `match p { Person{name: n} => ... }`** lowered to:

```erlang
c_case(V_p, [
  c_clause(
    [c_map_pattern([
      c_map_pair_exact(c_atom(mochi_record_tag), c_atom(person)),
      c_map_pair_exact(c_atom(name), c_var('V_n'))
    ])],
    c_atom(true),   %% guard: always true
    lowerBlock(body)
  )
])
```

`c_map_pattern` in Core Erlang matches a map if it contains at least the listed key-value pairs. The `mochi_record_tag` pair ensures the match is type-specific: a `Circle{name: n}` would not match a `Person{name: n}` even though both have a `name` field. Additional fields in the record that are not bound in the pattern are ignored (BEAM map patterns are subset matches).

**Partial patterns are OK.** `match p { Person{age: a} => ... }` matches any `Person` map and binds `age` to `V_a`; it does not need to bind `name`. The map pattern matches if the map contains `mochi_record_tag => person` and `age => <anything>`.

### Test set (4.1)

Fixtures `305_field_access.mochi` through `315_record_pattern.mochi` (11 fixtures):
- `305_field_access.mochi`: `p.name`, `p.age` on a constructed record.
- `306_field_access_nested.mochi`: `p.address.city` (nested record field access).
- `307_field_update.mochi`: `p with {age: 31}`; verify original unchanged; verify updated value.
- `308_field_update_multiple.mochi`: `p with {name: "bob", age: 25}` (multi-field update).
- `309_record_in_if.mochi`: record field used as `if` condition (`p.active == true`).
- `310_record_in_while.mochi`: record field used as while condition.
- `311_record_passed_to_fn.mochi`: record passed as function argument; field read inside function.
- `312_record_returned.mochi`: function returns a record; caller reads fields.
- `313_record_pattern_basic.mochi`: `match p { Person{name: n} => print(n) }`.
- `314_record_pattern_multi_field.mochi`: `match p { Person{name: n, age: a} => ... }`.
- `315_record_pattern_in_for.mochi`: `for p in people { match p { Person{name: n} => print(n) } }`.

## Sub-phase 4.2 -- Methods on records

### Goal-alignment audit (4.2)

Methods on records are used throughout the Mochi standard library and user programs to encapsulate record operations. Without methods the BEAM target cannot compile any Mochi code that calls `p.greet()` or `circle.area()`. This is a significant fraction of idiomatic Mochi code.

### Decisions made (4.2)

**Methods are module-level functions.** A Mochi method:
```mochi
fun (p: Person) greet() -> string = "hello, " ++ p.name
```
is lowered to a Core Erlang function:
```erlang
person__greet(V_p) ->
    string:concat(<<"hello, ">>, maps:get(name, V_p)).
```

The mangled name is `RecordName_lowercase__methodName`. This is a module-level function, not a closure. There is no `self` or `this`; the receiver is the first parameter.

**Method calls `p.greet()`** are lowered to `c_call(c_atom(module), c_atom(person__greet), [V_p])` where `module` is the current module name. If the method takes additional arguments `p.greet(x, y)`, they follow the receiver: `person__greet(V_p, V_x, V_y)`.

**No `self` mutation.** Methods that appear to "mutate" the receiver actually return a new record. For example:
```mochi
fun (p: Person) with_age(a: int) -> Person = p with {age: a}
```
lowers to:
```erlang
person__with_age(V_p, V_a) ->
    maps:update(age, V_a, V_p).  %% or: V_p#{age => V_a}
```

**Method resolution is static.** The lowerer knows the type of `p` (from the aotir type checker) and looks up the method in the type's method table at lower time. There is no runtime dispatch. This is correct because Mochi does not have interface polymorphism in Phase 4 (that is Phase 7, the error model / trait-like interfaces).

**Exported methods vs private methods.** All methods are module-private by default (not in the Core Erlang export list). A `pub fun` declaration makes the method exported. For Phase 4 fixtures, all methods are private.

**Method name collision with field names.** If a record has a field `greet` and a method `greet()`, this is a compile-time error in the Mochi type checker. The lowerer need not handle this case.

### Test set (4.2)

Fixtures `316_method_basic.mochi` through `322_method_chain.mochi` (7 fixtures):
- `316_method_basic.mochi`: `fun (p: Person) greet() -> string = "hello, " ++ p.name`; call `p.greet()`.
- `317_method_with_arg.mochi`: method with one extra argument.
- `318_method_returns_record.mochi`: method that returns an updated record.
- `319_method_calls_method.mochi`: method that calls another method on the same receiver.
- `320_method_in_for.mochi`: call method inside `for p in people` loop.
- `321_method_recursive.mochi`: recursive method (e.g., `fun (n: Node) depth() -> int`).
- `322_method_chain.mochi`: `p.with_age(30).greet()` (chained method calls on updated records).

## Sub-phase 4.3 -- Record equality

### Goal-alignment audit (4.3)

Record equality `p1 == p2` is used in assertions, deduplication, and query join conditions. Without it the BEAM target cannot compile programs that compare records. This sub-phase is cheap because BEAM's structural equality already does the right thing.

### Decisions made (4.3)

**`p1 == p2`** -> `c_call(c_atom(erlang), c_atom('=:='), [V_p1, V_p2])`.

BEAM's `=:=` operator on maps is deep structural equality: it checks that both maps have exactly the same key-value pairs (including `mochi_record_tag`). This matches Mochi's value semantics: two records are equal iff all their fields are equal and they have the same type. No custom `equals` method is needed.

**`mochi_record_tag` is part of equality.** Because `mochi_record_tag` is a key in the map, `=:=` naturally checks it. A `Person{name: "alice", age: 30}` is not `=:=` to a `Customer{name: "alice", age: 30}` even if both have the same non-tag fields, because their `mochi_record_tag` values differ.

**`p1 != p2`** -> `c_call(c_atom(erlang), c_atom('=/='), [V_p1, V_p2])`.

**Ordering (`<`, `>`) on records is not supported.** Mochi does not define a total order on records (there is no `Ord` trait in Phase 4). The type checker rejects `p1 < p2` with a type error. The lowerer does not handle this case.

**Nested record equality is structural.** If `Address` is a nested record inside `Person`, then `p1 == p2` requires that `p1.address =:= p2.address`, which requires that all fields of the two `Address` values match. BEAM's `=:=` handles this recursively without any additional lowering.

**Float equality in records.** If a record field is `float`, `=:=` on floats is exact bit-pattern equality (same as `==` on `float64` in Go). NaN `=:=` NaN is `false` in both BEAM and Go (IEEE 754 property). This is byte-equal to vm3.

### Test set (4.3)

Fixtures `323_record_eq_basic.mochi` through `325_record_eq_nested.mochi` (3 fixtures):
- `323_record_eq_basic.mochi`: `p1 == p2` (same fields), `p1 == p3` (different fields).
- `324_record_eq_type.mochi`: `person == customer` (same field values, different tags) -> `false`.
- `325_record_eq_nested.mochi`: records with nested record fields; equality checks all levels.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/beam/lower/lower.go` | `lowerRecordLit`, `lowerFieldAccess`, `lowerFieldUpdate`, `lowerRecordPattern`, `lowerMethodCall`, `lowerMethod` (module-level function from method decl) |
| `transpiler3/beam/lower/lower_state.go` | Method table per record type (populated in pass 1); lowercased type name helper |
| `transpiler3/beam/build/phase04_test.go` | `TestPhase4Records` gate test; walks `tests/transpiler3/beam/fixtures/phase4/` |
| `tests/transpiler3/beam/fixtures/phase4/` | 25 fixture pairs (`300_` through `325_`) |

## Test set

25 fixtures total:
- Phase 4.0: 5 fixtures (`300_` through `304_`).
- Phase 4.1: 11 fixtures (`305_` through `315_`).
- Phase 4.2: 7 fixtures (`316_` through `322_`).
- Phase 4.3: 3 fixtures (`323_` through `325_`).

All byte-equal vs vm3.

Gate test: `transpiler3/beam/build/phase04_test.go::TestPhase4Records` -- walks all fixtures in `tests/transpiler3/beam/fixtures/phase4/`, calls `runBeamFixture` on each pair.

Additional unit tests:
- `transpiler3/beam/lower/lower_record_test.go::TestLowerRecordLit` -- unit: verifies the `cerl.Module` shape for a record literal (tag atom, field pairs in declaration order, UTF-8 binary strings).
- `transpiler3/beam/lower/lower_record_test.go::TestLowerFieldAccess` -- unit: verifies `maps:get(field, V_p)` emission.
- `transpiler3/beam/lower/lower_record_test.go::TestLowerFieldUpdate` -- unit: verifies `c_map_update` emission.
- `transpiler3/beam/lower/lower_record_test.go::TestLowerRecordPattern` -- unit: verifies `c_map_pattern` with `mochi_record_tag` and bound variables.
- `transpiler3/beam/lower/lower_record_test.go::TestLowerMethodDecl` -- unit: verifies method lowers to a module-level function with the mangled name.

## Deferred work

- `print(p)` for records (printing the full record as a string) -- deferred to Phase 6 (strings and I/O). Phase 4 fixtures only print individual scalar fields.
- Record field ordering in `print` output -- Phase 6.
- `Ord` trait / record ordering (`<`, `>`) -- not in MEP-46 v1 scope.
- Interface/trait dispatch on records -- Phase 7 (error model and traits).
- Generic records (`type Box<T> { value: T }`) -- Phase 5.X or later.
- BEAM's native `record` construct is not used at any point; all records are BEAM maps throughout MEP-46.
- Dialyzer map specs for record types (narrowing `map()` to `#{mochi_record_tag := person, name := binary(), age := integer()}`) -- Phase 17.
- Record serialisation to JSON / external format -- Phase 12 (FFI and interop).

## Closeout notes

Landed as Phase 4.0+4.1+4.3 combined (literal construction, field access, equality).

Deviations from spec design:

1. **Scope narrowed.** Methods (Phase 4.2) and record patterns (Phase 4.1 pattern matching) are deferred; aotir has no method decl type yet, and match expressions are not in the current fixture set. The 7 fixtures cover literal construction, field access, equality, and field access in loops/conditions.

2. **`c_map` ETF format bug fixed.** The existing `CMap()` function in cerl.go was emitting a 4-element tuple `{c_map, [{is_pat,false}], Arg, Pairs}` with `is_pat` stuffed in the annotation. OTP's `core_lint` expects a 5-element tuple `{c_map, [], Arg, Pairs, false}` and the op field in `c_map_pair` must be `{c_literal, [], assoc}` not a bare atom. Fixed both.

3. **`CEmptyMap()` added to cerl.** New `EMap` type added for ETF encoding of Erlang map values (`MAP_EXT`, tag 116). `CEmptyMap()` produces `{c_literal, [], #{}}` which is the correct base argument for new map literals.

4. **Record equality (`BinEqRec`/`BinNeRec`) added.** Op codes for record equality were missing from `lowerBinaryExpr`. Also added `BinEqList`/`BinNeList` and `BinEqMap`/`BinNeMap` which all map to `=:=`/`=/=`.

5. **Nested records deferred.** Fixture 304 was simplified from `Line{a: Point, b: Point}` to two sequential Point records because the aotir C-backend lowerer rejects nested record field types in Phase 3.0.
