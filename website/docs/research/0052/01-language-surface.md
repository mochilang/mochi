---
title: "Language surface: Mochi onto TypeScript 5.6 / ES2024"
description: "Per-feature lowering obligation for the Mochi-to-TypeScript transpiler. Maps every Mochi surface construct to its TypeScript 5.6 / ECMAScript 2024 form, with tsc --strict and tsc --noUncheckedIndexedAccess dual-gate considerations."
sidebar_position: 1
---

# Language surface: Mochi onto TypeScript 5.6 / ES2024

Author: research pass for MEP-52 (Mochi to TypeScript / JavaScript transpiler).
Date: 2026-05-23 (GMT+7).
Sources: `docs/features/*.md`, `docs/index.md`, `docs/common-language-errors.md`,
`mcp/cheatsheet.mochi`, `ROADMAP.md`, `examples/v0.2`-`v0.7`, the normative
security specs `docs/security/threat-model.md` and `docs/security/memory-safety.md`,
the TC39 finished-proposals list (ES2024 was ratified by the TC39 General
Assembly in June 2024), the TypeScript 5.6 release notes (Sept 2024), the
TypeScript 5.7 release notes (Nov 2024), the TypeScript 5.8 beta notes
(Feb 2025), the Node.js 22 LTS announcement (April 2024), the Deno 2.0
launch notes (Oct 2024), the Bun 1.0 launch notes (Sept 2023) and Bun 1.1
release notes (April 2024), the npm CLI 10.x release notes (Sept 2023),
the sibling MEP-45 note 01 (C target), MEP-46 note 01 (Erlang/BEAM),
MEP-47 note 01 (JVM), MEP-48 note 01 (.NET), MEP-49 note 01 (Swift),
MEP-50 note 01 (Kotlin), MEP-51 note 01 (Python), whose section structure
this note deliberately mirrors so all eight backends can be diffed line
for line.

This note records the user-visible language surface that the
TypeScript target must faithfully reproduce. It is written *from the
spec downward* and ignores the existing Go runtime (vm3), the vm3
bytecode, the C target under MEP-45, the Erlang/BEAM target under MEP-46,
the JVM target under MEP-47, the .NET target under MEP-48, the Swift
target under MEP-49, the Kotlin target under MEP-50, the Python target
under MEP-51, and any other backend implementation. The goal is a
transpiler design that would be correct against the *language*, not
against the present implementations.

The surface decomposes into the same eight orthogonal sub-languages
identified in the prior notes: (1) the value core, (2) the function and
method core, (3) the collection core, (4) the algebraic-data-type core,
(5) the query DSL, (6) the stream and agent core, (7) the
logic-programming core, and (8) the AI and FFI shells. Each section
below names every form a Mochi program can write, then states a
*lowering obligation* the TypeScript backend must honour.

Where MEP-45 maps Mochi types to C struct plus helper-function pairs,
MEP-46 maps them to BEAM terms (atoms, tagged tuples, maps, binaries,
funs, PIDs), MEP-47 maps them to JVM values directly via bytecode,
MEP-48 maps them to .NET values, MEP-49 maps them to Swift values
(`Int64`, `Double`, structs, enums with associated values, actors,
`AsyncStream`), MEP-50 maps them to Kotlin values (`Long`, `Double`,
`data class`, `sealed interface`, custom actor with `Channel<Message>`,
`Flow<T>`), MEP-51 maps them to Python values (arbitrary-precision
`int`, IEEE-754 `float`, dataclass records, PEP 695 type alias sums,
asyncio.Queue agents), this note maps them to TypeScript / ECMAScript
2024 values: `bigint` (arbitrary precision) or `number` (IEEE 754
double) per monomorphisation, `boolean`, `string` (UTF-16 internally,
`len(s)` returns code-point count), `Uint8Array` for bytes,
`readonly T[]` / `T[]` for lists, `Map<K, V>` (insertion-ordered by
spec), `Set<T>` (insertion-ordered by spec, with ES2024 set methods),
frozen-property classes for records, discriminated unions via
literal-tagged `type Foo = A | B | C` for sum types, `T | null` for
options, custom `MochiResult<T, E>` discriminated union for errors,
custom agent class wrapping `AsyncIterableQueue<Message>` plus an
`AbortController` for supervision, `AsyncIterable<T>` for streams, and
`(args) => R` for function types. The target IR is discussed in
[[05-codegen-design]] (the default path emits TypeScript source via a
Mochi-internal CST builder, then a `prettier 3.x` formatter pass for
layout); the runtime is the Web platform plus a thin `mochi_runtime`
npm package (see [[04-runtime]]).

Throughout, "TypeScript" means **TypeScript 5.6.0** (released
2024-09-09) and later, and "ECMAScript" means **ES2024** (the 15th
edition of ECMA-262, ratified June 2024). TypeScript 5.5 (June 2024)
is *not* the floor because TS 5.6 introduces `--noUncheckedSideEffectImports`
and `--rewriteRelativeImportExtensions`, both of which the build
pipeline depends on for `.ts` source imports surviving emit cleanly.
Node.js 20 LTS (which became LTS in October 2023 and goes EOL April
2026) is *not* the floor because it lacks native `Promise.withResolvers`
(ES2024, requires Node 22+). Node.js 22 LTS (April 2024 release, became
LTS October 2024) is the floor; older Node runtimes are out of scope.
Deno 1.x is *not* the floor because Deno 2.0 (October 2024) shipped
npm-package interop, the `deno.json` workspace model, and JSR
(`jsr.io`) publishing. Bun 1.0 (September 2023) is the floor for Bun,
with Bun 1.1 (April 2024) the recommended baseline. Browsers: the
floor is "browsers shipped after April 2024" (Chrome 124+, Firefox
125+, Safari 17.4+), which is the cohort that ships `Promise.withResolvers`
natively. Older browsers receive a polyfill via the runtime package.

## 1. Value core

### 1.1 Bindings

Mochi has exactly two binding forms.

- `let name = expr`, immutable. Re-assignment is a **compile-time**
  error, not a runtime panic. TypeScript has two relevant binding
  forms: `const` (immutable binding, mutable referent) and `let`
  (mutable binding). The Mochi-emitted form is:
  ```typescript
  const name: number = expr;
  ```
  TypeScript's `const` is enforced at compile time by the type
  checker. Mochi's `let` semantic ("the binding may not be reassigned")
  maps to TypeScript's `const` exactly. Note that TypeScript `const`
  does not deep-freeze the value (a `const` array can still have
  elements mutated); Mochi's immutability discipline for collections
  is enforced separately via `readonly` modifiers on collection types
  (see §3) and via `Object.freeze()` calls inserted by the runtime at
  record construction sites (see §2.3).

- `var name = expr`, mutable. Re-assignment is unrestricted within the
  variable's lexical scope. Emitted as a TypeScript `let` binding:
  ```typescript
  let name: number = expr;
  ```
  Note the deliberate cross-language inversion: Mochi `let` becomes
  TypeScript `const`, Mochi `var` becomes TypeScript `let`. The
  Mochi-`var`-to-TypeScript-`let` mapping is mechanical; the
  Mochi-`let`-to-TypeScript-`const` mapping is the one to remember.
  The emitter never emits the TypeScript `var` keyword (function-scoped
  hoisting semantics, deprecated in style guides since ES2015). Both
  Standard and Airbnb style guides forbid `var`; ESLint's
  `no-var` rule is on by default in `@typescript-eslint/recommended`.

Mochi blocks are expressions in the sense that the last expression is
the block's value. TypeScript / JavaScript blocks are statements (only
arrow function bodies and a handful of expression contexts evaluate to
values). The backend lowers block-valued constructs in one of three
ways: (a) for a one-line conditional, into a TypeScript conditional
expression `cond ? a : b`; (b) for a more complex block, into an
immediately-invoked arrow function `(() => { ... return v; })()`
returning the block value; (c) for sum-type matches with no side
effects in arms, into a `switch (x.kind)` statement with assignment of
the result inside each arm. See [[05-codegen-design]] §6 for the full
block-lowering table.

A binding may carry an explicit type: `let x: int = 0` becomes
`const x: bigint = 0n` (or `const x: number = 0` if monomorphisation
proves the value fits in i53; see §1.2). The type annotation is
attached to the declaration, not on a separate line; TypeScript's
"PEP 526-equivalent" is just standard variable declaration syntax.

Mochi supports destructuring at `let`:

```mochi
let [a, b] = [1, 2]
let {"name": n, "age": age} = {"name": "Ana", "age": 22}
```

TypeScript / ECMAScript natively supports both array and object
destructuring (ES2015). The lowering is direct:

```typescript
const [a, b]: [bigint, bigint] = [1n, 2n];
const { name: n, age }: { name: string; age: bigint } =
  { name: "Ana", age: 22n };
```

For array destructuring with an arity check (Mochi's `let [a, b] =
xs` panics if `xs.length != 2`), the lowering adds a runtime guard
because the destructure itself does not check arity (excess elements
are dropped, missing elements become `undefined`):

```typescript
if (xs.length !== 2) {
  throw new MochiPatternError(`arity expected 2, got ${xs.length}`);
}
const [a, b] = xs;
```

The `MochiPatternError` class is exported from `mochi_runtime/errors`.

For object destructuring, the key strings are statically known at
emit time, so the type system narrows correctly. If the source map
type is `Record<string, unknown>`, the destructured names take type
`unknown` and an explicit `cast` (a type assertion `as bigint`) is
emitted at the use site, with `--noPropertyAccessFromIndexSignature`
preventing accidental property access. See [[06-type-lowering]] §7.

For record types the backend uses property access since classes do
not destructure by position (they destructure by key just like
plain objects):

```typescript
const n: string = person.name;
const age: bigint = person.age;
```

Scoping is lexical and block-based. TypeScript's `const` and `let`
are block-scoped (ES2015 semantics), matching Mochi's block scoping.
This is unlike Python (function-scoped) and unlike pre-ES2015
JavaScript (function-scoped `var`). No rename pass is required for
inner shadowing; the emitter can reuse the Mochi-level name directly.

Reserved-word collisions are handled with a trailing underscore: Mochi
`class` becomes TypeScript `class_`, Mochi `function` becomes
TypeScript `function_`, Mochi `import` becomes TypeScript `import_`.
The full list (TypeScript 5.6 has 67 reserved words: 36 keywords, 11
future-reserved, 4 strict-mode reserved, 16 contextual keywords) is
in §1.6 and [[06-type-lowering]] §2.

### 1.2 Primitive types

Surfaced by the docs and the cheatsheet, with the TypeScript-side
representation:

| Mochi | Width / semantics | TypeScript lowering |
|-------|-------------------|----------------------|
| `int` | 64-bit signed integer (inferred from integer literals) | `bigint` (arbitrary precision) OR `number` (when monomorphisation proves the value fits in [-(2^53-1), 2^53-1]) |
| `float` | 64-bit IEEE 754 double | `number` (the only floating-point type in JS) |
| `bool` | `true` / `false` | `boolean` |
| `string` | UTF-8 text, indexable as code points, immutable | `string` (UTF-16 internal; `len(s)` is code-point count via a runtime helper or `[...s].length`) |
| `bytes` | immutable byte sequence | `Uint8Array` (the ECMAScript-standard byte container) |
| `time` | absolute timestamp | `Date` (millisecond precision) or `Temporal.Instant` (TC39 Stage-3, ES2025+) wrapped in a `MochiTime` class |
| `duration` | time interval | `number` (milliseconds) wrapped in a `MochiDuration` class, with `Temporal.Duration` planned for ES2025 |
| `image` (preview) | binary blob | `Uint8Array` wrapped in a `MochiImage` class |

TypeScript / JavaScript has *two* numeric primitive types and they do
not mix: `number` is IEEE 754 double-precision (53 bits of integer
precision), and `bigint` is arbitrary-precision integer (ES2020).
The two cannot be mixed in arithmetic without an explicit conversion;
`1n + 1` throws `TypeError: Cannot mix BigInt and other types, use
explicit conversions`. Mochi's `int` is documented as 64-bit signed,
which overflows JS's safe-integer range at 2^53. The emitter
monomorphises Mochi `int` based on IR analysis: if the IR proves a
value (and all operations on it) stay within [-(2^53-1), 2^53-1], the
emitted type is `number` and arithmetic uses `+`, `-`, `*`,
`Math.trunc(a / b)` (true integer division), `((a % b) + b) % b`
(floor remainder); if the IR cannot prove i53 containment, the emitted
type is `bigint` and arithmetic uses `+`, `-`, `*`, `/` (BigInt's `/`
is integer division), `%` (BigInt's `%` is truncated remainder, not
floor; the emitter inserts `((a % b) + b) % b` for floor semantics).

The monomorphisation pass runs at the aotir level (see
[[05-codegen-design]] §8). The default for ambiguous cases is
`bigint`, because correctness beats performance: a Mochi program that
silently corrupts integer arithmetic due to i53 overflow is a worse
failure mode than slow `bigint` arithmetic. The `--prefer-number`
flag inverts the default (use `number` everywhere, emit a runtime
overflow check on every arithmetic op); off by default. See
[[02-design-philosophy]] §6 for the i53-versus-bigint cost analysis.

Both type checkers (tsc and the JetBrains TypeScript engine bundled
in WebStorm) enforce the bigint/number separation strictly. The
emitter never produces mixed-mode arithmetic; if a `bigint` and a
`number` must interact (e.g., when reading a JSON value that decoded
to a number and the Mochi type is `int`), the emitter inserts an
explicit `BigInt(num)` or `Number(bn)` cast at the boundary, with the
documented loss of precision recorded in a comment.

Implicit numeric conversions are **not** allowed in Mochi. `int +
float` is a Mochi type error; the program must `float(x)` first.
TypeScript's `number + number` (where one operand is float and the
other is int-stored-as-number) succeeds silently with no warning,
because both are `number` at the JS level. The Mochi type checker
catches the mismatch upstream, so the emitter never attempts the
mixed expression. For the `bigint`-vs-`number` case, TS catches the
mismatch (the types are distinct), giving us a safety net.

Integer overflow under the `bigint` path: arbitrary precision, no
overflow. Under the `number` path: silent IEEE 754 rounding for
values beyond i53. The emitter chooses `bigint` whenever the IR
cannot prove i53 containment, so the default path never silently
overflows. For the `--prefer-number` opt-in, the emitter inserts a
runtime helper `mochiCheckI53(x)` that throws `MochiOverflowError` if
`x` exceeds the safe range. See [[06-type-lowering]] §5.

### 1.3 Operators

Arithmetic `+ - * / %`; comparison `== != < <= > >=`; boolean
`&& || !`; membership `in`; string concatenation overloads `+`.

| Mochi | TypeScript (number path) | TypeScript (bigint path) |
|-------|--------------------------|---------------------------|
| `a + b` (int)    | `a + b` (53-bit precision) | `a + b` (bigint) |
| `a + b` (float)  | `a + b` (IEEE NaN propagates) | N/A |
| `a + b` (string) | `a + b` (string concatenation) | N/A |
| `a + b` (list)   | `[...a, ...b]` (spread, fresh array) | N/A |
| `a - b` | `a - b` | `a - b` |
| `a * b` | `a * b` | `a * b` |
| `a / b` (float) | `a / b` (true division returns IEEE float) | N/A |
| `a / b` (int)   | `Math.trunc(a / b)` (truncated division; emitter wraps to floor when Mochi requires floor) | `a / b` (BigInt division is truncated; emitter wraps to floor when Mochi requires floor) |
| `a % b` | `((a % b) + b) % b` (floor remainder; JS `%` is truncated remainder by default) | `((a % b) + b) % b` |
| `a == b` (primitive) | `a === b` (strict equality, no type coercion) | `a === b` |
| `a == b` (record/object) | structural via `mochiEqual(a, b)` runtime helper | N/A |
| `a != b` | `a !== b` | `a !== b` |
| `a < b`, `<=`, `>`, `>=` | numeric: native; string: `a < b` uses UTF-16 code-unit ordering by default, but the emitter wraps with `mochiStrCompare(a, b)` for code-point ordering | numeric: native |
| `a && b` | `a && b` (JS `&&` returns the first falsy operand or the last operand; type checker narrows to `boolean` when both operands are typed `boolean`) | N/A |
| `a \|\| b` | `a \|\| b` | N/A |
| `!a` | `!a` | N/A |
| `x in xs` (list) | `xs.includes(x)` (since ES2016) | N/A |
| `x in m` (map) | `m.has(x)` | N/A |
| `x in s` (set) | `s.has(x)` | N/A |

JavaScript's `==` and `!=` perform type coercion ("ToPrimitive" plus
"ToNumber" by default), which can produce surprising results: `0 ==
""` is `true`, `"1" == 1` is `true`, `null == undefined` is `true`.
The emitter **never** emits the loose `==` / `!=` operators; it
always emits `===` / `!==`. ESLint's `eqeqeq` rule is on by default
in our config and would fail the build if `==` ever appeared.

JavaScript's `&&` and `||` return one of the operands (not always a
`boolean`). For example, `1 && 2 === 2`, `0 || 3 === 3`. Mochi's `&&`
and `||` always return `bool`. The mismatch only matters when the
result is bound to a non-bool variable (which Mochi rejects at
type-check time). The backend never has to coerce because Mochi's
type checker constrained the result type to `boolean` upstream.

JavaScript's `in` operator tests for *property existence* on an
object (`"x" in {x: 1}` returns true). This is **not** what Mochi's
`x in xs` means (membership in a list). The emitter uses
`Array.prototype.includes(x)` (ES2016) for list membership,
`Map.prototype.has(x)` for map key membership, and
`Set.prototype.has(x)` for set membership. The bare `in` operator is
never emitted in user code.

`a == b` for record types lowers to a `mochiEqual(a, b)` runtime
helper that walks structurally. JavaScript has no `__eq__` equivalent;
`===` on two distinct objects is always false even if they have the
same fields. Mochi's record-equality contract requires field-by-field
comparison. See [[06-type-lowering]] §4.

### 1.4 Strings as read-only code-point sequences

```mochi
let text = "hello"
print(text[1])     // "e"
for ch in text { ... }
```

Indexing yields a 1-character string (not a code point integer).
Iteration yields 1-character strings in **code-point** order.
JavaScript / TypeScript strings are **UTF-16 internally**: each
JavaScript string is a sequence of UTF-16 code units, with characters
outside the Basic Multilingual Plane represented as surrogate pairs
(two code units). This produces several subtle mismatches with
Mochi's specified semantic:

- `s.length` returns the **code unit count**, not the **code point
  count**. For an emoji like `"\u{1F600}"` (grinning face, code point
  U+1F600), `s.length === 2` because the character is stored as a
  surrogate pair. Mochi's `len(s)` returns 1 for the same string.
- `s[i]` returns the i-th **code unit** as a single-character string,
  which may be a lone surrogate (illegal UTF-16) for emoji or
  astral-plane characters.
- `s.charAt(i)` is identical to `s[i]` (legacy method, same
  semantics).
- `s.codePointAt(i)` returns the code point at the i-th *code unit*
  position, which is correct for code-point reading but the index is
  still in code units. Skipping forward by code points requires
  walking and watching for surrogates.

The Mochi emitter inserts a runtime helper layer:

- `mochiStrLen(s)` returns `[...s].length` (the array-spread idiom
  iterates by code point, giving correct count). For ASCII-heavy
  strings this is O(n); the runtime caches the result on a hidden
  weakmap-keyed property for repeated calls on the same string.
- `mochiStrIndex(s, i)` returns the i-th code point as a 1-character
  string, via `[...s][i]`. Same O(n) per call; runtime caches the
  iterator array for repeated indexing into the same string.
- `for ch in text` lowers to `for (const ch of text) { ... }` (the
  `for...of` iteration of a string yields code-point characters
  natively in ES2015+; this is correct without a helper).

For HTTP, JSON, file I/O the conversion to bytes uses
`TextEncoder.encode(s)` which returns a `Uint8Array` of UTF-8 bytes.
This is a one-pass O(n) conversion. `TextDecoder.decode(bytes)` is
the reverse direction; both are spec-standard Web APIs available in
Node 22, Deno 2, Bun 1.1, and all evergreen browsers. The encoder is
SIMD-optimised on modern V8 (Node 22 ships V8 12.4, which has
auto-vectorised UTF-8 encoding for ASCII strings).

The cost of UTF-16 internal storage compared to Python's PEP 393
variable-width is the surrogate-pair walk: every code-point operation
on a string containing characters outside the BMP pays an O(n) walk.
For text-heavy workloads this is the second-largest performance cost
of the TypeScript target relative to Python (after the `bigint`-versus-
`number` choice for integers). For the vast majority of Mochi
programs that process ASCII or BMP-only text, the cost is zero (the
spread idiom shortcuts to code-unit count when no surrogates are
present, since V8 13.0+ in Node 23 optimised this path; Node 22 ships
V8 12.4 which has the same optimisation).

See [[02-design-philosophy]] §6 for the UTF-16 cost analysis and the
comparison against Python's PEP 393, Kotlin's UTF-16 + StringBuilder,
and Swift's grapheme-cluster default.

### 1.5 Literals

Integer literals (`42`); floating literals (`3.14`); boolean
(`true`/`false`); string with C-style escapes; triple-quoted
multi-line strings (TypeScript template literals); list `[...]`; map
`{key: val, ...}`; set `{a, b, c}`; record constructor `T { field:
val }`.

The set literal `{a, b, c}` is distinguished from the empty map
literal `{}` by the absence of `:` after the first element. The
grammar must keep these unambiguous; the TypeScript lowering picks
the right constructor accordingly. Note that TypeScript's `{}` is
the type of "any non-nullish value" (or an empty object literal,
depending on context); neither maps to Mochi's empty map. The
emitter uses `new Map()` for an empty map and `new Set()` for an
empty set.

Lowering forms:

| Mochi | TypeScript |
|-------|-------------|
| `42` (int, fits i53) | `42` |
| `42` (int, bigint path) | `42n` (BigInt literal suffix) |
| `3.14` | `3.14` (JS number literal, IEEE 754 double) |
| `true` / `false` | `true` / `false` |
| `"hello"` | `"hello"` |
| `[1, 2, 3]` | `[1n, 2n, 3n]` (bigint path) or `[1, 2, 3]` (number path) |
| `"""multi\nline"""` | `` `multi\nline` `` (template literal; TS template literals support multi-line and `${...}` interpolation) |
| `{"a": 1, "b": 2}` | `new Map<string, bigint>([["a", 1n], ["b", 2n]])` (Map constructor with entry array; insertion order guaranteed by ES2015 spec) |
| `{1, 2, 3}` (set) | `new Set<bigint>([1n, 2n, 3n])` (Set constructor; insertion order guaranteed by ES2015 spec) |
| `Book { title: "X", pages: 10 }` | `new Book("X", 10n)` or `Book.of({ title: "X", pages: 10n })` (record class with factory) |

JavaScript's plain object literal `{a: 1, b: 2}` is **not** what Mochi
`map` means. The plain object is a *record* (string-keyed property
bag), not a *map*. Plain objects do not have a `has(key)` method
(only the `in` operator, which inherits up the prototype chain), they
do not have an iteration order guarantee for integer-string keys
(integer-like string keys are iterated first, then other strings in
insertion order, per ES2015 OrdinaryOwnPropertyKeys), and they have
the prototype-pollution hazard (a key named `"__proto__"` modifies
the prototype). The emitter always uses `Map<K, V>` for Mochi maps,
never plain objects. `Map`:

- has a `has(key)` method with O(1) lookup;
- guarantees insertion-order iteration for all key types (not just
  strings);
- has no prototype-pollution hazard (keys are stored in a separate
  slot, not on the object itself);
- supports non-string keys (numbers, bigints, objects, symbols).

Similarly, the emitter uses `Set<T>` for Mochi sets, never plain
objects-as-sets (which were a common JS idiom pre-ES2015 but are no
longer canonical).

ECMAScript 2015 introduced `Map` and `Set` with the insertion-order
iteration guarantee. ECMAScript 2024 adds new methods on `Set`:
`union(other)`, `intersection(other)`, `difference(other)`,
`symmetricDifference(other)`, `isSubsetOf(other)`,
`isSupersetOf(other)`, `isDisjointFrom(other)`. These map directly to
Mochi's set operations (see §3), making the TS target's set
implementation one of the cheapest among all eight backends.

The frozen record class is the default record representation:

```typescript
class Book {
  readonly title: string;
  readonly pages: bigint;

  constructor(title: string, pages: bigint) {
    this.title = title;
    this.pages = pages;
    Object.freeze(this);
  }

  static of(props: { title: string; pages: bigint }): Book {
    return new Book(props.title, props.pages);
  }
}
```

`readonly` on class fields is a TypeScript-only modifier (not part of
JavaScript); it prevents assignment to the field from outside the
constructor and is enforced at compile time by tsc. `Object.freeze()`
adds runtime enforcement: any attempt to reassign a frozen object's
property throws `TypeError` in strict mode (and silently fails in
sloppy mode, but Mochi-emitted code is always strict due to ES modules
implying strict mode). See §4 for the full ADT discussion.

The static `of(props)` factory takes a plain object with the same
fields and constructs the record. The factory is the canonical Mochi
construction form, matching the Mochi syntax `Book { title: "X",
pages: 10 }`. Direct construction via `new Book("X", 10n)` is also
emitted when the call site is positional.

### 1.6 Identifier mangling

TypeScript identifiers may begin with letter, `$`, or `_` and continue
with letter / digit / `$` / `_`. Mochi identifiers are stricter, so
every Mochi identifier is a legal TypeScript identifier *until* it
collides with a TypeScript reserved word. TypeScript 5.6 reserves 67
words total (a superset of JavaScript reserved words because TS adds
type-system keywords like `type`, `interface`, `keyof`, `infer`); the
emitter mangles collisions with a trailing underscore:

| Mochi name | TS name (after mangling) |
|------------|----------------------------|
| `class`    | `class_`                   |
| `function` | `function_`                |
| `import`   | `import_`                  |
| `export`   | `export_`                  |
| `new`      | `new_`                     |
| `delete`   | `delete_`                  |
| `void`     | `void_`                    |
| `typeof`   | `typeof_`                  |
| `instanceof` | `instanceof_`            |
| `in`       | `in_`                      |
| `of`       | `of_`                      |
| `yield`    | `yield_`                   |
| `async`    | `async_`                   |
| `await`    | `await_`                   |
| `let`      | `let_`                     |
| `const`    | `const_`                   |
| `var`      | `var_`                     |
| `type`     | `type_`                    |
| `interface`| `interface_`               |
| `enum`     | `enum_`                    |
| `extends`  | `extends_`                 |
| `implements`| `implements_`             |
| `keyof`    | `keyof_`                   |
| `infer`    | `infer_`                   |
| `as`       | `as_`                      |
| `satisfies`| `satisfies_`               |

Mochi variables that collide with a TypeScript built-in (`Array`,
`Object`, `String`, `Number`, `Boolean`, `Date`, `Map`, `Set`,
`Promise`, `Symbol`, `Iterator`, `Math`, `JSON`, `console`, `globalThis`)
are mangled as well, even though TypeScript allows shadowing of
built-ins. The mangling preserves the no-shadow lint rule (`eslint
no-shadow-restricted-names`) and avoids cognitive load for readers.
The full list (TypeScript 5.6 has approximately 120 built-in global
names accessible without import) is in [[06-type-lowering]] §3.

Mochi package paths `mathutils/extra` produce TypeScript module
`src/generated/mathutils/extra.ts` for user code (configurable via
`--ts-module-prefix`; default `src/generated`). The `generated`
segment makes the runtime / user distinction visible in stack traces
and in `package.json` `"exports"` graphs. Each Mochi source file
becomes one TypeScript module; Mochi packages become TypeScript
namespaces via the directory structure plus `index.ts` re-export
files.

Mochi record type names become TypeScript class names in PascalCase,
unchanged (`Book` becomes `Book`). On collision with a TypeScript
global (`Array`, `Function`, `Object`, `Error`, `Type`, `Promise`),
the emitter renames `Type` to `Type_` and emits a module-scope alias.

Mochi sum-type variant constructors become TypeScript classes nested
inside a `namespace` block with the sum type's name, plus a
discriminated-union type alias (PascalCase preserved). Field labels
are preserved verbatim. See §4 ADT lowering.

The mangling is deterministic ([[05-codegen-design]] §3) and
reversible via TypeScript line comments (`// mochi:source file.mochi:line`)
which the emitter writes for every Mochi source line. TypeScript's
source-map machinery is the formal reverse-mapping tool; the emitter
generates `.ts.map` files alongside every emitted `.ts` so that
Node's `--enable-source-maps` flag and the V8 inspector point at the
original Mochi source. See [[10-build-system]] §15.

### 1.7 Optionality

Mochi has no `null` at the language level. Optional values are
expressed via the `Option<T>` sum type. The TypeScript lowering uses
**TypeScript's native `T | null`** representation for Mochi
`Option<T>`. This is the choice the Swift target (MEP-49), Kotlin
target (MEP-50), and Python target (MEP-51) all made, and matches the
TypeScript ecosystem's predominant style (the React community and
the FastAPI-equivalent NestJS community both use `T | null`).

The decision: the TypeScript target **uses `T | null` for Mochi
`Option<T>`, not `T | undefined`**. The reasoning:

- TypeScript distinguishes `null` (explicitly absent) from `undefined`
  (unset). The two are similar but not identical: `JSON.stringify({a:
  undefined})` produces `'{}'` (omitted), while `JSON.stringify({a:
  null})` produces `'{"a":null}'` (preserved). The semantic of "the
  value is absent and the absence is meaningful" matches `null`; the
  semantic of "I forgot to set this property" matches `undefined`.
  Mochi `Option<T>` means the former, so `T | null` is the right
  match.
- `--strictNullChecks` (part of `--strict`) enforces that `T | null`
  values must be narrowed with `x === null` or `x !== null` (or the
  nullish-coalescing operators `??`, `?.`) before use. This catches
  the entire class of "null reference exception" bugs at compile time.
- `--exactOptionalPropertyTypes` (TypeScript 4.4+, off by default but
  on in our `tsconfig.json`) prevents `undefined` from sneaking in as
  a substitute for `null`. A property declared `field?: T` (which is
  `T | undefined` by default) cannot be assigned `undefined` explicitly
  unless declared `field?: T | undefined`. This keeps the distinction
  rigorous.
- `--noUncheckedIndexedAccess` makes array and map indexing return `T
  | undefined` (the absence is `undefined`, the absent-from-an-Option
  case is `null`). This separation keeps "the index was out of range"
  distinct from "the value at the index was the absent option".

Concretely: Mochi `Some(x)` becomes TypeScript `x` (the implicit wrap
when assigning to `T | null`), Mochi `None` becomes TypeScript `null`.

For lowering pattern matches that consume Mochi `Option`:

```mochi
match opt {
  Some(x) => x + 1
  None    => 0
}
```

Lowers to a TypeScript conditional expression or `if`/`else`:

```typescript
const result: bigint = opt !== null ? opt + 1n : 0n;
```

For multi-line arms with statements, the lowering uses an `if` /
`else` block. For pure expression arms, the conditional expression is
preferred (it is a single TypeScript expression, type-checks cleanly
via the `!== null` narrowing, and produces less generated code).

Optional chaining (`?.`) and nullish coalescing (`??`) are ES2020
features. The emitter uses them when the expression structure allows:

```mochi
let n = opt?.name ?? "anon"
```

Lowers to:

```typescript
const n: string = opt?.name ?? "anon";
```

At the FFI boundary, any value coming in from JS code that is typed
as `T | null | undefined` is funnelled through Mochi `Option<T>` (the
emitter inserts `x ?? null` to collapse `undefined` to `null`); values
typed as `T` (non-optional) bypass the wrapper. The type checker
enforces this distinction statically through its narrowing analysis,
so no runtime check is required for pure-TypeScript code paths.

The Mochi `Result<T, E>` type, by contrast, is **not** mapped to a
single-type `T | E` union, because that conflates success and failure
when `T` and `E` overlap (e.g., `Result<int, int>`). The backend emits
a custom `MochiResult<T, E>` discriminated union, discussed in §4
and §9.

## 2. Function and method core

### 2.1 Top-level functions

```mochi
fun add(a: int, b: int): int { return a + b }
```

Lowers to a top-level TypeScript function declaration with explicit
parameter types and return type:

```typescript
export function add(a: bigint, b: bigint): bigint {
  return a + b;
}
```

Every Mochi source file produces one TypeScript module file named
after the source file (`example.mochi` becomes `example.ts`)
declaring all top-level functions, top-level `const` bindings, and any
helper classes at module scope. The module exports the public surface
via `export` keywords on declarations; the backend computes the
export set from the set of Mochi top-level declarations that are not
file-private (`priv`).

TypeScript / ECMAScript modules execute their top level at import
time. For Mochi top-level expressions that have side effects (e.g.,
`let cache = compute_cache()`), the lowering emits the side-effecting
expression at module scope and relies on the ES Module specification's
"each module body runs exactly once, the first time it is imported"
contract.

The reason we use module-level top-level declarations and not a
wrapping class for the module namespace is that ES modules are the
canonical namespace unit, they reduce nesting depth in generated
code by one level, they support tree-shaking by static analysis (a
bundler can drop unused exports trivially), and they have first-class
support in every TS-aware tool. This is the TypeScript equivalent of
Swift's "namespacing enum" idiom or Python's top-level declarations.

Mochi `return` is explicit. The TypeScript lowering preserves
`return` directly: `return e;` becomes TypeScript `return e;`. The
emitter always emits an explicit `return` at the end of a non-`void`
function. For `void` functions (Mochi `fun foo(): unit`), the
emitter emits `function foo(): void { ... }` with no explicit return
needed (the implicit `undefined` return is fine; `void` in TS means
"the return value is not consumed").

The docs warn there is **no implicit tail-call optimisation** in
Mochi. ECMAScript 2015 mandated proper tail calls (PTC) for strict
mode, but as of 2026 only Safari/JavaScriptCore implements it; V8
(Node, Chrome) and SpiderMonkey (Firefox) do not. Mochi-emitted
code that recurses deeply will hit the V8 default stack limit (~10000
frames for V8 12.4 in Node 22, configurable via `--stack-size`) and
throw `RangeError: Maximum call stack size exceeded`. The backend
detects deep recursion patterns at the Mochi IR level
([[05-codegen-design]] §15) and emits a trampoline helper when the
recursion depth can statically exceed a safe limit. The trampoline
uses an iterative `while` loop and a stack of work items, preserving
Mochi semantics without the JS stack-limit hazard.

TypeScript supports generics natively (since TS 1.0, 2014):

```mochi
fun first<T>(xs: list<T>): T { return xs[0] }
```

Lowers to:

```typescript
export function first<T>(xs: readonly T[]): T {
  const elem: T | undefined = xs[0];
  if (elem === undefined) {
    throw new MochiBoundsError("first: list is empty");
  }
  return elem;
}
```

The `xs[0]` access returns `T | undefined` under
`--noUncheckedIndexedAccess`; the bounds check is mandatory. The
runtime helper `MochiBoundsError` is exported from
`mochi_runtime/errors`. Mochi's semantic for `xs[0]` on an empty list
is "compile-time error if statically empty, runtime panic if
dynamically empty"; the TS emitter implements the runtime panic via
the bounds check.

TypeScript generic syntax has been stable since TS 1.0. PEP 695 in
Python (3.12) introduced PEP 695 generic syntax; TypeScript has had
the equivalent for a decade. Mochi generics map directly to TS
generics with no syntactic transformation beyond the angle-bracket
notation.

### 2.2 First-class function values

```mochi
let square = fun(x: int): int => x * x
fun apply(f: fun(int): int, value: int): int { return f(value) }
```

Lower to TypeScript callable types and arrow functions:

```typescript
const square: (x: bigint) => bigint = (x) => x * x;

export function apply(
  f: (x: bigint) => bigint,
  value: bigint,
): bigint {
  return f(value);
}
```

TypeScript arrow functions (ES2015) are the canonical first-class
function form. Unlike Python lambdas, TS arrows can have
multi-statement bodies (with `{ ... }` block syntax). The emitter
prefers arrow functions over `function` declarations for first-class
values because (a) arrows do not have their own `this` binding,
matching Mochi's no-implicit-this semantic; (b) arrows are more
syntactically concise; (c) the type inference for arrow functions in
TS 5.6+ is strictly better than for `function` expressions (the
inference engine has special-case handling for arrow-function
parameter type inference from context).

For multi-statement Mochi closures:

```typescript
const process = (x: bigint): bigint => {
  const y = x * 2n;
  return y + 1n;
};
```

For closures that must be invoked from an async context (i.e., that
may `await`), the arrow is `async`:

```typescript
const fetchOne = async (id: string): Promise<Response> => {
  const r = await fetch(`/api/${id}`);
  return r;
};
```

The Mochi `async` keyword maps directly to TypeScript's `async`. The
type systems agree: calling an `async` function from a non-async
context produces a `Promise<T>` object that must be `await`ed (or
chained with `.then(...)`); tsc flags forgotten promises as
"Promise-returned value is not awaited" under
`@typescript-eslint/no-floating-promises`. See [[02-design-philosophy]]
§12 on the coroutine model.

Closures escape freely; captured variables in JavaScript are captured
by **reference** (not by value), and JavaScript has the classic "loop
variable capture" trap when `var` is used (since `var` is
function-scoped). With `let` and `const` (block-scoped) the trap
disappears because each loop iteration creates a fresh binding:

```typescript
// Mochi: for i in range(3) { fns.append(fun() => i) }
const fns: Array<() => bigint> = [];
for (let i = 0n; i < 3n; i++) {
  fns.push(() => i);  // i is block-scoped, fresh per iteration
}
// fns[0]() === 0n, fns[1]() === 1n, fns[2]() === 2n -- correct
```

Without the `let` block-scoping (i.e., if `var` were used), all three
closures would return `3n` (the final value of `i`). The emitter
always uses `let` (or `const`) for loop variables, never `var`. See
[[05-codegen-design]] §16 for the capture policy.

### 2.3 Methods on type blocks

```mochi
type Circle {
  radius: float
  fun area(): float { return 3.14 * radius * radius }
}
```

A method receives an implicit `self`; field names inside the block
are unqualified. Lowering: the record is a TypeScript class with
`readonly` fields plus `Object.freeze` and the method is an instance
method:

```typescript
export class Circle {
  readonly radius: number;

  constructor(radius: number) {
    this.radius = radius;
    Object.freeze(this);
  }

  area(): number {
    return 3.14 * this.radius * this.radius;
  }

  static of(props: { radius: number }): Circle {
    return new Circle(props.radius);
  }
}
```

TypeScript classes have the standard JS class semantics: constructor,
instance methods, static methods, `instanceof` testing. `readonly`
fields are enforced at compile time by tsc and at runtime by
`Object.freeze()`. The `Object.freeze(this)` call at the end of the
constructor freezes the instance, preventing any further property
assignment; combined with `readonly` modifiers, this makes the
record fully immutable.

The combination of `readonly` + `Object.freeze` gives a record type
that is:

- compile-time immutable (`obj.field = newVal` is a tsc error);
- runtime immutable (`obj.field = newVal` throws `TypeError` in
  strict mode);
- structurally inspectable (`Object.keys(obj)` returns the field
  names; `JSON.stringify(obj)` serialises them);
- debugger-friendly (Node and Chrome devtools show the fields
  directly).

TypeScript methods take an implicit `this`; field access inside the
method body uses `this.fieldName`. The backend rewrites Mochi
unqualified field references inside methods to `this.<field>` during
lowering.

For records that need to participate in sorting, the emitter adds a
static `compare(a: T, b: T): number` method that returns -1/0/+1, and
generated `sort()` calls pass `Class.compare` as the comparator. For
records that need JSON serialization, the emitter adds a `toJSON()`
method that returns a plain object with the fields; `JSON.stringify`
calls `toJSON` automatically when present. See [[02-design-philosophy]]
§7 on the choice of class + Object.freeze over Zod, io-ts, Effect's
Schema, and class-validator.

For records with mutable fields (Mochi `var` field), the lowering
removes the `readonly` modifier from the field and removes the
`Object.freeze` call:

```typescript
export class Counter {
  count: bigint;

  constructor(count: bigint) {
    this.count = count;
    // no Object.freeze, fields are mutable
  }
}
```

Mochi's value-semantics contract on records (records are copied by
value when assigned or passed to a function) is preserved by:

(a) the default `readonly` + `Object.freeze` immutability for records
without `var` fields, which lets aliasing be safe;

(b) explicit `Object.assign(new Counter(0n), { count: newVal })` or
the equivalent factory `Counter.of({...this, count: newVal})` calls
at every Mochi mutation site for `var` records, which preserves
value semantics by producing a fresh instance per mutation.

See [[06-type-lowering]] §4.

### 2.4 Built-in `print`

Variadic, prints with default formatting and inserts spaces (cheatsheet:
`print("name = ", name, ...)`); newline at end.

Lowers to a `mochiPrint` helper from `mochi_runtime/io`:

```typescript
import { print as mochiPrint } from "mochi_runtime/io";

mochiPrint("name =", name);
```

JavaScript's built-in `console.log(*args)` is already variadic with
space separators by default and newline termination. Mochi's print
semantics nearly match `console.log` exactly, with one wrinkle:
Mochi's list, map, set, and record format should match Mochi's
documented format, not Node's `util.inspect` format (which produces
`Map(2) { 'a' => 1, 'b' => 2 }` for a Map, not `{a: 1, b: 2}` as
Mochi specifies). The lowering uses `mochi_runtime/io/print` instead
of bare `console.log`:

```typescript
export function print(...args: unknown[]): void {
  const formatted = args.map(formatValue).join(" ");
  console.log(formatted);
}

function formatValue(v: unknown): string {
  if (v === null) return "nil";
  if (typeof v === "string") return v;
  if (typeof v === "bigint") return v.toString();
  if (Array.isArray(v)) return `[${v.map(formatValue).join(", ")}]`;
  if (v instanceof Map) {
    const entries = [...v.entries()]
      .map(([k, val]) => `${formatKey(k)}: ${formatValue(val)}`)
      .join(", ");
    return `{${entries}}`;
  }
  if (v instanceof Set) {
    return `{${[...v].map(formatValue).join(", ")}}`;
  }
  if (typeof v === "object" && v !== null && "toMochiString" in v) {
    return (v as { toMochiString(): string }).toMochiString();
  }
  return String(v);
}
```

The `toMochiString()` method on record classes produces the
`Book { title: "X", pages: 10 }` form. The emitter synthesises
`toMochiString` for every record class.

See [[04-runtime]] §3 for the `mochi_runtime/io` module.

## 3. Collection core

Three primitive containers, all with structural typing:

- `list<T>`, ordered, growable. Lowers to TypeScript `readonly T[]`
  (immutable view) or `T[]` (mutable). The `readonly` modifier is a
  type-system-only mark; the underlying object is always a JS
  `Array`.
- `map<K, V>`, keyed lookup, with insertion-order iteration. Lowers
  to TypeScript `Map<K, V>` (the ES2015 Map class; insertion order
  guaranteed by spec since 2015).
- `set<T>`, unique members, with insertion-order iteration in Mochi
  semantics. Lowers to TypeScript `Set<T>` (the ES2015 Set class;
  insertion order guaranteed by spec; ES2024 adds the algebraic set
  operations).

Lowering obligations (full per-type details in [[06-type-lowering]] §1):

- `list<T>` is the workhorse. JavaScript `Array` is a dense
  resizable array with O(1) amortised append, O(1) random access,
  and O(n) insertion at the head. Element storage is **boxed** for
  every JavaScript object reference (including `number` which is
  stored as a 64-bit double, and `bigint` which is heap-allocated).
  V8 specialises `Array` instances with "elements kinds": an array
  of integers stored in i31 (SMI) form is more compact than a
  general object array. The emitter does not control this
  specialisation directly; V8's elements-kind machinery picks based
  on observed contents. For the `number` path of Mochi `int`, V8 may
  unbox into the SMI form for arrays of small ints; for `bigint`, no
  unboxing happens.

- `map<K, V>` defaults to TypeScript `Map<K, V>`. Mochi's iteration
  order is insertion order. JS's `Map` has guaranteed insertion-order
  iteration since ES2015. The default lowering is therefore
  insertion-ordered without a wrapper. Hash-based key lookup is
  amortised O(1) (V8 implements `Map` via a hash table with open
  addressing); ordered iteration is O(n).

- `set<T>` is TypeScript `Set<T>`. Mochi's `set<T>` is insertion-
  ordered, matching JS `Set` exactly. ES2024 adds the algebraic
  operations:

  ```typescript
  const a = new Set([1, 2, 3]);
  const b = new Set([2, 3, 4]);
  a.union(b);              // Set { 1, 2, 3, 4 }
  a.intersection(b);       // Set { 2, 3 }
  a.difference(b);         // Set { 1 }
  a.symmetricDifference(b);// Set { 1, 4 }
  a.isSubsetOf(b);         // false
  a.isSupersetOf(b);       // false
  a.isDisjointFrom(b);     // false
  ```

  These map directly to Mochi's `union`, `intersect`, `except`, `xor`,
  `subset`, `superset`, `disjoint` set operations. Node 22, Deno 2,
  Bun 1.1, and Safari 17.4+ all ship the ES2024 set methods. For
  older browsers, the runtime polyfills via a side import.

- All collections are **value-semantically copied** at language level.
  JavaScript reference types do not provide this for free (every
  collection is heap-allocated and aliased on assignment), so the
  backend emits explicit defensive copies at function-call boundaries.
  The VM enhancement spec 0951 §1 ("each function call must allocate a
  fresh copy of any list/map literal") is satisfied by emitting
  `[...arg]` (array spread, fresh array), `new Map(arg)` (Map copy
  constructor), or `new Set(arg)` (Set copy constructor) at every
  callsite where a collection is passed to a function that may mutate
  it. The cost is one O(n) copy per call; for hot loops the user can
  opt into the `--no-defensive-copy` flag to disable (at the cost of
  giving up the value-semantics contract). The TypeScript type system
  helps here: `readonly T[]` parameters cannot be mutated through the
  parameter, so the emitter can skip the defensive copy when the
  callee's parameter is typed `readonly`. See [[02-design-philosophy]] §7.

Mutation operations (`xs.add(x)`, `m[k] = v`) lower to direct
mutating method calls (`xs.push(x)`, `m.set(k, v)`) when the target
is a `var` binding (TypeScript `let`). For `let` bindings
(TypeScript `const`), the lowering emits a copy-and-mutate via
helpers in `mochi_runtime/collections`:

```typescript
// Mochi: let xs = [1, 2, 3]; xs.add(4)
const xs = [1n, 2n, 3n];
const xs1 = appended(xs, 4n);  // returns a fresh array

// Helper:
export function appended<T>(xs: readonly T[], x: T): readonly T[] {
  return [...xs, x];
}
```

The helpers `appended`, `inserting`, `removing`, `updating`,
`mapped`, `filtered` are in `mochi_runtime/collections` and take the
collection, return a fresh mutated copy, and the caller rebinds. See
[[06-type-lowering]] §11.

`for x in xs` lowers to a TypeScript `for...of` loop:

```typescript
for (const x of xs) {
  // ...
}
```

For maps, `for (k, v) in m` lowers to:

```typescript
for (const [k, v] of m.entries()) {
  // ...
}
```

JS `Map.prototype.entries()` returns an iterator yielding
`[key, value]` pairs in insertion order. The Mochi semantic matches
exactly.

ECMAScript 2024 also introduces **Iterator helpers** (the
`Iterator.from()` static method and instance methods `map`, `filter`,
`take`, `drop`, `flatMap`, `reduce`, `toArray`, `forEach`, `some`,
`every`, `find`). These map directly to Mochi's query DSL primitives
(see §5). Node 22 ships V8 12.4 which has iterator helpers behind a
flag; Node 22.5+ enables them by default. Deno 2 ships them. Bun
1.1.5+ ships them. Safari 18.4+ ships them. For Firefox 131+ (Oct
2024) they are on by default. For older runtimes, the runtime
polyfills via the `core-js/proposals/iterator-helpers` import.

### 3.1 List-of-records

A common Mochi pattern is a `list<Record>` for dataset-shaped data.
The TypeScript lowering uses `readonly T[]` directly:

```typescript
export class Person {
  readonly name: string;
  readonly age: bigint;

  constructor(name: string, age: bigint) {
    this.name = name;
    this.age = age;
    Object.freeze(this);
  }

  static of(props: { name: string; age: bigint }): Person {
    return new Person(props.name, props.age);
  }
}

export const people: readonly Person[] = Object.freeze([
  Person.of({ name: "Ana", age: 22n }),
  Person.of({ name: "Ben", age: 30n }),
]);
```

The outer `Object.freeze` makes the array itself immutable (so
`people.push(...)` throws at runtime). The inner records are already
frozen by their constructors. For very large datasets (millions of
rows), the TypeScript target offers two opt-in alternatives: (a)
typed-array views (`Int32Array`, `Float64Array`) when all fields are
numeric, and (b) Apache Arrow tables via `apache-arrow` (the JS port
of Arrow) when columnar layout is needed for analytics workloads.
Both are opt-in via `@dataset` annotations on the Mochi record type.
See [[08-dataset-pipeline]] for the data pipeline lowering.

## 4. Algebraic data type core

Mochi's sum-of-products data types (`type Tree = Leaf | Node { ... }`)
lower to a combination of TypeScript class declarations plus a
**discriminated union** type alias. There is no TypeScript equivalent
of Swift's `enum` with associated values, Kotlin's `sealed interface`,
or Rust's `enum`. The closest is the union of classes with a literal
discriminator field, with exhaustive matching enforced by the type
checker via the never-trick.

```mochi
type Tree =
  | Leaf
  | Node { value: int, left: Tree, right: Tree }
```

Lowers to:

```typescript
export class Leaf {
  readonly kind = "Leaf" as const;

  constructor() {
    Object.freeze(this);
  }

  static of(): Leaf {
    return new Leaf();
  }
}

export class Node {
  readonly kind = "Node" as const;
  readonly value: bigint;
  readonly left: Tree;
  readonly right: Tree;

  constructor(value: bigint, left: Tree, right: Tree) {
    this.value = value;
    this.left = left;
    this.right = right;
    Object.freeze(this);
  }

  static of(props: { value: bigint; left: Tree; right: Tree }): Node {
    return new Node(props.value, props.left, props.right);
  }
}

export type Tree = Leaf | Node;
```

The `kind` field is a **literal-type discriminator**: TypeScript
narrows the union based on `t.kind === "Leaf"` checks. The `as const`
assertion locks the literal type to `"Leaf"` rather than the wider
`string`. The discriminator field is *always* named `kind` for
consistency across the emitter; the `kind` name was chosen because it
is the canonical TypeScript style guide recommendation (Effect-TS,
fp-ts, NestJS all use `kind`).

The `type Tree = Leaf | Node` declaration creates a TypeScript union
that tsc recognises as a discriminated union (because all members
share a literal `kind` field with distinct values). Pattern matching:

```mochi
match t {
  Leaf => 0
  Node { value, left, right } => value + sum(left) + sum(right)
}
```

Lowers to a TypeScript `switch` statement on the discriminator with
the exhaustiveness-via-never trick:

```typescript
function visitTree(t: Tree): bigint {
  switch (t.kind) {
    case "Leaf":
      return 0n;
    case "Node": {
      const { value, left, right } = t;
      return value + visitTree(left) + visitTree(right);
    }
    default: {
      const _exhaustive: never = t;
      throw new Error(`Unreachable: ${_exhaustive}`);
    }
  }
}
```

The `const _exhaustive: never = t` line is the exhaustiveness trick:
if any variant of `Tree` is not handled by the switch, `t` will have
a non-never type after the switch, and the assignment `const _: never
= t` will fail at compile time. This forces the emitter to cover
every variant.

TypeScript 5.0+ also supports `switch (true)` exhaustive checks
without the explicit `never` annotation in some cases (the inference
engine improved for discriminated unions in TS 4.9), but the explicit
form is more robust and works under all tsc versions we support.

Block-valued matches lower to an immediately-invoked function
expression:

```typescript
const result: bigint = ((): bigint => {
  switch (t.kind) {
    case "Leaf":
      return 0n;
    case "Node":
      return t.value + visitTree(t.left) + visitTree(t.right);
    default: {
      const _exhaustive: never = t;
      throw new Error(`Unreachable: ${_exhaustive}`);
    }
  }
})();
```

For single-expression match arms, the lowering can use a ternary
chain only when there are exactly two variants (an `Option`-shape):

```typescript
const result: bigint = opt !== null ? opt + 1n : 0n;
```

For more variants, the `switch` form is preferred.

Guarded patterns:

```mochi
match shape {
  Circle { radius } if radius > 10.0 => "big"
  Circle { radius }                  => "small"
  _                                  => "other"
}
```

TypeScript's `switch` does not support guards directly; the lowering
uses `if` / `else if` chains for guarded matches:

```typescript
function classifyShape(shape: Shape): string {
  if (shape.kind === "Circle" && shape.radius > 10.0) {
    return "big";
  }
  if (shape.kind === "Circle") {
    return "small";
  }
  return "other";
}
```

The narrowing on `shape.kind === "Circle"` lets tsc see `shape` as
`Circle` inside the branch, giving access to `shape.radius` typed as
`number`.

Generic ADTs:

```mochi
type Option<T> = | Some { value: T } | None
type Result<T, E> = | Ok { value: T } | Err { error: E }
```

Mochi `Option<T>` lowers to TypeScript `T | null` (per §1.7), no
custom type emitted. For other generic sums:

```typescript
export class Ok<T> {
  readonly kind = "Ok" as const;
  readonly value: T;

  constructor(value: T) {
    this.value = value;
    Object.freeze(this);
  }

  static of<T>(value: T): Ok<T> {
    return new Ok(value);
  }
}

export class Err<E> {
  readonly kind = "Err" as const;
  readonly error: E;

  constructor(error: E) {
    this.error = error;
    Object.freeze(this);
  }

  static of<E>(error: E): Err<E> {
    return new Err(error);
  }
}

export type MochiResult<T, E> = Ok<T> | Err<E>;
```

The generic parameters on the classes give clean inference. The
`MochiResult<T, E>` type alias combines them into a discriminated
union. The backend emits the user sealed union (always named
`MochiResult<T, E>` to avoid collision with external libraries that
may export a `Result` type, e.g., `neverthrow`, `ts-results`,
`@badrap/result`, `effect`'s `Either`).

For exhaustive matching on `MochiResult`:

```typescript
function unwrap<T, E>(r: MochiResult<T, E>): T {
  switch (r.kind) {
    case "Ok":
      return r.value;
    case "Err":
      throw new MochiThrownError(r.error);
    default: {
      const _: never = r;
      throw new Error("Unreachable");
    }
  }
}
```

See [[12-risks-and-alternatives]] for the typed-throw interaction
and the rationale for the Result-not-exceptions design.

## 5. Query DSL

Mochi's query DSL (`from x in xs select { ... }`, with `join`, `group
by`, `order by`, `limit`, `offset`, `where`) is the densest sub-language.
Its TypeScript lowering uses **iterator helpers** (ES2024) as the
primary IR for synchronous finite collections, plus
`mochi_runtime/query` helpers for the operations the stdlib does not
directly support (group-by, hash-join). For streaming queries, the
lowering uses `AsyncIterable<T>` chains via the same iterator-helper
methods on the async iterator. Full lowering is in
[[08-dataset-pipeline]]; this section names the surface forms only.

```mochi
let adults =
  from p in people
  where p.age >= 18
  order by p.name
  select { name: p.name, age: p.age }
```

Lowers to:

```typescript
const adults: readonly AdultRow[] = Iterator.from(people)
  .filter((p) => p.age >= 18n)
  .map((p) => AdultRow.of({ name: p.name, age: p.age }))
  .toArray()
  .sort((a, b) => mochiStrCompare(a.name, b.name));
```

The `Iterator.from(people)` lifts the iterable into an iterator
helper; the chained `.filter`, `.map` are lazy. `.toArray()`
materialises. The final `.sort()` is JS's `Array.prototype.sort` with
the comparator returning -1/0/+1.

`group by` lowers to a helper from `mochi_runtime/query` (JavaScript's
ES2024 `Object.groupBy(items, keyFn)` and `Map.groupBy(items, keyFn)`
provide a built-in group-by, but they return `Record<K, V[]>` /
`Map<K, V[]>` rather than the Mochi-shaped iterator-of-groups):

```typescript
import { groupBy } from "mochi_runtime/query";

const byDept: Map<string, readonly Person[]> = groupBy(
  people,
  (p) => p.dept,
);
```

ES2024's `Map.groupBy` is in fact a perfectly fine implementation;
the `mochi_runtime` re-exports it under the Mochi-shaped name:

```typescript
export function groupBy<T, K>(
  items: Iterable<T>,
  keyFn: (item: T) => K,
): Map<K, readonly T[]> {
  // ES2024 Map.groupBy
  return Map.groupBy(items, keyFn) as Map<K, readonly T[]>;
}
```

`join` lowers to a hash-join helper in `mochi_runtime/query` (since
JS's stdlib has no built-in cross-collection join). `limit` /
`offset` are `Iterator.prototype.take` and `.drop` (ES2024) for lazy
queries, or array slice for materialised.

Important: the **Mochi `stream<T>` type and JavaScript's iterator are
not the same thing**. JS iterators are synchronous and on-demand-
iterable; Mochi's `stream<T>` is a *time-evolving* asynchronous
publisher (closer to `AsyncIterable<T>`). The query DSL uses
iterators for *finite synchronous* collection queries; the public
`stream<T>` type uses `AsyncIterable<T>`. See [[09-agent-streams]] for
the agent / stream lowering and [[02-design-philosophy]] §13 for the
type-level distinction.

The ES2024 async iterator helpers (Stage-3 in TC39 as of 2024-Q4,
expected ES2025) include `AsyncIterator.from()`, `.map`, `.filter`,
`.take`, `.drop`, `.flatMap`, `.toArray`. The emitter uses them where
available; for runtimes that lack them, the runtime polyfills via
hand-rolled async iterator helpers.

## 6. Stream and agent core

```mochi
stream Tick = { time: time }

agent ticker {
  every 1s emit Tick { time: now() }
}
```

Streams lower to TypeScript `AsyncIterable<T>` (with concrete type
often `AsyncGenerator<T, void, undefined>`). The implementation is an
`async function*` generator function that yields values:

```typescript
import { setTimeout as sleep } from "node:timers/promises";

export async function* ticker(): AsyncGenerator<Tick, void, undefined> {
  while (true) {
    yield Tick.of({ time: now() });
    await sleep(1000);
  }
}
```

For browser environments where `node:timers/promises` is not
available, the runtime exposes `mochi_runtime/io/sleep`:

```typescript
export function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
```

Agents lower to a custom class wrapping an `AsyncIterableQueue<Message>`
plus an `AbortController`-supervised receiver task. JavaScript's
standard library has no actor builder; the canonical agent shape uses
the hand-rolled `AsyncIterableQueue` plus an `AbortController` for
supervision. See [[02-design-philosophy]] §8.

```typescript
import { AsyncIterableQueue } from "mochi_runtime/async/queue";

export class Ticker {
  private mailbox = new AsyncIterableQueue<Message>();

  constructor(private signal: AbortSignal) {
    void this.loop();
    signal.addEventListener("abort", () => this.mailbox.close());
  }

  cast(msg: Message): void {
    this.mailbox.push(msg);
  }

  async call(req: Omit<Message, "reply">): Promise<Reply> {
    const { promise, resolve } =
      Promise.withResolvers<Reply>();
    this.mailbox.push({ ...req, reply: resolve } as Message);
    return promise;
  }

  private async loop(): Promise<void> {
    for await (const msg of this.mailbox) {
      if (this.signal.aborted) break;
      await this.handle(msg);
    }
  }

  private async handle(msg: Message): Promise<void> {
    // dispatch to per-message handler
  }
}
```

Key design points:

- The mailbox is an `AsyncIterableQueue<Message>` with default
  unbounded capacity. Mochi's `bounded(N)` qualifier lowers to a
  bounded variant `AsyncIterableQueue<Message>(N)` whose `push`
  awaits a slot when full.
- The actor's main loop is `for await (const msg of this.mailbox) {
  ... }`. When the queue is closed (the supervising `AbortController`
  fires), the iterator returns `{ done: true }` and the `for await`
  exits.
- `cast(msg)` is `this.mailbox.push(msg)`, returning immediately for
  fire-and-forget. If the queue is bounded and full, the bounded
  variant's `push` returns a `Promise<void>` and the caller awaits;
  for the unbounded default, `push` returns `void` synchronously.
- `call(req)` uses `Promise.withResolvers()` (ES2024) to create the
  reply future, attaches the resolve function to the message
  payload, enqueues, and awaits. The receiver calls the resolve
  function when it has produced the reply. This is the canonical
  request-reply pattern for actor systems on AsyncIterable mailboxes.
- Spawning child tasks is `void childAgent.start(parentSignal)` where
  the child stores `parentSignal` and uses it for cancellation. The
  parent's `AbortController` is the supervision scope; if the parent
  aborts, all children see the signal and exit their loops.
- Cancellation propagates via `AbortSignal.addEventListener("abort", ...)`.
  Cooperative cancellation checkpoints are at every `await`; long-
  running loops check `signal.aborted` explicitly.

Supervision is built into the `AbortController`-tree shape: the
parent has a controller, and each child receives `parent.signal`. On
any child failure, the supervising parent calls `controller.abort()`,
which fires the signal in all children; they exit their loops, run
their cleanup code, and the parent collects any aggregated errors
into an `AggregateError` (ES2021). This matches OTP's `one_for_all`
strategy. For `one_for_one` (restart only the failed agent), the
lowering uses a try/catch around the agent's loop with a manual
restart:

```typescript
async function superviseOneForOne(spawn: () => Agent): Promise<void> {
  while (true) {
    try {
      const child = spawn();
      await child.completion;
      break;
    } catch (e) {
      console.error("Child failed; restarting", e);
    }
  }
}
```

The `AggregateError` constructor (`new AggregateError([e1, e2, ...],
"message")`) is available in Node 15+, Deno 1.0+, Bun 1.0+, all
evergreen browsers. The MEP-52 supervisor uses `AggregateError` to
report multiple simultaneous child failures, matching Python's
`ExceptionGroup` semantically (different syntactic surface, same
intent). See [[09-agent-streams]] §5 for the supervision tree
implementation.

Agents talk via typed message classes (Mochi `stream Tick = {...}`
becomes a TypeScript record class, and `ticker ! Tick { time: now() }`
becomes `ticker.cast(Message.of({ payload: Tick.of({ time: now() }) }))`):

```mochi
ticker ! Tick { time: now() }
```

Lowers to:

```typescript
ticker.cast(Message.of({ payload: Tick.of({ time: now() }) }));
```

JavaScript has no `Sendable`-equivalent annotation or compile-time
check. Structural protection comes from:

- The queue boundary: a value sent through `AsyncIterableQueue<Message>`
  is captured by the queue and the receiver gets the same reference,
  so the sender must voluntarily not mutate it after sending.
- Mochi's value-semantics contract on records (immutable `readonly`
  fields plus `Object.freeze()` by default) makes the sender-after-send
  mutation impossible at the JavaScript level: any attempt to assign
  `msg.field = newVal` throws `TypeError` at runtime.
- For records with mutable fields (Mochi `var` records), the backend
  emits a defensive copy at the send site if the message is later
  mutated by the sender. The default policy is to defensively copy on
  send; the cost is one allocation per send.

See [[02-design-philosophy]] §8 for the Sendability discussion and
the comparison against RxJS observables, Web Streams ReadableStream,
and Node EventEmitter.

The Web Platform provides several alternative concurrency primitives
that the emitter does *not* use as the default agent mailbox but does
expose for interop:

- **Web Streams** (`ReadableStream<T>`, `WritableStream<T>`,
  `TransformStream<T>`). Spec since 2017, broad runtime support. The
  emitter uses Web Streams only for the `--target=stream-pipe`
  feature (where a Mochi `stream<T>` interoperates with a Web Stream
  for upload/download). For mailboxes the backpressure semantics are
  heavier than needed.
- **MessageChannel / Worker postMessage**. The structured-clone-
  serialised channel between workers and the main thread. The
  emitter uses MessageChannel only for `--target=worker-bundle`
  (where a Mochi agent runs in a Web Worker). For in-process agents
  the AsyncIterableQueue is faster (no structured-clone overhead).
- **EventTarget / CustomEvent**. The DOM event system, available
  also in Node 19+ as `globalThis.EventTarget`. The emitter uses
  EventTarget only for the `AbortController` interaction.

The standard library provides higher-level async primitives that the
Mochi stream DSL maps onto: `Promise.all`, `Promise.allSettled`,
`Promise.race`, `Promise.any`, `Promise.withResolvers`. For
synchronisation primitives the emitter uses (in `mochi_runtime/sync`):
`MochiMutex` (Promise-based mutual exclusion), `MochiSemaphore`
(counting semaphore), `MochiEvent` (one-shot signal), `MochiCondition`
(condition variable with notify/notifyAll). These are hand-rolled on
top of `Promise.withResolvers`; no third-party dependency required.
See [[09-agent-streams]] §6.

## 7. Logic programming core

```mochi
fact parent(alice, bob).
fact parent(bob, charlie).

rule ancestor(X, Y) :- parent(X, Y).
rule ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).

query ancestors_of(X) := ancestor(alice, X).
```

The logic core targets a small embedded Datalog engine. The
TypeScript lowering emits a runtime call into
`mochi_runtime/datalog/Engine`, with facts and rules registered at
module init time. The engine implements semi-naive bottom-up
evaluation; magic-set transforms are a v2 concern.

Datalog terms are represented as a discriminated-union class
hierarchy:

```typescript
export class Atom {
  readonly kind = "Atom" as const;
  constructor(readonly name: string) {
    Object.freeze(this);
  }
}

export class IntTerm {
  readonly kind = "IntTerm" as const;
  constructor(readonly value: bigint) {
    Object.freeze(this);
  }
}

export class StringTerm {
  readonly kind = "StringTerm" as const;
  constructor(readonly value: string) {
    Object.freeze(this);
  }
}

export class Compound {
  readonly kind = "Compound" as const;
  constructor(
    readonly head: string,
    readonly args: readonly MochiDatalogTerm[],
  ) {
    Object.freeze(this);
    Object.freeze(args);
  }
}

export type MochiDatalogTerm = Atom | IntTerm | StringTerm | Compound;
```

The `readonly MochiDatalogTerm[]` typing (immutable, hashable
sequence via the `Object.freeze`) is critical for using terms as
canonical keys in the engine's index structures. JavaScript objects
are not natively hashable by structure (only by reference); the
engine uses a JSON-serialisation-based canonical-key derivation via
`mochiCanonicalKey(term)` to produce string keys for the term
indices.

Predicates are `(args: readonly MochiDatalogTerm[]) => boolean`
functions. Unification is a recursive walk that binds variables in a
`Map<string, MochiDatalogTerm>` substitution map. See
[[08-dataset-pipeline]] §4 for the engine internals.

The TypeScript target's Datalog engine has one differentiator:
JavaScript's `Map` (V8-backed in Node 22) is a high-performance hash
table that benefits from V8's hidden-class optimisations for
frequent keys, which on million-fact workloads can be competitive
with native-code engines. For first-evaluation workloads (cold
cache), the C and Kotlin engines win on raw CPU.

For *very* large fact bases (>10M rows), the TypeScript engine
offers an opt-in DuckDB-WASM backend that uses
`@duckdb/duckdb-wasm` for vectorised joins. This is a v2 feature; see
[[12-risks-and-alternatives]].

## 8. AI and FFI shells

### 8.1 AI shell

```mochi
let summary = ai("summarise this", text)
let result = generate("write a haiku about ", topic)
```

Mochi has two AI builtins: `ai(...)` (synchronous one-shot) and
`generate(...)` (streaming token-by-token). Both lower to async
functions returning the appropriate type.

`ai(...)` lowers to `mochi_runtime/ai/call(prompt: string, ...args:
unknown[]): Promise<string>` as an `async` function. The body of
`call(...)` dispatches based on provider configuration (env vars at
runtime, not codegen choices):

- The default backend uses the **OpenAI Node SDK** (`openai >= 4.50`)
  for OpenAI-compatible endpoints (also covers Azure OpenAI, Mistral
  AI, Together AI, OpenRouter, Groq, and dozens of other vendors that
  expose an OpenAI-compatible API).
- For Anthropic, the backend uses the **Anthropic Node SDK**
  (`@anthropic-ai/sdk >= 0.30`).
- For local inference via Ollama, the backend uses the built-in
  `fetch` directly against the Ollama REST API (no SDK; the API is
  small enough that a thin wrapper is sufficient).
- For Google Gemini, the backend uses the **Google Generative AI
  Node SDK** (`@google/generative-ai >= 0.20`).
- The provider selection happens at runtime via the
  `mochi_runtime/ai/Provider` interface; codegen always emits the
  interface-typed call and lets the runtime pick. See [[04-runtime]] §10.

`generate(...)` returns `AsyncIterable<MochiToken>` where `MochiToken`
is a small record class carrying the token text plus metadata
(logprobs, finish-reason). The implementation wraps the provider's
streaming response in an `async function*` generator:

```typescript
export async function* generate(
  prompt: string,
  ...args: unknown[]
): AsyncIterable<MochiToken> {
  const stream = await mochi_runtime.ai.stream(prompt, ...args);
  for await (const chunk of stream) {
    yield MochiToken.of({ text: chunk.text, logprob: chunk.logprob });
  }
}
```

See [[04-runtime]] §10.

### 8.2 FFI shell

```mochi
let result = ffi("std/json/parse", raw)
extern fun sqrt(x: float): float = "c:sqrt"
```

The `ffi(...)` builtin lowers to a
`mochi_runtime/ffi/call(path: string, ...args: unknown[]): unknown`
function that looks up the named function in a module registry. For
Mochi-to-Mochi FFI calls the registry just dispatches to the right
module's top-level function.

The `extern fun` form is the rich case: it declares a foreign
function and lets Mochi code call it directly. The lowering depends
on the target binding type:

For pure JavaScript external libraries (`extern fun parse(s: string):
Json = "js:JSON.parse"`) the backend emits a TypeScript wrapper
function that calls the external library directly:

```typescript
import { wrapJson } from "mochi_runtime/ffi";

export function parse(s: string): MochiResult<Json, string> {
  try {
    return Ok.of(wrapJson(JSON.parse(s)));
  } catch (e) {
    return Err.of((e as Error).message);
  }
}
```

For native libraries via Node's N-API (`extern fun sqrt(x: float):
float = "node-api:libm.sqrt"`), the backend emits an `import` from a
prebuilt native addon:

```typescript
import { sqrt as nativeSqrt } from "@mochi/native-libm";

export function sqrt(x: number): number {
  return nativeSqrt(x);
}
```

Node N-API is the canonical ABI-stable C-extension interface. As of
Node 22, N-API version 9 is the latest. The `node-gyp` or `prebuildify`
tooling produces the `.node` binary; the user must build separately
or download a prebuilt artifact.

For Deno's FFI (`extern fun sqrt(x: float): float = "deno-ffi:libm:sqrt"`),
the backend emits `Deno.dlopen`:

```typescript
const libm = Deno.dlopen("libm.dylib", {
  sqrt: { parameters: ["f64"], result: "f64" },
});

export function sqrt(x: number): number {
  return libm.symbols.sqrt(x);
}
```

For Bun's FFI (`extern fun sqrt(x: float): float = "bun-ffi:libm:sqrt"`),
the backend emits `bun:ffi`:

```typescript
import { dlopen, FFIType } from "bun:ffi";

const { symbols } = dlopen("libm.dylib", {
  sqrt: { args: [FFIType.f64], returns: FFIType.f64 },
});

export function sqrt(x: number): number {
  return symbols.sqrt(x);
}
```

For browser environments, native FFI is not available; the emitter
either falls back to a pure-JS implementation or fails the build with
a clear error message indicating the runtime mismatch.

For WebAssembly modules (`extern fun fast_hash(s: string): int =
"wasm:./fast_hash.wasm:fast_hash"`), the backend emits a WebAssembly
instantiation:

```typescript
const wasmModule = await WebAssembly.compileStreaming(
  fetch("./fast_hash.wasm"),
);
const wasmInstance = await WebAssembly.instantiate(wasmModule);
const wasmExports = wasmInstance.exports as {
  fast_hash(ptr: number, len: number): bigint;
  __wbindgen_malloc(size: number): number;
  __wbindgen_free(ptr: number, size: number): void;
  memory: WebAssembly.Memory;
};

export function fast_hash(s: string): bigint {
  const bytes = new TextEncoder().encode(s);
  const ptr = wasmExports.__wbindgen_malloc(bytes.byteLength);
  const memory = new Uint8Array(wasmExports.memory.buffer);
  memory.set(bytes, ptr);
  const result = wasmExports.fast_hash(ptr, bytes.byteLength);
  wasmExports.__wbindgen_free(ptr, bytes.byteLength);
  return result;
}
```

WebAssembly is the universal cross-runtime FFI target; the same
`.wasm` binary runs in Node, Deno, Bun, and the browser. See
[[11-testing-gates]] for the full FFI matrix.

For Mochi-as-library exports (Mochi code called by TypeScript
consumers), the emitted TypeScript module is itself the library; the
user imports `import { myfunc } from "@mochi/mypkg"` and calls Mochi
code directly. TypeScript's structural typing and module system give
clean interop.

The Mochi-to-TypeScript FFI surface is one of the load-bearing
reasons we want a TypeScript target at all (see
[[02-design-philosophy]] §1): it unlocks the entire npm ecosystem
(React, Vue, Angular, Svelte, Next.js, Vite, esbuild, Astro, NestJS,
Express, Fastify, Hono, Drizzle, Prisma, TypeORM, Apollo, tRPC,
GraphQL, Playwright, Puppeteer, Jest, Vitest, Mocha, Sinon, sharp,
canvas, three.js, d3, plotly.js, chart.js, lodash, ramda, date-fns,
zod, valibot, yup, ws, socket.io, axios, ky, undici), which is the
largest single-language web ecosystem in the world by package count
(~3.2M npm packages as of 2025-Q4, though most are deduplicates and
abandoned packages; the "actively maintained, downloaded by 1000+
users / week" set is closer to 80,000 packages). See
[[11-testing-gates]].

## 9. Strings, errors, concurrency colouring

### 9.1 Strings

Covered in §1.4 above. The TypeScript target is the *most expensive*
of all eight backends for string handling on non-ASCII text:
JavaScript's UTF-16 internal storage forces a code-unit-to-code-point
walk on every character access for strings containing astral-plane
code points. The cost is paid only on the boundary to byte-oriented
operations (UTF-8 encoding for HTTP, file I/O) or on indexing into a
non-BMP string.

Cross-boundary string costs:

- Mochi `string` to JS `string`: zero copy (both are the same `string`
  object).
- JS `string` to Mochi `string`: zero copy.
- Mochi `string` to `Uint8Array` (UTF-8 encoded for HTTP, JSON, file
  I/O): one O(n) encoding pass via `new TextEncoder().encode(s)`.
  V8's TextEncoder is SIMD-optimised for ASCII strings (since V8
  12.0, shipped in Node 22).
- Mochi `string` to native `char *` via Node N-API: one O(n) encoding
  pass via `napi_get_value_string_utf8(env, value, buf, buf_size,
  written)`. The native binding handles the encoding internally.
- Mochi `string` to WebAssembly UTF-8 buffer: one O(n) encoding pass
  via `TextEncoder` plus a memcpy into WASM linear memory.

See [[02-design-philosophy]] §6 for the full string-cost table and
the comparison against Python's PEP 393, Swift's
String.UTF8View, and Kotlin's UTF-16 + StringBuilder.

### 9.2 Errors

Mochi's error story is built on the `Result<T, E>` sum type and on
typed `throw e` / `catch e` blocks. JavaScript has **no checked
exception mechanism** (every function may throw any value), and the
JavaScript culture is heavily exception-based: idiomatic JS uses
`try/catch` for error handling, with most APIs documented to throw on
failure.

To preserve Mochi's typed-error semantic, we **do not use exceptions**
for Mochi error reporting; instead, every Mochi function that
declares `throws E` lowers to a TypeScript function returning
`MochiResult<T, E>`. This is a deliberate divergence from idiomatic
TypeScript (which would use exceptions); the rationale is in
[[02-design-philosophy]] §11.

Lowering rules:

- Mochi `fun foo(): T throws E { ... }` becomes TypeScript
  `function foo(): MochiResult<T, E> { ... }`. The body returns
  `Ok.of(value)` for the success path and `Err.of(error)` for the
  failure path.
- Mochi `try foo()` becomes TypeScript `unwrap(foo())` (which
  rethrows by wrapping `Err.error` in a `MochiThrownError(error)` for
  callers that want exception-style propagation), or `getOrNull(foo())`
  for the `T | null` variant.
- Mochi `try foo() catch e => handler` becomes TypeScript:
  ```typescript
  const r = foo();
  let result: T;
  switch (r.kind) {
    case "Ok":
      result = r.value;
      break;
    case "Err":
      result = handler(r.error);
      break;
  }
  ```
- Mochi `Result<T, E>` becomes TypeScript `MochiResult<T, E>` (the
  discriminated union defined above). Mochi `result.ok(x)` becomes
  `Ok.of(x)`, `result.err(e)` becomes `Err.of(e)`.
- Mochi `try?` (optional unwrap of result) becomes TypeScript
  `getOrNull(foo())`, returning `T | null`.
- Mochi `try!` (forced unwrap) becomes TypeScript `unwrap(foo())`,
  throwing `MochiThrownError` on `Err`.

The `MochiResult<T, E>` discriminated union is defined in
`mochi_runtime/result`:

```typescript
export class Ok<T> {
  readonly kind = "Ok" as const;
  readonly value: T;

  constructor(value: T) {
    this.value = value;
    Object.freeze(this);
  }

  static of<T>(value: T): Ok<T> {
    return new Ok(value);
  }
}

export class Err<E> {
  readonly kind = "Err" as const;
  readonly error: E;

  constructor(error: E) {
    this.error = error;
    Object.freeze(this);
  }

  static of<E>(error: E): Err<E> {
    return new Err(error);
  }
}

export type MochiResult<T, E> = Ok<T> | Err<E>;

export function unwrap<T, E>(r: MochiResult<T, E>): T {
  switch (r.kind) {
    case "Ok":
      return r.value;
    case "Err":
      throw new MochiThrownError(r.error);
  }
}

export function getOrNull<T, E>(r: MochiResult<T, E>): T | null {
  return r.kind === "Ok" ? r.value : null;
}

export class MochiThrownError extends Error {
  readonly mochiError: unknown;

  constructor(error: unknown) {
    super(typeof error === "string" ? error : String(error));
    this.mochiError = error;
    this.name = "MochiThrownError";
  }
}
```

For interop with TypeScript code that does throw exceptions (the
entire Web Platform and 99% of npm), the backend wraps every FFI
call in a try/catch that catches the documented exception types and
converts them to `Err` values:

```typescript
export function parse(s: string): MochiResult<Json, string> {
  try {
    return Ok.of(wrapJson(JSON.parse(s)));
  } catch (e) {
    if (e instanceof SyntaxError) {
      return Err.of(e.message);
    }
    throw e;  // unexpected exception type, propagate
  }
}
```

The exception types caught at the FFI boundary come from the Mochi
`extern fun` declaration's `throws E` clause, which the user must
write explicitly. See [[06-type-lowering]] §9.

ECMAScript 2021 introduced `AggregateError` for grouping multiple
errors into a single throw (typically used with `Promise.any`).
ECMAScript 2022 introduced the `cause` property on `Error` (`new
Error("outer", { cause: innerError })`) for error chaining. The
emitter uses `AggregateError` for supervisor failures (§6) and the
`cause` property for `Err.error` rewrap (when an `Err` is converted to
a thrown `MochiThrownError`, the original error is preserved as the
`cause`).

### 9.3 Concurrency colouring

Mochi distinguishes synchronous and asynchronous functions (`fun` vs
`async fun`). JavaScript's `async` modifier and Promise machinery
maps cleanly:

- Mochi `fun foo(): T { ... }` becomes TypeScript `function foo(): T
  { ... }`.
- Mochi `async fun foo(): T { ... }` becomes TypeScript `async
  function foo(): Promise<T> { ... }`.
- Mochi `await foo()` becomes TypeScript `await foo()`.
- Mochi `spawn foo()` (fire-and-forget) becomes TypeScript `void
  foo()` (the `void` operator discards the returned Promise to
  satisfy `@typescript-eslint/no-floating-promises`).

Isolation domains:

- An `agent` lowers to a custom class wrapping `AsyncIterableQueue<Message>`
  plus an `AbortController`-supervised receiver task (see §6). The
  methods on the agent are conceptually isolated to the actor's
  single-threaded receiver loop.
- A `@main` Mochi program (the top-level entry) lowers to a
  TypeScript `async function main(): Promise<void> { ... }` plus a
  `main().catch((e) => { console.error(e); process.exit(1); })` driver
  at the bottom of the module.
- Mochi code that needs to run in a separate isolate (Web Workers,
  Node `worker_threads`) uses the `--target=worker-bundle` build flag
  to produce a worker entry point.

JavaScript has no compile-time data-race check (the analogue of Swift
6's strict-concurrency mode). Structural protection comes from:

- The single-threaded event loop: in a single isolate (one Node
  process, one Deno isolate, one browser tab), all JavaScript code
  runs on a single OS thread, with concurrency only at `await`
  points. This eliminates many data-race classes by construction.
  Web Workers and Node `worker_threads` introduce true parallelism
  with separate isolates; the only communication channel is
  `postMessage` with structured clone, so direct shared state is not
  possible by default.
- The async-await coroutine model: within an async function, only
  one piece of code runs at a time (true concurrency only at `await`
  points). This naturally serialises coroutine bodies without
  additional locking.
- Mochi's value-semantics contract: records are immutable by default
  (frozen), collections are defensively copied at call boundaries, so
  the natural Mochi style produces code that is already free of data
  races.

This is the largest semantic gap between MEP-52 (TypeScript) and
MEP-49 (Swift): Swift's strict-concurrency catches sharing bugs at
compile time, TypeScript catches none of them (tsc does not model
thread safety). The mitigation is: (a) Mochi's own type checker
rejects sharing patterns at the Mochi level (so the TypeScript
codegen never emits unsafe sharing), (b) the Mochi runtime's
collection wrappers defensively copy at boundaries, and (c) Mochi
agents serialise via AsyncIterableQueue so cross-agent state is
naturally isolated. See [[02-design-philosophy]] §8 for the
Sendability discussion.

Other ECMAScript 2024 and TypeScript 5.6 features the lowering uses:

- **`Promise.withResolvers()`** (ES2024). Used for the agent
  `call(req)` pattern, the runtime mutex / semaphore / condition
  implementations, and any place where a future is created in one
  place and resolved in another. Node 22, Deno 2, Bun 1.1, Chrome
  119+, Firefox 121+, Safari 17.4+ all ship it natively.
- **`Set.prototype.union`, `intersection`, `difference`,
  `symmetricDifference`, `isSubsetOf`, `isSupersetOf`,
  `isDisjointFrom`** (ES2024). Used for Mochi set operations
  directly.
- **`Object.groupBy`, `Map.groupBy`** (ES2024). Used for the query
  DSL `group by` clause.
- **`Iterator.from()` + iterator helpers** (ES2024). Used pervasively
  in the query DSL lowering.
- **`Symbol.dispose`, `Symbol.asyncDispose`, `using` declarations**
  (ES2024 / TC39 Stage-3, in TS 5.2+, in Node 22+, in Deno 2+, in
  Bun 1.1+). Used by `mochi_runtime` for resource management:
  ```typescript
  using stream = await openFile("input.txt");
  // stream.close() called automatically at end of scope
  ```
- **`AbortController` / `AbortSignal`** (DOM Standard, in Node 15+,
  Deno 1.0+, Bun 1.0+, all evergreen browsers). Used for agent
  supervision.
- **`AggregateError`** (ES2021). Used for supervisor multi-failure
  reporting.
- **`Error.prototype.cause`** (ES2022). Used for error chaining.
- **`Array.prototype.toSorted`, `toReversed`, `toSpliced`** (ES2023).
  Used for immutable list operations (return a new array, don't
  mutate). Maps directly to Mochi's collection helpers.
- **`Array.prototype.findLast`, `findLastIndex`** (ES2023). Used for
  Mochi's `last_match` / `last_index_of` queries.
- **`structuredClone()`** (HTML living standard, in Node 17+, Deno
  1.20+, Bun 1.0+, all evergreen browsers). Used by the
  `mochi_runtime/copy/deepCopy` helper for deep-clone semantics
  where structural copying is needed at the message-passing
  boundary.

TypeScript 5.6 features:

- **`--noUncheckedSideEffectImports`**. Catches `import "./foo"` where
  the file does not exist (under the old behavior, side-effect-only
  imports were not checked for file existence).
- **`--rewriteRelativeImportExtensions`**. Lets Mochi-emitted `.ts`
  source import `./foo.ts` and have tsc rewrite the import to
  `./foo.js` in dist. Critical for the dual-source/dist build.
- **Region-aware narrowing for `using`** declarations. Lets `using
  x = ...` narrow `x`'s type after the disposable is acquired.
- **Iterator method types**. The standard library now has typings
  for `Iterator.from`, `.map`, `.filter`, etc. (added in TS 5.6).

## 10. What this surface does *not* include

- **Untyped `any`**: Mochi rejects it at the type layer. TypeScript
  has `any` and the temptation to weaken Mochi's type system to allow
  it is real, especially given JavaScript's dynamic-typing culture;
  we resist. `tsc --strict` warns on any `any` leakage, and the
  build gate fails if Mochi-emitted code contains `any` outside of
  explicitly-allowed positions. The emitter uses `unknown` for
  truly-dynamic values (which forces narrowing before use).
- **Implicit conversions**: ruled out above. Required to keep all
  eight backends identical.
- **`undefined` at the language level**: see §1.7. Mochi has no
  `undefined`; the emitter uses `null` for `Option<T>` and treats
  `undefined` as the "missing value" sentinel for
  `--noUncheckedIndexedAccess` only.
- **Class inheritance**: Mochi has no class inheritance (only
  sealed-union ADTs and protocol composition). The TypeScript
  `class Foo extends Bar` form is unused for user code. Internal
  helpers and FFI bridges to TS frameworks (which use inheritance,
  e.g., React class components, NestJS DI tokens) are the only
  places inheritance appears.
- **Decorators in user code**: not exposed at the Mochi language
  level. TC39 decorators (Stage-3 in 2023, ratified in 2024)
  provide method/class/field annotation; they require dynamic
  dispatch and they break some type-checker inference patterns.
  Internally the runtime may use decorators (`@cache`, `@deprecated`),
  but the user-facing surface never exposes them as customisation
  points.
- **Symbols beyond `Symbol.dispose`, `Symbol.iterator`, `Symbol.asyncIterator`**:
  not exposed at the Mochi language level. The emitter uses
  well-known symbols only for protocol implementations
  (`Symbol.iterator` for collection iteration, `Symbol.asyncIterator`
  for stream iteration, `Symbol.dispose` for `using`-managed
  resources).
- **Operator overloading**: TypeScript has no operator overloading.
  Mochi-emitted code never simulates it via `.add()` / `.sub()` /
  `.mul()` methods on user types; the runtime types
  (`MochiOrderedSet` etc.) do define iterator protocols, but the
  user-facing surface never exposes them as customisation points.
- **`typeof` type queries on values**: not exposed at the Mochi
  language level. The emitter uses `typeof x === "number"` only
  internally for narrowing.
- **`keyof`, `infer`, mapped types, conditional types**: not exposed
  at the Mochi language level. The emitter generates concrete types
  rather than computed types; the type system is nominal at the
  Mochi level.
- **Module-augmentation (`declare module`)**: not exposed. The
  emitter does not augment third-party module types.
- **`@ts-ignore` / `@ts-expect-error`**: never emitted in Mochi-
  generated code. Any `tsc` diagnostic on Mochi-emitted code is a
  bug in the emitter, not a thing to suppress.
- **JSX**: not exposed at the Mochi language level. The emitter does
  not produce `.tsx` files. For React/Vue/Svelte interop, the user
  writes the framework-specific code separately and the Mochi
  modules expose plain TypeScript functions that the framework code
  consumes.
- **`namespace` declarations** (TypeScript's pre-ES-modules
  namespace mechanism): not exposed at the Mochi language level.
  The emitter uses ES modules exclusively. Internally, sum-type
  variant constructors are sometimes grouped inside a `namespace`
  block scoped to the type name (e.g., `namespace Tree { export
  class Leaf { ... } }`), but this is an emitter detail.
- **`enum` declarations**: TypeScript's `enum` keyword produces a
  runtime object with both forward and reverse mappings (for numeric
  enums). The emitter does not use `enum`; it uses literal union
  types (`type Color = "red" | "green" | "blue"`) for closed-world
  string enums and discriminated unions for full ADTs.
- **`const enum` declarations**: not used. `const enum` inlines
  values at compile time, which breaks tree-shaking and produces
  hard-to-debug stack traces.
- **Triple-slash directives** (`/// <reference path="..." />`): not
  used. ES module imports replace them.

## 11. Surface-to-TypeScript cheat sheet (cross-reference)

| Mochi form | TypeScript lowering | Note |
|------------|---------------------|------|
| `let x = ...` | `const x: T = ...` | §1.1, [[05-codegen-design]] §4 |
| `var x = ...` | `let x: T = ...` | §1.1 |
| `int` (i53-fits) | `number` | §1.2, [[06-type-lowering]] §5 |
| `int` (default) | `bigint` | §1.2 |
| `float` | `number` | §1.2 |
| `bool` | `boolean` | §1.2 |
| `string` | `string` (UTF-16 internal) | §1.4 |
| `bytes` | `Uint8Array` | §1.2 |
| `list<T>` | `readonly T[]` (immutable) or `T[]` (mutable) | §3, [[06-type-lowering]] §10 |
| `map<K,V>` | `Map<K, V>` | §3 |
| `set<T>` | `Set<T>` (ES2024 methods) | §3 |
| `record T { ... }` | `class T` with `readonly` fields + `Object.freeze()` | §2.3 |
| `type T = A \| B` (sum) | discriminated union via `kind` literal field | §4 |
| `Option<T>` | `T \| null` | §1.7 |
| `Result<T, E>` | `MochiResult<T, E>` = `Ok<T> \| Err<E>` | §9.2 |
| `match` | `switch (x.kind)` with `never` exhaustiveness | §4 |
| `fun(...) => ...` | arrow function | §2.2 |
| `from ... select ...` | `Iterator.from(...).filter(...).map(...).toArray()` | §5, [[08-dataset-pipeline]] |
| `agent ...` | custom class with `AsyncIterableQueue` + `AbortController` | §6 |
| `stream<T>` | `AsyncIterable<T>` (`async function*` generator) | §6 |
| `fact / rule / query` | runtime Datalog engine | §7, [[08-dataset-pipeline]] §4 |
| `ai(...)` | `mochi_runtime/ai/call` dispatch (OpenAI / Anthropic / Gemini / Ollama) | §8.1 |
| `generate(...)` | `async function*` returning `AsyncIterable<MochiToken>` | §8.1 |
| `extern fun ... = "node-api:..."` | Node N-API binding | §8.2 |
| `extern fun ... = "deno-ffi:..."` | `Deno.dlopen` binding | §8.2 |
| `extern fun ... = "bun-ffi:..."` | `bun:ffi` binding | §8.2 |
| `extern fun ... = "wasm:..."` | WebAssembly instantiation | §8.2 |
| `extern fun ... = "js:..."` | direct JS function call | §8.2 |
| `throws E` | return `MochiResult<T, E>` (no exceptions) | §9.2 |
| `async fun` | `async function` | §9.3 |
| `await x` | `await x` | §9.3 |
| `spawn f()` | `void f()` (fire-and-forget Promise) | §9.3 |
| `@ui` | `queueMicrotask(...)` or similar | §9.3 |
| `linear T` | not yet supported (TS has no affine/linear types) | §9.3 |
| `borrowed T` | not yet supported | §9.3 |
| identifier `class` | identifier `class_` | §1.6 |
| identifier `function`, `import`, `export`, etc. | trailing underscore | §1.6 |
| doc comment `/// ...` | `/** ... */` JSDoc comment | §1.1 |

## 12. Doc comments and JSDoc

Mochi supports doc comments via `///` (triple-slash, Rust-style). The
TypeScript lowering maps doc comments to **JSDoc** comments: a
`/** ... */` block immediately before the declaration. JSDoc is the
de-facto TypeScript documentation format; tsc consumes JSDoc tags
for type information when `--allowJs` is used, and IDE tooling (VS
Code, WebStorm) renders JSDoc in hover tooltips.

```mochi
/// Returns the area of a circle given its radius.
fun area(radius: float): float {
  return 3.14 * radius * radius
}
```

Lowers to:

```typescript
/**
 * Returns the area of a circle given its radius.
 */
export function area(radius: number): number {
  return 3.14 * radius * radius;
}
```

For multi-line doc comments with parameter and return descriptions,
the lowering uses standard JSDoc tags:

```typescript
/**
 * Parse the input string into a Mochi AST.
 *
 * @param s - The source string.
 * @returns Ok(ast) on success, or Err(message) on parse failure.
 */
export function parse(s: string): MochiResult<AST, string> {
  // ...
}
```

The `@param`, `@returns`, `@throws`, `@example`, `@deprecated`,
`@since`, `@see` JSDoc tags are all supported. The TypeScript
compiler reads JSDoc tags for several purposes: `@deprecated` produces
a strikethrough in IDE autocomplete, `@param` types (when written in
JSDoc form like `@param {string} s`) are honoured as type info under
`--allowJs`, `@see` links are rendered as clickable references. The
emitter generates JSDoc in TSDoc-compatible format (the formalised
subset used by Microsoft's API Extractor and TypeScript's official
documentation pipeline).

Module-level Mochi doc comments lower to module-level JSDoc as the
first statement of the module (which by convention documents the
module itself). Class-level Mochi doc comments lower to JSDoc
immediately before the `class` keyword.

See [[06-type-lowering]] §13.

## 13. Modules and imports

Mochi modules map to TypeScript ES modules. A Mochi file
`src/mathutils/extra.mochi` becomes a TypeScript file
`src/generated/mathutils/extra.ts`, with the parent directories
getting `index.ts` re-export files:

```typescript
// src/generated/mathutils/index.ts
export * from "./extra.ts";
export * as extra from "./extra.ts";
```

The `.ts` extension on the import is required under TypeScript 5.6's
`--rewriteRelativeImportExtensions` flag: source files import `.ts`
explicitly, and the emitted `.js` files have `.js` imports rewritten
by tsc. This unblocks the dual-source / dual-dist workflow (you can
run the `.ts` files directly under `ts-node` / `tsx` / `bun`, or you
can run the compiled `.js` files under plain `node`).

Mochi imports map to TypeScript imports. The lowering rules:

| Mochi | TypeScript |
|-------|------------|
| `import "mathutils/extra"` | `import * as extra from "./mathutils/extra.ts"` |
| `import "mathutils/extra" as me` | `import * as me from "./mathutils/extra.ts"` |
| `import { add, sub } from "mathutils/extra"` | `import { add, sub } from "./mathutils/extra.ts"` |
| `import "github.com/foo/bar"` | `import { ... } from "@org/bar"` (with the runtime path coming from `package.json` `"dependencies"`) |

External dependencies (npm packages) are declared in the Mochi
project's `mochi.toml` and surfaced into the generated `package.json`
under `"dependencies"`:

```json
{
  "name": "myapp",
  "version": "0.1.0",
  "type": "module",
  "engines": {
    "node": ">=22.0.0",
    "deno": ">=2.0.0",
    "bun": ">=1.1.0"
  },
  "dependencies": {
    "openai": "^4.50.0",
    "@anthropic-ai/sdk": "^0.30.0",
    "@google/generative-ai": "^0.20.0",
    "undici": "^6.0.0"
  },
  "devDependencies": {
    "typescript": "^5.6.0",
    "@types/node": "^22.0.0",
    "prettier": "^3.3.0",
    "eslint": "^9.0.0",
    "@typescript-eslint/parser": "^8.0.0",
    "@typescript-eslint/eslint-plugin": "^8.0.0"
  },
  "exports": {
    ".": {
      "types": "./dist/index.d.ts",
      "deno": "./dist/deno/index.js",
      "bun": "./dist/bun/index.js",
      "browser": "./dist/browser/index.js",
      "node": "./dist/node/index.js",
      "default": "./dist/node/index.js"
    }
  }
}
```

The user's Mochi project lock file `mochi.lock` is translated to a
`package-lock.json` (the canonical lock file format for npm) or a
`pnpm-lock.yaml` / `bun.lockb` for alternate package managers. The
two lock files are kept in sync by the Mochi build driver. See
[[10-build-system]] for the build system.

## 14. Visibility

Mochi has three visibility levels: `pub` (public), the default
(package-private), and `priv` (file-private). TypeScript has two
formal visibility levels at the class level (`public`, `private`,
`protected`) plus the ECMAScript private-field syntax (`#field`)
which is enforced at runtime, plus the module-level export/no-export
distinction:

- A name **with** `export` is exported from the module and accessible
  to importers.
- A name **without** `export` is module-private (no syntactic
  enforcement, but the name is not in the module's export set).

The Mochi lowering uses:

- Mochi `pub` becomes TypeScript `export`-ed names.
- Mochi default (package-private) becomes TypeScript names exported
  with a `_internal` prefix (the convention is documented but not
  enforced).
- Mochi `priv` becomes TypeScript names *without* `export` keyword
  (module-private, no way to import from outside).

For class members:

- Mochi `pub` field on a `type` block becomes TypeScript `readonly
  field: T` (no modifier; public is the default).
- Mochi default field becomes TypeScript `protected readonly field:
  T` (not strictly correct since Mochi has no inheritance, but
  protected is the closest "internal" notion).
- Mochi `priv` field becomes TypeScript `#field: T` (ECMAScript
  private field, runtime-enforced; access from outside the class
  throws `SyntaxError` at parse time).

The `#field` ECMAScript private syntax (Stage-4 since 2021, in TS
3.8+, in Node 12+, in Deno 1.0+, in Bun 1.0+) is the strongest
private mechanism in JavaScript: even at runtime, no introspection
can read the private field. This is unlike TypeScript's `private`
modifier (compile-time only, accessible at runtime via
`obj["fieldName"]`). The emitter prefers `#field` for true privacy.

## 15. Reflection

Mochi has limited reflection: a Mochi value's type can be queried at
runtime via the `typeof` builtin, and a record's field names can be
enumerated via the `fields_of` builtin. Both lower to TypeScript:

- `typeof(x)` becomes a runtime helper `mochiTypeName(x)` that
  returns the class name for class instances (`x.constructor.name`),
  the JS `typeof` string for primitives, "Map" / "Set" / "Array" for
  collection types.
- `fields_of(record)` becomes `Object.keys(record)` (which returns the
  enumerable own-property names of the frozen record).

The TypeScript target does not expose JavaScript's full reflection
capabilities (`Reflect`, `Proxy`, `Object.getPrototypeOf`,
`Object.getOwnPropertyDescriptors`) to Mochi code, because those would
break Mochi's static-typing guarantees. The two builtins above are
the only reflection surface in v1.

For more advanced reflection (e.g., the Mochi `serialize` builtin
that walks a record's fields and produces a Map), the lowering uses
the record's `toJSON()` method when available, falling back to
`Object.fromEntries(Object.entries(record))` for the generic case.

## 16. Identifier visibility and exports

Each Mochi-generated TypeScript module emits explicit `export`
declarations:

```typescript
export function publicFn() { ... }
export class PublicClass { ... }
export type PublicSum = A | B;

function privateFn() { ... }  // not exported
```

This is the canonical TypeScript way to control module-level
visibility. The Mochi codegen computes the export set from the set of
public top-level declarations.

The emitter also produces a re-export `index.ts` file at the top of
the generated module tree:

```typescript
// src/generated/index.ts
export * from "./mathutils/extra.ts";
export * from "./other/module.ts";
// ...
```

This single-file entry is what `package.json` `"main"` (or `"exports"`
default) points to.

## 17. Open questions for [[02-design-philosophy]]

- **Codegen IR**: a Mochi-internal CST builder versus the TypeScript
  Compiler API's `factory` module versus `ts-morph`. (Resolved in
  [[02-design-philosophy]] §3 and [[05-codegen-design]] §1: a
  hand-rolled Mochi-side CST builder that emits source strings,
  formatted post-hoc by `prettier 3.x`.)
- **Type checker version pinning**: TS 5.6 versus TS 5.7 (Nov 2024,
  added `--noUncheckedSideEffectImports` regressions fixed) versus
  TS 5.8 (Feb 2025, deferred decorator metadata). Pin TS 5.6.3 as the
  baseline; allow 5.7 / 5.8 / 5.9 / 6.0 as upper bounds with rolling
  warning-only secondary gates.
- **ECMAScript target floor**: ES2024 versus ES2022 (older but
  broader runtime support) versus ES2025 (too aggressive,
  `Promise.try`, `RegExp.escape`, async iterator helpers are still
  Stage-3). Pin ES2024 as the floor.
- **Node version floor**: Node 22 LTS (Apr 2024) versus Node 20 LTS
  (Oct 2023, EOL Apr 2026). Pin Node 22 as the floor because Node 20
  lacks `Promise.withResolvers` natively and the ES2024 set methods.
- **Deno version floor**: Deno 2.0 (Oct 2024) versus Deno 1.46
  (Sept 2024). Pin Deno 2.0 as the floor because Deno 2.0 brings npm
  interop and the JSR registry integration.
- **Bun version floor**: Bun 1.1 (April 2024) versus Bun 1.0 (Sept
  2023). Pin Bun 1.1 as the floor.
- **Browser baseline**: Chrome 124+, Firefox 125+, Safari 17.4+
  (cohort after April 2024) versus an older baseline with polyfills.
  Pin the April-2024 cohort as the floor; older browsers need
  polyfills from the runtime.
- **prettier version**: prettier 3.3 (June 2024) is the v1 baseline;
  3.4 (Dec 2024) and beyond are acceptable upper bounds.
- **ESLint version**: ESLint 9 (April 2024) with flat config is the
  v1 baseline; the legacy `.eslintrc` format is not used.
- **Wheel reproducibility equivalent for npm**: SOURCE_DATE_EPOCH for
  npm tarballs, plus sorted tarball entries (npm 10 supports
  reproducible tarballs natively via `--pack-destination` plus
  `SOURCE_DATE_EPOCH`). See [[02-design-philosophy]] §16 and
  [[11-testing-gates]] §6.
- **Sigstore provenance**: npm 10.5+ supports `npm publish
  --provenance` with GitHub Actions OIDC; the build pipeline assumes
  this. Alternative CI providers (GitLab CI, CircleCI) require
  manual token configuration.

## 18. Cross-references

- [[02-design-philosophy]] (next note): why each of the choices above
  was made, including the case for TS 5.6 as the floor and the
  comparison against alternative Mochi-to-JS pathways (Babel,
  esbuild, SWC, ts-blank-space).
- [[03-prior-art-transpilers]]: survey of CoffeeScript, ReScript,
  ReasonML, Fable, Elm, PureScript, Kotlin/JS, ScalaJS, GopherJS,
  TinyGo for JS, Pyodide, Brython, Transcrypt, Skulpt,
  AssemblyScript, Hegel, Flow, JSII, plus the toolchains (Babel,
  SWC, esbuild, TypeScript, sucrase, Bun's transpiler,
  ts-blank-space).
- [[04-runtime]]: the `mochi_runtime` npm package layout, including
  the `async`, `collections`, `io`, `ai`, `ffi`, `datalog`, `query`,
  and `errors` submodules.
- [[05-codegen-design]]: the IR layer that turns this surface into
  emitted TypeScript source via a Mochi-side CST builder.
- [[06-type-lowering]]: the per-type details glossed here, including
  the bigint-vs-number monomorphisation rules and the freeze policy.
- [[07-runtime-portability]]: the TypeScript / runtime version matrix
  (TS 5.6, 5.7, 5.8; Node 22, 23, 24; Deno 2.0, 2.1, 2.2; Bun 1.1,
  1.2; Chrome / Firefox / Safari cohorts).
- [[08-dataset-pipeline]]: the query DSL lowering in full, including
  the DuckDB-WASM / Arrow.js optional backends and the Datalog
  engine design.
- [[09-agent-streams]]: agent and stream lowering on
  AsyncIterableQueue + AbortController, including the supervision
  tree design and the AggregateError integration.
- [[10-build-system]]: npm + tsc + package.json integration, the
  generated `package.json`, the dual-dist build matrix (Node, Deno,
  Bun, browser), and the publishing pipeline (npm + JSR + esbuild
  bundle).
- [[11-testing-gates]]: the test-suite gates for v0.10 ship; what
  fraction of `examples/v0.2`-`v0.7` must transpile, type-check
  (`tsc --strict --noUncheckedIndexedAccess`), build (npm pack), and
  run on each runtime (Node 22, Deno 2, Bun 1.1, browser via
  Playwright).
- [[12-risks-and-alternatives]]: the risk register and the
  alternatives considered (notably Babel for transpilation, SWC for
  speed, esbuild for bundling, ts-blank-space for type-strip-only
  emit).
- [[../0051/01-language-surface]]: the Python-target analogue, the
  closest in spirit since both target dynamically-checked runtimes
  with mature ecosystems, first-class generics, and async/await
  coroutines.
- [[../0050/01-language-surface]]: the Kotlin-target analogue, which
  shares the typed-Result error story and the actor-as-class shape.
- [[../0049/01-language-surface]]: the Swift-target analogue, which
  shares the actor-as-class shape and the AsyncStream / AsyncIterable
  type for streams.
- [[../0048/01-language-surface]]: the .NET-target analogue, which
  shares the dynamic-language-feel-from-static-type-system pattern
  via C#'s `dynamic` (which we never use, similar to how we never
  use TS `any`).
- [[../0047/01-language-surface]]: the JVM-bytecode-target analogue,
  which shares the bytecode-runtime model (both JVM and V8 run on a
  managed VM).
- [[../0046/01-language-surface]]: the BEAM-target analogue, whose
  agent / mailbox / supervision design directly inspired the
  TypeScript AsyncIterableQueue + AbortController lowering.
- [[../0045/01-language-surface]]: the C-target analogue, whose
  C-ABI extern story is mirrored via Node N-API, Deno FFI, Bun FFI,
  and WebAssembly bindings.
