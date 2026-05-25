---
title: "Type lowering: per-Mochi-type details onto TypeScript 5.6"
description: "Per-Mochi-type lowering rules onto TypeScript 5.6 / ECMAScript 2024: int as bigint vs number (driven by interval-analysis monomorphisation), float as number, bool as boolean, string with code-point semantics layered over UTF-16 code units, bytes as Uint8Array, list<T> as readonly T[] or T[] depending on escape analysis, map<K, V> as Map<K, V>, set<T> as Set<T> with ES2024 methods, record as class with readonly fields plus private constructor plus static factory, sum type as discriminated union with literal kind discriminator, option as T | null (not T | undefined), Result as MochiResult<T, E> discriminated union, callable as arrow function type, async iterator as AsyncIterable<T> at parameter positions and AsyncGenerator<T, void, undefined> at return positions, agent as class subclassing AgentBase<Msg>. Plus the variance story: readonly T[] is covariant, T[] is invariant, Map<K, V> variance gotchas, function param contravariance, declaration-site variance does not exist in TS so we work around with helper types."
sidebar_position: 6
---

# Type lowering for MEP-52 (Mochi to TypeScript)

**Author**: Mochi compiler team, internal note.
**Date**: 2026-05-23 17:10 (GMT+7).
**Method**: take the type-lowering one-pager from the shared decisions anchor and expand each row to a section with motivation, the chosen TS spelling, the rejected alternatives, the runtime helper(s), and a complete example. Cross-reference [[mep-0051]] note 06 for the Python target's parallel decisions; the TS target diverges where TS has features Python lacks (variance, bigint, structural narrowing) and where TS lacks features Python has (named tuples via NamedTuple, frozen dataclasses).

This note specifies the type lowering rules for **every** Mochi type onto TypeScript 5.6 with ECMAScript 2024 emit target. Each section follows the same structure:

1. **Annotation form**: what TS syntax appears in the generated `.ts` file.
2. **Runtime representation**: what JS object backs the value at runtime.
3. **Choice rationale**: why this spelling vs the alternatives.
4. **Variance**: how the type behaves under subtyping.
5. **Caveats**: edge cases, runtime cost, source-map gotchas.
6. **Example**: a Mochi snippet plus its TS lowering.

## 1. `int` -> `bigint` or `number` (monomorphisation per-producer)

### 1.1 Annotation form

`bigint` when the IntFit pre-pass cannot prove the value fits in `[-(2^53 - 1), 2^53 - 1]`. `number` when it can.

```typescript
const counter: bigint = 0n;       // arbitrary precision
const small: number = 42;          // proven to fit in i53
```

### 1.2 Runtime representation

- `bigint`: JS native BigInt. V8 (Node 22 / Chrome 122+) and JSC (Safari 17+) implement BigInt as a tagged pointer to a heap-allocated arbitrary-precision integer; for values that fit in 64 bits the underlying storage is two 32-bit limbs; for larger values it grows. Bun and Deno share the V8 implementation.
- `number`: IEEE 754 double-precision float, 64 bits. Integers up to `Number.MAX_SAFE_INTEGER === 9_007_199_254_740_991` are exactly representable.

### 1.3 Choice rationale

Mochi `int` is **arbitrary-precision** in the spec. The natural target is `bigint`. However, `bigint` operations are 5x to 50x slower than `number` operations in V8 (per V8 microbenchmarks, 2024). Monomorphisation lets us pick `number` whenever the interval analysis proves it is safe.

Rejected alternatives:

- **Always `number`**: violates Mochi semantics for values larger than 2^53.
- **Always `bigint`**: 5x to 50x perf hit on loops over small counters; agent mailbox throughput would tank.
- **`bigint` + autobox to `number`**: TS forbids `bigint + number`, so the bridge becomes verbose and error-prone.
- **`Long` library**: a third-party dep we do not want; native `bigint` is free.

### 1.4 Variance

`bigint` and `number` are both **invariant** primitives in TS (they have no subtypes). Literal types `42n` and `42` are subtypes of `bigint` and `number` respectively; the codegen does not use literal types for int values because the IntFit interval is enough.

### 1.5 Caveats

- Mixing `bigint` and `number` in arithmetic is a TS type error. If the IntFit analysis flips a producer between the two representations between IR builds, the codegen must emit explicit casts: `Number(b)` or `BigInt(n)`. The cost: one allocation per cast. Hot loops should be monomorphised consistently.
- Comparison `5n === 5` is `false` (different types). The codegen always normalises to the IntFit representation before comparison.
- Mochi `print(n)` for `n: int` emits `n.toString()` for `bigint` (which omits the `n` suffix) and `String(n)` for `number`. Both produce the same human-readable text.
- JSON serialisation: `bigint` is not natively JSON-encodable. The runtime's `JsonValue` lowering (see [[04-runtime]] §11) stringifies bigints to `"42"` strings and tags them with a sentinel for round-trip.

### 1.6 Example

Mochi:

```mochi
fun add(a: int, b: int) -> int { return a + b }

fun fib(n: int) -> int {
  if n < 2 { return n }
  return fib(n - 1) + fib(n - 2)
}
```

TS (when IntFit picks bigint):

```typescript
export function add(a: bigint, b: bigint): bigint {
  return a + b;
}

export function fib(n: bigint): bigint {
  if (n < 2n) {
    return n;
  }
  return fib(n - 1n) + fib(n - 2n);
}
```

TS (when IntFit picks number; e.g. caller proves `n <= 92` so `fib(n)` fits in i53):

```typescript
export function add_n(a: number, b: number): number {
  return a + b;
}

export function fib_n(n: number): number {
  if (n < 2) {
    return n;
  }
  return fib_n(n - 1) + fib_n(n - 2);
}
```

The `_n` suffix distinguishes the monomorphised number variant from the default bigint variant.

### 1.7 Fixed-width integer subtypes

Mochi also has `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`. These lower as follows:

| Mochi | TS spelling | Wrap semantics |
|-------|-------------|-----------------|
| `i8`  | `number` | `BigInt.asIntN(8, BigInt(x))` then `Number(...)` |
| `i16` | `number` | `BigInt.asIntN(16, BigInt(x))` then `Number(...)` |
| `i32` | `number` | `(x \| 0)` (the `\| 0` idiom) |
| `i64` | `bigint` | `BigInt.asIntN(64, x)` |
| `u8`  | `number` | `(x & 0xff)` |
| `u16` | `number` | `(x & 0xffff)` |
| `u32` | `number` | `(x >>> 0)` (the unsigned shift idiom) |
| `u64` | `bigint` | `BigInt.asUintN(64, x)` |

These map to the wrapping arithmetic semantics required by Mochi's MEP-45 (`-fwrapv` for C).

## 2. `float` -> `number`

### 2.1 Annotation form

`number` for Mochi `float`, `f32`, and `f64`.

```typescript
const pi: number = 3.14159;
const sqrtTwo: number = Math.sqrt(2);
```

### 2.2 Runtime representation

IEEE 754 double-precision binary64. Both `f32` and `f64` Mochi types lower to the same JS `number`; the only difference is that `f32` values are rounded to 32-bit precision at every binary operation via `Math.fround`.

### 2.3 Choice rationale

JS has only one floating-point type. There is no f32 primitive. We accept the precision over-allocation for `f32` (8 bytes of storage instead of 4) but emit `Math.fround` to preserve f32 rounding semantics.

Rejected alternatives:

- `Float32Array` for f32 fields: works at the storage level but does not propagate through expressions.
- Bignum floats via a library: too heavy for the throughput Mochi targets.

### 2.4 Variance

`number` is invariant. Literal types `3.14` exist but the codegen does not use them.

### 2.5 Caveats

- NaN comparison: `NaN === NaN` is `false` in JS. Mochi semantics match. The codegen emits `Number.isNaN(x)` for explicit NaN tests.
- Infinity: `Infinity` and `-Infinity` are JS literals; `1.0 / 0.0` produces `Infinity` (no division-by-zero exception in JS).
- Float-to-int conversion: `Number(BigInt(Math.trunc(f)))` for `f as i64`; `Math.trunc(f) | 0` for `f as i32`.

### 2.6 Example

Mochi:

```mochi
fun hypot(a: float, b: float) -> float {
  return sqrt(a * a + b * b)
}
```

TS:

```typescript
export function hypot(a: number, b: number): number {
  return Math.sqrt(a * a + b * b);
}
```

For `f32`:

Mochi:

```mochi
fun mulF32(a: f32, b: f32) -> f32 {
  return a * b
}
```

TS:

```typescript
export function mulF32(a: number, b: number): number {
  return Math.fround(Math.fround(a) * Math.fround(b));
}
```

The wrapping `Math.fround` calls ensure the result is exactly the f32-rounded value, matching IEEE 754 binary32 semantics.

## 3. `bool` -> `boolean`

### 3.1 Annotation form

`boolean`.

```typescript
const flag: boolean = true;
```

### 3.2 Runtime representation

JS native boolean. One bit semantically; engines store as a tagged immediate.

### 3.3 Choice rationale

Trivial; no alternatives.

### 3.4 Variance

Invariant primitive. Literal types `true` and `false` are subtypes but the codegen does not use them.

### 3.5 Caveats

- JS truthiness: `if (x)` with `x: bigint = 0n` is **false**; `x: bigint = 1n` is **true**; same as Mochi. But `x: string = ""` is **false** in JS, while Mochi `if` requires explicit `bool`, so the codegen never emits a non-boolean as the condition. The type checker (tsc) flags this for free.
- `==` vs `===`: the codegen always emits `===` and `!==` to avoid JS coercion rules.

### 3.6 Example

```mochi
fun and3(a: bool, b: bool, c: bool) -> bool {
  return a && b && c
}
```

```typescript
export function and3(a: boolean, b: boolean, c: boolean): boolean {
  return a && b && c;
}
```

## 4. `string` -> `string`

### 4.1 Annotation form

`string`.

```typescript
const s: string = "hello";
```

### 4.2 Runtime representation

JS native string: UTF-16 code-unit sequence. V8 / JSC use cons strings + sliced strings + interned strings under the hood; observable behaviour is "an indexable sequence of UTF-16 code units".

### 4.3 Choice rationale

The natural target. No alternatives in the JS ecosystem.

### 4.4 Variance

Invariant primitive. Literal types like `"hello"` are subtypes.

### 4.5 Caveats

This is the **biggest semantic mismatch** in the type lowering.

Mochi `string` is a sequence of **Unicode code points**. JS `string` is a sequence of **UTF-16 code units**. For strings within the Basic Multilingual Plane (BMP, U+0000 to U+FFFF) the two are equivalent (one code unit per code point). For supplementary plane code points (emoji, rare CJK, math symbols above U+FFFF), each Mochi code point is **two** JS code units (a surrogate pair).

This affects:

| Mochi op | Naive JS | Correct JS |
|----------|----------|------------|
| `len(s)` | `s.length` | `[...s].length` or `mochiStrLen(s)` |
| `s[i]` (i-th code point) | `s[i]` | `[...s][i]` or `mochiStrAt(s, i)` |
| `s[a..b]` (slice by code point) | `s.slice(a, b)` | `[...s].slice(a, b).join("")` or `mochiStrSlice(s, a, b)` |
| `for c in s` | `for (let i = 0; i < s.length; i++)` | `for (const c of s)` (correct!) |
| `s + t` (concat) | `s + t` | `s + t` (correct!) |
| `s == t` (equality) | `s === t` | `s === t` (correct!) |
| `contains(s, t)` (substring) | `s.includes(t)` | `s.includes(t)` (correct!) |
| `indexOf(s, t)` | `s.indexOf(t)` (in code units!) | runtime helper that returns code-point index |

The runtime helpers `mochiStrLen`, `mochiStrAt`, `mochiStrSlice`, `mochiStrIndexOf`, `mochiStrReverse` are in `@mochi/runtime/strings` (see [[04-runtime]] §5). The codegen always emits the helper, never the naive form, except for `for-of` (which is correct natively) and concatenation / equality / `includes` (where code-unit and code-point semantics coincide).

### 4.6 Example

Mochi:

```mochi
fun firstChar(s: string) -> string {
  return s[0]
}

fun strReverse(s: string) -> string {
  var out = ""
  for c in s {
    out = c + out
  }
  return out
}
```

TS:

```typescript
import { mochiStrAt } from "@mochi/runtime/strings";

export function firstChar(s: string): string {
  return mochiStrAt(s, 0n);
}

export function strReverse(s: string): string {
  let out = "";
  for (const c of s) {
    out = c + out;
  }
  return out;
}
```

The `for-of` loop iterates by code point natively (TC39 spec); no helper needed.

### 4.7 String literal narrowing

Mochi-string literals lower to TS double-quoted string literals. Escape sequences:

| Mochi | TS |
|-------|----|
| `"\n"` | `"\n"` |
| `"\t"` | `"\t"` |
| `"\r"` | `"\r"` |
| `"\""` | `"\""` |
| `"\\"` | `"\\"` |
| `"\u{1F600}"` (smiley emoji) | `"\u{1F600}"` (ES2015 syntax; same in TS) |

Multi-line strings:

```mochi
let s = "line1
line2"
```

lower to template literals:

```typescript
const s: string = `line1
line2`;
```

The template literal preserves newlines verbatim.

### 4.8 String interpolation

Mochi `"hello ${name}"` lowers to TS template literal `\`hello ${name}\``. The interpolated expressions are coerced via `String(...)` so non-string types stringify correctly:

```mochi
let n = 42
let s = "count: ${n}"
```

```typescript
const n: bigint = 42n;
const s: string = `count: ${String(n)}`;
```

The `String(n)` wrapper handles bigint (which template literals do not coerce directly; `${n}` emits `42`, which is correct, but `String(n)` is explicit and avoids the JS spec's bigint-in-template-literal edge case).

## 5. `bytes` -> `Uint8Array`

### 5.1 Annotation form

`Uint8Array`.

```typescript
const buf: Uint8Array = new Uint8Array([0xde, 0xad, 0xbe, 0xef]);
```

### 5.2 Runtime representation

JS typed array backed by an `ArrayBuffer`. Each element is a `number` in `[0, 255]`. Indexing `buf[i]` returns `number` (in TS, `number | undefined` under `noUncheckedIndexedAccess`).

### 5.3 Choice rationale

`Uint8Array` is the JS standard for byte buffers. Alternatives:

- `Buffer` (Node only): not portable; only ships in Node.
- `number[]`: 8x to 16x memory overhead; no zero-copy interop with `fetch` / `crypto.subtle`.
- `ArrayBuffer`: untyped; need a view to read/write; an extra step.

`Uint8Array` is the universal choice across Node, Deno, Bun, and browsers.

### 5.4 Variance

`Uint8Array` is a class type, so it is invariant. There is no `ReadonlyUint8Array` standard type; `readonly Uint8Array` in TS is informally "the read-only methods", which the codegen does not enforce at type level.

### 5.5 Caveats

- `Uint8Array.length` is in bytes (matches Mochi `len(b)`); native indexing is by byte (also matches).
- Slice: `b.slice(a, c)` returns a new `Uint8Array` with copied bytes; `b.subarray(a, c)` returns a view into the same buffer (zero-copy). Mochi `b[a..c]` lowers to `.slice` by default (matches Mochi's by-value semantics); `mochi.bytes.view(b, a, c)` lowers to `.subarray` for explicit zero-copy.
- Concatenation: `Uint8Array` has no `+` operator; the codegen emits a runtime helper `concatBytes(a, b)` that allocates a new buffer.

### 5.6 Example

Mochi:

```mochi
fun first4(b: bytes) -> bytes {
  return b[0..4]
}

fun magic() -> bytes {
  return bytes([0x4d, 0x6f, 0x63, 0x68, 0x69])  // "Mochi"
}
```

TS:

```typescript
export function first4(b: Uint8Array): Uint8Array {
  return b.slice(0, 4);
}

export function magic(): Uint8Array {
  return new Uint8Array([0x4d, 0x6f, 0x63, 0x68, 0x69]);
}
```

### 5.7 Bytes-string interop

Mochi `string(b)` (utf-8 decode) lowers to:

```typescript
const decoder = new TextDecoder("utf-8", { fatal: true });
const s = decoder.decode(b);
```

Mochi `bytes(s)` (utf-8 encode) lowers to:

```typescript
const encoder = new TextEncoder();
const b = encoder.encode(s);
```

`TextEncoder` / `TextDecoder` are universal (Node 11+, Deno, Bun, all browsers).

## 6. `list<T>` -> `readonly T[]` or `T[]`

### 6.1 Annotation form

`readonly T[]` when the escape analysis proves the list is not mutated after creation; `T[]` otherwise.

```typescript
const xs: readonly bigint[] = [1n, 2n, 3n];           // immutable
const ys: bigint[] = [];                              // mutable
ys.push(4n);
```

### 6.2 Runtime representation

JS native array. Both spellings back the same `Array` object at runtime; the difference is purely in the TS type.

### 6.3 Choice rationale

`readonly T[]` is **covariant** in TS: `readonly Cat[]` is assignable to `readonly Animal[]`. `T[]` is **invariant**: `Cat[]` is NOT assignable to `Animal[]` (because TS could not check the assumption that callers will not push a `Dog` through the `Animal[]` view).

The escape analysis pass picks the narrowest type:

- A list literal `[1, 2, 3]` that is returned without mutation: `readonly T[]`.
- A list created and `push`-ed to inside a loop: `T[]`.
- A list passed as a parameter and read but not mutated: `readonly T[]`.

The benefit is covariance: a function `fun sum(xs: list<int>) -> int` parameter lowers to `readonly bigint[]`, which accepts both immutable and mutable arrays at the call site.

Rejected alternatives:

- **Always `T[]`**: loses covariance; restricts callers.
- **Always `readonly T[]`**: forbids mutation; requires `as T[]` cast at every mutation site, ugly and unsafe.
- **`ReadonlyArray<T>`**: same as `readonly T[]`, just longer; we prefer the bracketed form.

### 6.4 Variance

| Type | Variance |
|------|----------|
| `readonly T[]` | covariant in `T` |
| `T[]` | invariant in `T` |
| `Iterable<T>` | covariant in `T` |
| `Array<T>` | invariant in `T` (same as `T[]`) |

The TS type system has no declaration-site variance, only use-site (covariant fields are inferred from `readonly`; contravariant from function-parameter positions).

### 6.5 Caveats

- `arr[i]` returns `T | undefined` under `noUncheckedIndexedAccess`. The codegen wraps in a `listGet(arr, i)` helper that bounds-checks and throws.
- `len(arr)` returns `BigInt(arr.length)` if the surrounding `int` rep is bigint; `arr.length` if number.
- Mutation methods on `readonly T[]`: TS reports as type error. The codegen will never emit `(xs as bigint[]).push(...)` for a `readonly` list because the escape analysis disagrees with the mutation site, surfacing the bug in the type checker.
- Spread is fine on both: `[...xs, ...ys]` produces `T[]` (mutable, then is downcast to readonly if escape analysis says so).

### 6.6 Example

Mochi:

```mochi
fun sumList(xs: list<int>) -> int {
  var s = 0
  for x in xs {
    s = s + x
  }
  return s
}

fun build(n: int) -> list<int> {
  var out = []
  for i in range(0, n) {
    append(out, i * i)
  }
  return out
}
```

TS:

```typescript
import { range, listGet } from "@mochi/runtime/collections";

export function sumList(xs: readonly bigint[]): bigint {
  let s: bigint = 0n;
  for (const x of xs) {
    s = s + x;
  }
  return s;
}

export function build(n: bigint): readonly bigint[] {
  const out: bigint[] = [];
  for (const i of range(0n, n)) {
    out.push(i * i);
  }
  return out;
}
```

The `build` function's local `out` is `bigint[]` (mutable; we `push` into it); the return type is `readonly bigint[]` (immutable view; the escape analysis proved no caller mutates the returned list). The implicit cast from `bigint[]` to `readonly bigint[]` is sound and free.

### 6.7 Tuples

Mochi tuples `(int, string)` lower to TS tuple types:

```typescript
const t: readonly [bigint, string] = [42n, "hello"];
```

Fixed-length, indexable, structural. The `readonly` prefix makes the tuple immutable (no mutation of individual slots).

For varlen positional records, Mochi prefers named records (see §9); the codegen rarely emits raw tuples.

## 7. `map<K, V>` -> `Map<K, V>`

### 7.1 Annotation form

`Map<K, V>` (or `ReadonlyMap<K, V>` for immutable views).

```typescript
const m: Map<string, bigint> = new Map([["a", 1n], ["b", 2n]]);
```

### 7.2 Runtime representation

JS native `Map`. Implementation detail per engine: V8 uses a robin-hood hash table; SpiderMonkey uses a hash table with separate-chaining. Both preserve insertion order per spec.

### 7.3 Choice rationale

`Map` is the spec'd ordered hash map. Alternatives:

- **Plain object `{[k: string]: V}`**: only string keys; loses non-string-keyed semantics; lookup is O(1) on average but with prototype-chain hazards. Rejected.
- **`Record<K, V>`**: a TS-only type for the plain-object pattern; same problems. Rejected.
- **Third-party `OrderedMap`**: redundant; `Map` is already ordered.

### 7.4 Variance

`Map<K, V>` is invariant in both `K` and `V` (because it has both read and write methods). `ReadonlyMap<K, V>` is covariant in `V`, invariant in `K`.

### 7.5 Caveats

- Insertion order is guaranteed by the ES spec. Mochi `map<K, V>` is also order-preserving. They match.
- Key equality: `Map` uses SameValueZero comparison (`===` but treating `NaN === NaN` as true). Mochi map keys use structural equality for primitives (matches `===`) and reference equality for records. For records-as-keys, the user must intern.
- `m.get(k)` returns `V | undefined`; the codegen wraps in `mapGet(m, k)` for missing-key semantics (throw) or `mapGetOpt(m, k)` for option-return.

### 7.6 Example

Mochi:

```mochi
fun countWords(words: list<string>) -> map<string, int> {
  var counts = {}
  for w in words {
    if has(counts, w) {
      counts[w] = counts[w] + 1
    } else {
      counts[w] = 1
    }
  }
  return counts
}
```

TS:

```typescript
import { mapGet } from "@mochi/runtime/collections";

export function countWords(words: readonly string[]): Map<string, bigint> {
  const counts: Map<string, bigint> = new Map();
  for (const w of words) {
    if (counts.has(w)) {
      counts.set(w, mapGet(counts, w) + 1n);
    } else {
      counts.set(w, 1n);
    }
  }
  return counts;
}
```

### 7.7 Map literals

Mochi `{"a": 1, "b": 2}` lowers to `new Map([["a", 1n], ["b", 2n]])`. The constructor takes an iterable of `[K, V]` pairs.

For empty maps, `new Map<K, V>()` requires explicit type args because TS cannot infer.

## 8. `set<T>` -> `Set<T>`

### 8.1 Annotation form

`Set<T>` (or `ReadonlySet<T>` for immutable views).

```typescript
const s: Set<bigint> = new Set([1n, 2n, 3n]);
```

### 8.2 Runtime representation

JS native `Set`. Same engine implementation as `Map` minus the value slot. Preserves insertion order per spec.

### 8.3 Choice rationale

`Set` is the spec'd ordered hash set. Alternatives rejected:

- `Map<K, true>`: equivalent functionally; cumbersome syntactically.
- `T[]` with `includes()`: O(n) lookup.

### 8.4 Variance

`Set<T>` is invariant in `T`. `ReadonlySet<T>` is covariant in `T`.

### 8.5 Caveats

- **ES2024 set methods**: `union`, `intersection`, `difference`, `symmetricDifference`, `isSubsetOf`, `isSupersetOf`, `isDisjointFrom`. These ship in Node 22+ (April 2024), Deno 1.42+, Bun 1.1+, Chrome 122+, Firefox 127+, Safari 17+. The codegen emits them directly under the ES2024 target. For older browsers, the runtime ships polyfills in `@mochi/runtime/collections/set-polyfill`.
- Element equality: SameValueZero, matching `Map`.

### 8.6 Example

Mochi:

```mochi
fun dedup(xs: list<int>) -> set<int> {
  var s = {}
  for x in xs {
    add(s, x)
  }
  return s
}

fun common(a: set<int>, b: set<int>) -> set<int> {
  return a & b
}
```

TS:

```typescript
export function dedup(xs: readonly bigint[]): Set<bigint> {
  const s: Set<bigint> = new Set();
  for (const x of xs) {
    s.add(x);
  }
  return s;
}

export function common(a: ReadonlySet<bigint>, b: ReadonlySet<bigint>): Set<bigint> {
  return a.intersection(b);
}
```

The ES2024 `intersection` method returns a new `Set<T>`, matching Mochi's by-value semantics.

### 8.7 Set literals

Mochi `{1, 2, 3}` lowers to `new Set([1n, 2n, 3n])`. The constructor takes an iterable of `T`.

For empty sets, `new Set<T>()` requires explicit type args.

## 9. record -> class with `readonly` fields + private constructor + static factory

### 9.1 Annotation form

A TypeScript class. All fields `readonly`. Constructor is `private`. A `static make` factory plus a `static with` functional-update method.

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
  static with(prev: Point, args: Partial<{ x: number; y: number }>): Point {
    return new Point(args.x ?? prev.x, args.y ?? prev.y);
  }
}
```

### 9.2 Runtime representation

JS class instance. Prototype chain provides `instanceof Point` checks. `readonly` is a compile-time TS marker (it does not enforce immutability at runtime; the runtime relies on the private constructor and the `static with` pattern to enforce by-construction immutability).

### 9.3 Choice rationale

Why class instead of `interface` or `type`?

- **Nominal typing**: TS's structural type system would let any `{x: number, y: number}` be a `Point`. A class introduces a brand: only instances created via `Point.make` are `Point`. `instanceof` works.
- **Discriminator-free**: sum types use the `kind` discriminator; records do not need one (they are not part of a union).
- **Source-map clarity**: `Point.make` is grep-able and shows up in stack traces.
- **Encapsulation hook**: private constructor lets the runtime add invariants (e.g. validation, normalisation) without changing the call site.

Rejected alternatives:

- `interface Point { readonly x: number; readonly y: number; }`: structural, no nominal brand, no factory.
- `type Point = { readonly x: number; readonly y: number; }`: same as interface.
- `Object.freeze({x, y})`: runtime overhead; no compile-time `readonly` enforcement; `Object.isFrozen` is the only check.

### 9.4 Variance

A class with `readonly` fields is **covariant** in its field types (because the fields are only read). A class with `readonly` plus mutable fields is invariant in the mutable ones, covariant in the readonly ones.

### 9.5 Caveats

- The static factory takes an `args: {...}` object, not positional args. This is to support default values (any field with a Mochi default becomes an optional in the args type) and to keep call sites readable for records with many fields.
- The `Partial<{...}>` in `static with` allows updating any subset of fields. The `??` operator falls back to the previous instance's value.
- `JSON.stringify(p)` produces `{"x":1,"y":2}`; TS class methods are not enumerable. For deep equality the codegen emits a generated `equals` method or uses the `@mochi/runtime/equality` library.

### 9.6 Example

Mochi:

```mochi
type Person = {name: string, age: int, email: string?}

fun makeAdult(name: string, email: string?) -> Person {
  return Person{name: name, age: 18, email: email}
}

fun celebrate(p: Person) -> Person {
  return p with {age: p.age + 1}
}
```

TS:

```typescript
export class Person {
  readonly name: string;
  readonly age: bigint;
  readonly email: string | null;
  private constructor(name: string, age: bigint, email: string | null) {
    this.name = name;
    this.age = age;
    this.email = email;
  }
  static make(args: { name: string; age: bigint; email: string | null }): Person {
    return new Person(args.name, args.age, args.email);
  }
  static with(prev: Person, args: Partial<{ name: string; age: bigint; email: string | null }>): Person {
    return new Person(
      args.name ?? prev.name,
      args.age ?? prev.age,
      args.email ?? prev.email
    );
  }
}

export function makeAdult(name: string, email: string | null): Person {
  return Person.make({ name, age: 18n, email });
}

export function celebrate(p: Person): Person {
  return Person.with(p, { age: p.age + 1n });
}
```

### 9.7 Records with methods

Mochi allows methods on records:

```mochi
type Vector = {x: float, y: float}

impl Vector {
  fun length() -> float {
    return sqrt(self.x * self.x + self.y * self.y)
  }
  fun scale(k: float) -> Vector {
    return Vector{x: self.x * k, y: self.y * k}
  }
}
```

These lower to class methods:

```typescript
export class Vector {
  readonly x: number;
  readonly y: number;
  private constructor(x: number, y: number) {
    this.x = x;
    this.y = y;
  }
  static make(args: { x: number; y: number }): Vector {
    return new Vector(args.x, args.y);
  }
  static with(prev: Vector, args: Partial<{ x: number; y: number }>): Vector {
    return new Vector(args.x ?? prev.x, args.y ?? prev.y);
  }
  length(): number {
    return Math.sqrt(this.x * this.x + this.y * this.y);
  }
  scale(k: number): Vector {
    return Vector.make({ x: this.x * k, y: this.y * k });
  }
}
```

`self` becomes `this`. The methods are non-static instance methods.

### 9.8 Records with default fields

Mochi:

```mochi
type Config = {host: string = "localhost", port: int = 8080, tls: bool = false}
```

Lowers to a factory with optional args:

```typescript
export class Config {
  readonly host: string;
  readonly port: bigint;
  readonly tls: boolean;
  private constructor(host: string, port: bigint, tls: boolean) {
    this.host = host;
    this.port = port;
    this.tls = tls;
  }
  static make(args: { host?: string; port?: bigint; tls?: boolean } = {}): Config {
    return new Config(
      args.host ?? "localhost",
      args.port ?? 8080n,
      args.tls ?? false
    );
  }
  static with(prev: Config, args: Partial<{ host: string; port: bigint; tls: boolean }>): Config {
    return new Config(
      args.host ?? prev.host,
      args.port ?? prev.port,
      args.tls ?? prev.tls
    );
  }
}

// Call sites
const c1: Config = Config.make();                       // all defaults
const c2: Config = Config.make({ port: 9090n });        // override port
const c3: Config = Config.with(c2, { tls: true });      // functional update
```

## 10. sum type -> discriminated union with literal `kind` discriminator

### 10.1 Annotation form

A `type` alias whose body is a union of object types, each with a `kind: "<VariantName>"` literal-string discriminator.

```typescript
export type Shape =
  | { kind: "Circle"; r: number }
  | { kind: "Square"; side: number }
  | { kind: "Triangle"; a: number; b: number; c: number };
```

Accompanied by a const namespace of factory functions:

```typescript
export const Shape = {
  Circle: (r: number): Shape => ({ kind: "Circle", r }),
  Square: (side: number): Shape => ({ kind: "Square", side }),
  Triangle: (a: number, b: number, c: number): Shape => ({ kind: "Triangle", a, b, c }),
};
```

### 10.2 Runtime representation

A plain JS object literal with a `kind` field of string-literal type. No class, no prototype, no `instanceof`. Discrimination is by inspecting the `kind` field.

### 10.3 Choice rationale

Discriminated unions are TS's idiom for ADTs. The narrowing is automatic in switch / if-else chains, and `_exhaustive: never` gives compile-time totality.

Rejected alternatives:

- **One class per variant + a shared base class**: more code; `instanceof` per variant is slower than string compare; TS's narrowing on class hierarchies is less powerful than on string-literal discriminants.
- **`class Shape` with `discriminate(): "Circle" | "Square" | "Triangle"`**: synthetic; loses the field-level narrowing.
- **Function `tag` + `data`**: as in OCaml `[`Circle of float | `Square of float ... `]; works but JS does not have polymorphic variants and the encoding is awkward.

### 10.4 Variance

A discriminated union `A | B | C` is covariant in each member (because object types with `readonly` fields are covariant in those fields). The whole union type is "as covariant as the meet of its members" intuitively.

### 10.5 Caveats

- The discriminator field name is fixed to `kind` across all Mochi sum types. The codegen does not let users pick a different field name. This keeps the runtime predictable (e.g. `JSON.parse` parsers can look for `kind` universally).
- Variant names are mangled only if they collide with JS reserved words (rare; `class`, `delete`, etc.).
- The `Shape.Circle` factory is a function, not a constructor. There is no `new Shape.Circle(...)`. This matches the pattern in TC39 proposals for "tagged objects" and avoids the prototype-chain complexity.

### 10.6 Example

Mochi:

```mochi
type Tree<T> = Leaf | Node{value: T, left: Tree<T>, right: Tree<T>}

fun sum(t: Tree<int>) -> int {
  match t {
    Leaf => 0,
    Node{value, left, right} => value + sum(left) + sum(right),
  }
}
```

TS:

```typescript
export type Tree<T> =
  | { kind: "Leaf" }
  | { kind: "Node"; value: T; left: Tree<T>; right: Tree<T> };

export const Tree = {
  Leaf: <T>(): Tree<T> => ({ kind: "Leaf" }),
  Node: <T>(value: T, left: Tree<T>, right: Tree<T>): Tree<T> => ({
    kind: "Node",
    value,
    left,
    right,
  }),
};

export function sum(t: Tree<bigint>): bigint {
  switch (t.kind) {
    case "Leaf":
      return 0n;
    case "Node": {
      const value = t.value;
      const left = t.left;
      const right = t.right;
      return value + sum(left) + sum(right);
    }
    default: {
      const _exhaustive: never = t;
      throw new Error("non-exhaustive: " + JSON.stringify(_exhaustive));
    }
  }
}
```

### 10.7 Recursive sum types and type parameters

`Tree<T>` references itself in `left: Tree<T>`. TS's type system handles this with a self-referential `type` alias (no forward declaration needed).

For mutually recursive sum types, TS handles them in any declaration order as long as both are in the same file or in transitively related modules.

### 10.8 Adding a new variant

When the user adds a new variant `Branch{...}` to `Tree<T>`:

```mochi
type Tree<T> = Leaf | Node{...} | Branch{...}
```

every existing `match` over `Tree<T>` that does not handle `Branch` will now fail to compile, because the `default: { const _exhaustive: never = t; ... }` line will see `t` typed as `{ kind: "Branch"; ... }` (not `never`) and tsc rejects the assignment. The compiler points to the unhandled variant.

This is the killer feature of discriminated unions: exhaustiveness checking by free type inference.

## 11. `T?` (option) -> `T | null`

### 11.1 Annotation form

`T | null`.

```typescript
function find(xs: readonly bigint[], target: bigint): bigint | null {
  for (const x of xs) {
    if (x === target) return x;
  }
  return null;
}
```

### 11.2 Runtime representation

The value `null` (JS singleton). TS `undefined` is also a candidate but we choose `null` deliberately.

### 11.3 Choice rationale

Why `null` and not `undefined`?

- **Explicitness**: `null` is "intentional absence"; `undefined` is "no value assigned, possibly a bug". Mochi's `T?` is intentional.
- **`exactOptionalPropertyTypes`**: with this tsconfig flag, `{x?: T}` differs from `{x: T | undefined}`; using `null` lets us spell both cleanly.
- **JSON round-trip**: `JSON.stringify({a: null})` produces `{"a":null}`; `JSON.stringify({a: undefined})` produces `{}` (the field is dropped). Mochi's option-with-value-`None` must round-trip.
- **Comparison**: `x == null` matches both `null` and `undefined`; `x === null` matches only `null`. The codegen emits `=== null` and `!== null` exclusively.

Rejected alternatives:

- `T | undefined`: round-trip hazards with JSON; ambiguous semantics.
- A discriminated union `{kind: "Some"; value: T} | {kind: "None"}`: heavier; awkward at use sites; matches the Mochi spec but not the JS idiom. The codegen treats `Option<T>` specially to produce the more idiomatic `T | null`.
- A class `Option<T>` with `Some` and `None` static methods: incurs allocation; loses TS narrowing.

### 11.4 Variance

`T | null` is covariant in `T` (because `null` is invariant by itself, and union of covariant + invariant is covariant in the variable part).

### 11.5 Caveats

- Nested options `Option<Option<T>>` cannot collapse (TS does not distinguish `T | null | null` from `T | null`). Mochi requires the user to use `Result<T, ()>` or a sum type for nested options. The type checker catches this.
- `null` itself is not a member of `T` (TS's strict null checks). So `string | null` excludes plain `null` from `string`-typed expressions.
- The codegen emits `?? defaultValue` for the Mochi `??` (option unwrap with default) operator.

### 11.6 Example

Mochi:

```mochi
fun firstOrDefault(xs: list<int>, d: int) -> int {
  if len(xs) == 0 {
    return d
  }
  return xs[0]
}

fun lookup(m: map<string, int>, k: string) -> int? {
  if has(m, k) {
    return m[k]
  }
  return None
}

fun process(opt: int?) -> int {
  return opt ?? -1
}
```

TS:

```typescript
import { mapGet } from "@mochi/runtime/collections";

export function firstOrDefault(xs: readonly bigint[], d: bigint): bigint {
  if (xs.length === 0) {
    return d;
  }
  return xs[0]!;
}

export function lookup(m: ReadonlyMap<string, bigint>, k: string): bigint | null {
  if (m.has(k)) {
    return mapGet(m, k);
  }
  return null;
}

export function process(opt: bigint | null): bigint {
  return opt ?? -1n;
}
```

The `xs[0]!` non-null assertion is allowed here because the prior `xs.length === 0` check narrows the type at the point of return; TS does not propagate this so we add `!` (or emit `listGet(xs, 0n)` for the bounds-check helper).

### 11.7 Optional fields on records

Mochi `type Foo = {a: int, b: int?}` lowers to:

```typescript
export class Foo {
  readonly a: bigint;
  readonly b: bigint | null;
  // ...
}
```

The field `b` is **always present** (it is just `null` for absent). Under `exactOptionalPropertyTypes` this is distinct from `b?: bigint` (which would mean "may be missing from the object entirely"). Mochi semantics say the field is always present, so we use `T | null`, not `T?: T`.

## 12. `Result<T, E>` -> `MochiResult<T, E>` discriminated union

### 12.1 Annotation form

```typescript
export type MochiResult<T, E> =
  | { kind: "ok"; value: T }
  | { kind: "err"; error: E };

export const MochiResult = {
  ok<T, E>(value: T): MochiResult<T, E> { return { kind: "ok", value }; },
  err<T, E>(error: E): MochiResult<T, E> { return { kind: "err", error }; },
};
```

This type lives in `@mochi/runtime/result` (see [[04-runtime]] §10). Mochi `Result<T, E>` references it by import.

### 12.2 Runtime representation

A plain object `{kind: "ok", value: ...}` or `{kind: "err", error: ...}`. Two allocations per Result; the JS engine inlines tags efficiently.

### 12.3 Choice rationale

We deliberately do **not** use JS `throw` for Mochi errors. Reasons:

- **Throws break the type system**: caught errors are `unknown` in TS (under `useUnknownInCatchVariables`). Result keeps the error type visible.
- **Throws are slow**: V8 stack-trace capture is 100x slower than allocating a Result object.
- **Throws cross async boundaries unpredictably**: a rejected Promise can hide a thrown error; the await callsite re-throws. Result is opaque to await: an `awaited` Result is just a value, not a thrown error.

Rejected alternatives:

- **`try`/`catch` with typed errors**: TS does not support typed catches.
- **`Result<T, E>` as a class with `.isOk()` / `.value()`**: class allocation overhead and no narrowing in switch.
- **`MochiResult` as a single object with both fields**: ambiguous (which field is set?); no narrowing.

### 12.4 Variance

`MochiResult<T, E>` is covariant in `T` and covariant in `E` (both are in read-only positions).

### 12.5 Caveats

- The discriminator strings are `"ok"` and `"err"` (lowercase). This is the only sum type in the codebase that uses lowercase variant names; the convention is set in the runtime for compactness.
- Bridging with thrown errors: `mochi.result.fromThrow(() => doThing())` catches any thrown exception and wraps it as `MochiResult.err`. Used at FFI boundaries.

### 12.6 Example

Mochi:

```mochi
fun divide(a: int, b: int) -> Result<int, string> {
  if b == 0 {
    return Err("division by zero")
  }
  return Ok(a / b)
}

fun useDivide() {
  match divide(10, 0) {
    Ok(v) => print("got " + str(v)),
    Err(e) => print("err: " + e),
  }
}
```

TS:

```typescript
import { MochiResult, type MochiResult as Result } from "@mochi/runtime/result";
import { print } from "@mochi/runtime/io";

export function divide(a: bigint, b: bigint): Result<bigint, string> {
  if (b === 0n) {
    return MochiResult.err("division by zero");
  }
  return MochiResult.ok(a / b);
}

export function useDivide(): void {
  const r = divide(10n, 0n);
  switch (r.kind) {
    case "ok": {
      const v = r.value;
      print("got " + v.toString());
      break;
    }
    case "err": {
      const e = r.error;
      print("err: " + e);
      break;
    }
    default: {
      const _exhaustive: never = r;
      throw new Error("non-exhaustive: " + JSON.stringify(_exhaustive));
    }
  }
}
```

### 12.7 The `?` operator (Mochi early-return on Err)

Mochi:

```mochi
fun chained() -> Result<int, string> {
  let a = divide(10, 2)?
  let b = divide(a, 3)?
  return Ok(a + b)
}
```

The `?` desugars to: if Err, return; if Ok, unwrap.

TS:

```typescript
export function chained(): Result<bigint, string> {
  const r1 = divide(10n, 2n);
  if (r1.kind === "err") return r1;
  const a = r1.value;
  const r2 = divide(a, 3n);
  if (r2.kind === "err") return r2;
  const b = r2.value;
  return MochiResult.ok(a + b);
}
```

TS does not have a sugar for this; we expand at the lowering pass.

## 13. Callable `(P1, P2, ...) -> R` -> arrow function type `(p1: P1, p2: P2, ...) => R`

### 13.1 Annotation form

```typescript
const add: (a: bigint, b: bigint) => bigint = (a, b) => a + b;
const adder: (n: bigint) => (x: bigint) => bigint = (n) => (x) => x + n;
```

### 13.2 Runtime representation

JS function object. Arrow functions specifically: no `this` binding, no `arguments`, no `new` (cannot be a constructor).

### 13.3 Choice rationale

Arrow functions are the modern idiom. They lexically scope `this`, which matches Mochi closures' free-variable semantics.

Rejected alternatives:

- `function` declarations: `this` binding hazards.
- `Function` (the type): too wide; loses type info.
- Class with `call` method: overkill for a callable.

### 13.4 Variance

Function types in TS:

- **Covariant in return type**: `() => Cat` is assignable to `() => Animal`.
- **Contravariant in parameter types** under `strictFunctionTypes`: `(x: Animal) => void` is assignable to `(x: Cat) => void`.

The contravariance flips the natural direction: a function that accepts `Animal` is "more general" than one that accepts `Cat`, because the former can be called wherever the latter is expected.

### 13.5 Caveats

- TS's contravariance under `strictFunctionTypes` does not apply to class methods (they are bivariant for legacy compatibility). The codegen emits free functions, not methods, for Mochi callables to get the strict contravariance.
- Optional parameters and rest parameters propagate from Mochi to TS:

| Mochi | TS |
|-------|-----|
| `(x: int) -> R` | `(x: bigint) => R` |
| `(x: int = 0) -> R` | `(x?: bigint) => R` (with default in body) |
| `(...xs: list<int>) -> R` | `(...xs: bigint[]) => R` |
| `(x: int, ...rest: list<int>) -> R` | `(x: bigint, ...rest: bigint[]) => R` |

- `void` return: Mochi `fun foo() -> () {...}` lowers to `(): void => {...}`.
- Async: Mochi `async fun foo() -> int` lowers to `(): Promise<bigint>` and the function is marked `async`.

### 13.6 Example

Mochi:

```mochi
fun makeMultiplier(k: int) -> (int) -> int {
  return |x| x * k
}

fun apply(f: (int) -> int, x: int) -> int {
  return f(x)
}
```

TS:

```typescript
export function makeMultiplier(k: bigint): (x: bigint) => bigint {
  const env = { k };
  return (x: bigint): bigint => x * env.k;
}

export function apply(f: (x: bigint) => bigint, x: bigint): bigint {
  return f(x);
}
```

The `env` record from phase C (closure conversion, see [[05-codegen-design]] §4) wraps the captured `k`.

### 13.7 Higher-kinded type concerns

TS lacks higher-kinded types: there is no `forall F: * -> *. F<T>`. Mochi avoids HKT by design (no `Functor<F>` type class), but where Mochi uses parametric polymorphism, the codegen emits generic functions:

```mochi
fun map<T, U>(xs: list<T>, f: (T) -> U) -> list<U> {
  var out = []
  for x in xs {
    append(out, f(x))
  }
  return out
}
```

```typescript
export function map<T, U>(xs: readonly T[], f: (x: T) => U): U[] {
  const out: U[] = [];
  for (const x of xs) {
    out.push(f(x));
  }
  return out;
}
```

## 14. async iterator -> `AsyncIterable<T>` at parameter positions, `AsyncGenerator<T, void, undefined>` at return positions

### 14.1 Annotation form

```typescript
// Parameter position: AsyncIterable for flexibility
async function consume(s: AsyncIterable<bigint>): Promise<bigint> {
  let sum: bigint = 0n;
  for await (const x of s) { sum = sum + x; }
  return sum;
}

// Return position: AsyncGenerator for precise type inference of yield/return/next
export async function* produce(): AsyncGenerator<bigint, void, undefined> {
  yield 1n;
  yield 2n;
  yield 3n;
}
```

### 14.2 Runtime representation

`AsyncIterable<T>` is the structural type with a `[Symbol.asyncIterator](): AsyncIterator<T>` method. Built-ins like async generators and the runtime's `AsyncIterableQueue<T>` (see [[04-runtime]] §6) implement it.

`AsyncGenerator<T, R, N>` extends `AsyncIterator<T, R, N>`. The three type parameters: yield type `T`, return type `R` (typically `void`), and next-argument type `N` (typically `undefined`).

### 14.3 Choice rationale

Two annotations because TS's bidirectional inference benefits:

- **At return positions**, picking `AsyncGenerator<T, void, undefined>` lets TS infer `yield x` correctly when `x: T`.
- **At parameter positions**, picking `AsyncIterable<T>` lets callers pass any async iterator (a generator, a queue, a stream).

Rejected alternatives:

- Always `AsyncIterable<T>`: weak inference at return positions.
- Always `AsyncGenerator<T, void, undefined>`: overly restrictive at parameter positions.

### 14.4 Variance

`AsyncIterable<T>` is **covariant** in `T` (read-only). `AsyncGenerator<T, R, N>` is covariant in `T` and `R`, **contravariant** in `N` (the `next` argument).

### 14.5 Caveats

- `for await` only works on `AsyncIterable<T>` (not on plain `AsyncIterator<T>`); the codegen always uses `AsyncIterable<T>` at consumer sites.
- The `AsyncGenerator.return()` and `AsyncGenerator.throw()` methods exist for early termination. Mochi `break` inside `for await` calls `return()` implicitly.
- Cancellation: the runtime's `AsyncIterableQueue<T>` honours an `AbortSignal`. When the signal aborts, the queue closes and the `for await` loop exits.

### 14.6 Example

Mochi:

```mochi
stream nats(start: int) -> stream<int> {
  var i = start
  loop {
    yield i
    i = i + 1
  }
}

async fun firstN(s: stream<int>, n: int) -> list<int> {
  var out = []
  var count = 0
  for await x in s {
    if count >= n { break }
    append(out, x)
    count = count + 1
  }
  return out
}
```

TS:

```typescript
export async function* nats(start: bigint): AsyncGenerator<bigint, void, undefined> {
  let i: bigint = start;
  while (true) {
    yield i;
    i = i + 1n;
  }
}

export async function firstN(
  s: AsyncIterable<bigint>,
  n: bigint
): Promise<bigint[]> {
  const out: bigint[] = [];
  let count: bigint = 0n;
  for await (const x of s) {
    if (count >= n) break;
    out.push(x);
    count = count + 1n;
  }
  return out;
}
```

### 14.7 Stream combinators

`@mochi/runtime/stream` provides:

- `merge(a, b): AsyncIterable<T>` -- interleaves two streams.
- `take(s, n): AsyncIterable<T>` -- first n elements.
- `map(s, f): AsyncIterable<U>` -- transform.
- `filter(s, p): AsyncIterable<T>` -- subset.
- `broadcast(s): [AsyncIterable<T>, AsyncIterable<T>]` -- tee.
- `periodic(ms): AsyncIterable<void>` -- emit every ms milliseconds.

All combinators take and return `AsyncIterable<T>` for composability.

## 15. agent -> class subclassing `AgentBase<Msg>`

### 15.1 Annotation form

```typescript
import { AgentBase } from "@mochi/runtime/agent";

interface CounterMsg_inc { kind: "inc"; n: bigint; reply: (v: bigint) => void; }
interface CounterMsg_reset { kind: "reset"; }
type CounterMsg = CounterMsg_inc | CounterMsg_reset;

export class Counter extends AgentBase<CounterMsg> {
  private state: bigint = 0n;
  constructor(signal: AbortSignal) { super(signal); }

  async inc(n: bigint): Promise<bigint> {
    const { promise, resolve } = Promise.withResolvers<bigint>();
    this.cast({ kind: "inc", n, reply: resolve });
    return promise;
  }

  reset(): void {
    this.cast({ kind: "reset" });
  }

  protected override handle(msg: CounterMsg): void {
    switch (msg.kind) {
      case "inc":
        this.state = this.state + msg.n;
        msg.reply(this.state);
        return;
      case "reset":
        this.state = 0n;
        return;
      default: {
        const _exhaustive: never = msg;
        throw new Error("non-exhaustive: " + String(_exhaustive));
      }
    }
  }
}
```

### 15.2 Runtime representation

A class instance owning:

- An `AsyncIterableQueue<Msg>` mailbox (in the `AgentBase` base class).
- An `AbortSignal` for cancellation.
- A `loop()` async method that reads the mailbox and dispatches.

### 15.3 Choice rationale

`AgentBase` factors the common machinery (mailbox + loop + abort handling) out of every agent. Subclasses just implement `handle`.

Rejected alternatives:

- Free functions returning a queue + a controller: loses the class encapsulation; harder to type-check.
- A Proxy-based meta-class: clever but slow; intercedes on every property access.
- One actor library (e.g. comedy.js, nact.js): adds an external dep; their API does not match Mochi's by-cast / by-call distinction.

### 15.4 Variance

`AgentBase<Msg>` is invariant in `Msg` (because the mailbox both reads and writes `Msg`).

### 15.5 Caveats

- Every agent message variant must include a `reply` field if the variant is used in `call` (synchronous reply). The codegen synthesises this from Mochi's `on name(args) -> R` declarations.
- Agent spawning lives in a `try`/`finally` that calls `controller.abort()` on exit, ensuring no leaked actors.
- Supervision strategies (`one_for_one`, `one_for_all`) live in `@mochi/runtime/agent/Supervisor` (see [[04-runtime]] §6.5).

### 15.6 Example

Already shown in §15.1.

For the full pattern with multiple message types and async replies, see [[05-codegen-design]] §13.14.

## 16. Variance recap

This section gathers the variance rules in one place for ease of reference.

### 16.1 Built-in variance

| TS type | Variance in T |
|---------|----------------|
| `readonly T[]` | covariant |
| `T[]` | invariant |
| `Array<T>` | invariant (same as `T[]`) |
| `Iterable<T>` | covariant |
| `Iterator<T>` | covariant in T, invariant in TReturn, contravariant in TNext |
| `AsyncIterable<T>` | covariant |
| `AsyncIterator<T>` | same shape as Iterator |
| `Map<K, V>` | invariant in K, invariant in V |
| `ReadonlyMap<K, V>` | invariant in K, covariant in V |
| `Set<T>` | invariant |
| `ReadonlySet<T>` | covariant |
| `Promise<T>` | covariant |
| `(...) => R` | contravariant in params, covariant in R |

### 16.2 User-defined records

A class with all-`readonly` fields is **covariant** in those field types. A class with a mutable field is **invariant** in that field type.

```typescript
class Box<T> { readonly value: T; ... }  // covariant in T
class MutBox<T> { value: T; ... }        // invariant in T
```

The codegen prefers `readonly` (so Mochi-generated records are covariant), which is more permissive at call sites.

### 16.3 Discriminated unions

A union `{kind: "A"; ... readonly a: T} | {kind: "B"; ...}` is **covariant** in `T` (because each variant's field is `readonly`).

### 16.4 Declaration-site variance: not available

TS does **not** support declaration-site variance annotations (no `<+T>` for covariant, `<-T>` for contravariant). All variance is inferred from use sites.

Where Mochi declarations imply variance that TS cannot infer (e.g. a phantom-type-parametric class with no field uses of `T`), the codegen inserts a synthetic phantom field:

```typescript
declare const __variance: unique symbol;

class Phantom<T> {
  private readonly [__variance]?: (x: T) => void;  // forces contravariance
  // ...
}
```

This pattern is rarely needed; most Mochi types have natural field uses that drive variance correctly.

### 16.5 Function parameter bivariance

TS's `strictFunctionTypes` flag (on under `--strict`) makes function parameter types **contravariant**. Without the flag, parameters are bivariant (a hole in the type system, retained for compatibility).

The codegen always emits function types in the contravariant-friendly form (free function declarations, not method declarations), so `--strictFunctionTypes` semantics apply.

### 16.6 Variance and IntFit monomorphisation

The `bigint` / `number` choice does not interact with variance because neither is a subtype of the other. A function `(x: bigint) => bigint` is not assignable to `(x: number) => number`; the codegen tracks both representations separately.

## 17. Edge cases and gotchas

### 17.1 `Object.freeze` and `Object.isFrozen`

Mochi's by-construction immutability does not call `Object.freeze` because:

- Freeze is shallow (does not freeze nested objects).
- Freeze prevents adding properties; TS already prevents adding properties via the type system.
- Freeze is slow (per V8 microbenchmark, 5x slower on hot paths after freeze).

If a user really wants runtime-enforced immutability, they call `mochi.freeze(x)` from `@mochi/runtime/freeze`, which is `Object.freeze` recursively.

### 17.2 Symbol keys

Mochi has no `Symbol` type. TS allows `Symbol` keys on objects, but the codegen does not emit any. All keys are strings or numbers.

### 17.3 Class private fields (`#field`)

TS supports `#field` for runtime-enforced private fields. The codegen prefers TS-keyword `private` (compile-time only) because:

- `#field` access has runtime overhead (a WeakMap lookup in some engines).
- `#field` cannot be accessed from generated test code that needs to inspect internal state.
- `private` is sufficient for type safety; runtime safety is achieved by the private constructor + factory pattern.

### 17.4 Bigint literals with `n` suffix vs `BigInt(...)` constructor

- Literal `42n`: parsed at parse time; no allocation per use (engines cache).
- `BigInt(42)`: runtime call; allocates per use unless engine caches.

The codegen always emits `42n` for bigint literals; never `BigInt(42)`.

### 17.5 Iterator helpers

ES2024 iterator helpers (`Iterator.from`, `Iterator.prototype.map/filter/take/drop/flatMap/reduce/toArray`) are useful for the query DSL. They are stage-4 and ship in Node 22+, Deno 2+, Bun 1.1+, Chrome 122+, Firefox 131+, Safari 18.

The codegen prefers helpers over manual loops for short queries; manual loops for hot loops (helpers are 1.5x to 3x slower than a hand-written `for` loop in V8 microbenchmarks).

### 17.6 Symbol.dispose and `using`

ES2024 `using` declarations + `Symbol.dispose` provide RAII for JS. The codegen lowers Mochi `with`-blocks to `using` declarations:

```mochi
with f = openFile("a.txt") {
  print(read(f))
}
```

```typescript
{
  using f = openFile("a.txt");
  print(read(f));
}
```

The file is closed automatically on block exit (success or exception). This is preferable to try/finally in modern JS.

The `using` syntax requires the resource to have a `[Symbol.dispose](): void` method (or `[Symbol.asyncDispose](): Promise<void>` for `await using`). The runtime ships these on every resource-managed object (file handles, db connections, etc.).

### 17.7 Top-level await

ES2022 top-level await ships in all four runtimes. The codegen uses it for module init that needs async setup:

```mochi
const config: Config = readConfig()  // sync if readConfig is sync
const config: Config = await readConfig()  // top-level await if async
```

```typescript
const config: Config = readConfig();
// or
const config: Config = await readConfig();
```

The module must be ESM (which all Mochi-emitted modules are).

### 17.8 `globalThis`

`globalThis` is the universal access point for the global object across Node, Deno, Bun, and browsers. The codegen uses it whenever a global is needed (e.g. for FFI symbol lookup). No `window` / `global` distinction.

### 17.9 `import.meta.url`

Modern ESM modules can introspect their own URL via `import.meta.url`. Useful for resolving sibling resources:

```typescript
const dataPath = new URL("./data.json", import.meta.url);
const data = await fetch(dataPath).then((r) => r.json());
```

The codegen uses this for any Mochi-source path-relative imports.

## 18. Per-runtime type-lowering deltas

While the type spelling is identical across Node, Deno, Bun, and browser, a few runtime-specific paths bind to different concrete types.

### 18.1 Buffers

| Mochi `bytes` | Node | Deno | Bun | Browser |
|---------------|------|------|------|----------|
| Type spelling | `Uint8Array` | `Uint8Array` | `Uint8Array` | `Uint8Array` |
| Native I/O class | `Buffer` (Node-only) | `Uint8Array` | `Buffer` (Bun-compat) | `Uint8Array` |
| Bridge | `Buffer.from(u8)` | identity | `Buffer.from(u8)` | identity |

The runtime layer bridges; user code always sees `Uint8Array`.

### 18.2 Files

The Mochi `FileHandle` type lowers to:

- Node: `import("node:fs/promises").FileHandle`.
- Deno: `Deno.FsFile`.
- Bun: `import("node:fs/promises").FileHandle` (Bun implements the Node API).
- Browser: a custom `BrowserFileHandle` class wrapping `FileSystemFileHandle` (File System Access API).

The Mochi-side type is `mochi.FileHandle`, a runtime-shipped interface that abstracts the per-runtime concrete type. See [[04-runtime]] §4.

### 18.3 Network sockets

| Mochi `Socket` | Node | Deno | Bun | Browser |
|----------------|------|------|------|----------|
| Concrete type | `import("node:net").Socket` | `Deno.Conn` | `Bun.SocketHandler` | (not available; HTTP only) |

Browser does not support raw sockets; the Mochi type checker rejects `Socket` use under `--target=browser`.

### 18.4 Crypto

| Mochi `mochi.crypto.sha256` | Node | Deno | Bun | Browser |
|-----------------------------|------|------|------|----------|
| Backend | `node:crypto.createHash("sha256")` | `crypto.subtle.digest` | `Bun.CryptoHasher` | `crypto.subtle.digest` |

All return `Uint8Array`; the Mochi type is just `bytes`.

## 19. Summary table

| Mochi type | TS spelling | Notes |
|------------|-------------|-------|
| `int` (default) | `bigint` | arbitrary precision |
| `int` (when IntFit fits) | `number` | i53 max |
| `i8`, `i16`, `i32` | `number` (wrapped) | `\| 0`, `& 0xff`, etc. |
| `i64`, `u64` | `bigint` (wrapped) | `BigInt.asIntN`, `BigInt.asUintN` |
| `float`, `f64` | `number` | IEEE 754 double |
| `f32` | `number` (rounded via `Math.fround`) | |
| `bool` | `boolean` | |
| `string` | `string` | UTF-16; code-point semantics via helpers |
| `bytes` | `Uint8Array` | |
| `list<T>` | `readonly T[]` or `T[]` | depends on escape analysis |
| `tuple` | `readonly [T1, T2, ...]` | |
| `map<K, V>` | `Map<K, V>` or `ReadonlyMap<K, V>` | insertion-ordered |
| `set<T>` | `Set<T>` or `ReadonlySet<T>` | insertion-ordered; ES2024 methods |
| record | class with `readonly` fields + `private` ctor + `static make`/`with` | nominal |
| sum type | discriminated union with `kind: "<Variant>"` | exhaustiveness via `_exhaustive: never` |
| `T?` | `T \| null` | not `T \| undefined` |
| `Result<T, E>` | `MochiResult<T, E>` from runtime | not via `throw` |
| `(P1, P2) -> R` | `(p1: P1, p2: P2) => R` | arrow function type |
| `() -> ()` | `() => void` | |
| `async fun ... -> R` | `() => Promise<R>` | |
| `stream<T>` (param) | `AsyncIterable<T>` | |
| `stream<T>` (return) | `AsyncGenerator<T, void, undefined>` | |
| agent | class extends `AgentBase<Msg>` | from runtime |

## 20. Comparison to MEP-51 (Python target)

### 20.1 Where TS is richer

- TS has `bigint` (arbitrary precision) AND `number` (fast 64-bit float). Python has only one `int` (arbitrary precision, always allocated).
- TS has structural subtyping plus declaration-site nominal brand via class. Python has nominal classes plus duck typing.
- TS has discriminated-union narrowing via `kind` literal-string types. Python has `match`/`case` with type guards but no flow-typed narrowing.
- TS has `readonly T[]` vs `T[]` for variance. Python's `list[T]` is invariant in mypy `--strict`; tuple is read-only-by-construction.

### 20.2 Where Python is richer

- Python has `dataclass(frozen=True)` with `__eq__`, `__hash__`, and `__repr__` for free. TS classes need codegen.
- Python's `NamedTuple` gives positional and keyword construction in one type. TS classes use named-args factories.
- Python's `Optional[T]` is `T | None`; TS's is `T | null`. Both are unions.

### 20.3 Identical decisions

- Both use discriminated unions for sum types (Python via class hierarchies with `match`; TS via union types with `switch`).
- Both use `Result<T, E>` not exceptions for error handling.
- Both rely on a strict type-checker as a build gate (mypy `--strict` for Python; tsc `--strict` for TS).
- Both ship a runtime library that provides collection helpers and the agent/queue machinery.

## 21. Future evolutions

TS/JS continues to evolve. The following pending proposals matter:

- **Type-only imports/exports**: stable in TS 5.0+. We use `import type` consistently.
- **Pipeline operator** (stage 2): `x |> f |> g` would shorten query chains. Not adopted yet.
- **Records and Tuples** (stage 2): native immutable records/tuples in JS. Would replace our class-with-readonly-fields pattern for records and our `readonly [...]` for tuples. Watch for stage-3 advancement; adopt then.
- **Pattern matching** (stage 1): `match (x) { when {kind: "ok"}: ...; }`. Would replace our `switch (x.kind)` lowering. Years away.
- **Decorators** (stage 3): used in some FFI libraries; not adopted by Mochi codegen.
- **Temporal API** (stage 3): the runtime ships a polyfill; types are `Temporal.ZonedDateTime` etc. Will become native in 2026-2027.
- **Explicit Resource Management** (stage 3, ships ES2026): `using` syntax. Already in TS 5.2+. We use it for Mochi `with` blocks.

When Records and Tuples (stage 3) lands, we will revisit the record lowering: a Mochi record might become a native JS Record (with structural equality and deep immutability) instead of a class. The compile-time API stays the same; the runtime behaviour changes.

## 22. Closing

This note specifies the per-Mochi-type lowering onto TypeScript 5.6 / ECMAScript 2024 for MEP-52. Each section covers the annotation, runtime representation, choice rationale, variance, caveats, and an example. The key invariants:

- Every Mochi value has exactly one TS type spelling, modulo the bigint/number monomorphisation for `int`.
- Every TS type spelling passes `tsc --strict --noUncheckedIndexedAccess --exactOptionalPropertyTypes`.
- Variance follows TS's built-in rules; Mochi records prefer `readonly` fields for covariance.
- Runtime backing is the most idiomatic JS shape (native `Map`, `Set`, `Uint8Array`; ES2024 set methods; iterator helpers when applicable).

Cross-references: [[04-runtime]] for the runtime library implementing the helpers referenced here (`listGet`, `mapGet`, `mochiStrLen`, `MochiResult`, `AgentBase`); [[05-codegen-design]] for the codegen phases (type lowering is phase B, after the IntFit pre-pass); [[01-language-surface]] for the Mochi side of the type system.
