---
title: "MEP-52 research note 04, The mochi_runtime npm package"
description: "The mochi_runtime npm/JSR package: stdlib, native fetch, agent, AsyncIterableQueue, datalog, JSONValue, MochiResult, Temporal, Node N-API FFI for Node, Deno, Bun, and the browser."
sidebar_position: 4
---

# MEP-52 research note 04, The `mochi_runtime` npm package

Author: research pass for [[mep-0052]]. Date: 2026-05-23 16:55 (GMT+7).
Method: structured walk over the Node 22.4 LTS reference, the Deno 2.0
runtime docs (October 2024), the Bun 1.1 docs, the V8 13.x release
notes, TypeScript 5.6 release notes (September 2024), MDN ECMAScript
2024 coverage (June 2024), the WHATWG `fetch` spec, the WHATWG Streams
spec, TC39 Stage 4 lists (2025 snapshot), the Temporal API stage-3
proposal (frozen 2025-09), the `@js-temporal/polyfill` 0.4.x release
notes, the `undici` 6.x fetch implementation, the Node N-API
documentation, the Deno FFI docs (`Deno.dlopen`), the Bun FFI docs
(`bun:ffi`), and direct reads of `transpiler3/aotir/program.go` and
the existing `transpiler3/c/lower/lower.go` shape for IR reference.
Cross-referenced with the prior `[[mep-0050]]` Kotlin runtime note
and the `[[mep-0051]]` Python runtime note (`04-runtime.md` in the
0051 tree) for parallels.

This note inventories the runtime services Mochi programs need at
execution time on the TypeScript/JavaScript target, and chooses for
each one a host facility (ECMAScript 2024 built-in, WHATWG/W3C
platform API, Node stdlib module, Deno/Bun stdlib equivalent), a
declared dependency, or a hand-written piece of `mochi_runtime`. The
output is the **module layout for the `mochi_runtime` npm package**
(section 23), the runtime library every Mochi-emitted `.ts` file
imports.

The companion notes are [[05-codegen-design]] (the lower/emit pipeline
that produces `.ts` files), [[06-type-lowering]] (per-Mochi-type
mapping decisions), [[10-build-system]] (npm + tsc + tarball
assembly), and [[11-testing-gates]] (vm3 byte-equal, `tsc --strict`,
ESLint, Prettier fixed points). The MEP-52 anchor decisions (TS 5.6
floor, ES2024 target, four-runtime conditional exports,
AsyncIterableQueue + AbortController for agents) are referenced as
"shared decisions" throughout.

Runtime baseline for MEP-52 is **Node.js 22.4.0 LTS** (April 2024
release, supported until April 2027), **Deno 2.0** (October 2024,
backwards-compatible `Deno.*` namespace), and **Bun 1.1** (early 2024,
stable enough by May 2026 timeframe). The browser tier is the
**baseline 2024** target as defined by web.dev's Baseline status
(Chrome 122+, Firefox 124+, Safari 17.4+ as of April 2024). Node 20
LTS is **below** the floor because it lacks `Promise.withResolvers`
natively (added in Node 22). Node 18 LTS reaches end-of-life April
2025, definitively out of scope by the May 2026 cut.

---

## 1. ECMAScript 2024 + TypeScript 5.6 surface

The runtime built-ins, importable without any package manager,
provide the value-type vocabulary Mochi lowers onto. Versions track
the engine release, not a separate library release.

**Number family**: `number` (IEEE 754 binary64, the single numeric
type from 1995 ECMA-262), `bigint` (arbitrary precision, Stage 4
Sept 2019, native in every tier-1 runtime since Node 10.4). Mochi
`int` lowers to **either** `bigint` (default, safety) **or** `number`
(when monomorphisation proves the IR value fits in
`[-Number.MAX_SAFE_INTEGER, Number.MAX_SAFE_INTEGER]`). Mochi `float`
lowers to `number`. The mixing trap (TypeScript rejects `1n + 1`
with `Operator '+' cannot be applied to types 'bigint' and 'number'`)
is a feature: it forces the emitter to commit to one or the other
per IR-type. Section 4 of [[06-type-lowering]] covers the choice
algorithm.

**`boolean`**: `true` / `false`. Maps directly to Mochi `bool`. Unlike
Python's `bool extends int`, TypeScript `boolean` is a primitive
type orthogonal to `number`. No silent coercion at the TS layer
(though `Number(true) === 1` exists at runtime; the type system
catches the coercion).

**`string`**: UTF-16 code unit sequence (ECMA-262 §6.1.4). Critically,
`s.length` returns the count of **UTF-16 code units**, not Unicode
code points. The string `"\u{1F600}"` (a single emoji code point
U+1F600) has `length === 2` because it occupies two UTF-16 units
(a surrogate pair). Mochi `len(s)` returns code points by spec, so
the runtime ships a `mochiStrLen(s)` helper using
`[...s].length` (the spread iterator iterates code points). Same for
indexing: `mochiStrAt(s, i)` returns the `i`-th code point's grapheme
("string" of length 1 or 2 UTF-16 units), not the `i`-th UTF-16
unit. Section 5 below covers the helper set.

**`Uint8Array`**: typed array of unsigned 8-bit integers (TC39 typed
arrays, ES2015 standardised but predating). Mochi `bytes` lowers to
`Uint8Array`. The `Buffer` class from Node 0.x is a `Uint8Array`
subclass with extra encoding helpers (`buf.toString('utf-8')`); the
emitter does **not** use `Buffer` because it is Node-specific.
WHATWG `TextEncoder` / `TextDecoder` (now baseline since 2020)
handle UTF-8 conversion cross-runtime.

**`Array<T>`**: dense growable array (ECMA-262 §22.1). Mochi `list<T>`
lowers to `T[]` (the canonical TS literal form, equivalent to
`Array<T>`). Element access `arr[i]` is `T | undefined` under
`--noUncheckedIndexedAccess` (a non-negotiable strict-mode gate).
Slicing (`arr.slice(a, b)`) produces a fresh array, matching Mochi
slice-independence semantics. ES2023 added `toReversed`,
`toSorted`, `toSpliced`, `with` for non-mutating list operations;
the emitter prefers these when monomorphisation infers a
`readonly T[]` view (section 5 of [[06-type-lowering]]).

**`Map<K, V>`**: hash map with **insertion-order iteration**
(ECMA-262 §24.1.1.1, "the Map iteration order is the order in which
key-value pairs are added"). Mochi `map<K, V>` lowers directly. The
Mochi-spec ordering matches by construction, no polyfill needed. Two
gotchas: (a) `===` key equality means `Map<number, V>` and `1`
versus `1n` are different keys; (b) `NaN` is a single key even
though `NaN !== NaN` by IEEE 754 (Maps use SameValueZero, not
strict equality).

**`Set<T>`**: hash set with insertion-order iteration (same spec
clause). ES2024 (June 2024 spec) added the seven methods
`intersection`, `union`, `difference`, `symmetricDifference`,
`isSubsetOf`, `isSupersetOf`, `isDisjointFrom`. The Mochi set
operators (`a + b`, `a & b`, `a - b` over sets) lower directly to
these. Browser support reached baseline 2024 in Q3 2024 (Chrome 122,
Firefox 127, Safari 17.4); Node 22 ships them.

**`ReadonlyMap<K, V>` / `ReadonlySet<T>`**: TS-only type-level views
(not separate runtime classes). Mochi const-typed maps/sets emit
the readonly view annotation; the underlying object is the mutable
`Map` / `Set` (TS variance is structural and forgives the widening).

**Iterators and iterables**: `Iterable<T>`, `Iterator<T>`,
`AsyncIterable<T>`, `AsyncIterator<T>` are TS type aliases over the
`Symbol.iterator` / `Symbol.asyncIterator` protocols. Generators
(`function*` and `async function*`) produce them directly. TC39
Stage 4 (2024) `Iterator` builtin (`Iterator.from`,
`Iterator.prototype.map/filter/take/drop/flatMap/reduce/toArray`)
landed in TS 5.6 and Node 22; the emitter uses these for the query
DSL (section 9).

**`Promise<T>`**: standard ES2015 promise. `Promise.withResolvers()`
(ES2024) returns `{promise, resolve, reject}` and is the
load-bearing primitive for the agent `call(req)` reply mechanism
(section 7). The fallback shim (Node 20-) is rejected; Node 22 is
the floor.

**`AbortController` / `AbortSignal`**: WHATWG DOM primitive, exposed
in Node since 15.x as a global. The standard cooperative-cancellation
mechanism. Mochi agent supervision propagates an `AbortSignal` from
parent scope to each child agent (section 7).

**`null` and `undefined`**: TS distinguishes these. Mochi `T?` lowers
to `T | null` (not `T | undefined`). The reasoning: `undefined`
arises naturally in TS from optional properties, missing object
keys, `void`-returning functions, etc. Using it for Mochi's
explicit-nullability semantic would conflate them. `T | null`
forces an explicit `null` literal and is unambiguous in `JSON.stringify`
(undefined-valued properties drop; null-valued properties serialise
as `null`).

For `mochi_runtime`: most of this section is zero-cost. The only
translation layers are (a) the `mochiStrLen` / `mochiStrAt` helpers
for code-point string semantics, (b) the `AsyncIterableQueue` class
(no platform equivalent), (c) the Temporal API polyfill while
native support stabilises.

---

## 2. TypeScript 5.6 typing surface

TypeScript 5.6 (September 2024) is the floor. The features we lean on:

**`--strict` flag bundle**: `strictNullChecks`, `strictFunctionTypes`,
`strictBindCallApply`, `noImplicitAny`, `noImplicitThis`,
`useUnknownInCatchVariables`, `alwaysStrict`. All on. The emitter
constrains itself to the intersection of "what tsc accepts under
all of these".

**`--noUncheckedIndexedAccess`**: `arr[i]` is `T | undefined`;
`map.get(k)` is `V | undefined`. Mochi semantics force the emitter
to either bounds-check before access (emitting an `if (i < arr.length)`
guard) or use a runtime helper `mochiListAt(arr, i)` that throws
on out-of-bounds (matching Mochi's runtime panic semantic).

**`--exactOptionalPropertyTypes`**: `{x?: number}` is **not** the
same as `{x: number | undefined}`. The former allows omission; the
latter requires the key with an `undefined` value. Mochi optional
fields lower to the former; Mochi `T?` fields lower to the latter.
The distinction surfaces in JSON encoding and Object.assign.

**`--noImplicitOverride`**: methods overriding a base must use the
`override` modifier. Mochi method overrides emit `override` keyword.

**`--noFallthroughCasesInSwitch`**: `switch` `case` blocks must end
in `break`, `return`, `throw`, or `continue`. Match-to-switch
lowering (section 6 of [[05-codegen-design]]) emits `return` from
each case body, so fallthrough does not arise.

**`--noPropertyAccessFromIndexSignature`**: `obj.foo` where `obj`
is typed as `{[k: string]: T}` is rejected; must use `obj["foo"]`.
Records use named keys, dicts use bracket notation; the emitter
follows the distinction.

**Discriminated unions and exhaustiveness**: TS infers the discriminant
from a shared literal-typed field. The canonical Mochi sum-type
shape (section 8 of [[06-type-lowering]]) is:

```typescript
type JsonValue =
  | { readonly kind: "null" }
  | { readonly kind: "bool"; readonly value: boolean }
  | { readonly kind: "num"; readonly value: number }
  | { readonly kind: "str"; readonly value: string }
  | { readonly kind: "arr"; readonly value: readonly JsonValue[] }
  | { readonly kind: "obj"; readonly value: ReadonlyMap<string, JsonValue> };

function describe(v: JsonValue): string {
  switch (v.kind) {
    case "null": return "null";
    case "bool": return `bool(${v.value})`;
    case "num":  return `num(${v.value})`;
    case "str":  return `str(${v.value})`;
    case "arr":  return `arr(len=${v.value.length})`;
    case "obj":  return `obj(keys=${v.value.size})`;
    default: {
      const _exhaustive: never = v;
      throw new Error(`unreachable: ${String(_exhaustive)}`);
    }
  }
}
```

The `const _exhaustive: never = v` line is the load-bearing
exhaustiveness assertion: if a new variant is added without
extending the `switch`, the assignment fails to type-check.
Section 6 of [[06-type-lowering]] expands on this.

**Type-only imports**: `import type { Foo } from "./bar";` is erased
at compile time. The emitter uses these for cross-module type
references where the runtime side has no need to evaluate the
imported module.

**`satisfies`** (TS 4.9+): `const config = { ... } satisfies Config;`
keeps the literal's narrow type while checking it against the
contract. Used in [[05-codegen-design]] section 14 for the runtime
register tables.

**Template literal types**: `\`prefix.${string}\`` typed strings.
Used in the agent module's message-tag dispatch.

**`const` type parameters** (TS 5.0+): `function f<const T>(x: T)`.
Mochi value-generic functions emit `const T` when the IR proves no
widening is needed.

**Tuple labelled elements**: `[name: string, age: number]`. Mochi
tuple types (not v1 surface) would lower here.

**PEP-695-equivalent**: TS already has type-parameter syntax (no
need for `from typing import TypeVar`). `function f<T>(x: T): T`
is unambiguous and zero-cost.

For `mochi_runtime`: we depend on this surface stable and constrain
the emitter to the intersection of "valid under TS 5.6 strict" and
"valid under TS-eslint strict-type-checked".

---

## 3. Module-layout overview

Source tree under `src/`:

```
src/
  index.ts                 # public re-exports + version constant
  collections/
    index.ts
    ordered_set.ts         # OrderedSet<T>
    frozen_list.ts         # FrozenList<T>
    frozen_map.ts          # FrozenMap<K, V>
    list_helpers.ts        # listAt, listSlice, listOfSize
    string_helpers.ts      # mochiStrLen, mochiStrAt, mochiStrSlice
  io/
    index.ts               # cross-runtime exports
    node.ts                # Node.js fs/process adapters
    deno.ts                # Deno.* adapters
    bun.ts                 # Bun.* adapters
    browser.ts             # console + fetch only; no fs
  agent/
    index.ts
    async_iterable_queue.ts
    agent_base.ts
    supervisor.ts          # restart strategies
  stream/
    index.ts
    merge.ts
    broadcast.ts
    periodic.ts
    from_iter.ts
    to_array.ts
    map_filter.ts
  query/
    index.ts
    hash_join.ts
    merge_join.ts
    nested_loop_join.ts
    group_by.ts
    order_by.ts
    distinct.ts
  datalog/
    index.ts
    semi_naive.ts
    magic_sets.ts
    stratified_negation.ts
  ai/
    index.ts
    provider.ts            # LLMProvider interface + registry
    openai.ts
    anthropic.ts
    google.ts
    ollama.ts
    llama_cpp.ts
  fetch/
    index.ts               # wraps platform fetch
  json_value/
    index.ts               # discriminated union + helpers
  result/
    index.ts               # Ok, Err, MochiResult
  time/
    index.ts               # Temporal polyfill dispatch
    polyfill.ts            # @js-temporal/polyfill re-export
  ffi/
    index.ts               # FFI dispatch
    node_napi.ts           # require("bindings")
    deno_ffi.ts            # Deno.dlopen
    bun_ffi.ts             # import { dlopen } from "bun:ffi"
  _internal/
    runtime_detect.ts      # which runtime are we on
    deterministic_sort.ts
    hashing.ts             # stable hash for records
    source_loc.ts          # source-map runtime hook
```

LOC budget: ~8500 lines total across all leaf modules. Each leaf is
independently testable; `_internal/` is an underscore-prefix folder
not re-exported from `src/index.ts`. This is heavier than the
Python runtime (`[[mep-0051]]` clocks in at ~5900 LOC) because the
JS runtime carries four-runtime conditional adapters (`io/node.ts`,
`io/deno.ts`, `io/bun.ts`, `io/browser.ts`) plus three FFI
back-ends (`ffi/node_napi.ts`, `ffi/deno_ffi.ts`, `ffi/bun_ffi.ts`).

`src/index.ts` re-exports the **stable** public surface (the names
Mochi-emitted code is allowed to reference). Internal helpers stay
under `_internal/` and not re-exported. This shape mirrors the
`[[mep-0050]]` Kotlin runtime layout and the `[[mep-0051]]` Python
package layout.

---

## 4. `package.json` and conditional exports

The runtime's `package.json` at the root of the npm package:

```json
{
  "name": "@mochi/runtime",
  "version": "0.1.0",
  "description": "Runtime library for the Mochi-to-TypeScript transpiler (MEP-52).",
  "type": "module",
  "license": "Apache-2.0",
  "author": "Mochi contributors <team@mochi-lang.dev>",
  "repository": {
    "type": "git",
    "url": "https://github.com/mochilang/mochi.git"
  },
  "homepage": "https://mochi-lang.dev",
  "engines": {
    "node": ">=22.0.0",
    "bun": ">=1.1.0",
    "deno": ">=2.0.0"
  },
  "main": "./dist/node/index.js",
  "types": "./dist/index.d.ts",
  "exports": {
    ".": {
      "deno": "./dist/deno/index.js",
      "bun":  "./dist/bun/index.js",
      "browser": "./dist/browser/index.js",
      "node": "./dist/node/index.js",
      "types": "./dist/index.d.ts",
      "default": "./dist/node/index.js"
    },
    "./agent": {
      "deno": "./dist/deno/agent/index.js",
      "bun":  "./dist/bun/agent/index.js",
      "browser": "./dist/browser/agent/index.js",
      "node": "./dist/node/agent/index.js",
      "types": "./dist/agent/index.d.ts",
      "default": "./dist/node/agent/index.js"
    },
    "./io": {
      "deno": "./dist/deno/io/index.js",
      "bun":  "./dist/bun/io/index.js",
      "browser": "./dist/browser/io/index.js",
      "node": "./dist/node/io/index.js",
      "types": "./dist/io/index.d.ts",
      "default": "./dist/node/io/index.js"
    },
    "./ffi": {
      "deno": "./dist/deno/ffi/index.js",
      "bun":  "./dist/bun/ffi/index.js",
      "node": "./dist/node/ffi/index.js",
      "types": "./dist/ffi/index.d.ts",
      "default": "./dist/node/ffi/index.js"
    }
  },
  "sideEffects": false,
  "dependencies": {
    "@js-temporal/polyfill": "^0.4.4"
  },
  "peerDependencies": {
    "undici": "^6.0.0"
  },
  "peerDependenciesMeta": {
    "undici": { "optional": true }
  },
  "optionalDependencies": {
    "openai": "^4.50.0",
    "@anthropic-ai/sdk": "^0.27.0",
    "@google/generative-ai": "^0.17.0"
  },
  "devDependencies": {
    "typescript": "~5.6.0",
    "@types/node": "^22.0.0",
    "prettier": "^3.3.0",
    "eslint": "^9.0.0",
    "@typescript-eslint/parser": "^8.0.0",
    "@typescript-eslint/eslint-plugin": "^8.0.0",
    "vitest": "^2.0.0"
  },
  "scripts": {
    "build": "tsc --build tsconfig.node.json tsconfig.deno.json tsconfig.bun.json tsconfig.browser.json",
    "typecheck": "tsc --noEmit",
    "lint": "eslint . --max-warnings 0",
    "format": "prettier --check .",
    "test": "vitest run"
  }
}
```

Five fields warrant comment:

- **`"type": "module"`**: every `.ts` and emitted `.js` is an ES
  module. No CommonJS. Imports use the `.js` suffix even when
  importing source `.ts` files; TS 5.6's
  `--rewriteRelativeImportExtensions` makes the source-side
  `import "./foo.ts"` rewrite to `import "./foo.js"` in `dist/`.

- **`"exports"` conditional map**: per-runtime entry points. The
  `node`, `deno`, `bun`, `browser` keys are matched by each runtime's
  resolver; the `default` clause catches everything else (Cloudflare
  Workers, edge runtimes). Order matters in older spec versions; we
  list `default` last per the Node.js conditional-exports docs.

- **`"sideEffects": false`**: tells Webpack/Rollup/esbuild
  tree-shakers that importing a module by name does not have side
  effects, so unused re-exports can be eliminated. Critical for the
  browser bundle (cuts the bundle from ~120kb to ~12kb when only
  `MochiResult` is imported).

- **`"peerDependencies"`** with `undici` optional: on Node 22 the
  native `fetch` works; on Node 18 (out of scope for the floor, but
  some users still bridge) `undici` is the high-quality polyfill.
  We mark it optional so users do not pull it transitively unless
  they need it.

- **`"optionalDependencies"`** for LLM SDKs: a Mochi program that
  never uses `ai.*` should not have to install `openai` and friends.
  npm allows install failure for optional deps without aborting the
  whole install.

### 4.1 `tsconfig.base.json` (extended by each runtime)

```json
{
  "$schema": "https://json.schemastore.org/tsconfig",
  "compilerOptions": {
    "target": "ES2024",
    "module": "ESNext",
    "moduleResolution": "Bundler",
    "strict": true,
    "noUncheckedIndexedAccess": true,
    "exactOptionalPropertyTypes": true,
    "noImplicitOverride": true,
    "noFallthroughCasesInSwitch": true,
    "noPropertyAccessFromIndexSignature": true,
    "useDefineForClassFields": true,
    "isolatedModules": true,
    "esModuleInterop": false,
    "verbatimModuleSyntax": true,
    "rewriteRelativeImportExtensions": true,
    "declaration": true,
    "declarationMap": true,
    "sourceMap": true,
    "skipLibCheck": false,
    "lib": ["ES2024"]
  }
}
```

`"moduleResolution": "Bundler"` (TS 5.0+) matches what bundlers
actually do (look at `package.json` `exports`, follow extensions
automatically). `"isolatedModules": true` ensures each file
type-checks alone, a prerequisite for `swc`/`esbuild` compilation
in parallel.

### 4.2 Per-runtime tsconfig.X.json

```json
// tsconfig.node.json
{
  "extends": "./tsconfig.base.json",
  "compilerOptions": {
    "outDir": "dist/node",
    "lib": ["ES2024"],
    "types": ["node"]
  },
  "include": ["src/**/*.ts"],
  "exclude": ["src/io/deno.ts", "src/io/bun.ts", "src/io/browser.ts",
              "src/ffi/deno_ffi.ts", "src/ffi/bun_ffi.ts"]
}
```

```json
// tsconfig.deno.json
{
  "extends": "./tsconfig.base.json",
  "compilerOptions": {
    "outDir": "dist/deno",
    "lib": ["ES2024", "DOM"],
    "types": []
  },
  "include": ["src/**/*.ts"],
  "exclude": ["src/io/node.ts", "src/io/bun.ts", "src/io/browser.ts",
              "src/ffi/node_napi.ts", "src/ffi/bun_ffi.ts"]
}
```

```json
// tsconfig.bun.json
{
  "extends": "./tsconfig.base.json",
  "compilerOptions": {
    "outDir": "dist/bun",
    "lib": ["ES2024"],
    "types": ["bun-types"]
  },
  "include": ["src/**/*.ts"],
  "exclude": ["src/io/node.ts", "src/io/deno.ts", "src/io/browser.ts",
              "src/ffi/node_napi.ts", "src/ffi/deno_ffi.ts"]
}
```

```json
// tsconfig.browser.json
{
  "extends": "./tsconfig.base.json",
  "compilerOptions": {
    "outDir": "dist/browser",
    "lib": ["ES2024", "DOM", "DOM.Iterable"],
    "types": []
  },
  "include": ["src/**/*.ts"],
  "exclude": ["src/io/node.ts", "src/io/deno.ts", "src/io/bun.ts",
              "src/ffi/**"]
}
```

Each variant excludes the files that import APIs the variant cannot
provide. `tsc --build` runs all four projects; the failure of one
fails the whole build (load-bearing for CI).

---

## 5. `mochi_runtime/collections`

### 5.1 `OrderedSet<T>`

JavaScript `Set` is already insertion-ordered per ECMA-262, so an
ordered-set polyfill is **not** strictly required. We still ship
`OrderedSet<T>` because:

- The class adds Mochi-shaped methods (`addAll`, `removeAll`,
  `union`, `intersection`, `difference`) with consistent return types
  (always returning new `OrderedSet`, not mixed `Set` / `Iterable`).
- Mochi `set<T>` is **immutable by spec**; `Set<T>` is mutable. The
  emitter returns `ReadonlySet<T>` from public APIs but the
  underlying instance is a regular `Set`. `OrderedSet` wraps the
  set and exposes only non-mutating reads.

```typescript
export class OrderedSet<T> implements Iterable<T> {
  readonly #inner: Set<T>;

  private constructor(inner: Set<T>) {
    this.#inner = inner;
  }

  static empty<T>(): OrderedSet<T> {
    return new OrderedSet<T>(new Set<T>());
  }

  static from<T>(items: Iterable<T>): OrderedSet<T> {
    return new OrderedSet<T>(new Set<T>(items));
  }

  get size(): number { return this.#inner.size; }
  has(x: T): boolean { return this.#inner.has(x); }
  [Symbol.iterator](): IterableIterator<T> {
    return this.#inner[Symbol.iterator]();
  }

  add(x: T): OrderedSet<T> {
    const next = new Set(this.#inner);
    next.add(x);
    return new OrderedSet<T>(next);
  }

  remove(x: T): OrderedSet<T> {
    if (!this.#inner.has(x)) return this;
    const next = new Set(this.#inner);
    next.delete(x);
    return new OrderedSet<T>(next);
  }

  union(other: OrderedSet<T>): OrderedSet<T> {
    return new OrderedSet<T>(this.#inner.union(other.#inner));
  }

  intersection(other: OrderedSet<T>): OrderedSet<T> {
    return new OrderedSet<T>(this.#inner.intersection(other.#inner));
  }

  difference(other: OrderedSet<T>): OrderedSet<T> {
    return new OrderedSet<T>(this.#inner.difference(other.#inner));
  }

  toArray(): readonly T[] {
    return Array.from(this.#inner);
  }
}
```

The `#inner` field uses ECMAScript private-class-field syntax (Stage
4 since 2021, native everywhere). Operator-level set methods
(`union`, `intersection`, `difference`) delegate to ES2024 native
`Set` methods. The class is **structurally immutable** (every
mutator returns a new instance); this matches Mochi `set<T>`
semantics, where set operations are non-mutating expressions.

Why not just expose `ReadonlySet<T>`? Two reasons:
- `ReadonlySet<T>` lacks methods like `union` until ES2024, and the
  TypeScript `ReadonlySet` lib def lags actual engine support.
- Mochi user code wants the explicit class name `OrderedSet` to
  document intent; a bare `Set` does not signal "this is intended
  as immutable".

### 5.2 `FrozenList<T>`

Mochi `readonly list<T>` lowers to TypeScript `readonly T[]`. No
class needed at the value level (an array is an array). The
runtime ships a `FrozenList<T>` factory that calls `Object.freeze`
on the array, useful for cross-FFI hand-offs where the consumer
might attempt to mutate:

```typescript
export type FrozenList<T> = readonly T[];

export function freezeList<T>(xs: readonly T[]): FrozenList<T> {
  return Object.freeze(xs.slice());
}
```

`Object.freeze` on an array prevents `push`, `pop`, element
assignment; it is shallow (nested objects remain mutable). For
deep freezing (rare in Mochi-emitted code), `deepFreezeList` is
provided.

### 5.3 `FrozenMap<K, V>`

Symmetric to `FrozenList`. Wraps a `Map<K, V>` and exposes only
read methods:

```typescript
export class FrozenMap<K, V> implements Iterable<[K, V]> {
  readonly #inner: Map<K, V>;

  private constructor(inner: Map<K, V>) {
    this.#inner = inner;
  }

  static from<K, V>(entries: Iterable<readonly [K, V]>): FrozenMap<K, V> {
    return new FrozenMap<K, V>(new Map<K, V>(entries));
  }

  get size(): number { return this.#inner.size; }
  get(k: K): V | undefined { return this.#inner.get(k); }
  has(k: K): boolean { return this.#inner.has(k); }
  keys(): IterableIterator<K> { return this.#inner.keys(); }
  values(): IterableIterator<V> { return this.#inner.values(); }
  entries(): IterableIterator<[K, V]> { return this.#inner.entries(); }
  [Symbol.iterator](): IterableIterator<[K, V]> {
    return this.#inner[Symbol.iterator]();
  }

  set(k: K, v: V): FrozenMap<K, V> {
    const next = new Map(this.#inner);
    next.set(k, v);
    return new FrozenMap<K, V>(next);
  }

  delete(k: K): FrozenMap<K, V> {
    if (!this.#inner.has(k)) return this;
    const next = new Map(this.#inner);
    next.delete(k);
    return new FrozenMap<K, V>(next);
  }
}
```

### 5.4 String helpers (code-point-aware)

Mochi `len(s)`, `s[i]`, `s[a..<b]` all use code-point semantics,
not UTF-16-code-unit semantics. The runtime ships three helpers:

```typescript
export function mochiStrLen(s: string): number {
  let n = 0;
  for (const _ of s) n++;
  return n;
}

export function mochiStrAt(s: string, i: number): string {
  if (i < 0) throw new RangeError(`mochiStrAt: negative index ${i}`);
  let k = 0;
  for (const ch of s) {
    if (k === i) return ch;
    k++;
  }
  throw new RangeError(`mochiStrAt: index ${i} out of bounds (len=${k})`);
}

export function mochiStrSlice(s: string, lo: number, hi: number): string {
  const chars: string[] = [];
  let k = 0;
  for (const ch of s) {
    if (k >= hi) break;
    if (k >= lo) chars.push(ch);
    k++;
  }
  return chars.join("");
}
```

The `for (const ch of s)` form iterates code points (the
`String.prototype[Symbol.iterator]` implementation walks UTF-16 with
surrogate-pair detection). `Array.from(s)` is equivalent, but
allocates an intermediate array; the helper above avoids the
allocation for the length-only case.

For programs that touch only ASCII (the vast majority), the helpers
are O(n) instead of O(1). The emitter could specialise to native
`s.length` and `s.charAt(i)` when the IR proves the string is
ASCII-only; this is a future optimisation tracked in [[12-risks-and-alternatives]].

### 5.5 `listAt`, `listSlice`, `listOfSize`

```typescript
export function listAt<T>(xs: readonly T[], i: number): T {
  if (i < 0 || i >= xs.length) {
    throw new RangeError(`list index ${i} out of bounds (len=${xs.length})`);
  }
  return xs[i]!;
}

export function listSlice<T>(xs: readonly T[], lo: number, hi: number): T[] {
  return xs.slice(lo, hi);
}

export function listOfSize<T>(n: number, init: T): T[] {
  return Array.from({ length: n }, () => init);
}
```

`listAt` wraps native indexing with a bounds check (matching Mochi
panic semantics). The `xs[i]!` non-null assertion is safe because
of the preceding check; `--noUncheckedIndexedAccess` would otherwise
require it.

`listOfSize(n, init)` is the equivalent of Python's `[init] * n`.
Critically, the factory `() => init` is called per element, so
mutable `init` would share state; the emitter only calls
`listOfSize` with immutable `init` values, and a `listOfSizeFn(n,
factoryFn)` variant exists for the lazy-default case.

---

## 6. `mochi_runtime/io`

The IO module is the only place we have **per-runtime conditional
adapters**. Public surface is shared; implementation differs.

### 6.1 Public surface (`src/io/index.ts`)

```typescript
export type WriteHandle = {
  write(s: string): Promise<void>;
  writeBytes(b: Uint8Array): Promise<void>;
};

export interface ReadHandle {
  readLine(): Promise<string | null>;
  readAll(): Promise<string>;
  readBytes(): Promise<Uint8Array>;
}

export function stdout(): WriteHandle { return _impl().stdout(); }
export function stderr(): WriteHandle { return _impl().stderr(); }
export function stdin(): ReadHandle { return _impl().stdin(); }

export function print(s: string): void { _impl().print(s); }
export function println(s: string): void { _impl().println(s); }
export function eprintln(s: string): void { _impl().eprintln(s); }

export function exit(code = 0): never { _impl().exit(code); }

export function readFileText(path: string): Promise<string> {
  return _impl().readFileText(path);
}

export function writeFileText(path: string, contents: string): Promise<void> {
  return _impl().writeFileText(path, contents);
}

export function readFileBytes(path: string): Promise<Uint8Array> {
  return _impl().readFileBytes(path);
}

export function writeFileBytes(path: string, contents: Uint8Array): Promise<void> {
  return _impl().writeFileBytes(path, contents);
}

export function env(name: string): string | null {
  return _impl().env(name);
}

export function args(): readonly string[] {
  return _impl().args();
}
```

The `_impl()` function is a one-time runtime detection:

```typescript
import type { IoImpl } from "./impl.js";

let _cached: IoImpl | null = null;

function _impl(): IoImpl {
  if (_cached !== null) return _cached;
  _cached = _detectAndLoad();
  return _cached;
}

function _detectAndLoad(): IoImpl {
  // Order matters: Deno reports as having 'window' too, so check it
  // before browser. Bun also exposes 'Bun' global.
  if (typeof Deno !== "undefined") {
    // dynamic import so the bundle for a different runtime does not
    // try to import this file
    throw new Error("io: Deno path must be selected via conditional exports");
  }
  if (typeof Bun !== "undefined") {
    throw new Error("io: Bun path must be selected via conditional exports");
  }
  if (typeof process !== "undefined" && process.versions?.node) {
    throw new Error("io: Node path must be selected via conditional exports");
  }
  throw new Error("io: browser path must be selected via conditional exports");
}
```

In practice, the conditional-exports map in `package.json` picks
the right `index.ts` per runtime; the runtime detection is a
defensive fallback (and helps catch misconfigured bundlers).

### 6.2 Node implementation (`src/io/node.ts`)

```typescript
import { stdin as nodeStdin, stdout as nodeStdout, stderr as nodeStderr } from "node:process";
import { readFile, writeFile } from "node:fs/promises";
import * as readline from "node:readline/promises";
import process from "node:process";

const _encoder = new TextEncoder();

export function stdout(): WriteHandle {
  return {
    write: async (s: string) => {
      await new Promise<void>((resolve, reject) =>
        nodeStdout.write(s, (err) => (err ? reject(err) : resolve())));
    },
    writeBytes: async (b: Uint8Array) => {
      await new Promise<void>((resolve, reject) =>
        nodeStdout.write(b, (err) => (err ? reject(err) : resolve())));
    },
  };
}

export function stderr(): WriteHandle { /* mirror */ }

let _rl: readline.Interface | null = null;
export function stdin(): ReadHandle {
  if (_rl === null) {
    _rl = readline.createInterface({ input: nodeStdin, terminal: false });
  }
  return {
    readLine: async () => {
      const it = _rl![Symbol.asyncIterator]();
      const { value, done } = await it.next();
      return done ? null : (value as string);
    },
    readAll: async () => {
      const chunks: string[] = [];
      for await (const line of _rl!) chunks.push(line);
      return chunks.join("\n");
    },
    readBytes: async () => {
      const buf: Buffer[] = [];
      for await (const chunk of nodeStdin) buf.push(chunk as Buffer);
      return new Uint8Array(Buffer.concat(buf));
    },
  };
}

export function print(s: string): void { nodeStdout.write(s); }
export function println(s: string): void { nodeStdout.write(s + "\n"); }
export function eprintln(s: string): void { nodeStderr.write(s + "\n"); }

export function exit(code: number): never {
  process.exit(code);
}

export async function readFileText(path: string): Promise<string> {
  return readFile(path, "utf-8");
}

export async function writeFileText(path: string, contents: string): Promise<void> {
  await writeFile(path, contents, "utf-8");
}

export async function readFileBytes(path: string): Promise<Uint8Array> {
  return new Uint8Array(await readFile(path));
}

export async function writeFileBytes(path: string, contents: Uint8Array): Promise<void> {
  await writeFile(path, contents);
}

export function env(name: string): string | null {
  return process.env[name] ?? null;
}

export function args(): readonly string[] {
  return process.argv.slice(2);
}
```

Notes:

- `node:` prefix on all imports. Required by the Node ESM resolver
  for built-in modules; also a TS lint rule.
- `Buffer.concat` returns a `Buffer`; we wrap in `new Uint8Array`
  to give the runtime a non-Node typed array (Buffer is a Node
  extension that some Mochi user code might not want to handle).
- `process.exit` returns `never` per Node's type defs; TypeScript
  honours it.
- `readline.createInterface` is cached at module scope because
  installing a second interface against the same stream causes
  double-read.

### 6.3 Deno implementation (`src/io/deno.ts`)

```typescript
const _encoder = new TextEncoder();
const _decoder = new TextDecoder();

export function stdout(): WriteHandle {
  return {
    write: async (s: string) => {
      await Deno.stdout.write(_encoder.encode(s));
    },
    writeBytes: async (b: Uint8Array) => {
      await Deno.stdout.write(b);
    },
  };
}

export function stderr(): WriteHandle {
  return {
    write: async (s: string) => {
      await Deno.stderr.write(_encoder.encode(s));
    },
    writeBytes: async (b: Uint8Array) => {
      await Deno.stderr.write(b);
    },
  };
}

export function stdin(): ReadHandle {
  return {
    readLine: async () => {
      const buf = new Uint8Array(4096);
      const lineBytes: number[] = [];
      while (true) {
        const n = await Deno.stdin.read(buf);
        if (n === null) return lineBytes.length === 0 ? null : _decoder.decode(new Uint8Array(lineBytes));
        for (let i = 0; i < n; i++) {
          if (buf[i] === 0x0a) {  // \n
            return _decoder.decode(new Uint8Array(lineBytes));
          }
          lineBytes.push(buf[i]!);
        }
      }
    },
    readAll: async () => {
      const buf = await new Response(Deno.stdin.readable).text();
      return buf;
    },
    readBytes: async () => {
      return new Uint8Array(await new Response(Deno.stdin.readable).arrayBuffer());
    },
  };
}

export function print(s: string): void {
  Deno.stdout.writeSync(_encoder.encode(s));
}

export function println(s: string): void {
  Deno.stdout.writeSync(_encoder.encode(s + "\n"));
}

export function eprintln(s: string): void {
  Deno.stderr.writeSync(_encoder.encode(s + "\n"));
}

export function exit(code: number): never {
  Deno.exit(code);
  throw new Error("unreachable");  // tsc does not know Deno.exit is `never`
}

export async function readFileText(path: string): Promise<string> {
  return Deno.readTextFile(path);
}

export async function writeFileText(path: string, contents: string): Promise<void> {
  await Deno.writeTextFile(path, contents);
}

export async function readFileBytes(path: string): Promise<Uint8Array> {
  return Deno.readFile(path);
}

export async function writeFileBytes(path: string, contents: Uint8Array): Promise<void> {
  await Deno.writeFile(path, contents);
}

export function env(name: string): string | null {
  return Deno.env.get(name) ?? null;
}

export function args(): readonly string[] {
  return Deno.args;
}
```

The Deno path uses **synchronous** `Deno.stdout.writeSync` for
`print` because the sync write path is the canonical pattern in
Deno; the async variant exists but is rarely needed in user code.

### 6.4 Bun implementation (`src/io/bun.ts`)

```typescript
const _encoder = new TextEncoder();

export function stdout(): WriteHandle {
  return {
    write: async (s: string) => {
      await Bun.write(Bun.stdout, s);
    },
    writeBytes: async (b: Uint8Array) => {
      await Bun.write(Bun.stdout, b);
    },
  };
}

export function stderr(): WriteHandle {
  return {
    write: async (s: string) => {
      await Bun.write(Bun.stderr, s);
    },
    writeBytes: async (b: Uint8Array) => {
      await Bun.write(Bun.stderr, b);
    },
  };
}

export function stdin(): ReadHandle {
  return {
    readLine: async () => {
      for await (const line of console as unknown as AsyncIterable<string>) {
        return line;
      }
      return null;
    },
    readAll: async () => {
      return await Bun.stdin.text();
    },
    readBytes: async () => {
      return new Uint8Array(await Bun.stdin.arrayBuffer());
    },
  };
}

export function print(s: string): void {
  // Bun's process.stdout is faster than Bun.write for small strings
  process.stdout.write(s);
}

export function println(s: string): void { process.stdout.write(s + "\n"); }
export function eprintln(s: string): void { process.stderr.write(s + "\n"); }

export function exit(code: number): never {
  process.exit(code);
}

export async function readFileText(path: string): Promise<string> {
  return Bun.file(path).text();
}

export async function writeFileText(path: string, contents: string): Promise<void> {
  await Bun.write(path, contents);
}

export async function readFileBytes(path: string): Promise<Uint8Array> {
  return new Uint8Array(await Bun.file(path).arrayBuffer());
}

export async function writeFileBytes(path: string, contents: Uint8Array): Promise<void> {
  await Bun.write(path, contents);
}

export function env(name: string): string | null {
  return Bun.env[name] ?? null;
}

export function args(): readonly string[] {
  return Bun.argv.slice(2);
}
```

Bun's `Bun.write` is the fastest write primitive on the Bun runtime
(it bypasses some of the standard Node-stream overhead). `Bun.file`
is a lazy file-handle abstraction; calling `.text()` reads.

### 6.5 Browser implementation (`src/io/browser.ts`)

The browser has no `fs`, no `process.stdin`. We expose only the
console subset and stub the rest:

```typescript
export function stdout(): WriteHandle {
  return {
    write: async (s: string) => { console.log(s); },
    writeBytes: async (_: Uint8Array) => {
      throw new Error("browser: stdout.writeBytes is not supported");
    },
  };
}

export function stderr(): WriteHandle {
  return {
    write: async (s: string) => { console.error(s); },
    writeBytes: async (_: Uint8Array) => {
      throw new Error("browser: stderr.writeBytes is not supported");
    },
  };
}

export function stdin(): ReadHandle {
  throw new Error("browser: stdin is not supported");
}

export function print(s: string): void { console.log(s); }
export function println(s: string): void { console.log(s); }
export function eprintln(s: string): void { console.error(s); }

export function exit(code: number): never {
  throw new Error(`browser: exit(${code}) called; programs cannot exit a tab`);
}

export async function readFileText(_: string): Promise<string> {
  throw new Error("browser: readFileText is not supported");
}

export async function writeFileText(_: string, __: string): Promise<void> {
  throw new Error("browser: writeFileText is not supported");
}

export async function readFileBytes(_: string): Promise<Uint8Array> {
  throw new Error("browser: readFileBytes is not supported");
}

export async function writeFileBytes(_: string, __: Uint8Array): Promise<void> {
  throw new Error("browser: writeFileBytes is not supported");
}

export function env(_: string): string | null { return null; }
export function args(): readonly string[] { return []; }
```

Mochi programs that target the browser ship with the
**no-fs** subset of the surface. The compile-time gate is
`tsconfig.browser.json`'s `exclude` list; the runtime gate is the
"throw on call" stub. Users who want filesystem access in the
browser go through OPFS or the file picker; that is application
code, not runtime concern.

---

## 7. `mochi_runtime/agent`

The load-bearing module. It defines the agent shape Mochi lowers
to: a class wrapping an `AsyncIterableQueue<Message>` mailbox and an
`AbortController` for supervision.

### 7.1 `AsyncIterableQueue<T>`

```typescript
export class AsyncIterableQueue<T> implements AsyncIterable<T> {
  #buffer: T[] = [];
  #waiters: Array<(v: IteratorResult<T>) => void> = [];
  #closed = false;

  push(value: T): void {
    if (this.#closed) {
      throw new Error("AsyncIterableQueue: push after close");
    }
    const waiter = this.#waiters.shift();
    if (waiter !== undefined) {
      waiter({ value, done: false });
    } else {
      this.#buffer.push(value);
    }
  }

  close(): void {
    if (this.#closed) return;
    this.#closed = true;
    for (const w of this.#waiters) {
      w({ value: undefined as unknown as T, done: true });
    }
    this.#waiters = [];
  }

  get closed(): boolean { return this.#closed; }
  get bufferedCount(): number { return this.#buffer.length; }

  [Symbol.asyncIterator](): AsyncIterator<T> {
    return {
      next: (): Promise<IteratorResult<T>> => {
        if (this.#buffer.length > 0) {
          return Promise.resolve({ value: this.#buffer.shift()!, done: false });
        }
        if (this.#closed) {
          return Promise.resolve({ value: undefined as unknown as T, done: true });
        }
        const { promise, resolve } = Promise.withResolvers<IteratorResult<T>>();
        this.#waiters.push(resolve);
        return promise;
      },
      return: (): Promise<IteratorResult<T>> => {
        this.close();
        return Promise.resolve({ value: undefined as unknown as T, done: true });
      },
    };
  }
}
```

Three design decisions:

- **`Promise.withResolvers`** (ES2024) is the load-bearing primitive.
  The pre-ES2024 alternative is the captured-resolve pattern: `let
  resolve!: (v: IteratorResult<T>) => void; const promise = new
  Promise(r => resolve = r);`. The ES2024 form is cleaner and one
  allocation less. Node 22 floor makes this fine.

- **`#buffer` is unbounded**. Backpressure semantics are not part
  of MEP-52 v1; producers that overrun consumers grow the buffer.
  A `BoundedAsyncIterableQueue<T>` with a `maxSize` parameter is a
  v2 candidate (tracked in [[12-risks-and-alternatives]]). The unboundedness matches
  asyncio.Queue's default in MEP-51 and Kotlin Channel.UNLIMITED in
  MEP-50.

- **`return` method on the iterator**: when a `for await ... of`
  loop exits early (break, throw, generator close), the iterator
  protocol calls `return()`. We honour it by closing the queue.
  This prevents lingering waiters that would never resolve.

### 7.2 `AgentBase`

```typescript
export abstract class AgentBase<M, S> {
  protected readonly mailbox = new AsyncIterableQueue<M>();
  protected state: S;
  protected readonly signal: AbortSignal;
  readonly #loopPromise: Promise<void>;

  protected constructor(initial: S, signal: AbortSignal) {
    this.state = initial;
    this.signal = signal;
    signal.addEventListener("abort", () => this.mailbox.close(), { once: true });
    this.#loopPromise = this.loop();
  }

  cast(msg: M): void {
    if (this.signal.aborted) return;
    this.mailbox.push(msg);
  }

  stop(): void {
    this.mailbox.close();
  }

  awaitTermination(): Promise<void> {
    return this.#loopPromise;
  }

  protected abstract handle(state: S, msg: M): Promise<S> | S;

  private async loop(): Promise<void> {
    try {
      for await (const msg of this.mailbox) {
        if (this.signal.aborted) break;
        this.state = await this.handle(this.state, msg);
      }
    } catch (err) {
      // propagate to supervisor via the abort signal
      if (!this.signal.aborted) {
        throw err;
      }
    }
  }
}
```

### 7.3 Call/reply (`call`)

Mochi `agent.call(msg)` is request/reply with a future:

```typescript
export type Reply<R> = (value: R) => void;

export interface CallEnvelope<Req, Rep> {
  readonly kind: "call";
  readonly payload: Req;
  readonly reply: Reply<Rep>;
}

export interface CastEnvelope<Cast> {
  readonly kind: "cast";
  readonly payload: Cast;
}

export type AgentMessage<Req, Rep, Cast> =
  | CallEnvelope<Req, Rep>
  | CastEnvelope<Cast>;

export abstract class CallableAgent<Req, Rep, Cast, S>
  extends AgentBase<AgentMessage<Req, Rep, Cast>, S>
{
  async call(req: Req): Promise<Rep> {
    if (this.signal.aborted) {
      throw new Error("agent: call on aborted agent");
    }
    const { promise, resolve } = Promise.withResolvers<Rep>();
    this.mailbox.push({ kind: "call", payload: req, reply: resolve });
    return promise;
  }

  cast(msg: AgentMessage<Req, Rep, Cast>): void {
    super.cast(msg);
  }

  protected async handle(
    state: S,
    msg: AgentMessage<Req, Rep, Cast>,
  ): Promise<S> {
    switch (msg.kind) {
      case "call": {
        const [next, reply] = await this.handleCall(state, msg.payload);
        msg.reply(reply);
        return next;
      }
      case "cast":
        return this.handleCast(state, msg.payload);
      default: {
        const _exhaustive: never = msg;
        throw new Error(`unreachable: ${String(_exhaustive)}`);
      }
    }
  }

  protected abstract handleCall(state: S, req: Req): Promise<[S, Rep]> | [S, Rep];
  protected abstract handleCast(state: S, cast: Cast): Promise<S> | S;
}
```

The shape mirrors the shared-decisions sketch: `Promise.withResolvers`
for the reply future, discriminated union for the envelope.

### 7.4 `Supervisor` and restart strategies

```typescript
export type RestartStrategy = "one-for-one" | "one-for-all" | "rest-for-one";

export interface ChildSpec<A> {
  readonly name: string;
  readonly factory: (signal: AbortSignal) => Promise<A> | A;
  readonly maxRestarts?: number;
  readonly periodSeconds?: number;
}

export class Supervisor {
  readonly #strategy: RestartStrategy;
  readonly #specs: Array<ChildSpec<unknown>> = [];
  readonly #abortController = new AbortController();
  #stopped = false;

  constructor(strategy: RestartStrategy) {
    this.#strategy = strategy;
  }

  addChild<A>(spec: ChildSpec<A>): void {
    this.#specs.push(spec as ChildSpec<unknown>);
  }

  async run(parentSignal?: AbortSignal): Promise<void> {
    if (parentSignal !== undefined) {
      parentSignal.addEventListener(
        "abort",
        () => this.#abortController.abort(parentSignal.reason),
        { once: true },
      );
    }

    switch (this.#strategy) {
      case "one-for-one":
        return this.runOneForOne();
      case "one-for-all":
        return this.runOneForAll();
      case "rest-for-one":
        return this.runRestForOne();
      default: {
        const _exhaustive: never = this.#strategy;
        throw new Error(`unreachable: ${String(_exhaustive)}`);
      }
    }
  }

  stop(): void {
    this.#stopped = true;
    this.#abortController.abort(new Error("supervisor stopped"));
  }

  private async runOneForAll(): Promise<void> {
    while (!this.#stopped) {
      const signal = this.#abortController.signal;
      try {
        const children = await Promise.all(
          this.#specs.map((spec) => spec.factory(signal)),
        );
        // wait for any failure; cancel siblings via the shared signal
        await Promise.race(
          children.map((c) =>
            c instanceof Object && "awaitTermination" in c
              ? (c as { awaitTermination(): Promise<void> }).awaitTermination()
              : Promise.resolve(),
          ),
        );
      } catch (err) {
        if (!this.shouldRestart()) throw err;
        await this.waitBackoff();
      }
    }
  }

  private async runOneForOne(): Promise<void> {
    // each child gets its own try/catch loop
    await Promise.all(
      this.#specs.map((spec) => this.runOneChild(spec)),
    );
  }

  private async runOneChild(spec: ChildSpec<unknown>): Promise<void> {
    while (!this.#stopped) {
      try {
        const a = await spec.factory(this.#abortController.signal);
        if (a instanceof Object && "awaitTermination" in a) {
          await (a as { awaitTermination(): Promise<void> }).awaitTermination();
        }
        return;
      } catch (err) {
        if (!this.shouldRestart()) throw err;
        await this.waitBackoff();
      }
    }
  }

  private async runRestForOne(): Promise<void> {
    // TODO: implement
    throw new Error("rest-for-one not yet implemented");
  }

  #restartLog: number[] = [];

  private shouldRestart(): boolean {
    const now = performance.now();
    const periodMs = (this.#specs[0]?.periodSeconds ?? 5) * 1000;
    const maxRestarts = this.#specs[0]?.maxRestarts ?? 3;
    this.#restartLog = this.#restartLog.filter((t) => now - t < periodMs);
    if (this.#restartLog.length >= maxRestarts) return false;
    this.#restartLog.push(now);
    return true;
  }

  private async waitBackoff(): Promise<void> {
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
}
```

The Erlang/OTP heritage shows: `one_for_one` restarts only the
failing child, `one_for_all` rebuilds the whole group, `rest_for_one`
restarts the failing child and all later-defined siblings. Restart
budgeting tracks timestamps in `#restartLog`; if the count exceeds
`maxRestarts` within `periodSeconds`, the supervisor itself fails up.

Compared to the MEP-50 Kotlin `SupervisorJob` and MEP-51 asyncio
`TaskGroup` approaches, the TypeScript version is more manual
because the platform has no built-in structured concurrency
primitive. `AbortController` plus `Promise.all` is the closest
analogue.

### 7.5 Why not RxJS, Effect.ts, or Web Streams?

- **RxJS** is a major dep with its own learning curve, and it
  forces all Mochi-emitted code to think in observables. Mochi's
  semantic is message-passing actors, not push-pull reactive
  streams. Rejected per shared decisions.
- **Effect.ts** is the modern functional-effect library with great
  structured concurrency. It is heavyweight (~50kb) and pulls in
  its own scheduler. Rejected for v1 to keep `mochi_runtime`
  closer to the platform.
- **Web Streams** (`ReadableStream`, `WritableStream`,
  `TransformStream`) are designed for I/O backpressure, not for
  agent mailboxes. The typed surface (`ReadableStream<T>`) lacks
  the cast/call distinction we need. Used only for
  `--target=stream-pipe` AsyncIterator interop.

### 7.6 Worked example: a counter agent

```typescript
import { CallableAgent } from "@mochi/runtime/agent";

type CounterReq = { op: "get" };
type CounterRep = number;
type CounterCast = { op: "inc"; by: number };

class CounterAgent extends CallableAgent<CounterReq, CounterRep, CounterCast, number> {
  constructor(signal: AbortSignal) {
    super(0, signal);
  }

  protected handleCall(state: number, _req: CounterReq): [number, number] {
    return [state, state];
  }

  protected handleCast(state: number, cast: CounterCast): number {
    return state + cast.by;
  }
}

// usage
const controller = new AbortController();
const counter = new CounterAgent(controller.signal);

counter.cast({ kind: "cast", payload: { op: "inc", by: 1 } });
counter.cast({ kind: "cast", payload: { op: "inc", by: 2 } });
const v = await counter.call({ op: "get" });
console.log(v);  // 3

controller.abort();
await counter.awaitTermination();
```

---

## 8. `mochi_runtime/stream`

Mochi `stream<T>` lowers to `AsyncIterable<T>` (often
`AsyncGenerator<T, void, undefined>` when the producer is an
`async function*`). The runtime provides combinators that work over
any `AsyncIterable<T>`:

### 8.1 `merge`

Fan-in N streams into one in arrival order:

```typescript
export async function* merge<T>(
  ...streams: ReadonlyArray<AsyncIterable<T>>
): AsyncGenerator<T, void, undefined> {
  const queue = new AsyncIterableQueue<{ idx: number; done: boolean; value?: T }>();
  let pending = streams.length;

  for (let i = 0; i < streams.length; i++) {
    (async (idx: number, src: AsyncIterable<T>) => {
      try {
        for await (const item of src) {
          queue.push({ idx, done: false, value: item });
        }
      } finally {
        queue.push({ idx, done: true });
      }
    })(i, streams[i]!);
  }

  for await (const { done, value } of queue) {
    if (done) {
      pending--;
      if (pending === 0) {
        queue.close();
        break;
      }
      continue;
    }
    yield value as T;
  }
}
```

Each child generator is launched as an IIFE that pushes to the
shared queue; the consumer iterates the queue. The `pending`
counter tracks how many source streams have finished. When all
finish, we close the queue.

Tradeoff: this approach lacks structured concurrency (a thrown
exception in one pump does not cancel the others). A more robust
version would use an `AbortController` to propagate cancellation;
the v1 version is the simple form and matches MEP-51's
`asyncio.gather`-based merge.

### 8.2 `broadcast`

Fan-out one stream to N consumers:

```typescript
export function broadcast<T>(
  source: AsyncIterable<T>,
  n: number,
): ReadonlyArray<AsyncIterable<T>> {
  const queues: AsyncIterableQueue<T>[] = [];
  for (let i = 0; i < n; i++) queues.push(new AsyncIterableQueue<T>());

  (async () => {
    try {
      for await (const item of source) {
        for (const q of queues) q.push(item);
      }
    } finally {
      for (const q of queues) q.close();
    }
  })();

  return queues;
}
```

The pump is a fire-and-forget async IIFE; the consumer queues hold
references via their `AsyncIterableQueue` instances, so the pump
stays alive until the queues are drained.

Backpressure propagates per-consumer (slow consumer grows its
queue; fast consumers drain theirs); future work is to add
per-consumer bounded queues with drop policies.

### 8.3 `periodic`

Emit a tick every `dtMs` milliseconds:

```typescript
export async function* periodic(
  dtMs: number,
  signal?: AbortSignal,
): AsyncGenerator<number, void, undefined> {
  const startMs = performance.now();
  let k = 0;
  while (true) {
    if (signal?.aborted) return;
    const deadlineMs = startMs + (k + 1) * dtMs;
    const delay = Math.max(0, deadlineMs - performance.now());
    await new Promise<void>((resolve, reject) => {
      const t = setTimeout(resolve, delay);
      if (signal !== undefined) {
        signal.addEventListener("abort", () => {
          clearTimeout(t);
          reject(new Error("aborted"));
        }, { once: true });
      }
    });
    if (signal?.aborted) return;
    k++;
    yield deadlineMs;
  }
}
```

Drift-corrected: each iteration's deadline is `start + k*dt`, not
`previous_deadline + dt`. A slow consumer does not cause cumulative
drift.

The `signal` parameter is the cancellation hook: pass an
`AbortSignal` from a parent scope and the timer terminates when
the signal aborts.

### 8.4 `fromIter` / `toArray`

Adapters between sync `Iterable<T>` and `AsyncIterable<T>`:

```typescript
export async function* fromIter<T>(
  xs: Iterable<T>,
): AsyncGenerator<T, void, undefined> {
  for (const x of xs) yield x;
}

export async function toArray<T>(s: AsyncIterable<T>): Promise<T[]> {
  const out: T[] = [];
  for await (const x of s) out.push(x);
  return out;
}
```

### 8.5 `mapStream`, `filterStream`, `flatMapStream`

```typescript
export async function* mapStream<T, U>(
  s: AsyncIterable<T>,
  f: (x: T) => U | Promise<U>,
): AsyncGenerator<U, void, undefined> {
  for await (const x of s) yield await f(x);
}

export async function* filterStream<T>(
  s: AsyncIterable<T>,
  pred: (x: T) => boolean | Promise<boolean>,
): AsyncGenerator<T, void, undefined> {
  for await (const x of s) {
    if (await pred(x)) yield x;
  }
}

export async function* flatMapStream<T, U>(
  s: AsyncIterable<T>,
  f: (x: T) => AsyncIterable<U>,
): AsyncGenerator<U, void, undefined> {
  for await (const x of s) {
    for await (const y of f(x)) yield y;
  }
}
```

### 8.6 Why not RxJS Operators or `web-streams-polyfill`?

- **RxJS Operators** (`pipe`, `map`, `filter`, ...) have a richer
  set than what Mochi needs. We inline the half-dozen we use.
- **`web-streams-polyfill`** is for environments missing native
  Web Streams; not a stream-combinator library. Not applicable.
- **TC39 Async Iterator Helpers** (stage 3 as of 2026, expected
  stage 4 2026-Q3) will provide `AsyncIterator.prototype.map`,
  `.filter`, etc. natively. Once stage 4 and shipping in all four
  runtimes, the runtime helpers above become thin wrappers (or
  deletable). Tracked in [[12-risks-and-alternatives]].

---

## 9. `mochi_runtime/query`

Mochi query DSL (the LINQ-shaped `from ... where ... select ...`
syntax) lowers to TS Iterator-helper chains where possible (since
TS 5.6 ships them) and to named runtime helpers for joins, group-by,
and order-by.

### 9.1 Iterator helpers (TS 5.6 + ES2024)

```typescript
// Mochi:
//   from x in xs where x > 0 select x * 2
// TS:
const result = Iterator.from(xs)
  .filter((x) => x > 0)
  .map((x) => x * 2)
  .toArray();
```

`Iterator.from(xs)` wraps any iterable in the `Iterator` builtin
that has the helper methods. The Mochi emitter picks this path
when the source is a sync iterable.

For async sources:

```typescript
// Async iterator helpers; will be TC39 stage 4 soon
const result = await AsyncIterator.from(asyncXs)
  .filter(async (x) => x > 0)
  .map(async (x) => x * 2)
  .toArray();
```

Async helpers reach stage 4 in 2026; the runtime ships a polyfill
under `_internal/async_iterator_helpers.ts` for engines that lag.

### 9.2 Hash join

```typescript
export function* hashJoin<L, R, K, T>(
  left: Iterable<L>,
  right: Iterable<R>,
  leftKey: (l: L) => K,
  rightKey: (r: R) => K,
  select: (l: L, r: R) => T,
): IterableIterator<T> {
  const index = new Map<K, R[]>();
  for (const r of right) {
    const k = rightKey(r);
    const bucket = index.get(k);
    if (bucket === undefined) {
      index.set(k, [r]);
    } else {
      bucket.push(r);
    }
  }
  for (const l of left) {
    const matches = index.get(leftKey(l));
    if (matches !== undefined) {
      for (const r of matches) {
        yield select(l, r);
      }
    }
  }
}
```

Indexes the right side, streams the left side. Memory is O(|right|).
Stable order: left-major, then right insertion order. Matches Mochi
spec ordering for `join`.

### 9.3 Merge join

```typescript
export function* mergeJoinSorted<L, R, K, T>(
  left: Iterable<L>,
  right: Iterable<R>,
  leftKey: (l: L) => K,
  rightKey: (r: R) => K,
  cmp: (a: K, b: K) => number,
  select: (l: L, r: R) => T,
): IterableIterator<T> {
  const li = left[Symbol.iterator]();
  const ri = right[Symbol.iterator]();
  let l = li.next();
  let r = ri.next();
  while (!l.done && !r.done) {
    const kl = leftKey(l.value);
    const kr = rightKey(r.value);
    const c = cmp(kl, kr);
    if (c < 0) {
      l = li.next();
    } else if (c > 0) {
      r = ri.next();
    } else {
      const run: R[] = [];
      while (!r.done && cmp(rightKey(r.value), kl) === 0) {
        run.push(r.value);
        r = ri.next();
      }
      while (!l.done && cmp(leftKey(l.value), kl) === 0) {
        for (const rr of run) yield select(l.value, rr);
        l = li.next();
      }
    }
  }
}
```

The `cmp` parameter is mandatory because TS does not have a builtin
`<` for arbitrary `K` (unlike Python where the `<` operator is
type-checked at parse and runtime). Mochi emits the comparator
based on the IR type of `K` (numeric, string, tuple, etc).

### 9.4 Nested-loop join

```typescript
export function* nestedLoopJoin<L, R, T>(
  left: Iterable<L>,
  right: Iterable<R>,
  pred: (l: L, r: R) => boolean,
  select: (l: L, r: R) => T,
): IterableIterator<T> {
  const rightArr = Array.from(right);
  for (const l of left) {
    for (const r of rightArr) {
      if (pred(l, r)) yield select(l, r);
    }
  }
}
```

Last-resort fallback for non-equi joins (`where left.x < right.y`).

### 9.5 `groupBy`

```typescript
export function groupBy<T, K>(
  xs: Iterable<T>,
  key: (x: T) => K,
): Map<K, T[]> {
  const out = new Map<K, T[]>();
  for (const x of xs) {
    const k = key(x);
    const bucket = out.get(k);
    if (bucket === undefined) {
      out.set(k, [x]);
    } else {
      bucket.push(x);
    }
  }
  return out;
}
```

Returns a `Map` (insertion-ordered), so iterating the result yields
groups in the order their first member was seen. Matches Mochi
`group by` semantics.

ES2024 adds `Object.groupBy` and `Map.groupBy` as static methods;
the runtime helper above predates them and matches the Mochi
semantic exactly. For typed maps, `Map.groupBy(xs, key)` is a TC39
stage 4 alternative the emitter can use (and the helper above is a
back-compat thin wrapper).

### 9.6 `orderBy` / `orderByDesc`

```typescript
export function orderBy<T, K>(
  xs: Iterable<T>,
  key: (x: T) => K,
  cmp: (a: K, b: K) => number,
): T[] {
  const arr = Array.from(xs);
  return arr.sort((a, b) => cmp(key(a), key(b)));
}

export function orderByDesc<T, K>(
  xs: Iterable<T>,
  key: (x: T) => K,
  cmp: (a: K, b: K) => number,
): T[] {
  const arr = Array.from(xs);
  return arr.sort((a, b) => cmp(key(b), key(a)));
}
```

`Array.prototype.sort` is **stable** since ES2019 (TC39 proposal-
stable-sort). Stable is load-bearing for Mochi's "secondary keys
preserve primary order" semantic.

The default `sort()` (without a comparator) coerces every element
to string and lexicographically compares; the emitter always passes
a comparator to avoid this trap.

### 9.7 `distinct`

```typescript
export function* distinct<T>(xs: Iterable<T>): IterableIterator<T> {
  const seen = new Set<T>();
  for (const x of xs) {
    if (!seen.has(x)) {
      seen.add(x);
      yield x;
    }
  }
}
```

For unhashable elements (a Mochi map or list slipped in), the
emitter inserts a JSON-string-key fallback (`distinctBy(xs,
JSON.stringify)`) at the lowering level.

---

## 10. `mochi_runtime/datalog`

Mochi Datalog programs lower to TypeScript. The runtime provides the
evaluator. Two algorithms ship:

### 10.1 Semi-naive bottom-up

The canonical Datalog evaluator (Ceri-Gottlob-Tanca 1989). For each
predicate, maintain (a) the full fact set and (b) the delta of
newly derived facts. At each iteration, evaluate each rule using
**at least one delta predicate in the body**, add new facts to the
delta, swap deltas and full sets. Fixed point reached when all
deltas are empty.

```typescript
export type Fact = readonly unknown[];
export type Predicate = string;
export type Atom = readonly [Predicate, readonly unknown[]];

export interface Rule {
  readonly head: Atom;
  readonly body: ReadonlyArray<Atom>;
}

export class DatalogProgram {
  readonly #rules: ReadonlyArray<Rule>;
  readonly #facts: Map<Predicate, Set<string>>;
  readonly #delta: Map<Predicate, Set<string>>;

  constructor(rules: ReadonlyArray<Rule>, edb: ReadonlyMap<Predicate, ReadonlySet<Fact>>) {
    this.#rules = rules;
    this.#facts = new Map();
    this.#delta = new Map();
    for (const [p, facts] of edb) {
      const enc = new Set<string>();
      for (const f of facts) enc.add(JSON.stringify(f));
      this.#facts.set(p, new Set(enc));
      this.#delta.set(p, new Set(enc));
    }
  }

  evaluate(): ReadonlyMap<Predicate, ReadonlySet<Fact>> {
    while (this.hasDeltas()) {
      const newDelta = new Map<Predicate, Set<string>>();
      for (const rule of this.#rules) {
        const derived = this.fireWithDelta(rule);
        const headPred = rule.head[0];
        const knownFacts = this.#facts.get(headPred) ?? new Set<string>();
        const fresh = new Set<string>();
        for (const d of derived) {
          if (!knownFacts.has(d)) fresh.add(d);
        }
        if (fresh.size > 0) {
          const bucket = newDelta.get(headPred);
          if (bucket === undefined) {
            newDelta.set(headPred, fresh);
          } else {
            for (const f of fresh) bucket.add(f);
          }
          const fb = this.#facts.get(headPred);
          if (fb === undefined) {
            this.#facts.set(headPred, new Set(fresh));
          } else {
            for (const f of fresh) fb.add(f);
          }
        }
      }
      this.#delta.clear();
      for (const [p, fs] of newDelta) this.#delta.set(p, fs);
    }
    const out = new Map<Predicate, Set<Fact>>();
    for (const [p, fs] of this.#facts) {
      const decoded = new Set<Fact>();
      for (const enc of fs) decoded.add(JSON.parse(enc) as Fact);
      out.set(p, decoded);
    }
    return out;
  }

  private hasDeltas(): boolean {
    for (const fs of this.#delta.values()) {
      if (fs.size > 0) return true;
    }
    return false;
  }

  private fireWithDelta(_rule: Rule): Set<string> {
    // unification with delta-pinned body atom; implementation elided
    return new Set<string>();
  }
}
```

The JSON-encoded fact strings work around the lack of value-based
hashing for arrays in JavaScript (a `Set<readonly unknown[]>` does
not hash; two arrays with identical contents are distinct keys).
JSON encoding is the simplest stable canonicalisation for
homogeneous fact tuples.

The fire-with-delta routine enumerates body atoms, picks each in
turn as the "delta atom" (drawn from `#delta` rather than `#facts`),
and unifies the remaining body atoms against `#facts`. The standard
semi-naive trick that avoids re-deriving old facts.

### 10.2 Magic sets

For query-driven evaluation (top-down with bottom-up engine), the
runtime supports a magic-set rewrite. The implementation rewrites
the rule set in-place: for each query goal `goal(X)`, introduce a
`m_goal(X)` predicate, propagate it through the rules, and
restrict bottom-up evaluation to facts reachable from the goal.
Reference: Bancilhon-Maier-Sagiv-Ullman 1986.

The rewrite is in `_internal/magic_sets.ts` (~280 LOC). It is
opt-in: the Mochi emitter chooses the strategy based on whether
the user wrote a `query` clause vs a `fact-set extraction`.

### 10.3 Stratified negation

Negation-as-failure with stratification (Apt-Blair-Walker 1988):
if `p` depends on `not q`, then all of `q`'s facts must be
computed before `p`'s. The runtime computes the predicate
dependency graph, finds strongly-connected components, topologically
sorts them, and evaluates one stratum at a time. Aggregates
(`count`, `sum`, `min`, `max`) ship in the same engine and are
stratified the same way.

### 10.4 Why not Souffle.js or Logica?

- **Souffle** is a C++ Datalog compiler with a JS subset, but
  Souffle-on-JS is not a published package; we would have to
  build/maintain it ourselves.
- **Logica** (Google) targets BigQuery, not in-process JS.
- **datascript** is an EAV-shaped Datalog (Clojure heritage); its
  surface differs from Mochi's positional fact tuples.

The ~900-LOC in-tree TS evaluator is cheaper to own. Tracked as a
non-issue.

---

## 11. `mochi_runtime/ai`

Mochi's `ai.generate` / `ai.stream` / `ai.embed` calls dispatch
through a single registry indexed by provider URL scheme.

### 11.1 Provider interface

```typescript
export interface LLMProvider {
  generate(prompt: string, opts: GenerateOptions): Promise<string>;
  stream(prompt: string, opts: GenerateOptions): AsyncIterable<string>;
  embed(text: string, opts: EmbedOptions): Promise<readonly number[]>;
}

export interface GenerateOptions {
  readonly model: string;
  readonly temperature?: number;
  readonly maxTokens?: number;
  readonly stop?: ReadonlyArray<string>;
  readonly signal?: AbortSignal;
}

export interface EmbedOptions {
  readonly model: string;
}

const REGISTRY = new Map<string, LLMProvider>();

export function register(scheme: string, provider: LLMProvider): void {
  REGISTRY.set(scheme, provider);
}

export function resolve(url: string): readonly [LLMProvider, string] {
  const idx = url.indexOf("://");
  if (idx < 0) {
    throw new Error(`ai: invalid provider URL ${url}`);
  }
  const scheme = url.slice(0, idx);
  const model = url.slice(idx + 3);
  const p = REGISTRY.get(scheme);
  if (p === undefined) {
    throw new Error(`ai: unknown LLM provider ${scheme}`);
  }
  return [p, model];
}

export async function generate(url: string, prompt: string, opts: Omit<GenerateOptions, "model"> = {}): Promise<string> {
  const [p, model] = resolve(url);
  return p.generate(prompt, { ...opts, model });
}
```

### 11.2 OpenAI provider

```typescript
import type { OpenAI } from "openai";

export class OpenAIProvider implements LLMProvider {
  readonly #client: OpenAI;

  constructor(apiKey?: string) {
    // lazy import; openai is an optionalDependency
    const key = apiKey ?? globalThis.process?.env?.OPENAI_API_KEY;
    if (typeof key !== "string" || key.length === 0) {
      throw new Error("OpenAIProvider: no API key");
    }
    // dynamic import keeps the SDK out of the bundle when not used
    const { default: OpenAIClass } = await import("openai");
    this.#client = new OpenAIClass({ apiKey: key });
  }

  async generate(prompt: string, opts: GenerateOptions): Promise<string> {
    const resp = await this.#client.chat.completions.create({
      model: opts.model,
      messages: [{ role: "user", content: prompt }],
      temperature: opts.temperature,
      max_tokens: opts.maxTokens,
      stop: opts.stop as string[] | undefined,
    }, { signal: opts.signal });
    return resp.choices[0]?.message.content ?? "";
  }

  async *stream(prompt: string, opts: GenerateOptions): AsyncIterable<string> {
    const stream = await this.#client.chat.completions.create({
      model: opts.model,
      messages: [{ role: "user", content: prompt }],
      stream: true,
      temperature: opts.temperature,
      max_tokens: opts.maxTokens,
      stop: opts.stop as string[] | undefined,
    }, { signal: opts.signal });
    for await (const chunk of stream) {
      const piece = chunk.choices[0]?.delta?.content;
      if (typeof piece === "string") yield piece;
    }
  }

  async embed(text: string, opts: EmbedOptions): Promise<readonly number[]> {
    const resp = await this.#client.embeddings.create({
      model: opts.model,
      input: text,
    });
    return resp.data[0]?.embedding ?? [];
  }
}
```

The `openai` package is declared as an **optionalDependency** in
`package.json`. Installation is `npm install openai` (or the user
omits it and the OpenAIProvider import path throws at first call).

### 11.3 Anthropic provider

Same shape, importing `@anthropic-ai/sdk`. The SDK's API is
similar enough that the only difference is `messages` shape
(Anthropic requires alternating user/assistant pairs).

### 11.4 Google provider

Same shape, importing `@google/generative-ai`. The Google API uses
`GenerativeModel.generateContent` instead of `chat.completions`.

### 11.5 Ollama provider

For local models running on `localhost:11434`:

```typescript
export class OllamaProvider implements LLMProvider {
  readonly #baseUrl: string;

  constructor(baseUrl = "http://localhost:11434") {
    this.#baseUrl = baseUrl;
  }

  async generate(prompt: string, opts: GenerateOptions): Promise<string> {
    const resp = await fetch(`${this.#baseUrl}/api/generate`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        model: opts.model,
        prompt,
        stream: false,
        options: {
          temperature: opts.temperature,
          num_predict: opts.maxTokens,
          stop: opts.stop,
        },
      }),
      signal: opts.signal,
    });
    if (!resp.ok) throw new Error(`ollama: ${resp.status} ${resp.statusText}`);
    const body = (await resp.json()) as { response: string };
    return body.response;
  }

  async *stream(prompt: string, opts: GenerateOptions): AsyncIterable<string> {
    const resp = await fetch(`${this.#baseUrl}/api/generate`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ model: opts.model, prompt, stream: true }),
      signal: opts.signal,
    });
    if (!resp.ok) throw new Error(`ollama: ${resp.status}`);
    const reader = resp.body!.getReader();
    const decoder = new TextDecoder();
    let buf = "";
    while (true) {
      const { value, done } = await reader.read();
      if (done) break;
      buf += decoder.decode(value, { stream: true });
      const lines = buf.split("\n");
      buf = lines.pop() ?? "";
      for (const line of lines) {
        if (!line) continue;
        const parsed = JSON.parse(line) as { response: string; done: boolean };
        if (parsed.response) yield parsed.response;
      }
    }
  }

  async embed(text: string, opts: EmbedOptions): Promise<readonly number[]> {
    const resp = await fetch(`${this.#baseUrl}/api/embeddings`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ model: opts.model, prompt: text }),
    });
    const body = (await resp.json()) as { embedding: number[] };
    return body.embedding;
  }
}
```

Uses the platform `fetch` (no `node-fetch` polyfill). Streaming
parses NDJSON (newline-delimited JSON) from the response body.

### 11.6 `llama.cpp` provider (Node-only)

For fully offline use via the `llama-server` HTTP shim:

```typescript
export class LlamaCppProvider implements LLMProvider {
  readonly #baseUrl: string;

  constructor(baseUrl = "http://localhost:8080") {
    this.#baseUrl = baseUrl;
  }

  // Implementation similar to Ollama; uses /completion endpoint.
}
```

For a direct binary-invocation path (no HTTP server), the runtime
ships a Node-only `LlamaCppCliProvider` that spawns `llama-cli` via
`node:child_process`. Bun has the same API; Deno's `Deno.Command`
works similarly. The CLI path is gated behind the runtime detection
because the browser has no subprocess primitive.

### 11.7 Default registration

```typescript
function registerDefaults(): void {
  try {
    register("openai", new OpenAIProvider());
  } catch { /* SDK not installed; ok */ }
  try {
    register("anthropic", new AnthropicProvider());
  } catch { /* ok */ }
  try {
    register("google", new GoogleProvider());
  } catch { /* ok */ }
  register("ollama", new OllamaProvider());
  register("llamacpp", new LlamaCppProvider());
}

registerDefaults();
```

Lazy registration tolerates missing optional deps. If `openai` is
not installed, the OpenAI provider is simply not registered; an
`openai://gpt-4o` URL throws a clean error at first call.

---

## 12. `mochi_runtime/fetch`

The platform `fetch` API is available on **all four** tier-1
runtimes (Node 22+, Deno 2+, Bun 1.1+, browser baseline 2024). The
runtime module is a thin wrapper that adds Mochi-shaped helpers:

```typescript
export interface FetchOptions {
  readonly headers?: Readonly<Record<string, string>>;
  readonly signal?: AbortSignal;
  readonly timeoutMs?: number;
}

export async function getBytes(url: string, opts: FetchOptions = {}): Promise<Uint8Array> {
  const signal = combineSignals(opts.signal, opts.timeoutMs);
  const resp = await fetch(url, {
    headers: { "User-Agent": userAgent(), ...opts.headers },
    signal,
  });
  if (!resp.ok) {
    throw new HttpError(resp.status, resp.statusText, url);
  }
  return new Uint8Array(await resp.arrayBuffer());
}

export async function getText(url: string, opts: FetchOptions = {}): Promise<string> {
  const signal = combineSignals(opts.signal, opts.timeoutMs);
  const resp = await fetch(url, {
    headers: { "User-Agent": userAgent(), ...opts.headers },
    signal,
  });
  if (!resp.ok) throw new HttpError(resp.status, resp.statusText, url);
  return resp.text();
}

export async function getJson<T>(url: string, opts: FetchOptions = {}): Promise<T> {
  const signal = combineSignals(opts.signal, opts.timeoutMs);
  const resp = await fetch(url, {
    headers: { Accept: "application/json", "User-Agent": userAgent(), ...opts.headers },
    signal,
  });
  if (!resp.ok) throw new HttpError(resp.status, resp.statusText, url);
  return (await resp.json()) as T;
}

export async function postJson<T>(
  url: string,
  body: unknown,
  opts: FetchOptions = {},
): Promise<T> {
  const signal = combineSignals(opts.signal, opts.timeoutMs);
  const resp = await fetch(url, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      Accept: "application/json",
      "User-Agent": userAgent(),
      ...opts.headers,
    },
    body: JSON.stringify(body),
    signal,
  });
  if (!resp.ok) throw new HttpError(resp.status, resp.statusText, url);
  return (await resp.json()) as T;
}

export async function* streamLines(
  url: string,
  opts: FetchOptions = {},
): AsyncGenerator<string, void, undefined> {
  const signal = combineSignals(opts.signal, opts.timeoutMs);
  const resp = await fetch(url, {
    headers: { "User-Agent": userAgent(), ...opts.headers },
    signal,
  });
  if (!resp.ok) throw new HttpError(resp.status, resp.statusText, url);
  const reader = resp.body!.getReader();
  const decoder = new TextDecoder();
  let buf = "";
  while (true) {
    const { value, done } = await reader.read();
    if (done) {
      if (buf.length > 0) yield buf;
      break;
    }
    buf += decoder.decode(value, { stream: true });
    const lines = buf.split("\n");
    buf = lines.pop() ?? "";
    for (const line of lines) yield line;
  }
}

export class HttpError extends Error {
  constructor(
    readonly status: number,
    readonly statusText: string,
    readonly url: string,
  ) {
    super(`HTTP ${status} ${statusText} for ${url}`);
    this.name = "HttpError";
  }
}

let _userAgent = "mochi-runtime/0.1.0";

export function setUserAgent(s: string): void { _userAgent = s; }
export function userAgent(): string { return _userAgent; }

function combineSignals(
  signal: AbortSignal | undefined,
  timeoutMs: number | undefined,
): AbortSignal | undefined {
  if (signal === undefined && timeoutMs === undefined) return undefined;
  if (signal !== undefined && timeoutMs === undefined) return signal;
  if (signal === undefined && timeoutMs !== undefined) {
    return AbortSignal.timeout(timeoutMs);
  }
  return AbortSignal.any([signal!, AbortSignal.timeout(timeoutMs!)]);
}
```

Notes:

- **No `node-fetch`, no `cross-fetch`**: native `fetch` is the
  baseline on every tier-1 runtime. Browsers had it since 2017
  (Chrome 42, Firefox 39); Node added it in 18.0 (April 2022),
  Deno since 1.0, Bun since launch.
- **`AbortSignal.timeout`** (Node 17.3+, Deno 1.32+, Bun 1.0+,
  browser baseline 2024) is the cross-runtime timeout primitive.
- **`AbortSignal.any`** (Node 20.3+, Deno 1.40+, browser baseline
  2024) combines multiple signals. The fallback for older Node 22
  point releases is a custom combinator; not needed at the 22.4 LTS
  floor.
- **`HttpError` extends `Error`**: standard JS error class with a
  named `name`. `instanceof HttpError` works for Mochi error
  pattern-match if the Mochi user wraps the call in `try/catch`.
  But the canonical Mochi error model is `MochiResult<T, E>`; the
  emitter usually wraps `getJson` in a `fromThrowing` adapter that
  converts to `Ok(...)` / `Err(...)`. Section 14.

### 12.1 Streaming responses

`fetch` returns a `Response` whose `body` is a `ReadableStream<Uint8Array>`.
The `streamLines` helper above shows the canonical line-buffered
consumer pattern. For more complex streaming protocols (Server-Sent
Events, NDJSON, JSONL), the emitter inserts the appropriate parser
between the byte reader and the consumer.

### 12.2 HTTP/2 and HTTP/3

Node 22's native `fetch` is built on `undici`, which supports
HTTP/1.1 only as of 6.x. HTTP/2 is on the undici roadmap; not v1
gated. Deno 2 supports HTTP/2 in fetch since 1.30. Bun supports
HTTP/2. Browsers all do.

### 12.3 Why not `undici` directly, `got`, `axios`?

- **undici**: lower-level (Streams + Dispatcher); we use it
  indirectly via Node's `fetch`. Adding it as a direct dep duplicates
  what platform fetch provides.
- **got**: feature-rich Node-only HTTP client; cross-runtime is
  not its goal.
- **axios**: legacy XHR-shape, originally browser; the modern
  replacement is platform `fetch`.

Sticking with `fetch` is the right call.

---

## 13. `mochi_runtime/json_value`

Mochi `json` is a sealed union of "null, bool, number, string,
array, object". Lowered to a discriminated union:

```typescript
export type JsonValue =
  | { readonly kind: "null" }
  | { readonly kind: "bool"; readonly value: boolean }
  | { readonly kind: "num";  readonly value: number }
  | { readonly kind: "str";  readonly value: string }
  | { readonly kind: "arr";  readonly value: readonly JsonValue[] }
  | { readonly kind: "obj";  readonly value: ReadonlyMap<string, JsonValue> };

export const jNull = (): JsonValue => ({ kind: "null" });
export const jBool = (v: boolean): JsonValue => ({ kind: "bool", value: v });
export const jNum  = (v: number):  JsonValue => ({ kind: "num",  value: v });
export const jStr  = (v: string):  JsonValue => ({ kind: "str",  value: v });
export const jArr  = (v: readonly JsonValue[]): JsonValue => ({ kind: "arr", value: v });
export const jObj  = (v: ReadonlyMap<string, JsonValue>): JsonValue => ({ kind: "obj", value: v });
```

### 13.1 Parsing from `JSON.parse`

`JSON.parse(s)` returns `unknown` (a value whose shape we know
structurally but the TS type system does not narrow for us). We
wrap that into our sealed union:

```typescript
export function fromUnknown(x: unknown): JsonValue {
  if (x === null) return jNull();
  if (typeof x === "boolean") return jBool(x);
  if (typeof x === "number") return jNum(x);
  if (typeof x === "string") return jStr(x);
  if (Array.isArray(x)) return jArr(x.map(fromUnknown));
  if (typeof x === "object") {
    const m = new Map<string, JsonValue>();
    for (const k of Object.keys(x as object)) {
      m.set(k, fromUnknown((x as Record<string, unknown>)[k]));
    }
    return jObj(m);
  }
  throw new TypeError(`not a JSON value: ${typeof x}`);
}

export function parse(s: string): JsonValue {
  return fromUnknown(JSON.parse(s));
}
```

Note `typeof null === "object"` so we check `x === null` first.
The `Object.keys` iteration is insertion-ordered (since ES2015 for
string keys), so `jObj` preserves source-document key order.

### 13.2 Serialising

```typescript
export function toUnknown(v: JsonValue): unknown {
  switch (v.kind) {
    case "null": return null;
    case "bool": return v.value;
    case "num":  return v.value;
    case "str":  return v.value;
    case "arr":  return v.value.map(toUnknown);
    case "obj": {
      const out: Record<string, unknown> = {};
      for (const [k, val] of v.value) out[k] = toUnknown(val);
      return out;
    }
    default: {
      const _exhaustive: never = v;
      throw new Error(`unreachable: ${String(_exhaustive)}`);
    }
  }
}

export function stringify(v: JsonValue): string {
  return JSON.stringify(toUnknown(v));
}

export function stringifyPretty(v: JsonValue, indent = 2): string {
  return JSON.stringify(toUnknown(v), null, indent);
}
```

The `default` clause with `_exhaustive: never` is the load-bearing
exhaustiveness assertion. TypeScript will reject the code if a new
variant is added to `JsonValue` and the `switch` does not handle
it; the assignment to `never` fails.

### 13.3 Deterministic stringify

`JSON.stringify` preserves the **insertion order** of object keys
in the input, with one exception: numeric-string keys ("0", "1",
"2", ...) are sorted before non-numeric in **objects** (not Maps).
Since we use `Map<string, JsonValue>` and convert to `Record` only
at serialise time, we hit this trap. Workaround: emit a sorted-key
form when determinism matters:

```typescript
export function stringifyCanonical(v: JsonValue): string {
  return JSON.stringify(toUnknownSorted(v));
}

function toUnknownSorted(v: JsonValue): unknown {
  switch (v.kind) {
    case "null": return null;
    case "bool": case "num": case "str": return v.value;
    case "arr": return v.value.map(toUnknownSorted);
    case "obj": {
      const sorted: Record<string, unknown> = {};
      const keys = Array.from(v.value.keys()).sort();
      for (const k of keys) sorted[k] = toUnknownSorted(v.value.get(k)!);
      return sorted;
    }
    default: {
      const _exhaustive: never = v;
      throw new Error(`unreachable: ${String(_exhaustive)}`);
    }
  }
}
```

The canonical form is the input to a SHA-256 hash for content
addressing; the regular form is the input for round-trippable
JSON.

### 13.4 Why not `zod`, `valibot`, `arktype`?

These are runtime-validation libraries that ship with elaborate
schema definitions. Mochi `json` is a **shape**, not a schema; the
runtime carries no validation logic. The schema lives in the
**Mochi type** of the value being parsed; if the user wrote `let
v: Person = parseJson<Person>(s)`, the emitter inserts a
parse-and-validate pair tailored to `Person`'s structure.

Tracked as a v2 candidate (provide an `@mochi/zod-interop` adapter
package) in [[12-risks-and-alternatives]].

---

## 14. `mochi_runtime/result`

Mochi `Result<T, E>` lowers to a discriminated union. Not to
exceptions, not to a class hierarchy.

```typescript
export type Ok<T> = { readonly kind: "ok"; readonly value: T };
export type Err<E> = { readonly kind: "err"; readonly error: E };

export type MochiResult<T, E> = Ok<T> | Err<E>;

export const ok = <T>(value: T): Ok<T> => ({ kind: "ok", value });
export const err = <E>(error: E): Err<E> => ({ kind: "err", error });

export const isOk  = <T, E>(r: MochiResult<T, E>): r is Ok<T>  => r.kind === "ok";
export const isErr = <T, E>(r: MochiResult<T, E>): r is Err<E> => r.kind === "err";
```

### 14.1 Combinators

```typescript
export function mapResult<T, U, E>(
  r: MochiResult<T, E>,
  f: (v: T) => U,
): MochiResult<U, E> {
  return r.kind === "ok" ? ok(f(r.value)) : r;
}

export function mapErr<T, E, F>(
  r: MochiResult<T, E>,
  f: (e: E) => F,
): MochiResult<T, F> {
  return r.kind === "err" ? err(f(r.error)) : r;
}

export function andThen<T, U, E>(
  r: MochiResult<T, E>,
  f: (v: T) => MochiResult<U, E>,
): MochiResult<U, E> {
  return r.kind === "ok" ? f(r.value) : r;
}

export function unwrapOr<T, E>(r: MochiResult<T, E>, fallback: T): T {
  return r.kind === "ok" ? r.value : fallback;
}

export function unwrap<T, E>(r: MochiResult<T, E>): T {
  if (r.kind === "ok") return r.value;
  throw new Error(`unwrap on Err: ${String(r.error)}`);
}

export function fromThrowing<T>(f: () => T): MochiResult<T, unknown> {
  try {
    return ok(f());
  } catch (e) {
    return err(e);
  }
}

export async function fromThrowingAsync<T>(f: () => Promise<T>): Promise<MochiResult<T, unknown>> {
  try {
    return ok(await f());
  } catch (e) {
    return err(e);
  }
}
```

### 14.2 Why not exceptions?

Two reasons mirror the MEP-51 Python decision:

- **Type checker sees them**. TypeScript tracks `MochiResult<T, E>`
  through the program; it does not track `throw new SomeError()`
  (TS has no checked exceptions, and `catch (e)` has `e: unknown`
  under strict mode).
- **Async-edge consistency**. A Promise that rejects propagates
  through `await` and has different shape than a Promise that
  resolves with `Err(...)`. Mochi's semantic is "errors are values";
  Result keeps the shape uniform.

The choice mirrors [[mep-0050]] Kotlin `MochiResult` and
[[mep-0051]] Python `Ok | Err`.

### 14.3 Interop with JS exceptions

Calls into JS libraries that throw (or Promises that reject) are
wrapped with `fromThrowing` / `fromThrowingAsync` at the boundary.
The emitter inserts this automatically when crossing from
JS-typed code to Mochi-typed code.

### 14.4 `AggregateError` and `Promise.any`

ES2021 added `AggregateError` for cases where multiple errors
occur (e.g., `Promise.any` rejects with one). The runtime exposes
a helper:

```typescript
export function fromAggregate(e: unknown): MochiResult<never, unknown[]> {
  if (e instanceof AggregateError) {
    return err(e.errors);
  }
  return err([e]);
}
```

Useful for Mochi `parallel { ... }` blocks that lower to
`Promise.allSettled` and collect both `Ok`s and `Err`s.

---

## 15. `mochi_runtime/time`

Mochi `time` is a zoned wall-clock value. The standard JS API is
`Date`, which is **naive about time zones** (always UTC internally,
formatted via the runtime's local zone). For zoned semantics, the
TC39 **Temporal API** (stage 3 as of 2026-Q1) is the right tool.

### 15.1 Temporal stage status (2026-05-23)

Temporal reached stage 3 in 2022 and has been in stage 3 for
roughly 3 years. Stage 4 has been blocked on Firefox having a
production implementation; that landed in Firefox 128 (mid-2024).
As of 2026 the proposal is **expected stage 4 in 2026-Q2 or Q3**
but is not yet shipping natively in V8 (Chrome/Node) or
JavaScriptCore (Safari/Bun). Deno ships an early Temporal
implementation since 2024-04.

The polyfill `@js-temporal/polyfill` (0.4.x as of 2026) tracks the
stage-3 spec and is the canonical Temporal implementation. We
depend on it and progressively delete polyfill calls as engines
ship native Temporal.

### 15.2 `ZonedDateTime` wrapper

```typescript
import { Temporal } from "@js-temporal/polyfill";

export class ZonedDateTime {
  readonly #inner: Temporal.ZonedDateTime;

  private constructor(inner: Temporal.ZonedDateTime) {
    this.#inner = inner;
  }

  static now(zone = "UTC"): ZonedDateTime {
    return new ZonedDateTime(Temporal.Now.zonedDateTimeISO(zone));
  }

  static fromUnix(seconds: number, zone = "UTC"): ZonedDateTime {
    const instant = Temporal.Instant.fromEpochMilliseconds(Math.floor(seconds * 1000));
    return new ZonedDateTime(instant.toZonedDateTimeISO(zone));
  }

  static parse(s: string): ZonedDateTime {
    return new ZonedDateTime(Temporal.ZonedDateTime.from(s));
  }

  toZone(zone: string): ZonedDateTime {
    return new ZonedDateTime(this.#inner.withTimeZone(zone));
  }

  add(opts: { days?: number; hours?: number; minutes?: number; seconds?: number }): ZonedDateTime {
    return new ZonedDateTime(this.#inner.add(opts));
  }

  formatIso(): string {
    return this.#inner.toString();
  }

  get year(): number { return this.#inner.year; }
  get month(): number { return this.#inner.month; }
  get day(): number { return this.#inner.day; }
  get hour(): number { return this.#inner.hour; }
  get minute(): number { return this.#inner.minute; }
  get second(): number { return this.#inner.second; }
  get zoneName(): string { return this.#inner.timeZoneId; }
}
```

The `Temporal.ZonedDateTime` from the polyfill provides all the
zoned-datetime semantics we need. The wrapper class is the public
Mochi surface; users do not see the polyfill type directly.

### 15.3 Native dispatch

Once `Temporal` is global (Stage 4 + engine support), the polyfill
import becomes a no-op:

```typescript
const Temporal = (globalThis as { Temporal?: typeof import("@js-temporal/polyfill").Temporal }).Temporal
  ?? require("@js-temporal/polyfill").Temporal;
```

The runtime detection is one-shot at module load. Native Temporal
on Deno is already on by default; the polyfill kicks in on Node,
Bun, and most browsers as of mid-2026.

### 15.4 Monotonic clocks

For benchmark timing:

```typescript
export function monotonicMs(): number {
  return performance.now();
}

export function monotonicNs(): bigint {
  // performance.now() is fractional ms; convert to ns
  return BigInt(Math.round(performance.now() * 1_000_000));
}
```

`performance.now()` is the standard cross-runtime monotonic clock.
Node, Deno, Bun, and browsers all expose it. Resolution is
typically microsecond (Node, Deno) or 5-microsecond clamp
(browser, anti-Spectre).

### 15.5 Why not date-fns, Luxon, Moment?

- **date-fns**: a collection of pure functions over native `Date`.
  Functional but inherits Date's zone-blindness.
- **Luxon**: ergonomic wrapper over `Intl.DateTimeFormat`. Better
  than date-fns but predates Temporal; will be obsoleted by it.
- **Moment**: deprecated by its own maintainers in 2020.

Temporal is the right shape. We bridge via the polyfill until
native ships.

---

## 16. `mochi_runtime/ffi`

Mochi calls into native libraries via three runtime-specific FFI
back-ends. The public surface is uniform:

```typescript
export interface FfiHandle {
  call<R>(name: string, args: ReadonlyArray<unknown>): R;
  close(): void;
}

export interface FfiSignature {
  readonly name: string;
  readonly args: ReadonlyArray<FfiType>;
  readonly ret: FfiType;
}

export type FfiType =
  | "i8" | "i16" | "i32" | "i64"
  | "u8" | "u16" | "u32" | "u64"
  | "f32" | "f64"
  | "pointer" | "buffer" | "cstring" | "void";

export function dlopen(
  path: string,
  signatures: ReadonlyArray<FfiSignature>,
): FfiHandle {
  return _impl().dlopen(path, signatures);
}
```

The `_impl()` shim dispatches per runtime:

### 16.1 Node N-API back-end

Node uses **N-API** (or its newer face `node-addon-api`) for C
addons. Pure-data FFI (no addon source code) is harder; Node ships
no equivalent of Deno's `Deno.dlopen`. Two options:

1. **`node-ffi-napi`**: a community FFI binding using libffi. Works
   but has been unstable historically (the original `node-ffi` was
   abandoned around 2018; `node-ffi-napi` is the maintained fork).
2. **N-API addon**: hand-write a small addon that loads the library
   and exposes it. Build-time C++ compilation required; uses
   `node-gyp` or `prebuildify`.

The MEP-52 runtime ships an N-API addon under
`@mochi/ffi-native-node` (separate package, optional install). The
addon exposes `dlopen(path, signatures)` returning a JS-side
`FfiHandle`. Build artefacts are pre-built per
{Linux x86_64, Linux aarch64, macOS x86_64, macOS arm64, Windows x86_64}
and shipped via npm.

```typescript
// src/ffi/node_napi.ts
import { createRequire } from "node:module";

const require_ = createRequire(import.meta.url);

// dynamic require so the addon is loaded only when ffi is used
let _addon: { dlopen(path: string, sigs: unknown[]): FfiHandle } | null = null;

function loadAddon() {
  if (_addon === null) {
    _addon = require_("@mochi/ffi-native-node") as typeof _addon;
  }
  return _addon!;
}

export function dlopen(path: string, signatures: ReadonlyArray<FfiSignature>): FfiHandle {
  return loadAddon().dlopen(path, [...signatures]);
}
```

### 16.2 Deno FFI back-end

Deno has built-in FFI via `Deno.dlopen`:

```typescript
// src/ffi/deno_ffi.ts
export function dlopen(
  path: string,
  signatures: ReadonlyArray<FfiSignature>,
): FfiHandle {
  const symbols: Record<string, Deno.NativeFunctionDef> = {};
  for (const s of signatures) {
    symbols[s.name] = {
      parameters: s.args.map(toDenoType),
      result: toDenoType(s.ret),
    };
  }
  const lib = Deno.dlopen(path, symbols);
  return {
    call<R>(name: string, args: ReadonlyArray<unknown>): R {
      const fn = (lib.symbols as Record<string, (...args: unknown[]) => unknown>)[name];
      if (fn === undefined) throw new Error(`ffi: no such symbol ${name}`);
      return fn(...args) as R;
    },
    close(): void {
      lib.close();
    },
  };
}

function toDenoType(t: FfiType): Deno.NativeType {
  switch (t) {
    case "i8":  return "i8";
    case "i16": return "i16";
    case "i32": return "i32";
    case "i64": return "i64";
    case "u8":  return "u8";
    case "u16": return "u16";
    case "u32": return "u32";
    case "u64": return "u64";
    case "f32": return "f32";
    case "f64": return "f64";
    case "pointer": return "pointer";
    case "buffer":  return "buffer";
    case "cstring": return "pointer";  // CString = pointer to UTF-8 bytes
    case "void":    return "void";
    default: {
      const _exhaustive: never = t;
      throw new Error(`unreachable: ${String(_exhaustive)}`);
    }
  }
}
```

`Deno.dlopen` requires the `--allow-ffi` flag. The Deno runtime
performs all the marshalling; no extra build step.

### 16.3 Bun FFI back-end

Bun has FFI via the `bun:ffi` module:

```typescript
// src/ffi/bun_ffi.ts
import { dlopen as bunDlopen, FFIType, type ConvertFns } from "bun:ffi";

export function dlopen(
  path: string,
  signatures: ReadonlyArray<FfiSignature>,
): FfiHandle {
  const sym: Record<string, { args: FFIType[]; returns: FFIType }> = {};
  for (const s of signatures) {
    sym[s.name] = {
      args: s.args.map(toBunType),
      returns: toBunType(s.ret),
    };
  }
  const { symbols, close } = bunDlopen(path, sym);
  return {
    call<R>(name: string, args: ReadonlyArray<unknown>): R {
      const fn = (symbols as Record<string, (...args: unknown[]) => unknown>)[name];
      if (fn === undefined) throw new Error(`ffi: no such symbol ${name}`);
      return fn(...args) as R;
    },
    close,
  };
}

function toBunType(t: FfiType): FFIType {
  switch (t) {
    case "i8":  return FFIType.i8;
    case "i16": return FFIType.i16;
    case "i32": return FFIType.i32;
    case "i64": return FFIType.i64;
    case "u8":  return FFIType.u8;
    case "u16": return FFIType.u16;
    case "u32": return FFIType.u32;
    case "u64": return FFIType.u64;
    case "f32": return FFIType.f32;
    case "f64": return FFIType.f64;
    case "pointer": return FFIType.pointer;
    case "buffer":  return FFIType.ptr;
    case "cstring": return FFIType.cstring;
    case "void":    return FFIType.void;
    default: {
      const _exhaustive: never = t;
      throw new Error(`unreachable: ${String(_exhaustive)}`);
    }
  }
}
```

Bun's `bun:ffi` uses `tinycc` for JIT-compiled wrappers; the FFI
call overhead is among the lowest of any JS runtime (sub-100ns
for simple int/int signatures).

### 16.4 Browser back-end

Browsers have no FFI to native libraries (and would not pass any
security review if they did). The browser path throws:

```typescript
// src/ffi/browser.ts (if it existed; we exclude it via tsconfig)
export function dlopen(): never {
  throw new Error("browser: FFI is not supported in browser runtime");
}
```

In practice, the conditional-exports map omits `./ffi` for the
browser variant, so `import "@mochi/runtime/ffi"` is a module-not-
found at bundle time.

### 16.5 Common gotchas

- **64-bit integers**: `i64` / `u64` round-trip as `bigint`. None
  of the three FFI back-ends silently truncate, but the Mochi user
  must annotate accordingly.
- **Strings**: C strings (NUL-terminated UTF-8) marshal via
  `TextEncoder.encode(s + "\0")` to `Uint8Array` and pass as
  `buffer`. The return side reads a `pointer`, walks until NUL,
  and `TextDecoder.decode`s.
- **Callbacks**: Deno supports JS-to-C callbacks via
  `Deno.UnsafeCallback`. Bun supports them via `JSCallback`. Node
  N-API addon needs custom wrapping. The MEP-52 runtime exposes
  `wrapCallback(fn, sig)` as a uniform surface; the implementations
  differ underneath.
- **Memory management**: `Uint8Array` lifetimes are managed by JS
  GC. If the C side retains a pointer beyond the JS call, the
  buffer may be collected; the user must keep a reference (or use
  `Bun.ptr` / `Deno.UnsafePointer.of` to get a stable address).

---

## 17. `mochi_runtime/_internal/runtime_detect`

Cross-runtime detection used by `io/index.ts` and `ffi/index.ts`:

```typescript
export type Runtime = "node" | "deno" | "bun" | "browser" | "unknown";

declare const Deno: { version: { deno: string } } | undefined;
declare const Bun:  { version: string }            | undefined;

export function detect(): Runtime {
  if (typeof Deno !== "undefined" && typeof Deno.version?.deno === "string") {
    return "deno";
  }
  if (typeof Bun !== "undefined" && typeof Bun.version === "string") {
    return "bun";
  }
  if (typeof globalThis.process !== "undefined" &&
      typeof globalThis.process.versions?.node === "string") {
    return "node";
  }
  if (typeof globalThis.window !== "undefined" &&
      typeof globalThis.document !== "undefined") {
    return "browser";
  }
  return "unknown";
}

let _cached: Runtime | null = null;
export function current(): Runtime {
  if (_cached === null) _cached = detect();
  return _cached;
}
```

In practice the conditional-exports map in `package.json` picks the
right `index.ts` per runtime; this detection is a defensive
fallback. The order matters: Deno used to expose `window` for web
compatibility, so we check `Deno` before `window`.

---

## 18. Public surface re-exports

`src/index.ts`:

```typescript
export const RUNTIME_VERSION = "0.1.0";

export * from "./collections/index.js";
export * from "./io/index.js";
export * from "./agent/index.js";
export * from "./stream/index.js";
export * from "./query/index.js";
export * from "./datalog/index.js";
export * from "./ai/index.js";
export * from "./fetch/index.js";
export * from "./json_value/index.js";
export * from "./result/index.js";
export * from "./time/index.js";
// FFI is intentionally not re-exported from index; users import
// from "@mochi/runtime/ffi" explicitly. This keeps the browser
// bundle free of FFI types.
```

Mochi-emitted code uses sub-path imports:

```typescript
import { ok, err, type MochiResult } from "@mochi/runtime/result";
import { AsyncIterableQueue, CallableAgent } from "@mochi/runtime/agent";
import { println } from "@mochi/runtime/io";
```

Sub-path imports give bundlers per-feature tree-shaking even if
the user code somehow defeats `sideEffects: false`.

---

## 19. JSR (Deno) publishing path

In addition to npm, the runtime publishes to **JSR (jsr.io)**, the
Deno-native registry that supports both Deno and Node consumers.
JSR's manifest is `jsr.json`:

```json
{
  "name": "@mochi/runtime",
  "version": "0.1.0",
  "exports": {
    ".": "./src/index.ts",
    "./agent": "./src/agent/index.ts",
    "./io": "./src/io/index.ts",
    "./ffi": "./src/ffi/index.ts"
  },
  "publish": {
    "include": ["src/**/*.ts", "README.md", "LICENSE", "jsr.json"]
  }
}
```

`deno publish` validates the package (no `any` types in exports, no
slow types) and uploads to JSR. The Deno consumer imports from
`jsr:@mochi/runtime`, the Node consumer imports from
`npm:@mochi/runtime`. Same source, two registries.

JSR's "no slow types" rule (declared exports must not use type
inference that requires loading the implementation to determine the
type) is strict. The emitter ensures every public function has an
explicit return-type annotation; this is part of the codegen
contract.

---

## 20. Dependencies and build

Three layers of dependencies:

1. **Hard deps**: `@js-temporal/polyfill` (for `Temporal` until
   native). That is the entire hard list. Everything else is
   peer/optional.
2. **Peer deps**: `undici` for Node-only HTTP/2 (optional).
3. **Optional deps**: LLM provider SDKs.
4. **Build/dev-only**: `typescript`, `prettier`, `eslint`, `vitest`,
   `@types/node`, `bun-types`. Declared under `devDependencies`
   only.

The hard list is tiny by design. A Mochi user installing the
runtime gets ~150kb of node_modules (Temporal polyfill is the
bulk); optional deps push to 5+MB when all installed.

---

## 21. Test gates

The runtime's own test suite, run on every CI commit:

1. `tsc --build` (zero errors across all four projects).
2. `tsc --noEmit --strict --noUncheckedIndexedAccess --exactOptionalPropertyTypes` (sanity check on the source tree).
3. `eslint . --max-warnings 0` with `@typescript-eslint/recommended-type-checked`.
4. `prettier --check .` (formatter fixed point).
5. `vitest run` on Node 22 (canonical).
6. `deno test --allow-all` on Deno 2 (separate test entry).
7. `bun test` on Bun 1.1 (separate test entry).
8. Browser: Playwright + esbuild bundle + headless Chromium + assert console output.
9. `npm pack` + install from tarball into fresh dir + `node test-install.js` (smoke test).
10. `deno publish --dry-run` succeeds.

The four-runtime matrix gate (5-8) is the master gate. A test must
pass on all four; failure on any is a release blocker.

---

## 22. Versioning and stability

`@mochi/runtime` follows SemVer with independent versioning from
the Mochi compiler. Same reasoning as MEP-51 ([[mep-0051]]
research note 04 §23): library consumers want the option to
upgrade independently; compiler emits a runtime-pinned
`peerDependency` declaration.

Stability tiers:

- **Stable** (1.x+): everything in `src/index.ts`'s re-exports.
  No breaking changes without a major bump and 6-month deprecation
  window.
- **Provisional** (0.x): subject to change. 0.1 is the first
  emit-able version; 1.0 is the "Mochi 1.0 compiler ships against
  this" point.
- **Internal**: anything under `_internal/`. No stability promise.

---

## 23. Module map summary

| Module                                | LOC budget | Hard deps                | Optional deps |
|---------------------------------------|-----------:|--------------------------|---------------|
| `src/index.ts`                        | 60         | -                        | -             |
| `src/collections/*`                   | 500        | -                        | -             |
| `src/io/index.ts` + adapters          | 600        | -                        | -             |
| `src/agent/*`                         | 900        | -                        | -             |
| `src/stream/*`                        | 700        | -                        | -             |
| `src/query/*`                         | 650        | -                        | -             |
| `src/datalog/*`                       | 1100       | -                        | -             |
| `src/ai/*`                            | 900        | -                        | openai, anthropic, google-ai |
| `src/fetch/*`                         | 350        | -                        | -             |
| `src/json_value/*`                    | 300        | -                        | -             |
| `src/result/*`                        | 250        | -                        | -             |
| `src/time/*`                          | 400        | `@js-temporal/polyfill`  | -             |
| `src/ffi/*` + per-runtime back-ends   | 1100       | -                        | `@mochi/ffi-native-node` |
| `src/_internal/*`                     | 800        | -                        | -             |
| **Total**                             | **~8610**  |                          |               |

Test suite (`tests/`) is roughly 1:1 with source, another ~8000
LOC. Total repo size at v0.1.0 lands around 16500 LOC.

---

## 24. CPython parallels (MEP-51 cross-reference)

For readers familiar with the MEP-51 Python runtime, this table
maps concept-by-concept:

| Concept                | MEP-51 (Python)           | MEP-52 (TypeScript)         |
|------------------------|---------------------------|------------------------------|
| Arbitrary-precision int| `int` (always)            | `bigint` (when needed)       |
| Mutable map            | `dict[K, V]`              | `Map<K, V>`                  |
| Insertion-ordered set  | `OrderedSet` (poly)       | `Set<T>` (native order)      |
| Record                 | `@dataclass(frozen=True, slots=True)` | `class` + readonly fields |
| Sum type               | `type Foo = A \| B \| C`  | `type Foo = A \| B \| C` (discriminated) |
| Result                 | `Ok[T] \| Err[E]`         | `Ok<T> \| Err<E>` (discriminated) |
| Agent mailbox          | `asyncio.Queue`           | `AsyncIterableQueue<T>` (custom) |
| Supervision            | `asyncio.TaskGroup`       | `AbortController` + manual loops |
| Stream                 | `AsyncIterator[T]`        | `AsyncIterable<T>`           |
| HTTP                   | `httpx.AsyncClient`       | platform `fetch`             |
| JSON union             | sealed dataclass variants | discriminated union          |
| Time                   | `datetime` + `zoneinfo`   | `Temporal` (polyfilled)      |
| FFI                    | `ctypes`                  | Per-runtime: N-API/Deno/Bun  |

Mochi's runtime layer is the **thinnest** layer in the language;
most heavy lifting lives in the codegen pipeline
([[05-codegen-design]]) and the host's stdlib. The TS column is
slightly fatter than the Python column because of the four-runtime
matrix; even so, ~8500 LOC for the entire runtime is small relative
to MEP-50 Kotlin (~12000 LOC including JVM-specific helpers) and
MEP-49 Swift (~10000 LOC).

---

## 25. Performance notes

The TypeScript target is not the performance flagship of Mochi (C
in MEP-45, JVM-with-Loom in MEP-47). Performance must not regress
visibly compared to hand-written TS.

**V8 / SpiderMonkey JIT**: both engines specialise hot functions
into native code. Mochi-emitted code uses monomorphic call sites
(one type per call site, post-monomorphisation), which is the
JIT's fast path. Polymorphic call sites (rare in Mochi-emitted
code) drop to inline-cache misses.

**Hidden classes**: V8 builds a hidden class per unique property
layout. Mochi records emit fields in fixed source order, so all
instances of a given record share one hidden class. Adding fields
post-construction (which `Object.freeze` prevents) would
transition the hidden class; we forbid it.

**Allocation rate**: Mochi's frozen-value semantics generate a lot
of short-lived allocations (map/set/list operations). V8's
generational GC handles this well (Scavenger for young gen, ~1%
overhead). Bun's GC is similar.

**Async overhead**: `Promise.withResolvers` allocates one Promise
plus the resolver pair (~120 bytes on V8 13.x). `AsyncIterableQueue`
`push` is ~50ns; `await q.get()` is ~200ns including the microtask
tick. Comparable to MEP-51 asyncio (~150ns / ~400ns) and MEP-50
Kotlin Channel (~80ns / ~250ns).

**`Map.get`**: ~40ns on a warm-cache 1k-entry map (V8 hash table
with quadratic probe). `Object[key]` is ~25ns but has the type-
safety issues `noPropertyAccessFromIndexSignature` catches.

**`switch (x.kind)`**: V8 specialises switch-over-literal-string-
discriminants into a jump table or interned-string comparison; no
slower than `if/else` chains.

---

## 26. Out of scope for this note

The following are referenced here but specified elsewhere:

- Codegen pipeline (lower / emit / prettier / tsc): see
  [[05-codegen-design]].
- Per-Mochi-type lowering rules: see [[06-type-lowering]].
- Build system (npm + tsc + tarball): see [[10-build-system]].
- Testing gates (vm3 byte-equal, tsc, eslint, prettier): see
  [[11-testing-gates]].
- Risks and v2 candidates: see [[12-risks-and-alternatives]].
- Mochi language surface: see [[01-language-surface]].

---

## 27. Open questions

Deferred to v2 or pending resolution; tracked in [[12-risks-and-alternatives]]:

1. **Temporal stage 4**: when Temporal lands stage 4 and ships in
   V8 and JavaScriptCore, the polyfill becomes opt-out. Track the
   2026-Q3 milestone.
2. **`AsyncIterator` helpers**: similar story; stage 3 -> 4 in
   2026. Once stage 4 the runtime stream helpers shrink to thin
   wrappers.
3. **Cloudflare Workers / Edge runtime support**: a fifth target.
   Likely fits under `default` in the conditional exports but
   needs validation. v2.
4. **WebAssembly target**: a Mochi-to-WASM-via-Binaryen path is a
   separate MEP. The TS runtime would not change.
5. **Bun-only optimisations**: `Bun.write`, `Bun.file`, `Bun.serve`
   are faster than the cross-runtime equivalents. The emitter
   could detect `bun` at lower time and emit Bun-specific code.
   v2 candidate.
6. **Browser SharedArrayBuffer / Workers**: agents could run in
   Web Workers with `SharedArrayBuffer`-backed mailboxes. Higher
   complexity; v2.
7. **Node N-API addon publish flow**: the `@mochi/ffi-native-node`
   package needs binary-per-platform publishing via prebuildify or
   Github Releases. Standard but multi-step. Documented in
   [[10-build-system]].
8. **JSR vs npm divergence**: JSR has stricter rules (no slow types,
   no any in exports); we live in the intersection but it adds
   constraints. Tracked.

---

## 28. Cross-MEP layout comparison

The `mochi_runtime` TS package compared to its siblings:

| MEP | Package name             | Backend           | Entry-point module     |
|-----|--------------------------|-------------------|------------------------|
| 45  | `libmochi_c`             | static + shared C | `mochi_runtime.h`      |
| 46  | `libmochi_erl`           | rebar3 / hex      | `mochi_runtime.beam`   |
| 47  | `libmochi_jvm`           | Maven Central     | `dev.mochi:runtime`    |
| 48  | `MochiRuntime` (.NET)    | NuGet             | `Mochi.Runtime`        |
| 49  | `MochiRuntime` (Swift)   | SwiftPM           | `MochiRuntime`         |
| 50  | `mochi-runtime` (KT)     | Maven Central     | `dev.mochi:runtime`    |
| 51  | `mochi-runtime` (Py)     | PyPI              | `mochi_runtime`        |
| 52  | `@mochi/runtime` (TS)    | npm + JSR         | `@mochi/runtime`       |

Public surface area roughly matches across all eight; module names
and types align where the host language permits. The TS variant is
unique in shipping to **two registries** (npm + JSR) for the same
source.

The shared lesson: the runtime is a thin layer; the codegen
pipeline carries the rest.

---

## 29. Summary

- `@mochi/runtime` is one npm package (also on JSR), ~8500 LOC,
  hard-dep `@js-temporal/polyfill` and a per-runtime adapter
  fanout (Node, Deno, Bun, browser).
- Sub-modules: `collections`, `io`, `agent`, `stream`, `query`,
  `datalog`, `ai`, `fetch`, `json_value`, `result`, `time`, `ffi`.
- Builds with `tsc --build` over four `tsconfig.X.json` projects.
- Publishes via `npm publish --provenance` (Sigstore + OIDC) and
  `deno publish` (JSR).
- TypeScript 5.6 + `--strict` + `--noUncheckedIndexedAccess` gates.
- Reuses platform where possible (`Set` already has insertion order,
  so no `OrderedSet` is strictly needed; Mochi shape wraps it);
  `Map` already has insertion order; `Promise.withResolvers` is
  ES2024-native; platform `fetch` works on all tier-1 runtimes.
- Adds polyfills only where platform is missing the Mochi semantic:
  code-point string helpers, `AsyncIterableQueue` (no native
  equivalent), `MochiResult` over exceptions, Temporal polyfill,
  FFI per-runtime dispatch.

The narrowness is intentional: a small runtime is one less moving
part across Mochi releases. The codegen pipeline ([[05-codegen-design]])
and per-type lowering ([[06-type-lowering]]) carry the rest.
