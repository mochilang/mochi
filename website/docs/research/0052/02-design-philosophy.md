---
title: "Design philosophy: why TypeScript, why ES2024 floor, why tsc --strict + --noUncheckedIndexedAccess dual gates, why AsyncIterableQueue + AbortController, why npm canonical with four-runtime conditional exports"
description: "The 'why' behind every load-bearing MEP-52 design choice. TypeScript 5.6 floor, ES2024 floor, tsc dual gates, AsyncIterableQueue + AbortController concurrency, npm canonical with conditional exports for Node/Deno/Bun/browser, frozen class records, MochiResult discriminated union, Sigstore provenance."
sidebar_position: 2
---

# Design philosophy: why TypeScript, why ES2024 floor, why tsc --strict + --noUncheckedIndexedAccess dual gates, why AsyncIterableQueue + AbortController, why npm canonical with four-runtime conditional exports

Author: research pass for MEP-52 (Mochi to TypeScript / JavaScript transpiler).
Date: 2026-05-23 (GMT+7).
Sources: companion notes [[01-language-surface]],
[[03-prior-art-transpilers]]; the TC39 finished-proposals list and ES2024
specification (ECMA-262 15th edition, ratified June 2024); the
TypeScript 5.6 release notes (Sept 2024), 5.7 (Nov 2024), 5.8 (Feb
2025); the Node.js 22 LTS announcement (April 2024) and 23 / 24
release notes; the Deno 2.0 release notes (Oct 2024) and Deno 2.1 / 2.2
follow-ups; the Bun 1.0 announcement (Sept 2023) and Bun 1.1 / 1.2
release notes; the npm CLI 10.x release notes; the V8 release blog
posts for v12.4 / 12.5 / 13.0; the SpiderMonkey, JavaScriptCore, and
Chakra (rest in peace) engine release notes; the WHATWG and W3C
specifications for AbortController, Web Streams, Web Workers,
structured clone; the Sigstore documentation and the npm Trusted
Publishing announcement (April 2024); the Astral team's release
posts for ruff and uv; the MEP-45 / MEP-46 / MEP-47 / MEP-48 / MEP-49 /
MEP-50 / MEP-51 design-philosophy notes whose structure this note
mirrors.

This note explains the load-bearing design choices behind MEP-52 and
the constraints they impose. It is the "why" companion to
[[01-language-surface]]'s "what" and [[05-codegen-design]]'s "how".

## 1. Why a TypeScript target

Mochi already has seven mature lowering targets: vm3 (the reference
tree-walker), MEP-45 (C, single-binary AOT), MEP-46 (BEAM, supervision
and hot reload), MEP-47 (JVM bytecode direct, Maven Central and Loom),
MEP-48 (.NET, NuGet and NativeAOT), MEP-49 (Swift, Apple platforms plus
Linux/Windows), MEP-50 (Kotlin source, JVM/Android/Native/JS/Wasm),
and MEP-51 (Python source, PyPI and Jupyter). Each target picks up an
ecosystem Mochi cannot reach from the others.

The JavaScript / TypeScript ecosystem is the second-largest unreached
pillar after Python, and arguably the most important for web-facing
applications. By 2025-Q4 npm hosted approximately 3.2 million
packages (most of them deduplicates and abandoned shells; the
"actively maintained, downloaded by 1000+ users / week" set is closer
to 80,000 packages, comparable in scale to PyPI's actively-maintained
core). The reason this matters for Mochi:

- **Web applications**: React, Vue, Angular, Svelte, Solid, Qwik,
  Astro. The web frontend is overwhelmingly JavaScript / TypeScript,
  and the major framework ecosystems are all TS-first. A Mochi-typed
  business-logic library that needs to run inside a React component
  has no choice but to expose TypeScript interop.
- **Web servers**: Express, Fastify, Hono, NestJS, Koa, Adonis. The
  Node.js server ecosystem is mature and the FastAPI-equivalent
  modern stack (Hono on Bun, or Fastify on Node) is the canonical
  "modern Node.js web service" since 2023.
- **Edge compute**: Cloudflare Workers, Vercel Edge Functions, Deno
  Deploy, Fastly Compute, Netlify Functions, AWS Lambda@Edge,
  Bun's Cloud (announced 2024). All run JavaScript / TypeScript on
  V8 isolates or equivalent. A Mochi program that needs to run on
  the edge has to compile to JS.
- **CLIs and dev tooling**: Vite, esbuild, Rollup, Webpack, Parcel,
  Turbopack, Bun's own bundler. The dev-tooling ecosystem is
  overwhelmingly JS; the few exceptions (Rust-written esbuild, SWC,
  Turbopack) still expose JS-facing APIs.
- **Mobile cross-platform**: React Native, Expo, NativeScript, Ionic.
  The cross-platform mobile space is dominated by JS-based frameworks
  for non-native-first development.
- **Desktop cross-platform**: Electron, Tauri (Rust + JS UI), Neutralino.
  Desktop apps written in JS are everywhere (VS Code, Slack, Discord,
  Figma desktop, 1Password). A Mochi-typed app that wants to embed in
  Electron must expose TS.
- **Notebook and interactive computing**: Observable notebooks (D3,
  Plot), Deno's Jupyter kernel (officially supported since April
  2024), VS Code's interactive TypeScript REPL. Browser-based
  interactive computing is a JavaScript story; Mochi compiling to JS
  drops Mochi into Observable and into Deno-Jupyter directly.
- **WebAssembly host**: While Mochi has multiple AOT-to-Wasm targets
  in concept (MEP-45 C-to-Wasm via Emscripten, MEP-47 JVM-to-Wasm via
  CheerpJ), the JS host is the universal Wasm runtime: every Wasm
  module needs a JS shim to bind to the DOM, fetch, and the rest of
  the Web Platform. A Mochi-to-TS target gives the shim native.

For Mochi to be a credible web language, JS interop is not optional.
The choices were: (a) emit JavaScript directly with type annotations
in JSDoc comments (the "Vanilla JS with JSDoc" approach), (b) emit
TypeScript source, (c) emit WebAssembly directly via the C target
plus a JS shim. Option (a) sacrifices the type-checker discipline
Mochi cares about (JSDoc-typed JS is type-checkable by tsc but the
narrowing is weaker and the IDE story is worse); option (c) is what
the C target already does for Wasm but does not give a Pythonic
debugger experience or natural Vite / Webpack integration; option
(b) is the path of least resistance and the only choice that
integrates naturally with the existing TS tooling.

Therefore MEP-52: Mochi compiles to idiomatic, typed TypeScript 5.6+
source, drops into an npm-managed `package.json` project, and produces
npm-publishable packages plus a JSR-publishable Deno-native variant
plus a browser bundle plus an esbuild-compatible source tree for
direct integration into existing Vite / Next.js / Astro / Remix /
Bun projects.

## 2. Why TypeScript over plain JavaScript

The first major decision was: do we emit `.ts` source (typed), or
`.js` source (untyped, optionally with JSDoc)?

The JavaScript-only path has three superficial attractions:

- **Lower toolchain barrier**: no `tsc` required, no `tsconfig.json`,
  no `@types/*` packages.
- **Closer to "the language users actually write" in many domains**:
  most npm packages still ship as untyped JS plus separate `@types`
  packages.
- **Smaller emitted code size**: no type annotations in output means
  smaller files; bundlers strip types anyway, but the un-bundled
  source is smaller.

These attractions evaporate under scrutiny:

- **The lower toolchain barrier is illusory.** Anyone serious about
  JavaScript development in 2026 has `tsc` installed (it ships with
  the official TS extension for VS Code; Cursor and other AI-first
  editors install it by default; even Vite's default project template
  includes it). The barrier is zero.
- **The "untyped JS plus @types" pattern is a legacy of the
  pre-TypeScript era.** Newer packages (React 18+, Express 5, Hono,
  tRPC, Drizzle, Prisma, Effect-TS, Zod, Valibot) ship native
  TypeScript with the type definitions bundled. The "JS plus
  @types" pattern is recognised as a code-smell in the 2025 TypeScript
  community.
- **The smaller-code argument backfires.** Mochi's type discipline is
  the load-bearing reason to use Mochi at all. Emitting JS without
  types means the consumer cannot use Mochi-emitted code with full
  type-checker confidence; the IDE autocomplete is worse; the
  consumer of a Mochi library must trust the documentation or read
  the source. Emitting TS means every Mochi-defined type is a TS
  type that flows into the consumer's type checker.

The deeper argument: **Mochi's whole pitch is "typed business logic
with multi-target deployment"**. Emitting untyped JS sacrifices the
"typed" half on the JS target. If we are going to compromise the
types, we may as well ship a different language. Therefore: TypeScript
source, always.

JSDoc-typed JavaScript was considered as a middle ground: emit `.js`
files with `/** @type {...} */` annotations that tsc parses under
`--allowJs --checkJs`. This works (the TypeScript team officially
supports JSDoc as a type-source under `--allowJs`), but the type
annotations are verbose, the narrowing is weaker (tsc cannot narrow
through JSDoc as effectively as through native TS syntax), and the
generic-parameter syntax in JSDoc is hideous (`@template T extends
{name: string}` versus TS's `<T extends {name: string}>`). The
TypeScript team's own internal tooling moved off JSDoc-typed JS to
native TS in 2023; we follow.

The dual-output strategy (resolved in the shared-decisions anchor §1):

- Default `mochi build --target=typescript-source` emits `.ts` files.
  The library author or application author runs `tsc` locally to
  produce `.js` for runtime use.
- `mochi build --target=npm-package` runs `tsc` internally and
  produces a publishable npm package with both `.ts` source (for
  source maps and for downstream consumers who want to type-check
  against the source) and `.js` dist (for runtime).

This dual output covers the two consumer cohorts:

- **Application authors** who use Vite / Next.js / Astro / Remix /
  Bun's bundler want `.ts` source so they can integrate Mochi-emitted
  code into their build pipeline (with hot reload, type checking,
  and source maps).
- **Library authors** who publish to npm want `.js` dist so consumers
  don't need to compile, plus `.d.ts` so consumers have types. The
  TypeScript source is also shipped (for source maps, and for
  consumers that compile from source for their own optimisation
  needs like `swc` or `esbuild`).

## 3. Why TypeScript 5.6 as the floor

TypeScript 5.6.0 shipped on 2024-09-09 and is the first release with
several language and tooling features that materially simplify Mochi's
codegen:

- **`--noUncheckedSideEffectImports`**. Catches `import "./foo.ts"`
  where the file does not exist or has no side effects. Prior versions
  silently accepted dead imports. Critical for Mochi-generated code
  that imports `mochi_runtime` modules: a typo in a runtime import
  used to be detected only at runtime; now it is caught by tsc.
- **`--rewriteRelativeImportExtensions`**. Lets Mochi-emitted `.ts`
  source import `./foo.ts` and have tsc rewrite the import to
  `./foo.js` in dist. Before TS 5.6, the canonical pattern was to
  write `./foo.js` in the import even when the source file was
  `./foo.ts` (the "Deno-style import"), which confused IDE
  navigation. TS 5.6's rewrite flag is critical for the dual-source
  / dual-dist build.
- **Iterator method types**. The standard library now has typings
  for `Iterator.from`, `Iterator.prototype.map`, `.filter`, `.take`,
  `.drop`, `.flatMap`, `.toArray`. Mochi's query DSL lowering uses
  these pervasively; without typings the emitted code would fail
  type-check.
- **Region-aware narrowing for `using` declarations**. Lets `using x
  = ...` narrow `x`'s type after the disposable is acquired. This
  matters for Mochi's `defer` block (which lowers to `using`-managed
  cleanup).

TypeScript 5.5 (June 2024) is *not* the floor because it lacks the
import-extension rewrite. TypeScript 5.4 (March 2024) lacks the
`Iterator.from` typings and the `--noUncheckedSideEffectImports`
flag.

TypeScript 5.7 (Nov 2024) brings the `Path Rewriting for Relative Paths`
fix for the corner cases TS 5.6 missed (specifically, the rewrite
under `--module nodenext`). TypeScript 5.8 (Feb 2025) brings deferred
decorator metadata and improved inference for `satisfies`. Both are
acceptable upper bounds.

CPython-style decisions on lower bounds: TS 5.0 (March 2023) is the
PEP-695-equivalent breakpoint (TS 5.0 introduced the new decorator
syntax aligned with TC39 Stage-3 decorators). TS 4.x is firmly
out of scope. TS 5.0 through 5.5 are also out of scope because of
the import-rewrite gap.

The version pinning policy: Mochi v1 generates code targeting TS
5.6.3 (the latest 5.6.x patch as of 2025-Q4). The `tsconfig.json`
generated by `mochi build --target=npm-package` declares
`"target": "ES2024"` and `"module": "NodeNext"` (or
`"module": "ESNext"` for browser builds). The user's `package.json`
declares `typescript` as a dev-dependency pinned to `^5.6.0`,
allowing minor and patch upgrades but not jumping to 5.7+.

For users who want to use a newer TS version, the `--ts-version`
flag overrides the pin. CI runs a rolling secondary gate against TS
5.7, 5.8, and the latest 5.9 / 6.0 nightly to catch breakage early.

## 4. Why ECMAScript 2024 as the floor

ECMAScript 2024 (15th edition of ECMA-262, ratified June 2024 by the
TC39 General Assembly) introduces several features that directly
simplify Mochi's codegen:

- **`Promise.withResolvers()`**. Returns `{ promise, resolve,
  reject }` for a Promise constructed outside an executor function.
  Critical for the agent `call(req)` pattern (which needs to create
  a Promise in one place and resolve it from a different message
  handler later). Without `Promise.withResolvers`, the canonical
  pattern is:
  ```typescript
  let resolveOuter: (v: T) => void;
  const promise = new Promise<T>((resolve) => {
    resolveOuter = resolve;
  });
  // ... later, somewhere else
  resolveOuter(value);
  ```
  The "declare outside, capture in executor" pattern is awkward,
  needs a non-null assertion (`resolveOuter!`), and tsc complains
  about "Variable 'resolveOuter' is used before being assigned"
  under `--strict`. `Promise.withResolvers()` collapses to:
  ```typescript
  const { promise, resolve } = Promise.withResolvers<T>();
  // ... later
  resolve(value);
  ```
  Clean. The agent mailbox uses this pattern N times per message.

- **`Set` methods**: `union`, `intersection`, `difference`,
  `symmetricDifference`, `isSubsetOf`, `isSupersetOf`,
  `isDisjointFrom`. Maps directly to Mochi set operations. Before
  ES2024, the canonical implementation was a runtime helper like:
  ```typescript
  function union<T>(a: Set<T>, b: Set<T>): Set<T> {
    const out = new Set(a);
    for (const x of b) out.add(x);
    return out;
  }
  ```
  ES2024 makes these built-in methods, removing the runtime helper.

- **`Object.groupBy`, `Map.groupBy`**. Built-in group-by that returns
  `Record<K, V[]>` or `Map<K, V[]>` respectively. Maps directly to
  Mochi's `group by` clause.

- **`Iterator.from()` + iterator helpers**. The static `Iterator.from(iterable)`
  lifts any iterable into an iterator object with chainable methods
  (`map`, `filter`, `take`, `drop`, `flatMap`, `reduce`, `toArray`,
  `some`, `every`, `find`, `forEach`). Maps directly to Mochi's
  query DSL.

- **`Symbol.dispose`, `Symbol.asyncDispose`, `using` declarations**.
  Stage-3 in TC39 since 2023, shipped in ES2024 (Stage 4). The
  `using x = resource()` declaration calls `x[Symbol.dispose]()`
  automatically at end of scope, equivalent to Python's `with`
  statement or C#'s `using` statement. Maps directly to Mochi's
  `defer` block.

ECMAScript 2022 (the previous "modern" floor) lacks
`Promise.withResolvers`, the Set methods, `Object.groupBy`, and
iterator helpers. The polyfills for all four are non-trivial:
`Promise.withResolvers` is one-line but pollutes the global Promise;
the Set methods need to be added via `Set.prototype.xxx = function()
{...}` which conflicts with future native versions; iterator helpers
need a full library (`core-js/proposals/iterator-helpers`). Pinning
ES2024 instead means the emitter assumes these natively, with
runtime polyfills only for older browser cohorts (Chrome 119-123,
Firefox 121-124, Safari 17.0-17.3). The April-2024 browser cohort
(Chrome 124, Firefox 125, Safari 17.4) is the floor; older browsers
need polyfills from the runtime.

ECMAScript 2025 is the upper bound. ES2025 features still in
Stage-3 as of 2026-Q1 (the relevant ones for Mochi):

- **`Promise.try`**. Wraps a function call in a Promise that catches
  sync exceptions. Useful but not critical (Mochi's error story uses
  `Result<T, E>` not Promises-with-throws).
- **`RegExp.escape`**. Escapes regex special characters. Critical
  for safe regex construction from user-supplied strings.
- **`Math.sumPrecise`** (Stage-3, TBD ES2025 or ES2026). Kahan
  summation for `number` arrays. Mochi numeric kernel could use it.
- **Async iterator helpers** (Stage-3 as of 2024-Q4). Maps directly
  to Mochi's `AsyncIterable<T>` chain. The runtime polyfills until
  natively available.

These ES2025 features are not relied on by the emitter; the runtime
polyfills any usage. Once a runtime ships them natively, the
polyfill becomes a no-op.

The decision to floor at ES2024 rather than ES2022 trades off two
things: (a) we lose the ability to run on older Node (Node 18 LTS,
which still lacks `Promise.withResolvers` and the ES2024 set methods)
and older browsers (the pre-April-2024 cohort); (b) we gain four
major language features that materially simplify generated code.
The user-base cost is small (Node 22 LTS is the recommended Node
version since October 2024; Node 18 reaches EOL April 2025 anyway;
Node 20 reaches EOL April 2026); the codegen win is large.

## 5. Why `tsc --strict` AND `tsc --noUncheckedIndexedAccess` as dual gates

TypeScript's type system has many opt-in strictness flags. The
relevant ones for Mochi:

- **`--strict`**. A meta-flag enabling: `--strictNullChecks` (T |
  null is not assignable to T), `--strictFunctionTypes` (contravariant
  function parameters), `--strictBindCallApply` (typed
  `bind`/`call`/`apply`), `--noImplicitAny` (every parameter must
  have a type), `--noImplicitThis` (every `this` reference must have
  a type), `--useUnknownInCatchVariables` (catch clause defaults to
  `unknown`, not `any`), `--alwaysStrict` (emit `"use strict"`),
  `--strictPropertyInitialization` (class properties must be
  initialised in constructor).
- **`--noUncheckedIndexedAccess`**. Makes `arr[i]` return `T |
  undefined` instead of `T`. Makes `map.get(k)` return `T | undefined`
  always (which it already did, but the flag makes the type checker
  refuse to assume `T` from `arr[i]` either).
- **`--exactOptionalPropertyTypes`**. Distinguishes `field?: T` (the
  field may be absent) from `field: T | undefined` (the field is
  present but undefined). Forbids assigning `undefined` to a `field?:
  T` declaration.
- **`--noImplicitOverride`**. Requires `override` keyword on methods
  that override a parent class method.
- **`--noFallthroughCasesInSwitch`**. Forbids switch case fallthrough
  (each case must end with `break`, `return`, or `throw`).
- **`--noPropertyAccessFromIndexSignature`**. Forbids dot-notation
  property access on objects with index signatures (only bracket
  access allowed).
- **`--noUnusedLocals`**. Flags unused local variables.
- **`--noUnusedParameters`**. Flags unused function parameters.
- **`--noImplicitReturns`**. Requires explicit return on every path
  of a non-void function.

The decision: Mochi-emitted code must pass **`--strict`,
`--noUncheckedIndexedAccess`, `--exactOptionalPropertyTypes`,
`--noImplicitOverride`, `--noFallthroughCasesInSwitch`,
`--noPropertyAccessFromIndexSignature`, `--noImplicitReturns`**.
All seven flags are required.

The reasoning for `--noUncheckedIndexedAccess` deserves its own
section: this flag is the single most load-bearing TypeScript
strictness flag for Mochi's lowering, because Mochi's array
semantics are bounds-checked (`xs[i]` is a runtime panic if `i` is
out of range) and the bounds check must surface in the type system.
Without the flag, `xs[0]` returns `T`, and tsc happily accepts
`xs[0].name` even when `xs` could be empty; with the flag, `xs[0]`
returns `T | undefined`, and tsc requires `xs[0]?.name` or an
explicit narrowing. The Mochi emitter inserts the narrowing as a
guard:

```typescript
const elem: T | undefined = xs[0];
if (elem === undefined) {
  throw new MochiBoundsError("xs is empty");
}
// elem is now narrowed to T
```

This pattern is mechanical and the emitter generates it
unconditionally. The cost is one branch per indexed access; for hot
loops the runtime can compile the bounds check away with V8's branch
prediction (modern V8 hoists invariant bounds checks out of loops).

The reasoning for `--strict` is uncontroversial: it is the
TypeScript community standard since TS 4.0 (2020). Every modern TS
project uses it. Every Mochi-emitted file passes it by construction.

The reasoning for `--exactOptionalPropertyTypes` is the
`undefined`-versus-absent distinction. Without it, `field?: T` and
`field: T | undefined` are interchangeable, which conflates "the
field was not provided" with "the field was explicitly set to
undefined". For Mochi's records, every field is required (or has a
default); the emitter never emits `field?:` declarations, and the
flag is on for safety.

The reasoning for `--noImplicitOverride`: Mochi has no class
inheritance for user code, so no `override` is needed in user-facing
emitted code. The flag matters only for internal runtime classes
(e.g., `MochiBoundsError extends Error` requires `override
toString()` etc.); the emitter sets `override` correctly there.

The reasoning for `--noFallthroughCasesInSwitch`: Mochi's `match`
statement lowers to a `switch (x.kind)` with each case ending in
`return` or `break`. The flag catches accidental fallthrough that
would silently change semantics.

The reasoning for `--noPropertyAccessFromIndexSignature`:
distinguishes typed object access from index-signature access. For
records, the emitter uses dot notation (`person.name`); for maps,
the emitter uses `map.get(key)`. Mixing the two (using `map.foo`
where `map: Map<string, T>`) is a code smell that the flag prevents.

ESLint dual-gate: in addition to tsc, the build pipeline runs ESLint
with `@typescript-eslint/recommended-type-checked` (the strongest
preset, which requires type information from tsc). Rules of note:

- **`@typescript-eslint/no-floating-promises`**. Any `Promise<T>`
  return value that is not awaited or assigned is a lint error. The
  emitter explicitly uses `void` operator to mark fire-and-forget
  promises (`void f()`).
- **`@typescript-eslint/no-explicit-any`**. Forbids the `any` type.
  The emitter never emits `any`.
- **`@typescript-eslint/strict-boolean-expressions`**. Forbids
  truthy/falsy checks on nullable values; requires explicit
  comparison. The emitter never emits `if (xs)` for nullable arrays;
  always `if (xs !== null && xs.length > 0)`.
- **`@typescript-eslint/no-unsafe-*`** family. Catches operations on
  `any`-typed values.
- **`@typescript-eslint/switch-exhaustiveness-check`**. Requires
  every union switch to be exhaustive. Backs up the `never` trick at
  the lint level.

The `prettier --check` pass is a third gate: the formatter checks
that the code is in canonical format. Mochi-generated code is run
through `prettier 3.x` before emit, so the check passes by
construction; the gate catches drift if a downstream tool reformats
the file.

The combination of `tsc --strict --noUncheckedIndexedAccess` plus
ESLint typed-check plus `prettier --check` is the **triple
type-checker gate**. All three must pass for Mochi emit to be
accepted. This is stricter than 99% of npm packages today; the
strictness is the Mochi value proposition.

## 6. Why bigint default for `int`, with monomorphisation to number

Mochi's `int` is documented as 64-bit signed (2^63 - 1 maximum). JS's
`number` type has 53 bits of integer precision (2^53 - 1 maximum
safe integer); values beyond that round silently. JS's `bigint`
type is arbitrary precision (since ES2020). The two cannot be mixed:
`1n + 1` throws `TypeError`.

The default choice: emit `bigint` everywhere for Mochi `int`. The
reasoning:

- **Correctness**: a Mochi program that silently corrupts arithmetic
  because of i53 overflow is a worse failure mode than a slow Mochi
  program. `bigint` arithmetic is correct for all 64-bit (and
  larger) integer values.
- **Surfaces the cost honestly**: `bigint` operations are 5-20x
  slower than `number` operations in V8 12.4. If the user cares
  about performance, the slowness is visible in profiling; the user
  can opt into monomorphisation by annotating the IR or by adding
  `--prefer-number` to the build.
- **Type-checker enforces no mixing**: `bigint` and `number` are
  distinct types in TS. The Mochi emitter cannot accidentally
  produce mixed-mode arithmetic because tsc would refuse it.

The monomorphisation pass (in [[05-codegen-design]] §8) attempts to
prove that a Mochi `int` value (and all operations on it) stay
within [-(2^53-1), 2^53-1]. If proven, the emitted type is `number`,
and operations are:

- `a + b` -> `a + b` (with overflow check if `--strict-int`)
- `a - b` -> `a - b`
- `a * b` -> `a * b` (with overflow check if `--strict-int`)
- `a / b` -> `Math.trunc(a / b)` (truncated integer division)
- `a % b` -> `((a % b) + b) % b` (floor remainder)

If not proven, the emitted type is `bigint`, and operations are:

- `a + b` -> `a + b` (BigInt addition)
- `a - b` -> `a - b`
- `a * b` -> `a * b`
- `a / b` -> `a / b` (BigInt division is truncated by default)
- `a % b` -> `((a % b) + b) % b` (floor remainder)

The pass is conservative: when in doubt, emit `bigint`. The default
target for an unannotated Mochi `int` value is `bigint`. The
`--prefer-number` flag inverts the default (use `number` everywhere,
emit a runtime overflow check on every arithmetic op); off by
default, on for builds that opt into the audit profile.

The cost trade-off:

- `bigint` arithmetic in V8 12.4: addition / subtraction ~5x slower
  than `number`; multiplication ~10x slower; division ~20x slower.
- `bigint` value storage in V8: heap-allocated, 32 bytes per value
  (for small values). `number` is 8 bytes inline (SMI tagged in V8).
- `bigint` array storage: heap-allocated boxed values. `Array<bigint>`
  is ~4x larger in memory than `Array<number>`.
- `bigint` JSON: not serialisable by default (`JSON.stringify(1n)`
  throws). The Mochi runtime serialiser converts `bigint` to string
  on JSON output and reconstructs on input.

For Mochi programs that process large integer data (cryptography,
arbitrary-precision math, large-key hashing), the `bigint` default
is the only correct choice. For Mochi programs that do small-int
arithmetic in hot loops (parsing, indexing, counters), the
monomorphisation to `number` is the win. The pass is heuristic-driven
but reliable for the common cases ([[06-type-lowering]] §5).

The string-encoding analogue is even more dramatic: JavaScript's
UTF-16 internal storage forces a code-unit-to-code-point walk on
every character access for non-BMP strings. The cost is paid only
when the string contains astral-plane characters (emoji, ancient
scripts, mathematical symbols beyond U+FFFF). For ASCII-heavy text
the cost is zero (V8 optimises the spread idiom for ASCII strings).

This is the area where the TypeScript target is *worst* among the
eight backends. Python's PEP 393 variable-width gives O(1) code-
point indexing; Swift 5.7+'s String uses grapheme clusters by default
(slower but semantically richer); Rust's `String` is UTF-8 with
explicit code-point iteration; Mochi's string semantic
(UTF-8-conceptual, indexed by code point) requires a runtime walk on
JS. The cost is amortised across the string lifetime via the spread
cache (each `mochiStrIndex(s, i)` call memoises the code-point array
on a hidden weakmap-keyed property), but the worst-case cost is
O(n) per access.

For text-heavy workloads, the user can opt into
`--ts-string-cache=true` (default true) which guarantees O(1)
amortised access at the cost of O(n) extra memory per string. For
memory-constrained environments, `--ts-string-cache=false` accepts
the per-access O(n) cost.

## 7. Why frozen-class records over Zod schemas, plain objects, immer, or io-ts

Mochi records (`type Book { title: string, pages: int }`) need a
TypeScript representation that:

1. Carries field types (compile-time).
2. Carries field types (runtime, for FFI / JSON-with-validation).
3. Is immutable by default (matches Mochi value semantics).
4. Is constructable with named arguments (matches Mochi `Book { title:
   "X", pages: 10 }`).
5. Supports value-equality (`mochiEqual(a, b)` is structural).
6. Serialises to JSON cleanly (the canonical web data format).
7. Is fast (allocation, access, comparison).
8. Plays nicely with structuredClone / postMessage (so records can
   cross worker boundaries).

The candidates considered:

### 7.1 Plain object literals + TypeScript `interface`

```typescript
interface Book {
  title: string;
  pages: number;
}

const b: Book = { title: "X", pages: 10 };
```

Pros: lightest weight, fastest construction, smallest emit size,
JSON-native, structured-clone-native.

Cons: mutable by default (no `readonly` on the value); no constructor
function; no `instanceof` test (TypeScript structural typing makes
this confusing); the runtime cannot tell a `Book` from any other
object with the same fields.

For Mochi this is rejected because of mutability. We want runtime
freezing.

### 7.2 Plain object literals + `readonly` modifiers + `Object.freeze`

```typescript
interface Book {
  readonly title: string;
  readonly pages: number;
}

const b: Book = Object.freeze({ title: "X", pages: 10 });
```

Pros: same as plain objects, plus runtime immutability via
`Object.freeze`.

Cons: still no `instanceof` test; no static factory; no clean way to
attach methods to the type.

For Mochi this is close, but we want methods (Mochi `type Circle {
fun area() ... }` requires a place to put the method). Falls short.

### 7.3 ES2022 class with `readonly` fields + `Object.freeze` (the chosen option)

```typescript
class Book {
  readonly title: string;
  readonly pages: number;

  constructor(title: string, pages: number) {
    this.title = title;
    this.pages = pages;
    Object.freeze(this);
  }

  static of(props: { title: string; pages: number }): Book {
    return new Book(props.title, props.pages);
  }
}

const b = Book.of({ title: "X", pages: 10 });
```

Pros:
- compile-time and runtime immutability;
- `instanceof Book` works for nominal type identity;
- methods attach naturally as instance methods;
- static factory `.of(props)` matches the Mochi `Book { title, pages
}` syntax;
- TypeScript narrows correctly;
- V8 generates hidden classes for the constructor, giving fast
  property access on repeated allocations.

Cons:
- one extra layer of indirection (class instance vs plain object);
- `JSON.stringify(b)` produces `{"title":"X","pages":10}` (clean) but
  `JSON.parse(s)` produces a plain object, not a `Book` instance;
  the user must explicitly re-instantiate via `Book.of(JSON.parse(s))`;
- `structuredClone(b)` works (preserves all own enumerable properties)
  but the cloned value is a plain object, not a Book; same
  re-instantiation problem.

The cons are acceptable trade-offs. The JSON / structured-clone
re-instantiation is handled by the `mochi_runtime/json` module which
provides typed parsers (`parseBook(s: string): MochiResult<Book,
string>`) that validate the shape and re-instantiate.

### 7.4 Zod schemas

```typescript
import { z } from "zod";

const BookSchema = z.object({
  title: z.string(),
  pages: z.number().int().nonnegative(),
}).readonly();

type Book = z.infer<typeof BookSchema>;
```

Pros: runtime validation; rich constraint vocabulary; widely adopted
in 2024-2026 TS community.

Cons: heavy runtime dependency (Zod is ~30KB minified); validation
overhead on every parse; the inferred `Book` type is a plain object,
not a class; field access uses dot notation but there is no
constructor function.

For Mochi this is rejected as the *default* but offered as an opt-in
under `--ts-record-style=zod`. Zod is the right choice when the
Mochi program is consuming data from external sources (HTTP requests,
JSON files); for internal data the class form is faster.

### 7.5 io-ts

```typescript
import * as t from "io-ts";

const Book = t.type({
  title: t.string,
  pages: t.number,
});

type Book = t.TypeOf<typeof Book>;
```

Pros: similar to Zod; integrates with fp-ts ecosystem.

Cons: smaller community than Zod (npm downloads 1.5M/week vs Zod's
12M/week as of 2025-Q4); fp-ts ecosystem is functional-programming-
heavy which conflicts with idiomatic TS; the inferred type is a
plain object.

Rejected for the same reasons as Zod, plus the smaller community.

### 7.6 immer

```typescript
import { produce } from "immer";

const b: Book = { title: "X", pages: 10 };
const b2 = produce(b, (draft) => {
  draft.pages = 20;
});
```

Pros: easy mutable-like syntax with immutable semantics; widely used
in React/Redux ecosystem.

Cons: runtime dependency; only useful when mutation-style code is
desired (which Mochi tries to avoid); does not provide construction
or validation.

Rejected as the default; the user can use immer if they want
mutation-style code on top of Mochi records. The `mochi_runtime`
provides equivalent helpers (`mochi_runtime/collections/updating`)
without the immer dep.

### 7.7 class-validator + class-transformer

```typescript
import { IsString, IsInt, Min } from "class-validator";

class Book {
  @IsString()
  title!: string;

  @IsInt()
  @Min(0)
  pages!: number;
}
```

Pros: decorator-based runtime validation; integrates with NestJS;
familiar to Java/C# developers.

Cons: requires `experimentalDecorators` + `emitDecoratorMetadata`
(the legacy decorator system, not TC39 decorators); the `!`
non-null assertion is ugly; instantiation requires
`plainToClass(Book, plain)`; the decorators do not enforce
immutability.

Rejected for the legacy-decorator dependency.

### 7.8 Effect Schema

```typescript
import { Schema } from "@effect/schema";

const Book = Schema.struct({
  title: Schema.string,
  pages: Schema.int(),
});
```

Pros: integrates with Effect-TS's broader functional ecosystem;
strong type inference.

Cons: Effect is a 200KB ecosystem; requires opt-in to the Effect
runtime model; the schema-as-type pattern is verbose.

Rejected as a default; offered as `--ts-record-style=effect` for
Effect-using projects.

### 7.9 Decision: class with `readonly` + `Object.freeze`

Option 7.3 is the chosen default. The reasoning:

- It satisfies all eight criteria from the top of this section.
- It uses only ECMAScript primitives (no runtime dependency).
- It is V8-friendly (hidden class generation, fast property access).
- It produces clean JSON output (no schema metadata leakage).
- It is the canonical TS record pattern (the official TypeScript
  Handbook uses it).

The factory pattern `.of(props)` is the canonical Mochi construction
form. The bare constructor `new Book(...)` is also emitted when the
call site is positional. Both produce identical instances.

## 8. Why AsyncIterableQueue + AbortController instead of RxJS, Web Streams, or EventEmitter

Mochi's agent and stream primitives need a TypeScript implementation
that:

1. Supports async pull (`for await (const msg of queue)`).
2. Supports backpressure (bounded queue with awaiting push).
3. Supports cancellation (parent abort propagates to children).
4. Supports request-reply (`call(req)` returns a reply Promise).
5. Has no third-party runtime dependency.
6. Is testable (mock-friendly, deterministic ordering).
7. Maps naturally to the cross-target story (matches MEP-46 BEAM
   mailboxes, MEP-49 Swift AsyncStream, MEP-50 Kotlin Channel, MEP-51
   Python asyncio.Queue).

The candidates considered:

### 8.1 RxJS Subject / BehaviorSubject / ReplaySubject

```typescript
import { Subject } from "rxjs";

const mailbox = new Subject<Message>();
mailbox.next(msg);  // send
mailbox.subscribe((msg) => handle(msg));  // receive
```

Pros: rich operator library (`map`, `filter`, `debounce`, `throttle`,
`merge`, `combineLatest`, etc.); mature; widely adopted in Angular.

Cons:
- 25KB runtime dependency (RxJS 7.x ESM bundle, tree-shaken);
- push-based by default (no built-in backpressure);
- the operator vocabulary is overkill for Mochi's needs;
- subscription leak hazard (forgotten subscriptions accumulate);
- harder to integrate with async/await (subscription-based, not
  Promise-based).

For Mochi this is rejected. The dep is too heavy; the operator
library is not what we need; the async/await mismatch is a problem.

### 8.2 Web Streams (`ReadableStream<T>` + `WritableStream<T>`)

```typescript
const { readable, writable } = new TransformStream<Message, Message>();
const writer = writable.getWriter();
await writer.write(msg);
const reader = readable.getReader();
const { value, done } = await reader.read();
```

Pros: WHATWG-standard; broad runtime support; backpressure built in;
async/await native.

Cons:
- heavyweight API (Reader / Writer / pipeTo / pipeThrough);
- harder to model an agent's `call(req)` pattern (Web Streams are
  unidirectional);
- the close/error semantics are tricky (need to cancel both reader
  and writer);
- typed surface (`ReadableStream<T>` lacks structured concurrency
  primitives);
- structured-clone serialisable, but the methods are not (so passing
  a `ReadableStream` to a Web Worker requires `transferable` flag
  and the original becomes unusable).

For Mochi this is rejected as the *default* mailbox but is used for
the `--target=stream-pipe` feature (where a Mochi `stream<T>`
interoperates with a Web Stream for upload/download).

### 8.3 EventEmitter (Node) / EventTarget (DOM)

```typescript
import { EventEmitter } from "node:events";

const mailbox = new EventEmitter();
mailbox.emit("message", msg);
mailbox.on("message", (msg) => handle(msg));
```

Pros: simple; familiar to Node developers; on-demand subscription.

Cons:
- not typed (event names are strings, payloads are `any`);
- no backpressure;
- no built-in async iteration;
- not cross-runtime (Node's `EventEmitter` and DOM's `EventTarget`
  have different APIs).

For Mochi this is rejected. The lack of typing is a deal-breaker.

### 8.4 Hand-rolled AsyncIterableQueue + AbortController (the chosen option)

```typescript
class AsyncIterableQueue<T> {
  private buffer: T[] = [];
  private waiters: Array<(v: IteratorResult<T>) => void> = [];
  private closed = false;

  push(value: T): void {
    const waiter = this.waiters.shift();
    if (waiter) waiter({ value, done: false });
    else this.buffer.push(value);
  }

  close(): void {
    this.closed = true;
    for (const w of this.waiters) w({ value: undefined, done: true });
    this.waiters = [];
  }

  [Symbol.asyncIterator](): AsyncIterator<T> {
    return {
      next: (): Promise<IteratorResult<T>> => {
        if (this.buffer.length > 0) {
          return Promise.resolve({ value: this.buffer.shift()!, done: false });
        }
        if (this.closed) return Promise.resolve({ value: undefined, done: true });
        const { promise, resolve } =
          Promise.withResolvers<IteratorResult<T>>();
        this.waiters.push(resolve);
        return promise;
      },
    };
  }
}
```

Pros:
- zero third-party dependency (only `Promise.withResolvers` from
  ES2024);
- typed surface (`T` is the message type);
- async/await native (`for await (const msg of queue)`);
- works with `AbortController` for cancellation (controller listener
  calls `queue.close()`);
- maps cleanly to the agent pattern (each agent has one queue);
- request-reply via `Promise.withResolvers` on the message payload;
- testable (the buffer and waiters are inspectable);
- cross-runtime (works on Node, Deno, Bun, browser; no Node-specific
  API).

Cons:
- no built-in backpressure (the unbounded variant is the default);
  the bounded variant adds backpressure but requires more code;
- the `waiters` array is FIFO but unsorted by priority (Mochi has no
  priority queue at the spec level, so this is fine);
- structured-clone-serialisable only via reconstruction (the queue
  object cannot cross worker boundaries directly).

The bounded variant for backpressure:

```typescript
class BoundedAsyncIterableQueue<T> {
  private buffer: T[] = [];
  private waiters: Array<(v: IteratorResult<T>) => void> = [];
  private pushers: Array<() => void> = [];
  private closed = false;

  constructor(private readonly capacity: number) {}

  async push(value: T): Promise<void> {
    if (this.buffer.length >= this.capacity) {
      const { promise, resolve } = Promise.withResolvers<void>();
      this.pushers.push(resolve);
      await promise;
      if (this.closed) throw new MochiQueueClosed();
    }
    const waiter = this.waiters.shift();
    if (waiter) waiter({ value, done: false });
    else this.buffer.push(value);
  }

  close(): void {
    this.closed = true;
    for (const w of this.waiters) w({ value: undefined, done: true });
    for (const p of this.pushers) p();
    this.waiters = [];
    this.pushers = [];
  }

  [Symbol.asyncIterator](): AsyncIterator<T> {
    return {
      next: (): Promise<IteratorResult<T>> => {
        if (this.buffer.length > 0) {
          const value = this.buffer.shift()!;
          // Wake one pusher if any
          const pusher = this.pushers.shift();
          if (pusher) pusher();
          return Promise.resolve({ value, done: false });
        }
        if (this.closed) return Promise.resolve({ value: undefined, done: true });
        const { promise, resolve } =
          Promise.withResolvers<IteratorResult<T>>();
        this.waiters.push(resolve);
        return promise;
      },
    };
  }
}
```

The bounded variant adds ~40 lines but gives full backpressure
semantics matching Kotlin's `Channel(N)` or Python's
`asyncio.Queue(maxsize=N)`.

Supervision via `AbortController`:

```typescript
const parent = new AbortController();

const child1 = new Agent1(parent.signal);
const child2 = new Agent2(parent.signal);
const child3 = new Agent3(parent.signal);

// On any child's failure, abort all siblings
child1.onError((e) => parent.abort(e));
child2.onError((e) => parent.abort(e));
child3.onError((e) => parent.abort(e));

// When parent aborts, children see signal.aborted and exit their loops
```

The `AbortSignal.abort()` static method (ES2022, in Node 17.4+, Deno
1.20+, Bun 1.0+) makes already-aborted signals easy to create for
testing. `AbortSignal.timeout(ms)` (ES2022) creates a signal that
auto-aborts after a timeout, useful for request timeouts. The
`AbortSignal.any([s1, s2, ...])` combinator (Node 20+, Deno 1.34+,
Chrome 116+, Firefox 124+) creates a signal that aborts when any of
its inputs abort, useful for composing supervision scopes.

The `AbortError` class (`new DOMException("aborted", "AbortError")`)
is the canonical signal-fired error. The Mochi runtime wraps it as
`MochiAborted` for cross-target consistency.

## 9. Why npm as canonical, with conditional exports for four runtimes

The TypeScript ecosystem has three major package managers in 2026:
npm (the default, bundled with Node), pnpm (faster, content-
addressable storage), and Bun's built-in package manager (faster
still, native binary install). Yarn (the historical alternative) has
declined to ~15% market share; Yarn 4 (Plug'n'Play) is still
maintained but not the canonical choice.

The choice: **npm as the canonical package manager**, with the
generated `package.json` working with pnpm and Bun's package manager
out of the box.

The reasoning for npm canonical:

- **Universal availability**: npm ships with every Node install.
  Users don't need to install anything extra.
- **Reference for `package.json` format**: npm defines the
  `package.json` spec; pnpm and Bun follow.
- **Sigstore Trusted Publishing**: npm 10.5+ supports `npm publish
  --provenance` with GitHub Actions OIDC, providing supply-chain
  security. pnpm and Bun do not currently have equivalent
  provenance pipelines.
- **`npm pack` for tarball validation**: `npm pack` produces a
  validatable `.tgz` for testing the published artefact before
  publishing.

The reasoning against pnpm canonical:

- pnpm requires installation (`brew install pnpm`, `npm install -g
  pnpm`, or `corepack enable`). The user-base cost is real (some
  CI environments don't have pnpm by default).
- pnpm's `pnpm-lock.yaml` is its own format; Mochi would have to
  emit two lock formats (npm + pnpm) for portability.

The reasoning against Bun canonical:

- Bun is fast but its install base is smaller (~10% of Node usage as
  of 2025-Q4). The user would have to install Bun separately.
- Bun's package manager is mostly compatible with npm but has
  edge-case differences (Bun's `bun.lockb` is binary; pnpm's
  `pnpm-lock.yaml` is text; npm's `package-lock.json` is JSON).
- Bun is supported as an alt-driver but not the canonical one.

The `package.json` `"exports"` field is the key cross-runtime
mechanism. The Mochi-generated `package.json` declares:

```json
{
  "name": "@org/mypkg",
  "version": "0.1.0",
  "type": "module",
  "engines": {
    "node": ">=22.0.0",
    "deno": ">=2.0.0",
    "bun": ">=1.1.0"
  },
  "exports": {
    ".": {
      "types": "./dist/index.d.ts",
      "deno": "./dist/deno/index.js",
      "bun": "./dist/bun/index.js",
      "browser": "./dist/browser/index.js",
      "node": "./dist/node/index.js",
      "default": "./dist/node/index.js"
    },
    "./io": {
      "types": "./dist/io.d.ts",
      "deno": "./dist/deno/io.js",
      "bun": "./dist/bun/io.js",
      "browser": "./dist/browser/io.js",
      "node": "./dist/node/io.js",
      "default": "./dist/node/io.js"
    }
  }
}
```

The conditional map is resolved at import time by each runtime:

- **Node 22+** matches `node` first (also matches `default` for older
  Node versions that don't recognise `node`).
- **Deno 2+** matches `deno` first. Deno also reads `package.json`
  natively in Deno 2 (an enormous improvement over Deno 1, which
  required `deno.json` separately).
- **Bun 1.1+** matches `bun` first.
- **Browsers** via `import` statements: the browser bundler (Vite,
  esbuild, Rollup) matches `browser` first.
- **Default** is the Node bundle for ambiguous cases.

The `types` condition is always first (so the type checker sees the
correct `.d.ts` regardless of which runtime is used).

The four-runtime build matrix (in [[10-build-system]] §3):

- `tsconfig.node.json` extends `tsconfig.base.json`, adds Node-
  specific lib (`["ES2024", "DOM", "WebWorker", "node"]`) and
  outputs to `dist/node/`.
- `tsconfig.deno.json` extends the base, adds Deno-specific lib and
  the `deno-types` reference, and outputs to `dist/deno/`.
- `tsconfig.bun.json` extends the base, includes Bun's `@types/bun`,
  outputs to `dist/bun/`.
- `tsconfig.browser.json` extends the base, removes Node lib,
  outputs to `dist/browser/`, and the `esbuild` post-step bundles
  to a single ESM file.

The shared `tsconfig.base.json`:

```json
{
  "compilerOptions": {
    "target": "ES2024",
    "module": "NodeNext",
    "moduleResolution": "NodeNext",
    "strict": true,
    "noUncheckedIndexedAccess": true,
    "exactOptionalPropertyTypes": true,
    "noImplicitOverride": true,
    "noFallthroughCasesInSwitch": true,
    "noPropertyAccessFromIndexSignature": true,
    "noImplicitReturns": true,
    "noUnusedLocals": true,
    "noUnusedParameters": true,
    "isolatedModules": true,
    "esModuleInterop": false,
    "forceConsistentCasingInFileNames": true,
    "skipLibCheck": false,
    "declaration": true,
    "declarationMap": true,
    "sourceMap": true,
    "rewriteRelativeImportExtensions": true,
    "noUncheckedSideEffectImports": true
  }
}
```

The `--isolatedModules` flag is critical: it ensures every file can
be transpiled independently (i.e., no cross-file type information is
needed at transpile time). This is what makes swc / esbuild / Bun's
transpiler interchangeable with tsc at the build step (tsc for type
checking, swc/esbuild for fast transpile-only).

The `--esModuleInterop` flag is **off** in our config. The classic
interop (`import * as fs from "fs"`) is what we use; the
"namespace import == default import" hack from `esModuleInterop=true`
introduces subtle bugs around named exports vs default exports.

The `--declaration` plus `--declarationMap` plus `--sourceMap` set
produces the full source-map chain: `.ts` source -> `.js` runtime
output, `.ts` source -> `.d.ts` types, `.d.ts` -> `.ts` source map
for the type-checker hover. Both the type-checker hover (in VS
Code) and the runtime debugger (Node `--enable-source-maps`, Chrome
DevTools) point to the original Mochi source through the chain.

JSR (jsr.io) is the Deno-native alternative to npm. JSR was launched
in March 2024 by the Deno team and accepts TypeScript source directly
(no `tsc` compilation needed at publish time; JSR compiles on import).
The Mochi build pipeline supports `deno publish` to JSR as a
secondary publication target. JSR matches the four-runtime semantic
(JSR packages have an `exports` field with conditional resolution),
and JSR packages can be consumed by npm (via `npm:jsr.io/@org/pkg`)
and by Bun (via `bun add jsr:@org/pkg`) too. The cross-registry
story is robust as of 2026-Q1.

## 10. Why hand-rolled `mochi_runtime` instead of depending on lodash, date-fns, neverthrow, ts-results, fp-ts, or Effect

Mochi-emitted TypeScript needs a small runtime library for things
the TS standard library doesn't cover:

- Bounds-checked array access helpers.
- Code-point-aware string operations.
- The `MochiResult<T, E>` discriminated union.
- The `AsyncIterableQueue<T>` for agents.
- The `mochiEqual(a, b)` structural-equality helper.
- The `mochiHash(a)` structural-hashing helper.
- The `mochi_runtime/io/print` formatter.
- The `mochi_runtime/datalog/Engine`.
- The `mochi_runtime/ai/call` LLM dispatcher.
- The `mochi_runtime/ffi/*` foreign-function bindings.

The choice: **a hand-rolled `mochi_runtime` npm package with zero
runtime dependencies**, vendored or imported as a peer dep.

The reasoning:

- **Supply-chain security**: every runtime dep is a vector. The
  Mochi value proposition is "trustable, typed business logic"; that
  trust is eroded by every transitive dependency. The
  `event-stream` incident (2018) and the `node-ipc` incident (2022)
  remain cautionary tales.
- **Bundle size**: tree-shakable hand-rolled helpers add ~5KB
  minified+gzip to a browser bundle. lodash adds 70KB; date-fns adds
  30KB tree-shaken; Effect adds 200KB+; fp-ts adds 50KB.
- **No version skew**: a single peer dep (`mochi_runtime`) has one
  version, locked to the emitter version. No transitive version
  conflicts.
- **No license drift**: every file in `mochi_runtime` is MIT licensed,
  the same as Mochi itself. No GPL surprise from a transitive dep.

The alternatives considered:

### 10.1 lodash / lodash-es

The most popular JS utility library (~50M downloads/week). Provides
deep-equality, deep-clone, group-by, partial, debounce, throttle,
etc.

Rejected: too heavy (70KB even tree-shaken in practice because
real-world bundles include a dozen lodash functions); legacy CommonJS
heritage causes import issues with strict ESM; the helpers are
generic enough to copy.

### 10.2 date-fns

A modular date / time library.

Rejected as a default; Mochi's `time` and `duration` types use
`Date` and a `MochiDuration` class internally. For ES2025+
`Temporal` will replace `Date`; the runtime wraps the necessary
helpers. Users who want full date-fns can add it themselves.

### 10.3 neverthrow

A small `Result<T, E>` library (~3KB minified).

Rejected: the Mochi runtime defines `MochiResult<T, E>` directly to
avoid a renamed type; the `Ok` / `Err` names are owned by
`mochi_runtime` and the user never imports from neverthrow. Adding
neverthrow as a dep would create a name conflict ("which `Result`?").

### 10.4 ts-results

Similar to neverthrow.

Rejected for the same reason as neverthrow.

### 10.5 fp-ts

A larger functional-programming library with `Either`, `Option`,
`Task`, `Reader`, etc.

Rejected: too heavy and too opinionated. fp-ts is the wrong style
for idiomatic typed business logic (it pulls users into a category-
theoretic mindset that conflicts with Mochi's pragmatic style).

### 10.6 Effect

A newer functional-programming ecosystem (200KB+) with rich
concurrency, dependency injection, and error-handling primitives.

Rejected as a default. Effect is impressive but heavy; it requires a
specific programming style; integrating with Effect would be a
choice that ties Mochi to the Effect ecosystem's lifecycle.

The Mochi `--ts-record-style=effect` flag lets users opt into Effect
integration on a per-project basis. The default is hand-rolled.

### 10.7 RxJS

Already discussed in §8. Rejected for the agent mailbox.

### 10.8 Decision: hand-rolled `mochi_runtime`

The cost of hand-rolling is ~2000 lines of TypeScript in
`mochi_runtime/src/`. The maintenance burden is real but bounded;
the per-release diff is small. The benefit (zero runtime deps, no
supply-chain risk, no bundle bloat, no version skew) is large.

The `mochi_runtime` package layout (see [[04-runtime]]):

```
mochi_runtime/
  package.json
  src/
    async/
      queue.ts            # AsyncIterableQueue, BoundedAsyncIterableQueue
      mutex.ts            # MochiMutex
      semaphore.ts        # MochiSemaphore
      event.ts            # MochiEvent
      condition.ts        # MochiCondition
    collections/
      list.ts             # appended, inserting, removing, mapped, filtered
      map.ts              # mochiMapEq, mochiMapKeys
      set.ts              # ES2024 polyfills, mochiSetEq
    errors/
      result.ts           # Ok, Err, MochiResult, unwrap, getOrNull
      thrown.ts           # MochiThrownError, MochiBoundsError, MochiQueueClosed
    io/
      print.ts            # mochiPrint, formatValue
      sleep.ts            # cross-runtime sleep
      stdin.ts            # cross-runtime stdin reading
    str/
      length.ts           # mochiStrLen (code-point length)
      index.ts            # mochiStrIndex (code-point indexing)
      compare.ts          # mochiStrCompare (code-point comparison)
    ai/
      call.ts             # provider-dispatching call helper
      stream.ts           # streaming generate helper
    ffi/
      napi.ts             # Node N-API loader
      deno.ts             # Deno.dlopen wrapper
      bun.ts              # bun:ffi wrapper
      wasm.ts             # WebAssembly loader
    datalog/
      engine.ts           # semi-naive bottom-up engine
      term.ts             # term ADT
    query/
      group-by.ts         # groupBy with order preservation
      join.ts             # hash-join helper
    eq/
      equal.ts            # mochiEqual structural equality
      hash.ts             # mochiHash structural hashing
  test/
    ... (parallel structure for tests)
```

The package is published as `@mochi/runtime` on npm (and as
`@mochi/runtime` on JSR for Deno). The Mochi-emitted code imports
from `@mochi/runtime/async/queue`, etc.; the conditional exports in
the runtime's `package.json` route to the right per-runtime entry.

## 11. Why MochiResult over thrown exceptions

JavaScript's culture is exception-based: every API throws on
failure, and `try/catch` is the canonical error-handling pattern.
TypeScript inherits this culture; `tsc` does not model thrown
exceptions in the type system (no `throws` clause on function
signatures).

Mochi has typed errors: `fun foo(): T throws E` says "this function
returns T or fails with E". The two error styles map awkwardly:

- **Option A: lower to thrown exceptions.** Every Mochi `throws E`
  function becomes a TS function that throws an `MochiThrownError`
  wrapping the `E` value. Callers `try/catch` and check the wrapped
  error.
- **Option B: lower to `MochiResult<T, E>`.** Every Mochi `throws E`
  function becomes a TS function returning `MochiResult<T, E>`.
  Callers `switch (r.kind)` to handle.

The choice: **Option B (Result-style)** for Mochi-internal code,
with conversion to exceptions only at the FFI boundary when calling
into JavaScript libraries that throw.

The reasoning:

- **Type-checker enforcement**: with Result-style, the type system
  forces the caller to handle the error path explicitly. With
  thrown-style, the type system says nothing about errors; the
  caller can silently ignore them.
- **Cross-target consistency**: MEP-45 (C), MEP-49 (Swift typed
  throws), MEP-50 (Kotlin MochiResult), MEP-51 (Python MochiResult)
  all use Result-style. MEP-52 matching is the cross-target win.
- **Exhaustiveness**: a `switch (r.kind)` on the `MochiResult` union
  with both `Ok` and `Err` cases is exhaustive (tsc enforces via
  the `never` trick); a `try/catch` is not exhaustive (the catch
  block catches any thrown value).
- **Async-await compatibility**: an `async function`'s return type
  is `Promise<T>` and a thrown error becomes a rejected Promise.
  This works but mixes the two error channels (Promise rejection +
  thrown error). With Result-style, the return type is
  `Promise<MochiResult<T, E>>` and the error channel is part of the
  type.

The cost of Result-style:

- Every Mochi-fallible call site has explicit `switch` or `unwrap`
  boilerplate, which is noisier than `try/catch`.
- Interop with thrown-throwing JS libraries requires a wrapper
  function (per [[01-language-surface]] §9.2).
- Stack traces are less natural: a `MochiResult.Err` does not carry
  a stack by default; the runtime wraps the underlying error with
  `cause` (ES2022) for stack preservation.

The cost is acceptable. The win is type-system-enforced error
handling, matching the cross-MEP pattern, and exhaustiveness checks
that `try/catch` cannot provide.

The exception/Result conversion at the FFI boundary is mechanical:

```typescript
// Mochi: extern fun parse(s: string): Json throws JsonError = "js:JSON.parse"
export function parse(s: string): MochiResult<Json, JsonError> {
  try {
    return Ok.of(wrapJson(JSON.parse(s)));
  } catch (e) {
    if (e instanceof SyntaxError) {
      return Err.of(new JsonError(e.message));
    }
    throw e;  // unexpected exception type, propagate
  }
}
```

The `try/catch` is the *only* place where Mochi-emitted code uses
`try/catch`. Internal Mochi code uses `match` on `MochiResult`.

## 12. The async coloring problem and Mochi's solution

JavaScript has "async coloring": async functions return `Promise<T>`
and require `await` to use; sync functions return `T` directly. A
sync function cannot call an async function and use its result
synchronously (no `await` in sync context). This forces all callers
of an async function to be async themselves, propagating up the
call graph.

Mochi has the same distinction (`fun` vs `async fun`). The mapping
is direct: Mochi `fun foo(): T` becomes TS `function foo(): T`,
Mochi `async fun foo(): T` becomes TS `async function foo(): Promise<T>`.

The complication: some operations are sync in some Mochi targets
and async in others. File I/O is sync on the Python target (via
`open(path).read()`), sync on the C target, sync on the JVM target;
on the TypeScript target, file I/O is async (Node's `fs/promises`
module). The cross-target story requires Mochi I/O operations to be
async at the language level so they map to async on every target.

For Mochi v1, the resolution is: **all I/O is async**. Mochi
programs that want to read a file declare `async fun main()` and
`await read_file(path)`. Sync Mochi code (pure computation) stays
sync. The async coloring matches every target.

The TypeScript backend uses `async function*` for streams,
`AsyncIterable<T>` for streaming, `Promise<T>` for one-shot async,
and ordinary `function` for sync. The `await` keyword propagates
correctly; tsc enforces the coloring via the type system.

## 13. Streams as AsyncIterable, not Observable

JavaScript has multiple "stream-like" abstractions:

- **AsyncIterable<T>** (ES2018). The native protocol for async
  iteration. `for await (const x of stream)` consumes.
- **ReadableStream<T>** (Web Streams, 2017). The WHATWG-standard
  backpressure-aware stream. Used in `fetch().body`.
- **Observable<T>** (RxJS, TC39 proposal stalled at Stage 1).
  Push-based reactive stream.
- **Node Streams** (Node-specific, since Node 0.6 in 2011).
  `Readable`, `Writable`, `Duplex`, `Transform`.

The choice: **AsyncIterable<T>** for Mochi `stream<T>`. The other
forms are interop options at the boundary.

The reasoning:

- **Native protocol**: `AsyncIterable<T>` is the ECMAScript-built-in
  protocol; no library required.
- **`for await` syntax**: clean consumption syntax built into the
  language.
- **`async function*` syntax**: clean production syntax built into
  the language.
- **Composable**: ES2024's async iterator helpers (Stage-3 as of
  2024-Q4, expected ES2025) give `.map`, `.filter`, `.take`, etc.
  directly on async iterators.
- **No backpressure built in**: this is a known limitation; for
  backpressure-aware streams the user opts into the `BoundedAsyncIterableQueue`
  or uses Web Streams via the `--target=stream-pipe` flag.

Conversion between forms:

- `AsyncIterable<T>` to `ReadableStream<T>`:
  ```typescript
  function toReadableStream<T>(iter: AsyncIterable<T>): ReadableStream<T> {
    const it = iter[Symbol.asyncIterator]();
    return new ReadableStream({
      async pull(controller) {
        const { value, done } = await it.next();
        if (done) controller.close();
        else controller.enqueue(value);
      },
    });
  }
  ```
- `ReadableStream<T>` to `AsyncIterable<T>` (since 2023, Node 18.0+,
  Deno 1.31+, browsers): the `ReadableStream` is itself
  `AsyncIterable` natively. Just use `for await (const x of stream)`.

The Mochi runtime exposes both forms; the emitter picks the right
one based on context.

## 14. Why prettier 3.x over Biome / dprint / `tsc`-only

Mochi-emitted code goes through a formatting pass after emit. The
candidates:

- **prettier 3.x** (the canonical JS formatter since 2017; 3.0
  shipped in 2023, 3.3 in June 2024).
- **Biome** (the Rust-written Rome successor, since 2023; Biome 1.0
  shipped Sept 2023, 1.9 in late 2024). Faster than prettier;
  pluginless.
- **dprint** (Rust-written formatter with TS support since 2020).
- **tsc itself** (no formatting; the raw emit is the output).

The choice: **prettier 3.x** as the canonical formatter, with
Biome and dprint supported as alt-formatters.

The reasoning:

- **Community adoption**: prettier is the de-facto standard. Every
  TS developer in 2026 knows prettier; the formatting choices it
  makes are the "obviously right" choices for most TS code.
- **Plugin ecosystem**: prettier plugins for Tailwind, Astro, Svelte,
  etc. The user's existing project likely has prettier configured
  for these; Mochi output integrates naturally.
- **Determinism**: prettier 3.x produces byte-identical output for
  the same input. Critical for reproducibility ([[10-build-system]] §14).
- **Slow but acceptable**: prettier is slower than Biome (about
  10-30x slower on large codebases), but Mochi-emitted output is
  not large; the format pass takes <1s for a typical project.

Biome is the runner-up. Biome is faster, plugin-free, and has a
single binary; the formatting choices are mostly prettier-compatible
but differ in some edge cases (Biome formats JSX slightly differently;
Biome's import sorting is opinionated). Mochi supports Biome via
`--ts-formatter=biome`.

The format-pass output is the file that gets written to disk. Both
the source emit and the formatter agree on style; the formatter
catches any drift in the emitter's style heuristics. Re-running
prettier on the file produces zero diff (fixed-point check).

## 15. Source maps and the dual-source / dual-dist build

TypeScript's source map system is mature. The Mochi build pipeline
emits the full chain:

- `.mochi` source -> `.ts` emitted by Mochi (with `.ts.map` mapping
  TS back to Mochi source).
- `.ts` -> `.js` emitted by tsc (with `.js.map` mapping JS back to
  TS).
- `.ts` -> `.d.ts` emitted by tsc (with `.d.ts.map` mapping declaration
  to TS).

The chain `.js.map -> .ts.map -> .mochi source` is consumed by:

- **Node** with `--enable-source-maps` (Node 12+, on by default in
  Node 22 per a 2024 change). Stack traces show original Mochi source
  lines.
- **V8 Inspector / Chrome DevTools**. Breakpoints in the `.mochi`
  source map to runtime locations.
- **VS Code debugger**. F11 step-into navigates from `.js` execution
  to `.ts` source to `.mochi` source.

The bundler integration: Vite, esbuild, Rollup, Webpack, Parcel,
Bun's bundler all consume `.ts.map` and `.js.map` natively. The
bundled output has the combined source map that points to Mochi
source.

For browser builds, source maps can either be:

- **Inline** (encoded as base64 in a `//# sourceMappingURL=data:...`
  comment at the end of the file). Pro: single file. Con: bloated.
- **External** (separate `.map` file linked via
  `//# sourceMappingURL=app.js.map`). Pro: lean main file. Con: two
  HTTP requests, but the source map is usually only fetched by the
  debugger.

The Mochi build pipeline defaults to external source maps, with an
inline option (`--ts-source-map=inline`) for testing or single-file
distribution.

## 16. Reproducible builds for npm

A reproducible build is one where the same source produces a
byte-identical artefact. For npm packages this means the published
`.tgz` (the npm tarball) has the same SHA256 across two CI hosts
given the same source.

npm 10 introduced `npm pack --pack-destination DIR` and the
`SOURCE_DATE_EPOCH` env var support. With both set, the tarball is
reproducible:

- File mtimes are clamped to `SOURCE_DATE_EPOCH`.
- File ordering in the tarball is sorted alphabetically.
- File modes are clamped to 0644 / 0755 deterministically.
- The `package.json` `"_resolved"` field is removed.

The Mochi build pipeline sets `SOURCE_DATE_EPOCH` to the commit
timestamp of the source revision (via `git log -1 --format=%ct
HEAD`). Reproducibility tests run the build on two CI hosts (Linux
x86_64 and Linux arm64) and compare the SHA256 of the tarball; the
gate fails if they differ.

For the TypeScript-emitted dist files (`.js`, `.d.ts`, `.map`), the
emitter must also be deterministic:

- The Mochi-internal CST builder produces deterministic output (no
  reliance on hash-map ordering, no PIDs in identifiers).
- `tsc` is deterministic given the same input.
- `prettier 3.x` is deterministic given the same input and config.

The combination is reproducible. CI verifies via the two-host
SHA256 comparison.

## 17. Sigstore provenance and npm Trusted Publishing

npm 10.5+ supports `npm publish --provenance`, which attaches a
Sigstore-signed attestation to the published package. The signature
is keyless (no private keys to manage); the attestation is generated
by GitHub Actions (or other supported OIDC providers) and signed by
Sigstore's transparency log Rekor.

The Mochi build pipeline opts into provenance by default for `mochi
build --target=npm-package --publish`. The generated `package.json`
declares:

```json
{
  "publishConfig": {
    "access": "public",
    "provenance": true
  }
}
```

And the CI workflow declares:

```yaml
permissions:
  id-token: write  # required for OIDC token
  contents: read

jobs:
  publish:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: "22"
          registry-url: "https://registry.npmjs.org"
      - run: npm ci
      - run: npm run build
      - run: npm publish --provenance --access public
        env:
          NODE_AUTH_TOKEN: ${{ secrets.NPM_TOKEN }}
```

The published package on npmjs.org displays a "Provenance" badge
linking to the GitHub Actions run that built it, the Git commit, and
the Sigstore transparency log entry. Downstream users can verify the
provenance via `npm audit signatures`.

This is the npm-target equivalent of PyPI's Trusted Publishing
(MEP-51 §17) and Maven Central's Sigstore integration (MEP-47).
Cross-MEP supply-chain story is consistent: every Mochi-published
package has cryptographic provenance back to the source repository.

For the Deno JSR publishing path: JSR has its own provenance system
(JSR signs with its own key, not Sigstore). JSR provenance is
generated automatically when publishing from GitHub Actions with the
JSR GitHub App installed. The Mochi pipeline supports both
publishing paths.

## 18. Why JSR as a secondary publication target

JSR (jsr.io) is the Deno-native registry, launched in March 2024.
JSR accepts TypeScript source directly (no tsc compilation at
publish time), supports the `package.json` `exports` field natively,
and provides per-import-resolution type-checking.

The Mochi build pipeline supports `deno publish` to JSR as a
secondary target. The reasoning:

- **Deno-native**: Deno users prefer JSR over npm because JSR's
  TypeScript-source-direct model avoids the npm-to-Deno
  `npm:package` proxy.
- **Reasonable provenance**: JSR provides its own attestation system,
  separate from Sigstore. Less mature than npm Trusted Publishing
  but sufficient.
- **Free-tier publishing**: JSR is free for open-source packages,
  matching npm.
- **Cross-runtime**: JSR packages can be consumed by Bun (`bun add
  jsr:@org/pkg`) and by npm (`npm install jsr@org/pkg` via a proxy).

The pipeline generates a `jsr.json` (the JSR-specific manifest, a
subset of `package.json`) alongside `package.json`. The two are
kept in sync by the build driver.

## 19. The browser bundle and `--target=browser-bundle`

For browser consumption, the Mochi build pipeline produces a
single-file ESM bundle via esbuild. The bundle:

- Contains all Mochi-emitted source, the `mochi_runtime` package,
  and any third-party deps (after tree-shaking).
- Excludes Node-only APIs (`node:fs`, `node:net`, `node:crypto`,
  etc.) via esbuild's `--external:node:*` flag.
- Excludes the `mochi_runtime/ffi/napi.ts` / `bun.ts` modules (which
  reference Node-only or Bun-only APIs).
- Is minified with esbuild's built-in minifier (terser is no longer
  needed; esbuild's minifier is comparable in output size).
- Has source maps inline or external (configurable).

The bundle is consumed via `<script type="module" src="bundle.js">`
or via an importmap:

```html
<script type="importmap">
{
  "imports": {
    "@org/mypkg": "/assets/mypkg.bundle.js"
  }
}
</script>
<script type="module">
  import { foo } from "@org/mypkg";
  foo();
</script>
```

For framework integration (Vite, Next.js, Astro, Remix, SvelteKit,
SolidStart, Qwik), the user typically does not consume the
pre-bundled file; they consume the `.ts` source directly via npm and
let the framework's bundler handle it. The pre-bundled file is for
"vanilla" browser use without a framework bundler.

## 20. The Deno Jupyter kernel target

Deno 1.37 (October 2023) added an official Jupyter kernel via
`deno jupyter --install`. The kernel runs TypeScript directly in
Jupyter notebooks (JupyterLab, VS Code Notebooks, Google Colab via
the Deno kernel extension).

The Mochi build pipeline supports `--target=deno-jupyter`, which:

- Generates the kernel-spec JSON.
- Generates a `_mochi.ipynb` template notebook.
- Installs the kernel to `~/Library/Jupyter/kernels/mochi/` (macOS),
  `~/.local/share/jupyter/kernels/mochi/` (Linux), or
  `%APPDATA%\jupyter\kernels\mochi\` (Windows).

The Mochi notebook cells contain Mochi source; the kernel transpiles
Mochi to TypeScript and runs in Deno. Outputs are JSON for primitive
values, HTML for rich displays (matplotlib-equivalent via the Deno
`jp.image()` helper, plotly-equivalent via the `jp.plot()` helper).

This is the TypeScript-target equivalent of MEP-51's Python ipykernel
support. The Jupyter ecosystem becomes accessible to Mochi
programmers; the existing notebook tooling (nbconvert, papermill,
Voila) consumes Mochi notebooks unchanged.

## 21. ESLint configuration and the typed-check ruleset

The default ESLint config generated for Mochi projects:

```javascript
// eslint.config.js
import tseslint from "typescript-eslint";
import eslint from "@eslint/js";

export default tseslint.config(
  eslint.configs.recommended,
  ...tseslint.configs.strictTypeChecked,
  ...tseslint.configs.stylisticTypeChecked,
  {
    languageOptions: {
      parserOptions: {
        project: ["./tsconfig.json"],
        tsconfigRootDir: import.meta.dirname,
      },
    },
    rules: {
      "@typescript-eslint/no-explicit-any": "error",
      "@typescript-eslint/no-floating-promises": "error",
      "@typescript-eslint/no-misused-promises": "error",
      "@typescript-eslint/strict-boolean-expressions": ["error", {
        allowString: false,
        allowNumber: false,
        allowNullableObject: false,
      }],
      "@typescript-eslint/switch-exhaustiveness-check": "error",
      "@typescript-eslint/no-unsafe-argument": "error",
      "@typescript-eslint/no-unsafe-assignment": "error",
      "@typescript-eslint/no-unsafe-call": "error",
      "@typescript-eslint/no-unsafe-member-access": "error",
      "@typescript-eslint/no-unsafe-return": "error",
      "@typescript-eslint/no-unnecessary-condition": "error",
      "@typescript-eslint/prefer-readonly": "error",
      "@typescript-eslint/prefer-readonly-parameter-types": "error",
      "@typescript-eslint/prefer-nullish-coalescing": "error",
      "@typescript-eslint/prefer-optional-chain": "error",
      "no-var": "error",
      "prefer-const": "error",
      "eqeqeq": ["error", "always"],
      "no-eval": "error",
      "no-implied-eval": "error",
      "no-new-func": "error",
    },
  },
);
```

The flat-config (ESLint 9.x, April 2024) is the modern format; the
legacy `.eslintrc.json` is deprecated. The Mochi pipeline supports
ESLint 9.x only.

The `strictTypeChecked` preset is the strongest typed-check level.
It catches:

- Operations on `any`-typed values.
- Unhandled Promises.
- Unintentional Promise misuse (`if (promise)` instead of `if (await
  promise)`).
- Non-exhaustive switches.
- Truthy/falsy checks on nullable values.
- Floating Promise rejection.

Combined with `tsc --strict --noUncheckedIndexedAccess`, the lint
gate catches most of the foot-guns left by the type checker.

The `prefer-readonly` and `prefer-readonly-parameter-types` rules
encourage immutability. Mochi-emitted code uses `readonly` modifiers
extensively, so it passes these rules by construction.

## 22. Cross-MEP diff

| MEP | Output | Build | Async | Errors | Records |
|-----|--------|-------|-------|--------|---------|
| 45  | C source | clang/gcc | pthread | -fwrapv int + Result | struct + helper functions |
| 46  | Core Erlang | rebar3 | actor + supervisor | Result + exit | tagged tuple |
| 47  | JVM bytecode | ASM direct | virtual threads (Loom) | Result | data class |
| 48  | C# source | dotnet | Task + Channel | Result | record |
| 49  | Swift source | SwiftPM | actor + AsyncStream | typed throws | struct |
| 50  | Kotlin source | Gradle | Channel + SupervisorJob | MochiResult | data class |
| 51  | Python source | uv + hatchling | asyncio.Queue + TaskGroup | MochiResult (Ok/Err) | frozen dataclass |
| 52  | TS source + JS dist | npm + tsc | AsyncIterableQueue + AbortController | MochiResult (Ok/Err discriminated union) | class with readonly + Object.freeze |

The TypeScript target is the closest cross-MEP cousin to Python
(both target dynamically-checked runtimes with mature ecosystems and
async/await coroutines) and to Kotlin (both use class-style records
with explicit nullability via `T?` / `T | null`). The diff between
TypeScript and Python is the largest on string handling (UTF-16 vs
PEP 393) and on the runtime concurrency primitives
(AsyncIterableQueue+AbortController vs asyncio.Queue+TaskGroup), but
the overall shape is similar.

## 23. Cross-references

- [[01-language-surface]] (previous note): the "what" companion to
  this note's "why".
- [[03-prior-art-transpilers]] (next note): the source-to-JS / TS
  tooling survey that informed these decisions.
- [[04-runtime]]: the `mochi_runtime` package layout in detail.
- [[05-codegen-design]]: the lowering pipeline that produces the
  TypeScript source described here.
- [[06-type-lowering]]: the bigint/number monomorphisation, the
  freeze policy, and the discriminated-union encoding.
- [[07-runtime-portability]]: the runtime version matrix in detail.
- [[08-dataset-pipeline]]: the query DSL and Datalog engine
  lowering.
- [[09-agent-streams]]: the AsyncIterableQueue + AbortController
  agent shape in detail.
- [[10-build-system]]: the dual-source / dual-dist build pipeline
  including npm + JSR + esbuild + Vite integration.
- [[11-testing-gates]]: the test-suite gates for v0.10 ship.
- [[12-risks-and-alternatives]]: the risk register and the
  alternatives considered (notably WebAssembly via the C target, the
  Hegel / Flow type-checker alternatives, the SWC / esbuild
  transpile-only paths).
- [[../0051/02-design-philosophy]]: the Python-target analogue, the
  closest in spirit since both target managed dynamic-ish runtimes.
- [[../0050/02-design-philosophy]]: the Kotlin-target analogue,
  which shares the typed-Result error story.
- [[../0049/02-design-philosophy]]: the Swift-target analogue, which
  shares the actor-as-class shape.
- [[../0048/02-design-philosophy]]: the .NET-target analogue.
- [[../0047/02-design-philosophy]]: the JVM-bytecode-target analogue.
- [[../0046/02-design-philosophy]]: the BEAM-target analogue, whose
  agent / mailbox / supervision design directly inspired the
  TypeScript AsyncIterableQueue + AbortController lowering.
- [[../0045/02-design-philosophy]]: the C-target analogue.
