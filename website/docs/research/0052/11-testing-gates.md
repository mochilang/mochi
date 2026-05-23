---
title: "Testing gates: per-phase Go test gates, four-runtime matrix (Node 22 / Deno 2 / Bun 1.1 / browser via Playwright), tsc --strict + --noUncheckedIndexedAccess + --exactOptionalPropertyTypes, eslint + prettier fixed-point, npm pack + install + execute, ~400-fixture corpus by Phase 18"
description: "The test gate plan for MEP-52 (Mochi-to-TypeScript). Eight ordered tiers: vm3 byte-equal master, tsc strict secondary, eslint tertiary, prettier quaternary, four-runtime execution quinary, npm pack + install + execute senary, reproducibility septenary, JSR dry-run octonary. CI matrix: ubuntu / macos / windows x Node 22 / Deno 2 / Bun 1.1 / browser via Playwright."
sidebar_position: 11
---

# Testing gates: per-phase fixtures, vm3 byte-equal, tsc --strict, four-runtime matrix, reproducibility

This note defines the test gate plan that MEP-52 must clear at each
of its 18 phases. The structure mirrors MEP-50 (Kotlin) and MEP-51
(Python), with deltas for the TypeScript-specific gates (tsc strict
mode, eslint + prettier fixed-point) and the four-runtime matrix that
no sibling MEP has at v1.

See the shared decisions anchor for the load-bearing decisions and
the [[10-build-system]] note for the build pipeline this note tests.

## The gate hierarchy

MEP-52 has eight ordered gate tiers. A fixture passes Phase N only
if it clears every tier in order. The master gate (Tier 1) is the
only one that compares observable behaviour; the others gate the
artifact, the type discipline, the lint state, the format state, the
publish dry run, or the reproducibility property.

### Tier 1 (master): vm3 byte-equal stdout

The Mochi reference interpreter `vm3` runs the source `.mochi`
fixture and captures stdout. The transpiler emits TypeScript from
the same fixture. The emitted TypeScript runs under each target
runtime (Node 22, Deno 2, Bun 1.1, browser via Playwright) and each
runtime's stdout (or `console.log` capture for browser) must be
byte-identical to vm3's.

byte-identical means:

- Same UTF-8 bytes.
- Same `\n` line endings (POSIX `\n`, never `\r\n`, even on Windows).
- Same trailing newline presence / absence.
- Same numeric formatting (e.g. floats as `1.5` not `1.50` or `1.5e0`,
  bigints printed without the `n` suffix).
- Same Unicode normalisation (NFC for our outputs).

The `expect.txt` golden file is the byte-equal target. It is
generated once from `vm3` and committed; subsequent CI runs compare
each runtime's stdout to `expect.txt`.

Normalised line endings: the CI runner sets `git config core.autocrlf
false` and `git config core.eol lf` to prevent Windows from rewriting
line endings on checkout. Mochi's emit uses POSIX `\n` only; the
runtime stub's `print` helper writes raw bytes (no platform-specific
line ending transform).

Per-runtime stdout capture:

- Node: `node dist/node/index.js > /tmp/actual.txt`
- Deno: `deno run --allow-read dist/deno/index.js > /tmp/actual.txt`
- Bun: `bun dist/bun/index.js > /tmp/actual.txt`
- Browser: Playwright spawns a headless chromium / firefox / webkit,
  loads a fixture HTML that imports `dist/browser/index.js` as a
  module and writes `console.log` to `document.body.innerText`;
  Playwright reads `innerText` and writes to `/tmp/actual.txt`.

Browser console output is tricky: `console.log` in browsers does not
write to stdout. We intercept by overriding `console.log` in the
fixture HTML's `<script type="module">` preamble:

```html
<!doctype html>
<meta charset="utf-8">
<title>Mochi fixture</title>
<pre id="out"></pre>
<script type="module">
  const out = document.getElementById("out");
  const origLog = console.log;
  console.log = (...args) => {
    out.textContent += args.map(String).join(" ") + "\n";
    origLog.apply(console, args);
  };
  await import("./dist/browser/index.js");
</script>
```

Playwright reads `document.getElementById("out").textContent` after
the page settles (`page.waitForLoadState("networkidle")` plus a short
delay for async-tick drain).

Test runner: `tests/transpiler3/typescript/runner.go` (Go test
driver, mirrors `tests/transpiler3/python/runner.go`).

### Tier 2 (secondary): tsc --strict + --noUncheckedIndexedAccess + --exactOptionalPropertyTypes

The TypeScript compiler runs on the emitted `.ts` source with strict
mode enabled. The invocation:

```sh
tsc --noEmit \
    --strict \
    --noUncheckedIndexedAccess \
    --exactOptionalPropertyTypes \
    --noImplicitOverride \
    --noFallthroughCasesInSwitch \
    --noPropertyAccessFromIndexSignature \
    --noUncheckedSideEffectImports \
    --verbatimModuleSyntax \
    --target ES2024 \
    --module ESNext \
    --moduleResolution Bundler \
    --skipLibCheck false \
    --project tsconfig.base.json
```

Zero diagnostics required. Any error fails the gate.

Why each flag matters:

- `--strict` enables the strict block: `strictNullChecks`,
  `strictFunctionTypes`, `strictBindCallApply`, `strictPropertyInitialization`,
  `noImplicitAny`, `noImplicitThis`, `useUnknownInCatchVariables`,
  `alwaysStrict`.
- `--noUncheckedIndexedAccess` makes `arr[i]` typed as `T |
  undefined`. Mochi's bounds-checked array semantics map to runtime
  helpers (`mochiIndex(arr, i)`) that throw on out-of-bounds; the
  type system surfaces the possibility via the union.
- `--exactOptionalPropertyTypes` distinguishes `T?` (property absent)
  from `T | undefined` (property present, value `undefined`). Mochi's
  optional types map to `T | null` (not `T | undefined`), and
  `exactOptionalPropertyTypes` enforces the discipline.
- `--noImplicitOverride` requires the `override` keyword on
  inherited method overrides. Mochi's emit always writes `override`.
- `--noFallthroughCasesInSwitch` errors on missing break/return in
  case labels. Mochi's emit always closes every case (typically with
  a `return` from a discriminated-union dispatcher).
- `--noPropertyAccessFromIndexSignature` blocks `obj.foo` on
  `Record<string, T>` types; must use `obj["foo"]`. Catches typos in
  index-signature access.
- `--noUncheckedSideEffectImports` (TypeScript 5.6) errors on
  `import "./side-effect.ts"` if the module has no declared exports.
  We don't emit side-effect imports; this catches accidental
  introduction.
- `--verbatimModuleSyntax` preserves `import type` / `export type`
  exactly. Without this, TypeScript may rewrite some type-only
  imports to value imports.
- `--target ES2024` matches our floor.
- `--module ESNext --moduleResolution Bundler` matches the emit
  shape.
- `--skipLibCheck false` actually checks the types of dependencies.
  Costs about 30% of typecheck time but catches dependency-introduced
  errors at build time.

The gate runs with `--noEmit` for speed (no output files written).
Production builds (Tier 6) re-run `tsc --build` with emit.

### Tier 3 (tertiary): eslint --max-warnings 0

eslint 9 with `@typescript-eslint/strict-type-checked` runs on the
emit. Zero warnings, zero errors required. Invocation:

```sh
eslint src/ --max-warnings 0 --config eslint.config.js
```

The lint configuration is in [[10-build-system]]. Highlights:

- `@typescript-eslint/recommended-type-checked`: rules that require
  type information. Includes `no-unsafe-assignment`,
  `no-unsafe-call`, `no-unsafe-member-access`, `no-unsafe-return`.
- `@typescript-eslint/strict-type-checked`: stricter ruleset.
  Includes `no-unnecessary-condition`, `no-misused-promises`,
  `no-floating-promises`.
- `@typescript-eslint/consistent-type-imports`: enforce `import type`
  for type-only imports. Mochi's emit always uses `import type` where
  applicable.
- `@typescript-eslint/no-explicit-any`: error on `any`. Mochi never
  emits `any`; if a type cannot be inferred, the emit uses `unknown`
  + narrowing.
- `@typescript-eslint/no-non-null-assertion`: error on `x!` (non-null
  assertion). Mochi uses explicit narrowing via `if (x !== null)`.

We do not use `eslint --fix` in CI. Auto-fix introduces drift; we
require the emit to produce lint-clean code from the start.

### Tier 4 (quaternary): prettier --check fixed-point

prettier 3.3+ runs on the emit in check mode. Any unformatted file
fails the gate. Invocation:

```sh
prettier --check src/
```

The format configuration is in [[10-build-system]] (
`.prettierrc.json`). Highlights:

- `printWidth: 100`
- `tabWidth: 2`
- `semi: true`
- `singleQuote: false`
- `trailingComma: "all"`
- `endOfLine: "lf"`

This is a fixed-point check: emit -> prettier -> emit again must
produce identical output. We test by running prettier once and
diffing:

```sh
$ prettier --write src/
$ git diff --quiet src/  # must succeed
```

If `git diff` shows changes, the emit is not prettier-stable. We fix
the emitter to produce prettier-stable output. The fixed-point
discipline catches drift between the emitter's output and prettier's
canonical form before it reaches the published artifact.

### Tier 5 (quinary): four-runtime execution

Run the produced JS on all four runtimes. Each runtime's stdout (or
console-log capture) must match the master gate's `expect.txt`.

Per-runtime invocation:

```sh
# Node 22
node dist/node/index.js | diff - expect.txt

# Deno 2
deno run --allow-read dist/deno/index.js | diff - expect.txt

# Bun 1.1
bun dist/bun/index.js | diff - expect.txt

# Browser via Playwright (chromium / firefox / webkit)
npx playwright test fixtures/<phase>/<name>/browser.spec.ts
```

The Playwright spec for a fixture:

```typescript
import { test, expect } from "@playwright/test";
import { readFileSync } from "node:fs";

test("phase-<n>-<name>", async ({ page }) => {
  const expected = readFileSync("fixtures/<phase>/<name>/expect.txt", "utf8");
  await page.goto("file://" + __dirname + "/index.html");
  await page.waitForLoadState("networkidle");
  await page.waitForFunction(() => (window as any).__mochiDone === true);
  const actual = await page.locator("#out").innerText();
  expect(actual).toBe(expected);
});
```

The `__mochiDone` flag is set by the emit's top-level `await`
completion (Mochi's `main` entry is an `async function`; the fixture
HTML's preamble sets `__mochiDone = true` after the import resolves).

The browser test runs across `chromium`, `firefox`, and `webkit`
projects (Playwright's `projects` config). Each browser has its own
quirks: webkit lags on `Promise.withResolvers` (Safari 17.4+ for
desktop, iOS 17.4+ for mobile); we test against the latest stable
release.

### Tier 6 (senary): npm pack + install + execute

```sh
npm pack
mkdir /tmp/install-test
cd /tmp/install-test
npm init -y
npm install <path>/*.tgz
node -e "import('mochi-example-app').then(m => console.log(m.version))"
```

The tarball must build without errors. The tarball must install into
a fresh `node_modules` without errors. The smoke test imports the
top-level package and prints something that proves it loaded.

Build errors usually mean `package.json` mis-emission (wrong
`exports`, missing `files` entry, wrong `types` path). Install errors
usually mean a dependency declaration mismatch. Smoke-test errors
mean the emit breaks at module import time (most often: typo in a
generated `index.ts` or a circular import).

This gate runs after Tier 1-5 pass. It is deliberately late because
it's the slowest (cold install on Windows can take 40 seconds).

We also verify the JSR install path:

```sh
deno run --allow-read --reload \
  -e "import { version } from 'jsr:@mochilang/example-app@0.1.0'; console.log(version)"
```

JSR's `--reload` flag bypasses Deno's cache; we want to test the
fresh-install path.

### Tier 7 (septenary): reproducibility

Two builds, two hosts (or two clean checkouts on the same host),
byte-identical tarball SHA512:

```sh
HOST_A: SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct) npm run build && npm pack
HOST_B: SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct) npm run build && npm pack
diff <(shasum -a 512 host_a/*.tgz) <(shasum -a 512 host_b/*.tgz)
```

Hosts cover ubuntu-24.04 (x86_64), ubuntu-24.04-arm (aarch64),
macos-14 (arm64) by Phase 16. Windows reproducibility is excluded
through Phase 16 due to npm's Windows tarball generator known
case-insensitivity deltas (npm issue #7234); Phase 16.1 adds Windows.

We compute SHA512 (not SHA256) because npm provenance uses SHA512.
Matching the registry's hash function lets us cross-check with the
published artifact.

### Tier 8 (octonary): JSR dry-run

```sh
deno publish --dry-run
```

JSR's `--dry-run` validates the package without uploading: checks
`deno.json` schema, type-checks the source, verifies exports map,
verifies file inclusion / exclusion. Any error fails the gate.

We also dry-run npm publish with provenance:

```sh
npm publish --dry-run --provenance --access public
```

The `--provenance` flag in `--dry-run` mode validates the OIDC token
exchange path (does not actually request a token; checks the workflow
permissions config).

This gate runs in PR CI to catch publish-config issues before they
block a release. The real publish happens only on release tag events.

## Per-phase gate definitions

Each phase has a Go test wrapper in `tests/transpiler3/typescript/`
that sets up fixtures, runs the transpiler, and walks the gate
tiers. The test wrapper for Phase N is `phase<N>_test.go` with a
top-level test `TestPhase<N>...TypeScript`.

The fixture directory layout:

```
tests/transpiler3/typescript/
├── runner.go
├── phase1_helloworld_test.go
├── phase2_scalars_test.go
├── phase3_1_lists_test.go
├── phase3_2_maps_test.go
├── phase3_3_sets_test.go
├── phase3_4_list_of_records_test.go
├── phase4_records_test.go
├── ...
├── phase18_trusted_publishing_test.go
└── fixtures/
    ├── phase1/
    │   └── helloworld/
    │       ├── source.mochi
    │       ├── expect.txt
    │       ├── meta.toml
    │       └── browser.spec.ts
    ├── phase2/
    │   └── ...
    └── ...
```

Each fixture has:

- `source.mochi`: the Mochi source.
- `expect.txt`: the byte-equal stdout target.
- `meta.toml`: optional metadata (e.g. `skip_browser = true` if a
  fixture probes a Node-only API, `skip_bun = true` for a known Bun
  quirk).
- `browser.spec.ts`: Playwright test for the browser runtime (only
  if `skip_browser != true`).

### Phase 1: hello world

**Goal**: `print("hello world")` round-trips through emit and runs
on all four runtimes.

**Fixtures**: 1.

- `helloworld`: prints `hello world\n`.

**Gate**: Tiers 1-8. tsc strict on a single `index.ts` plus runtime
stub. eslint + prettier clean. Bundles via esbuild for browser.
Tarball builds + installs. Sha matches across two builds. JSR
dry-run clean.

**Output**:

```typescript
// src/generated/foo.ts
import { print } from "../mochi_runtime/io.ts";

export async function main(): Promise<void> {
  print("hello world");
}
```

**Test**: `TestPhase1HelloWorldTypeScript`.

### Phase 2: scalars

**Goal**: `int`, `float`, `bool`, `str`, `bytes` lower correctly,
arithmetic and comparisons work, formatting matches Mochi's.

**Fixtures**: 14.

- `int_arith_small`: `1 + 2 * 3` lowers to `number` (fits in i53).
- `int_arith_big`: `2n ** 100n` lowers to `bigint`.
- `int_mixed_forbidden`: `1n + 2` is rejected by emit (TS forbids
  mixed bigint / number arithmetic).
- `int_overflow_check`: `Number.MAX_SAFE_INTEGER + 1n` triggers the
  bigint path.
- `float_arith`: IEEE 754 addition.
- `float_special`: NaN, +Infinity, -Infinity. Mochi prints `NaN`,
  `+Inf`, `-Inf`; JS prints `NaN`, `Infinity`, `-Infinity`. Emitter
  uses a runtime helper to align.
- `bool_logic`: `&&`, `||`, `!`, short-circuit.
- `string_concat`: `"a" + "b"`.
- `string_len_codepoints`: code-point length via `[...s].length`,
  not UTF-16 `.length`.
- `string_unicode_emoji`: emoji (surrogate pairs) + combining marks.
- `string_format`: template literal `` `${x.toFixed(2)}` `` .
- `bytes_literal`: `Uint8Array.of(0x00, 0x01)`.
- `bytes_concat`: `Uint8Array.from([...a, ...b])`.
- `print_mixed`: print of mixed types via runtime `toMochiString`.

**Gate**: Tiers 1-8. Special attention to:

1. Float formatting: JS's `String(1.5)` returns `"1.5"`; matches
   Mochi. `String(0.1 + 0.2)` returns `"0.30000000000000004"`; Mochi
   prints `0.3`. Emitter uses explicit `toFixed` or a `mochiFmt`
   runtime helper.
2. bigint formatting: `String(42n)` returns `"42"` (no `n` suffix);
   matches Mochi.
3. Empty string handling differs across runtimes; we verify all
   four print identical output.

### Phase 3.1: lists

**Goal**: `list[T]` lowering to `T[]` (or `readonly T[]` for
immutable view), including literals, indexing, slicing, `len`,
`push`, comprehensions.

**Fixtures**: 20.

- `list_literal_int`, `list_literal_str`, `list_literal_record`,
  `list_literal_nested`, `list_literal_empty_typed`.
- `list_index_positive`, `list_index_negative` (Mochi: from end; JS:
  `arr.at(-1)`), `list_index_out_of_bounds`.
- `list_slice_basic`, `list_slice_step`, `list_slice_negative_step`.
- `list_len_via_helper`.
- `list_push`, `list_pop`, `list_shift`, `list_unshift`,
  `list_concat`, `list_reverse`, `list_sort`.
- `list_iter_for_of`, `list_iter_entries`.

**Gate**: Tiers 1-8. tsc with `noUncheckedIndexedAccess` requires
the emit to handle `T | undefined` from `arr[i]`. The emit uses a
runtime helper `mochiIndex(arr, i)` that throws `MochiBoundsError`
on out-of-bounds; the helper's return type is `T` (the helper either
returns `T` or throws), so the union is removed at the call site.

Out-of-bounds indexing: Mochi raises a structured error; JS's
`arr[i]` returns `undefined`. The runtime helper bridges.

### Phase 3.2: maps

**Goal**: `map<K, V>` lowering to `Map<K, V>` (insertion order
guaranteed by JS spec), including literal, get, set, has, keys,
values, entries, for-each iteration.

**Fixtures**: 22.

- `map_literal_str_int`, `map_literal_int_str`,
  `map_literal_str_str`, `map_literal_str_list`,
  `map_literal_str_record`.
- `map_get`, `map_get_missing_throws`, `map_get_missing_default`.
- `map_set`, `map_delete`, `map_update`, `map_clear`.
- `map_len_via_size`, `map_keys`, `map_values`, `map_entries`.
- `map_has`.
- `map_iter_for_of`, `map_iter_keys`, `map_iter_values`,
  `map_iter_entries`.
- `map_merge`, `map_to_object`.

**Gate**: Tiers 1-8. Mochi's `m["k"]` on missing key raises a typed
error; JS's `m.get("k")` returns `undefined`. Emit uses a runtime
helper `mochiMapGet(m, k)` that throws `MochiKeyError` on missing.
For `m.get(k, default)` Mochi syntax, emit lowers to
`m.has(k) ? m.get(k)! : default`.

We do NOT use `Object` (`{}`) as a map. Reasons:

1. Object keys are coerced to strings; Mochi's `Map<int, V>` would
   silently break.
2. Object iteration order is not guaranteed for integer-looking
   keys.
3. Prototype pollution: `m["__proto__"]` mutates the prototype on
   plain objects.

### Phase 3.3: sets

**Goal**: `set<T>` lowering to `Set<T>` (insertion order guaranteed
by JS spec; ES2024 methods available).

**Fixtures**: 16.

- `set_literal`, `set_add`, `set_remove` (via `delete`),
  `set_contains` (via `has`), `set_iter`, `set_len` (via `size`),
  `set_union`, `set_intersection`, `set_difference`,
  `set_symmetric_difference`, `set_is_subset` (via `isSubsetOf`),
  `set_is_superset` (via `isSupersetOf`), `set_is_disjoint` (via
  `isDisjointFrom`), `set_comprehension`, `set_from_list`,
  `set_to_list`.

**Gate**: Tiers 1-8. ES2024 Set methods (`union`, `intersection`,
`difference`, `isSubsetOf`, `isSupersetOf`, `isDisjointFrom`,
`symmetricDifference`) are supported in Node 22+, Deno 2+, Bun 1.1+,
and modern browsers. Webkit added the methods in Safari 17 (2023-09);
we test against Safari 17.4+ for Playwright.

For browsers without ES2024 Set methods we ship a runtime fallback
in `mochi_runtime/set-polyfill.ts`, conditionally loaded:

```typescript
if (typeof Set.prototype.union !== "function") {
  await import("./set-polyfill.ts");
}
```

The polyfill is tree-shaken out for modern Node / Deno / Bun builds.

### Phase 3.4: list of records

**Goal**: `list<Record>` where Record is a class with `readonly`
fields.

**Fixtures**: 18.

- `list_record_basic`, `list_record_filter`, `list_record_sort`,
  `list_record_map`, `list_record_index`, `list_record_push`,
  `list_record_nested`, `list_record_with_option`,
  `list_record_with_list`, `list_record_with_map`,
  `list_record_query` (select-from-where), `list_record_groupby`,
  `list_record_distinct`, `list_record_aggregate`,
  `list_record_join`, `list_record_serialise_json`,
  `list_record_serialise_jsonl`, `list_record_deserialise_json`.

**Gate**: Tiers 1-8. Records emit as classes with `readonly` fields
and a private constructor exposed via a static `make` method:

```typescript
class User {
  readonly name: string;
  readonly age: number;
  private constructor(name: string, age: number) {
    this.name = name;
    this.age = age;
  }
  static make(name: string, age: number): User {
    return new User(name, age);
  }
}
```

tsc strict + `useDefineForClassFields: true` enforce the field
declaration discipline. `noUncheckedIndexedAccess` flags any
`list[i].field` access; the emit uses `mochiIndex(list, i).field`.

### Phase 4: records (class with readonly fields)

**Goal**: standalone record types, structural equality, hash,
`toString`.

**Fixtures**: 22.

- `record_basic`, `record_equality`, `record_hash`, `record_repr`,
  `record_nested`, `record_with_optional`, `record_with_list`,
  `record_with_map`, `record_with_set`, `record_pattern_match`,
  `record_clone_with`, `record_serialise_json`,
  `record_deserialise_json`, `record_generic`,
  `record_recursive` (linked-list node), `record_field_default`,
  `record_field_factory`, `record_inheritance_disallowed`,
  `record_class_vs_interface`, `record_with_method`,
  `record_immutability_freeze`, `record_compare`.

**Gate**: Tiers 1-8. Equality is structural via a runtime helper
`mochiEq(a, b)` (deep, type-aware). JS's `===` is reference equality
for objects; Mochi's `==` is structural. The emit always uses
`mochiEq` for record comparisons.

`record_immutability_freeze` checks that the record class's
constructor calls `Object.freeze(this)` (defensive runtime
immutability). Cost: about 50 ns per construction; benefit: catches
mutation bugs early.

### Phase 5: sum types

**Goal**: discriminated union via type alias + `kind` discriminator,
exhaustive `switch` with `assertNever`.

**Fixtures**: 20.

- `sum_basic_two_variants`, `sum_three_variants`,
  `sum_variant_with_data`, `sum_variant_no_data`,
  `sum_nested`, `sum_recursive` (tree), `sum_generic`,
  `sum_match_exhaustive`, `sum_match_non_exhaustive_error`,
  `sum_match_guards`, `sum_match_wildcard`,
  `sum_serialise`, `sum_deserialise`,
  `sum_option_some_none` (Mochi `T?` to `T | null`),
  `sum_result_ok_err`,
  `sum_either_left_right`, `sum_complex_records`,
  `sum_with_options_in_variants`, `sum_visitor_pattern`,
  `sum_pattern_in_query`.

**Gate**: Tiers 1-8. Discriminated unions use a literal `kind` tag:

```typescript
type Shape =
  | { readonly kind: "circle"; readonly radius: number }
  | { readonly kind: "square"; readonly side: number }
  | { readonly kind: "rectangle"; readonly width: number; readonly height: number };

function area(s: Shape): number {
  switch (s.kind) {
    case "circle":
      return Math.PI * s.radius * s.radius;
    case "square":
      return s.side * s.side;
    case "rectangle":
      return s.width * s.height;
    default: {
      const _: never = s;
      throw new Error("unreachable");
    }
  }
}
```

The `default` branch with `const _: never = s` enforces
exhaustiveness at compile time. tsc strict flags any missing case.

### Phase 6: closures + higher-order

**Goal**: closures capture variables, arrow functions have correct
inferred types, higher-order functions (map, filter, reduce,
fold) work.

**Fixtures**: 18.

- `closure_basic`, `closure_mutable_capture` (Mochi `var`),
  `closure_immutable_capture` (Mochi `let`), `closure_late_binding`,
  `closure_arrow_one_arg`, `closure_arrow_multi_arg`,
  `closure_higher_order_map`, `closure_higher_order_filter`,
  `closure_higher_order_reduce`, `closure_higher_order_compose`,
  `closure_curry`, `closure_partial_application`,
  `closure_returns_closure`, `closure_captures_this`,
  `closure_recursive`, `closure_in_method`,
  `closure_iife`, `closure_generator_function`.

**Gate**: Tiers 1-8. `closure_late_binding` checks that JS's
classic loop-variable-capture gotcha (var vs let) is handled. Mochi
emits `let` (block-scoped) for loop variables; JS's `let` in
`for (let i = ...)` creates a fresh binding per iteration. The
fixture's expected output shows the per-iteration capture working
correctly.

### Phase 7: query DSL

**Goal**: Mochi's `from x in xs select x.y where ... order by ...`
DSL lowers to iterator helpers (ES2024) plus generator functions.

**Fixtures**: 22.

- `query_select`, `query_where`, `query_orderby`,
  `query_groupby`, `query_distinct`, `query_join_inner`,
  `query_join_left`, `query_join_right`, `query_join_full`,
  `query_aggregate_count`, `query_aggregate_sum`,
  `query_aggregate_avg`, `query_aggregate_max`,
  `query_aggregate_min`, `query_nested`,
  `query_subquery`, `query_correlated_subquery`,
  `query_lazy_iterator`, `query_async_iterator`,
  `query_pipeline_dataflow`, `query_take_drop`,
  `query_iterator_helpers`.

**Gate**: Tiers 1-8. `query_lazy_iterator` checks that the emit uses
a generator (not eager array) when the result is iterated only once.
`query_async_iterator` uses `AsyncIterable<T>` for streams.

ES2024 iterator helpers (`Iterator.prototype.map`, `.filter`,
`.take`, `.drop`, `.toArray`) are used where available. Polyfill for
older runtimes is shipped in `mochi_runtime/iterator-helpers.ts`.

### Phase 8: datalog

**Goal**: Mochi's datalog blocks compile to seminaive evaluation in
TypeScript.

**Fixtures**: 14.

- `datalog_transitive_closure`, `datalog_path_finding`,
  `datalog_ancestor`, `datalog_same_generation`,
  `datalog_negation_stratified`, `datalog_negation_unstratified_error`,
  `datalog_arithmetic`, `datalog_aggregation_count`,
  `datalog_aggregation_sum`, `datalog_recursion_terminates`,
  `datalog_recursion_unsound_warning`, `datalog_large_dataset_perf`,
  `datalog_with_records`, `datalog_with_options`.

**Gate**: Tiers 1-8 plus a runtime budget check: each datalog
fixture must finish within 5 seconds on the CI host's reference
machine (github-hosted ubuntu-24.04, 4-core).

V8's bigint performance is materially slower than its number
performance (about 10x for typical arithmetic); datalog fixtures
that use small integers should compile to `number`, not `bigint`. The
monomorphisation pass decides per IR type.

### Phase 9: agents

**Goal**: `agent` syntax lowers to a class with `AsyncIterableQueue`
mailbox and `AbortController` supervision.

**Fixtures**: 20.

- `agent_basic`, `agent_state_mutation`, `agent_message_handler`,
  `agent_cast`, `agent_call_reply`, `agent_call_timeout`,
  `agent_supervisor_one_for_all`, `agent_supervisor_one_for_one`,
  `agent_supervisor_rest_for_one`,
  `agent_cancellation_propagation`,
  `agent_aggregate_error`, `agent_spawn_child`,
  `agent_child_failure_restart`, `agent_graceful_shutdown`,
  `agent_message_ordering_fifo`, `agent_backpressure_bounded_queue`,
  `agent_two_agents_interact`, `agent_pool_of_workers`,
  `agent_promise_with_resolvers`, `agent_signal_abort_event`.

**Gate**: Tiers 1-8 plus an unhandled-promise-rejection check. Node
22's `--unhandled-rejections=throw` flag converts unhandled
rejections to fatal errors. Deno and Bun have similar flags. The
agent fixtures must not produce unhandled rejections; the gate fails
on any.

`Promise.withResolvers` is ES2024; supported in Node 22+, Deno 2+,
Bun 1.1+, Chrome 119+, Firefox 121+, Safari 17.4+. For browsers
without it, the runtime stub ships a polyfill (about 200 bytes).

### Phase 10: streams

**Goal**: `stream<T>` lowers to `AsyncIterable<T>` (often
`AsyncGenerator<T, void, undefined>`). Stream combinators (map,
filter, fold, take, drop, zip) emit as async generators.

**Fixtures**: 16.

- `stream_basic`, `stream_finite`, `stream_infinite_take`,
  `stream_map`, `stream_filter`, `stream_fold`,
  `stream_zip`, `stream_flatmap`, `stream_throttle`,
  `stream_buffer`, `stream_close_on_drop`,
  `stream_backpressure`, `stream_error_propagation`,
  `stream_two_consumers_split`, `stream_to_async_iterator`,
  `stream_from_event_emitter`.

**Gate**: Tiers 1-8. `stream_close_on_drop` checks `return()` is
called when the iterator is no longer iterated (via for-await early
exit). The emit handles this via `try { ... } finally { ... }` in
the async generator.

### Phase 11: async coloring, MochiResult, AggregateError

**Goal**: every async-capable function returns a Promise; sync
functions stay sync; errors are `MochiResult<T, E>` not exceptions
for explicit error paths.

**Fixtures**: 22.

- `result_ok`, `result_err`, `result_match`, `result_map`,
  `result_chain`, `result_from_exception`,
  `result_into_exception_at_boundary`,
  `aggregate_error_basic`, `aggregate_error_nested`,
  `promise_all_one_failure`, `promise_all_two_failures`,
  `promise_all_settled`, `promise_any`, `promise_race`,
  `async_function_definition`, `async_function_call`,
  `async_function_concurrency_limit`,
  `async_generator_yield`, `async_generator_close`,
  `async_with_disposable` (Symbol.dispose),
  `async_with_disposable_async` (Symbol.asyncDispose),
  `top_level_await`.

**Gate**: Tiers 1-8. `async_with_disposable` and
`async_with_disposable_async` test the ES2024 `using` declarations:

```typescript
{
  using x = new Resource();
  // x.[Symbol.dispose]() called at scope exit
}
```

```typescript
{
  await using x = new AsyncResource();
  // await x.[Symbol.asyncDispose]() called at scope exit
}
```

`AggregateError` (ES2021) is used for multi-failure aggregation. The
shape matches Python's `ExceptionGroup` and Kotlin's
`SupervisorJob` exception-collection.

### Phase 12: FFI

**Goal**: Mochi `extern` declarations lower to N-API for native
libraries (Node-specific) and to direct TS imports for pure-TS
deps.

**Fixtures**: 14.

- `ffi_napi_libc_strlen`, `ffi_napi_libc_qsort`,
  `ffi_napi_struct_pack`, `ffi_napi_callback`,
  `ffi_pure_ts_import`, `ffi_typed_stub_only`,
  `ffi_wasm_alternative` (browser-capable via WebAssembly.Module),
  `ffi_error_propagation`, `ffi_memory_safety`,
  `ffi_thread_safety_worker`, `ffi_node_addon_api`,
  `ffi_platform_specific_linux`, `ffi_platform_specific_macos`,
  `ffi_platform_specific_windows`.

**Gate**: Tiers 1-8. Platform-specific fixtures use Playwright /
Node-specific guards. The runner reads `meta.toml`:

```toml
platforms = ["linux"]
runtimes = ["node"]
```

and skips on other platforms / runtimes. Skipping is logged in CI
output, not silent.

`ffi_wasm_alternative` is the browser-compatible FFI path: ship a
WebAssembly module instead of a native addon. Mochi compiles the
Mochi source's `extern wasm` block to a `WebAssembly.Module`
instantiation; Node, Deno, Bun, and browsers all support
`WebAssembly` natively.

### Phase 13: LLM provider dispatch

**Goal**: Mochi's `llm.chat` lowers to a `mochi-runtime` call that
dispatches to the right provider (Anthropic, OpenAI, local Ollama).

**Fixtures**: 10 (with `--llm=mock` to avoid network).

- `llm_basic_completion`, `llm_streaming_completion`,
  `llm_tool_use`, `llm_multi_turn`,
  `llm_provider_anthropic`, `llm_provider_openai`,
  `llm_provider_local_ollama`, `llm_error_handling`,
  `llm_token_usage_tracking`, `llm_message_role_validation`.

**Gate**: Tiers 1-8. Real network calls are gated to a nightly run
with `--llm=real` and credentials from secrets; PR CI uses mock.

### Phase 14: fetch

**Goal**: Mochi `fetch` (HTTP GET / POST) lowers to the built-in
`fetch` API, available natively in Node 18+, Deno, Bun, and all
modern browsers.

**Fixtures**: 12.

- `fetch_get_text`, `fetch_get_json`, `fetch_post_json`,
  `fetch_redirect`, `fetch_timeout` (via `AbortSignal.timeout`),
  `fetch_auth_basic`, `fetch_auth_bearer`,
  `fetch_streaming_response` (ReadableStream),
  `fetch_error_status`, `fetch_proxy`,
  `fetch_request_init_options`, `fetch_response_clone`.

**Gate**: Tiers 1-8. Fixtures hit a local test server
(`fastify` for Node, `Deno.serve` for Deno, `Bun.serve` for Bun,
service worker for browser). No real network in CI.

`AbortSignal.timeout` (ES2023) is supported in Node 19+, Deno 2+,
Bun 1.1+, Chrome 103+, Firefox 100+, Safari 16.4+.

### Phase 15: npm package build via tsc + npm pack

**Goal**: `tsc --build && npm pack` produces a tarball that installs
and runs on Node, Deno (via `npm:`), and Bun.

**Fixtures**: 6.

- `package_basic_install_node`, `package_with_exports_install_node`,
  `package_basic_install_deno_via_npm`,
  `package_basic_install_bun`,
  `package_browser_bundle_via_esbuild`,
  `package_workspaces_two_subpackages`.

**Gate**: Tier 6 dominates. The fixture runs `tsc --build`, then
`npm pack`, installs the tarball into a fresh directory, runs a
smoke test. Repeats for Deno (`deno run npm:<pkg>`) and Bun
(`bun add <tarball-path>`).

### Phase 16: reproducible build

**Goal**: byte-identical tarball SHA512 across hosts.

**Fixtures**: 3.

- `reproducibility_basic`, `reproducibility_with_runtime`,
  `reproducibility_with_browser_bundle`.

**Gate**: Build on ubuntu-24.04 (x86_64), ubuntu-24.04-arm
(aarch64), macos-14 (arm64); compare SHA512. Windows reproducibility
deferred to Phase 16.1.

Sub-phase 16.1 (Windows reproducibility): adds a fourth fixture and
extends the SHA comparison to windows-2022. Outstanding issues:
filesystem case sensitivity in tar entries (npm issue #7234),
CRLF / LF handling in generated sources (already normalised, but
the gate verifies).

### Phase 17: Deno JSR publish + Jupyter kernel + browser bundle

**Goal**: TypeScript source is publishable to JSR, the Deno Jupyter
kernel runs Mochi code cell-by-cell, and the browser bundle loads in
a static HTML page.

**Fixtures**: 12.

- `jsr_publish_dryrun`, `jsr_publish_with_exports`,
  `jsr_publish_with_workspaces`,
  `jupyter_notebook_helloworld`,
  `jupyter_notebook_variable_persistence`,
  `jupyter_notebook_function_redefinition`,
  `jupyter_notebook_query_dsl`,
  `jupyter_notebook_record_definition`,
  `browser_bundle_helloworld`,
  `browser_bundle_with_fetch`,
  `browser_bundle_size_under_budget`,
  `browser_bundle_tree_shake_verification`.

**Gate**: Tiers 1-3 do not apply directly for notebook fixtures (the
source is `.ipynb`, not `.mochi`). Tier 5 is replaced by the
notebook execution diff:

```sh
deno jupyter --execute fixtures/<name>.ipynb --output /tmp/actual.ipynb
diff (filtered) /tmp/actual.ipynb fixtures/<name>.expect.ipynb
```

Filter: remove `execution_count`, `id`, cell metadata; preserve
`outputs` text/plain entries.

`browser_bundle_size_under_budget` checks the bundle size in KB
gzipped:

```sh
gzip -9 < dist/browser/index.js | wc -c
```

The fixture's `meta.toml` declares the budget:

```toml
size_gzip_budget_bytes = 358400  # 350 KB
```

Failing the budget fails the gate. Bumping the budget requires an
explicit PR.

### Phase 18: npm Trusted Publishing

**Goal**: end-to-end publish flow including OIDC and Sigstore
provenance.

**Fixtures**: 2.

- `publish_dryrun_trusted_publishing_npm`,
  `publish_dryrun_trusted_publishing_jsr`.

**Gate**: `npm publish --dry-run --provenance --access public` exits
0 when the OIDC token claims match the configured npm trust.
`deno publish --dry-run` exits 0 when JSR's OIDC validation passes.
Real publish runs only on release tags, not PR CI.

## Total fixture count target

Approximate target by Phase 18: 400 fixtures.

Running total (cumulative):

- After Phase 1: 1
- After Phase 2: 15
- After Phase 3.1: 35
- After Phase 3.2: 57
- After Phase 3.3: 73
- After Phase 3.4: 91
- After Phase 4: 113
- After Phase 5: 133
- After Phase 6: 151
- After Phase 7: 173
- After Phase 8: 187
- After Phase 9: 207
- After Phase 10: 223
- After Phase 11: 245
- After Phase 12: 259
- After Phase 13: 269
- After Phase 14: 281
- After Phase 15: 287
- After Phase 16: 290
- After Phase 17: 302
- After Phase 18: 304

The 400 target leaves room for ad-hoc fixtures added post-phase
(regression captures, user bug reports, runtime-specific edge
cases). We expect about 100 such fixtures to land between Phase 18
ratification and v1 release.

The four-runtime matrix multiplies the effective fixture-runtime
combination count. 304 fixtures x 4 runtimes = 1216 fixture
executions per CI run, not counting the three Playwright browser
projects (which adds 304 x 3 = 912 browser executions for the
fixtures that include browser specs). Total CI gate executions
per release at Phase 18: about 2100.

## Go test wrappers

Following the existing `tests/transpiler3/c/` precedent, each phase
gets a Go test file:

```go
// tests/transpiler3/typescript/phase1_helloworld_test.go
package typescript_test

import (
    "testing"

    "mochi/tests/transpiler3/typescript/runner"
)

func TestPhase1HelloWorldTypeScript(t *testing.T) {
    runner.RunPhase(t, "phase1", "helloworld")
}
```

The `runner.RunPhase` helper:

1. Loads the fixture directory under `fixtures/<phase>/<name>/`.
2. Reads `meta.toml` for skip flags (`skip_browser`, `skip_bun`,
   `skip_deno`, etc.).
3. Runs vm3 on `source.mochi` to capture the reference stdout.
4. Compares vm3 stdout to `expect.txt` (sanity check).
5. Invokes the Mochi transpiler with `--target=typescript`.
6. Writes emitted TypeScript to a temp directory.
7. Runs Tier 2: `tsc --noEmit --strict ...`.
8. Runs Tier 3: `eslint --max-warnings 0`.
9. Runs Tier 4: `prettier --check`.
10. Runs `tsc --build` to emit JS for each runtime.
11. Runs Tier 5 per runtime: Node, Deno, Bun, browser (if not
    skipped).
12. Compares each runtime's stdout to `expect.txt` (Tier 1 master
    gate).
13. Runs Tier 6: `npm pack` + install + smoke test.
14. (Phase 16+) Runs Tier 7: reproducibility build + SHA diff.
15. (Phase 17+) Runs Tier 8: `deno publish --dry-run` +
    `npm publish --dry-run --provenance`.

Failures at any tier print a diff and exit with a tier-specific code
so CI can surface which tier failed.

The runner is parallelised: each fixture runs in its own goroutine
with its own temp dir. The Tier 6 build step is the bottleneck (~6
seconds per fixture for cold `npm install`); we cap parallelism at
`GOMAXPROCS / 2` to avoid disk IO contention.

## CI matrix

The full per-release CI matrix:

| OS              | Runtime  | Version    | Notes                  |
|-----------------|----------|------------|------------------------|
| ubuntu-24.04    | node     | 22.7.0     | floor                  |
| ubuntu-24.04    | node     | 22.8.0     | latest patch           |
| ubuntu-24.04    | deno     | 2.0.x      | floor                  |
| ubuntu-24.04    | deno     | 2.1.x      | next                   |
| ubuntu-24.04    | bun      | 1.1.x      | floor                  |
| ubuntu-24.04    | browser  | chromium   | Playwright             |
| ubuntu-24.04    | browser  | firefox    | Playwright             |
| ubuntu-24.04    | browser  | webkit     | Playwright             |
| ubuntu-24.04-arm | node    | 22.7.0     | ARM verification       |
| macos-14        | node     | 22.7.0     | Apple Silicon          |
| macos-14        | deno     | 2.0.x      | Apple Silicon          |
| macos-14        | bun      | 1.1.x      | Apple Silicon          |
| windows-2022    | node     | 22.7.0     | Windows                |
| windows-2022    | deno     | 2.0.x      | Windows                |

14 cells. Tier 1-6 gates run on every cell. Reproducibility (Tier 7)
runs on the three non-Windows linux + macos node cells. JSR dry-run
(Tier 8) runs on ubuntu-24.04 / deno 2.0.x. Jupyter kernel tests run
only on ubuntu-24.04 / deno (Phase 17). Bun is not tested on Windows
because Bun's Windows support is still listed as preview as of 2026.

Each cell takes about 12 minutes to clear all tiers for the full
fixture set as of Phase 18 (304 fixtures, 400 with regression). Total
CI wall-clock per release: about 90 minutes for the test job (14
cells run in parallel; the slowest cell is the bottleneck), plus 10
minutes for the build / reproducibility / publish jobs.

We do NOT test on macOS x86_64. GitHub deprecated x86_64 macOS
runners in 2024; ARM is the default.

## Test stability: pinned tool versions

The gate must not drift due to checker upgrades. We pin exact
versions:

| Tool        | Version  | Reason                                       |
|-------------|----------|----------------------------------------------|
| typescript  | 5.6.2    | stable strict mode + ES2024 lib              |
| eslint      | 9.12.0   | flat config stable                            |
| prettier    | 3.3.3    | format stable                                 |
| @typescript-eslint | 8.8.0 | strict-type-checked stable                |
| esbuild     | 0.24.0   | ESM emit stable                               |
| playwright  | 1.48.0   | webkit 17.4 supported                         |
| node        | 22.7.0   | latest LTS patch                              |
| deno        | 2.0.x    | latest stable v2                              |
| bun         | 1.1.x    | latest stable v1                              |

The pins live in `package.json` `devDependencies` and in
`package-lock.json` (auto-pinned from the spec). Node / Deno / Bun
versions are pinned via `actions/setup-node@v4`, `denoland/setup-deno@v2`,
`oven-sh/setup-bun@v2` in the CI workflow.

We bump pins quarterly in a dedicated PR with the diff of new
diagnostics (any new strict-mode error becomes a fix-in-PR; any new
warning becomes an audit ticket). The TypeScript major-version
boundary (5.6 -> 5.7 -> 5.8 -> 6.0) is a special case: we evaluate
the release notes for breaking changes and either bump or block, with
a documented decision.

## Golden file management

Each fixture has `expect.txt` (Tier 1 master gate). Conventions:

- Line endings: LF (`\n`), not CRLF. Enforced via `.gitattributes`
  `* text=auto eol=lf`.
- Trailing newline: file ends with `\n`. Mochi's print adds a
  trailing newline by default.
- UTF-8 encoding, no BOM.
- For floats: emit uses an explicit `toFixed` or `Intl.NumberFormat`
  to align with vm3's truncation.
- For bigints: emit prints without the `n` suffix to match vm3.

Regenerating goldens: `mochi tests regen --phase=<n>` runs vm3 on
every fixture in the phase and overwrites `expect.txt`. This is a
last-resort tool; in normal flow goldens are stable.

## Error path testing

Not every fixture tests the success path. Some test the error path:

- Phase 3.1: `list_index_out_of_bounds` tests the runtime helper's
  thrown `MochiBoundsError` with a Mochi-aware error message.
- Phase 5: `sum_match_non_exhaustive_error` tests that the emit's
  `assertNever` default catches a non-exhaustive match at compile
  time (tsc strict enforces) and at runtime (the assert throws).
- Phase 9: `agent_child_failure_restart` tests cancellation +
  restart paths via `AbortController`.
- Phase 11: `aggregate_error_nested` tests `AggregateError` with
  nested causes.

Error-path fixtures have `expect.txt` matching the expected error
message verbatim. Error message stability is a contract: changing
the emitted error text breaks fixtures. We bump fixtures
intentionally when the error format is improved.

## Platform-specific tests

Some fixtures test platform-specific behaviour. Phase 12 (FFI) has
`ffi_platform_specific_linux`, `ffi_platform_specific_macos`,
`ffi_platform_specific_windows`. The runner reads `meta.toml`:

```toml
platforms = ["linux"]
runtimes = ["node"]
```

and skips the fixture on other platforms / runtimes. Skipping is
logged in CI output, not silent.

Phase 16 reproducibility excludes Windows by similar mechanism: the
reproducibility job has `if: runner.os != 'Windows'` in the
workflow.

## Performance gates

Some phases enforce a wall-clock budget per fixture:

- Phase 8 (datalog): 5 seconds per fixture on github-hosted
  ubuntu-24.04.
- Phase 9 (agents): 2 seconds per fixture (longer ones use
  `AbortSignal.timeout(2000)`).
- Phase 13 (LLM with mock): 1 second per fixture.

Wall-clock gates are noisy on shared runners. We use a 2x tolerance
(`timeout * 2` triggers a flake re-run before failing). After two
consecutive fails the test is marked failed.

Bundle size budgets (Phase 17): browser bundle gzipped under 350 KB.
The budget is checked in CI; bumping requires an explicit PR with
justification.

## Snapshot stability

The emitted TypeScript source itself is not a gate. We do not
snapshot emit output. Reasons:

- Emit output changes frequently as the emitter is refactored.
- The gate is on observable behaviour (Tier 1) and the static
  artifact (Tier 6), not on the intermediate source.
- Snapshotting emit output creates churn: every refactor breaks
  thousands of golden files.

If a developer wants to inspect the emit, `mochi transpile
--target=typescript --print-source` prints to stdout without writing
files. No CI gate compares the print.

## Browser-specific gate considerations

The browser runtime (Playwright + chromium / firefox / webkit)
introduces gate concerns the Node / Deno / Bun runtimes do not have:

1. **Console output capture**. Browsers do not write to stdout. The
   fixture HTML overrides `console.log` and writes to a DOM
   element; Playwright reads the element. The capture must be
   lossless: every `console.log` call must appear, in order, with
   correct stringification.
2. **Async settlement**. Browser tests need an explicit "done" signal
   because the page does not exit. We use `window.__mochiDone =
   true` at the end of `main`; Playwright `waitForFunction` blocks
   until set.
3. **CORS / file:// URLs**. ESM modules loaded via `file://` URLs hit
   CORS restrictions in some browsers. We serve fixtures via a local
   HTTP server (`python3 -m http.server` or `npx serve`) for
   Playwright runs.
4. **Browser version skew**. Chromium / firefox / webkit on
   Playwright are pinned to specific versions. Playwright 1.48
   bundles chromium 130, firefox 131, webkit 18.0. Bumping
   Playwright bumps the browser versions; we audit the browser
   release notes for ES2024 / iterator helper / Set methods
   regressions.
5. **Headless rendering**. We run Playwright in headless mode for
   speed. Some tests (the ones that visually verify DOM rendering)
   are tagged `@headed-required` and run on a separate cell.

The browser cell is the slowest in the matrix because of the
Playwright cold start (about 15 seconds per browser project per
fixture). We mitigate by running fixtures in parallel within a single
Playwright run (Playwright handles parallelism internally).

## Comparison to MEP-50 (Kotlin) and MEP-51 (Python) test gates

| Concern              | MEP-50 (Kotlin)         | MEP-51 (Python)              | MEP-52 (TypeScript)             |
|----------------------|-------------------------|------------------------------|---------------------------------|
| Master gate          | vm3 byte-equal stdout   | vm3 byte-equal stdout         | vm3 byte-equal stdout (4 runtimes) |
| Compile gate         | `kotlinc -Werror`       | mypy + pyright (both strict)  | tsc --strict + extras           |
| Lint gate            | `ktlint`                 | `ruff check`                  | eslint 9 + strict-type-checked   |
| Format gate          | `ktlint --format check` | `ruff format --check`         | `prettier --check`              |
| Build gate           | `gradle build` + jar    | `uv build` + wheel install   | `tsc --build` + `npm pack` + install |
| Reproducibility      | `gradle --reproducible` | `SOURCE_DATE_EPOCH` + sort   | `SOURCE_DATE_EPOCH` + sort      |
| Notebook gate        | Kotlin Notebook          | ipykernel                     | Deno Jupyter                    |
| OIDC publish         | central-portal OIDC     | PyPI Trusted Publishing       | npm Trusted Publishing + JSR    |
| Runtime matrix       | JVM 17 / 21 (1 to 2)   | CPython 3.12 / 3.13 (1 to 2) | Node / Deno / Bun / browser (4) |

Three differences worth calling out:

1. **Four-runtime matrix**. Only MEP-52 has a four-runtime matrix.
   The matrix multiplies CI cost; we accept it because the
   four-runtime story is the user-facing value proposition for
   MEP-52.
2. **Browser gate**. MEP-50 has Kotlin/JS (compiles to JS, runs in
   browser via Webpack); MEP-51 has Pyodide (CPython compiled to
   WASM, runs in browser). Both are deferred to v2 (MEP-50 F3,
   MEP-51 F3). MEP-52 has the browser as a v1 target via Playwright
   + esbuild.
3. **Publish gate (dual)**. MEP-50 publishes to Maven Central only;
   MEP-51 publishes to PyPI only; MEP-52 publishes to both npm and
   JSR. Both publish dry-runs are gated.

## Open questions

1. **Bun's bundler vs esbuild**. Bun 1.1 has a built-in bundler.
   We use esbuild for the browser bundle because esbuild's ESM
   output is more mature. v2 may evaluate Bun's bundler.
2. **WebAssembly tests**. Phase 12 has WASM via N-API for Node and
   WebAssembly for browser. We do not have a comprehensive WASM
   test plan in v1; Phase 12 covers the basic cases. v2 expands.
3. **JSR rate limits**. JSR publishing has rate limits (currently
   undocumented; observed about 10 publishes per hour per org).
   The dry-run path avoids rate limits but the publish path can be
   rate-limited. We document the recovery (wait + retry).
4. **Deno Jupyter version skew**. Deno's Jupyter integration has
   been stable since 2024-04 but the kernel protocol can shift.
   We pin the Deno version and the kernelspec format.

## References

- TypeScript 5.6 / 5.7 release notes,
  `devblogs.microsoft.com/typescript/`
- tsc CLI reference, `typescriptlang.org/docs/handbook/compiler-options.html`
- eslint 9 flat config, `eslint.org/docs/latest/`
- @typescript-eslint configs, `typescript-eslint.io/users/configs/`
- prettier configuration, `prettier.io/docs/en/configuration`
- Playwright documentation, `playwright.dev`
- Playwright browser versions, `playwright.dev/docs/release-notes`
- Node 22 LTS release notes, `nodejs.org/en/blog/release/v22.0.0`
- Deno 2.0 release notes, `deno.com/blog/v2.0`
- Bun 1.1 release notes, `bun.sh/blog/bun-v1.1`
- esbuild bundling guide, `esbuild.github.io/api/`
- npm Trusted Publishing, `docs.npmjs.com/trusted-publishers/`
- JSR documentation, `jsr.io/docs`
- The shared decisions anchor for the load-bearing decisions
- [[10-build-system]] for the build pipeline this gate tests
- [[12-risks-and-alternatives]] for divergence + tool-version risks
