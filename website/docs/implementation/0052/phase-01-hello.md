---
title: "Phase 1. Hello world"
sidebar_position: 2
sidebar_label: "Phase 1. Hello world"
description: "MEP-52 Phase 1, end-to-end pipeline from print(\"hello, world\") to a runnable TypeScript module on Node 22, Deno 2, Bun 1.1, and Chromium 130; both --target=typescript-source and --target=npm-package gates."
---

# Phase 1. Hello world

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 1](/docs/mep/mep-0052#phase-plan) |
| Status         | LANDED (1.0, 1.1, 1.3 partial: Node + Deno + Bun); browser deferred to Phase 17 |
| Started        | 2026-05-29 16:30 (GMT+7) |
| Landed         | 2026-05-29 16:50 (GMT+7) |
| Tracking issue | n/a |
| Tracking PR    | n/a |

## Gate

`TestPhase1Hello`: 5 fixtures green on all four tier-1 runtimes (Node 22.11.0 LTS, Deno 2.0, Bun 1.1.30, Chromium 130 via Playwright) and all four OS cells (x86_64-linux-gnu, aarch64-linux-gnu, aarch64-darwin, x86_64-windows). Secondary gates: `tsc --noEmit --strict --noUncheckedIndexedAccess --exactOptionalPropertyTypes --noImplicitOverride --noFallthroughCasesInSwitch --noPropertyAccessFromIndexSignature` produces zero diagnostics; `eslint --max-warnings 0` clean; `prettier --check` is a fixed point.

Fixtures:
1. `hello.mochi`: `print("hello, world")`, stdout `hello, world\n`
2. `hello_int.mochi`: `print(42)`, stdout `42\n`
3. `hello_bool.mochi`: `print(true)`, stdout `true\n`
4. `hello_float.mochi`: `print(3.14)`, stdout `3.14\n`
5. `hello_newline.mochi`: `print("line1\nline2")`, two lines

## Goal-alignment audit

Phase 1 is the first point where the TypeScript pipeline produces a runnable artefact. Before Phase 1, the Go packages under `transpiler3/typescript/` are stubs and the runtime TypeScript project compiles but does nothing. After Phase 1, `mochi build --target=typescript-source hello.mochi -o out/` writes a complete `package.json` + `tsconfig` chain + `src/generated/hello.ts` that compiles under `tsc --build` and runs identically on Node, Deno, Bun, and Chromium. Every later phase extends Phase 1's pipeline without replacing it.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 1.0 | `print("hello, world")` end-to-end: parse, typecheck, aotir lower, colour stub, tstree lower, emit `.ts`, execute on Node 22 | LANDED | (this PR) |
| 1.1 | `print(int)`, `print(bool)`, `print(float)` via inline `mochi_print_{str,i64,f64,bool}` helpers matching vm3 contract (incl. NaN/+Inf/-Inf canonical labels) | LANDED | (this PR) |
| 1.2 | `--target=typescript-source` writes the `.ts` layout without invoking `tsc`; `--target=npm-package` (composite tsconfig + `tsc --build` + `npm pack`) deferred to Phase 15 | LANDED (typescript-source); Phase 15 owns npm-package | (this PR) |
| 1.3 | Execute on Deno 2 (`deno run`), Bun 1.1, Chromium 130 (Playwright) | PARTIAL: Node + Deno + Bun green; browser deferred to Phase 17 (esbuild bundle target) | (this PR) |
| 1.4 | SHA-256 content-addressed build cache under `~/.cache/mochi/typescript/<hash>/` | DEFERRED (helper plumbing in `Driver.cacheKey` reserved; integration follows Phase 15 once tsc invocations dominate the wall-clock budget) | n/a |

## Sub-phase 1.0, End-to-end pipeline

### Goal-alignment audit (1.0)

The pipeline must produce a runnable artefact on the first sub-phase so that 1.1 through 1.4 each have something real to extend. `print("hello, world")` is the minimal non-trivial program: it exercises the entire pipeline (parser, type checker, aotir, colour pass, lower, emit, `tsc --build`, runtime) without requiring records, closures, or async.

### Decisions made (1.0)

**Pipeline entry point**: `Driver.Build(src, outDir, target Target)` in `transpiler3/typescript/build/build.go`:

1. `parser.Parse(src)` → AST
2. `types.Check(ast)` → typed AST
3. `aotir.Lower(typed)` → `*aotir.Program` (reused from MEP-45, unchanged)
4. `colour.Colour(prog)` → `ColourMap` (Phase 1: all functions sync)
5. `lower.Lower(prog, colours)` → `*tssrc.SourceFile`
6. `emit.Emit(sf, workDir)` → writes `src/generated/*.ts`
7. `emit.WriteProject(workDir)` → writes `package.json`, `tsconfig.base.json`, per-runtime `tsconfig.{node,deno,bun,browser}.json`, root composite `tsconfig.json`, `.eslintrc.json`, `.prettierrc.json`
8. For `--target=npm-package` only: subprocess `tsc --build` then `npm pack`

**Lowering of `print("hello, world")`**: `aotir.PrintStmt` with a `StringLit` lowers to an `ExprStmt` wrapping a `CallExpr` to `print` imported from `@mochi/runtime/io`:

```typescript
// src/generated/hello.ts
import { print } from "@mochi/runtime/io";

export function main(): void {
  print("hello, world");
}

main();
```

**Module naming**: Mochi source file `hello.mochi` → emitted `src/generated/hello.ts`. Package name in `package.json` defaults to `mochi-<pkgname>` (kebab-case). The `name` field is overridable via `mochi build --pkg-name=@scope/foo`.

**Entry point**: a generated `src/index.ts` re-exports `main` from the user module and imports it for side-effect execution. For programs whose main is sync, `src/index.ts` is:

```typescript
import { main } from "./generated/hello.ts";
main();
```

For async main (Phase 9 and later), `src/index.ts` becomes `await main();` (top-level await, ESM-only).

**`@mochi/runtime/io.print`**: Phase 1 adds the `io` sub-path immediately rather than calling `console.log` directly:

```typescript
// runtime3/typescript/src/io/index.ts
export function print(v: unknown): void {
  if (typeof v === "boolean") {
    console.log(v ? "true" : "false");
    return;
  }
  if (typeof v === "number") {
    if (Number.isNaN(v)) { console.log("NaN"); return; }
    if (v === Infinity)  { console.log("Infinity"); return; }
    if (v === -Infinity) { console.log("-Infinity"); return; }
    console.log(String(v));
    return;
  }
  if (typeof v === "bigint") { console.log(v.toString()); return; }
  console.log(String(v));
}
```

This indirection (a) lets tests redirect `console.log` to a buffer without touching generated code, (b) gives the browser bundle a single seam to swap for `document.body.appendChild` style output if needed, (c) centralises the `bool`/`float`/`bigint` formatting that has to match vm3 byte-for-byte.

**`tsc` subprocess vs in-process**: Phase 1 uses a `tsc --build` subprocess. In-process TypeScript Compiler API is rejected because it would force the Go build to depend on a JavaScript runtime. The cost is ~400ms cold-start for the TypeScript binary; Phase 1.4's SHA-256 cache amortises it.

## Sub-phase 1.1, Scalar print

### Decisions made (1.1)

**`print(int)`**: `aotir.PrintStmt` with `IntLit(42)` lowers to `print(42n)` by default (Mochi `int` defaults to `bigint`). Monomorphisation specialises to `number` when the IR proves the value fits in i53 and the producer never overflows. Phase 1's hello fixtures use small constants that all fit; the IR carries the chosen representation per occurrence so the emitter never mixes `bigint` and `number` in the same expression.

**`print(bool)`**: `print(true)` must print lowercase `true\n` to match vm3. `console.log(true)` natively prints `"true"` on all four runtimes (no need to stringify), but `print` routes through the helper above for symmetry with the `bigint`/`float` paths and for redirect-friendliness under tests.

**`print(float)`**: `print(3.14)` → `print(3.14)`. `String(3.14) === "3.14"`. Edge cases (NaN, +Inf, -Inf) match the vm3 output `"NaN"`, `"Infinity"`, `"-Infinity"`.

## Sub-phase 1.2, Two packaging targets

### Decisions made (1.2)

**`--target=typescript-source`**: writes the full project layout (see MEP-52 §16) and stops. The user (or downstream tooling like Vite, Next.js, or `bun run`) runs `tsc`. No subprocess invoked from `mochi build`.

**`--target=npm-package`**: writes the layout, runs `tsc --build` (which produces `dist/{node,deno,bun,browser}/`), then runs `npm pack` to produce `<pkg>-<ver>.tgz` in the output directory.

**`package.json` minimum for Phase 1**:

```json
{
  "name": "mochi-hello",
  "version": "0.0.1",
  "type": "module",
  "engines": { "node": ">=22.0.0" },
  "exports": {
    ".": {
      "types": "./dist/index.d.ts",
      "deno": "./dist/deno/index.js",
      "bun":  "./dist/bun/index.js",
      "browser": "./dist/browser/index.js",
      "node": "./dist/node/index.js",
      "default": "./dist/node/index.js"
    }
  },
  "files": ["dist", "README.md", "LICENSE"],
  "scripts": { "build": "tsc --build" },
  "dependencies": { "@mochi/runtime": "^0.1.0" }
}
```

## Sub-phase 1.3, Four-runtime execution

### Decisions made (1.3)

The same `dist/` tree is executed under each runtime; the test harness captures stdout and `diff`s against `expect.txt`.

- Node: `node dist/node/index.js`
- Deno: `deno run --allow-read --allow-env dist/deno/index.js` (Phase 1 needs only read+env; later phases tighten or open the permission set per fixture)
- Bun: `bun dist/bun/index.js`
- Chromium: Playwright loads `tests/transpiler3/typescript/harness/runner.html` which `<script type="module" src="dist/browser/index.js">`-imports the bundle and serialises `console.log` to a `<pre id="out">`; the harness reads the `<pre>` text and writes it to `stdout.txt`.

Playwright runs headless against `Chromium 130` from the npm `@playwright/test 1.48+` package, pinned per CI image.

## Sub-phase 1.4, SHA-256 build cache

### Decisions made (1.4)

**Cache key**: SHA-256 of `source_bytes || tsc_version_string || node_version_string || target_label`.

- `source_bytes`: raw bytes of the `.mochi` source file.
- `tsc_version_string`: from `tsc --version`, e.g., `"Version 5.6.3"`.
- `node_version_string`: from `node --version`, e.g., `"v22.11.0"`.
- `target_label`: `"typescript-source"`, `"npm-package"`, etc.

**Cache directory**: `~/.cache/mochi/typescript/` (XDG Base Directory). Overridable via `$MOCHI_CACHE_DIR`. Cache entry: `<key>/{src,dist,package.json,...}`.

**Hit path**: `os.Stat(cacheEntry)` succeeds, copy tree to `outDir`, return. Elapsed: roughly 5 ms to 50 ms depending on tree size.

**Miss path**: full pipeline, write output, copy to cache, return.

## Files

| File | Purpose |
|------|---------|
| `transpiler3/typescript/doc.go` | Package doc describing sub-packages and phase plan |
| `transpiler3/typescript/colour/colour.go` | Sync/async colour pass; Phase 1 trivially returns all-Blue; signature stable across phases |
| `transpiler3/typescript/tstree/tstree.go` | TS syntax-tree model: SourceFile, FuncDecl, ImportDecl, ExprStmt, ReturnStmt, RawStmt, CallExpr, IdentExpr, four literal types; each implements `TsString(indent int) string` |
| `transpiler3/typescript/lower/lower.go` | `aotir.Program` → `tstree.SourceFile`; CallStmt → inline mochi_print_{str,i64,f64,bool} runtime helpers; explicit error on unsupported stmt/expr kinds |
| `transpiler3/typescript/emit/emit.go` | `tstree.SourceFile` → `outDir/main.ts`; LF, two-space indent, single trailing newline |
| `transpiler3/typescript/build/build.go` | `Driver.Build`, Target enum (TypeScriptSource, NodeRun, DenoRun, BunRun); resolves node/deno/bun with MOCHI_NODE_PATH / well-known paths / PATH fallback |
| `transpiler3/typescript/build/build_test.go` | Per-runtime `runTsFixture` helper, `resolveRuntime`, fixture-loading test plumbing |
| `transpiler3/typescript/build/phase01_test.go` | `TestPhase1HelloNode` + `Deno` + `Bun`; `TestPhase1EmitWithoutRuntime`; `TestPhase1EmitFragments`; `TestPhase1DeterministicEmit`; `TestPhase1UnsupportedFails` |
| `tests/transpiler3/typescript/fixtures/phase01-hello/hello.mochi` | `print("hello, world")` |
| `tests/transpiler3/typescript/fixtures/phase01-hello/hello.out` | `hello, world\n` |
| `tests/transpiler3/typescript/fixtures/phase01-hello/hello_int.mochi` | `print(42)` |
| `tests/transpiler3/typescript/fixtures/phase01-hello/hello_int.out` | `42\n` |
| `tests/transpiler3/typescript/fixtures/phase01-hello/hello_bool.mochi` | `print(true)` |
| `tests/transpiler3/typescript/fixtures/phase01-hello/hello_bool.out` | `true\n` |
| `tests/transpiler3/typescript/fixtures/phase01-hello/hello_float.mochi` | `print(3.14)` |
| `tests/transpiler3/typescript/fixtures/phase01-hello/hello_float.out` | `3.14\n` |
| `tests/transpiler3/typescript/fixtures/phase01-hello/hello_newline.mochi` | `print("line1\nline2")` |
| `tests/transpiler3/typescript/fixtures/phase01-hello/hello_newline.out` | `line1\nline2\n` |

## Test set

- `TestPhase1HelloNode`, walks all 5 fixtures, runs the emitted `.ts` under Node 22's native TypeScript loader (`--experimental-strip-types` default-on since 22.18), diffs stdout byte-for-byte against `.out`.
- `TestPhase1HelloDeno`, same 5 fixtures executed via `deno run`. Skips gracefully when `deno` is missing.
- `TestPhase1HelloBun`, same 5 fixtures executed via `bun`. Skips gracefully when `bun` is missing.
- `TestPhase1EmitWithoutRuntime`, asserts the canonical hello fixture lowers to a TS source containing the expected helper names and call sites without invoking any JS runtime. Isolates the Go-side pipeline from the runtime gate.
- `TestPhase1EmitFragments`, locks the emitted TS shape for each of the four print families (str, i64, f64, bool) plus the embedded-newline fixture. A regression that breaks the f64 helper's NaN/+Inf/-Inf branches fails this test at Phase 1 (where the contract lives) instead of leaking into Phase 2.
- `TestPhase1DeterministicEmit`, runs `Driver.Build` five times against `hello.mochi` and asserts every output is byte-identical. Phase 16 reproducibility precondition: emit must already be deterministic at Phase 1 (no map-iteration leaks, no time/PID/random sources).
- `TestPhase1UnsupportedFails`, asserts the lowerer returns an `unsupported` error on `let x = 1`. Pins the "fail clear, not silent" contract.

## Deferred work

- In-process tsc (eliminates `tsc --build` subprocess startup). Deferred to Phase 16.
- Multi-file programs. Deferred to Phase 4 (records introduce multi-file layout).
- `--target=deno-jsr`, `--target=browser-bundle`, `--target=deno-jupyter`. Deferred to Phase 17.
- Source map second layer (Mochi to TS). Deferred (Open Q6, v1.5).
- Identifier mangling for reserved words (`class_`, `import_`). Phase 1 has no collisions; full table lands in Phase 4.
- `--target=npm-package` (composite `tsconfig` + `tsc --build` + `npm pack`). Phase 15 owns the full package-build gate; Phase 1 ships `--target=typescript-source` only.
- Chromium browser execution. Phase 17 owns the esbuild bundle target; Phase 1's gate covers the three Node-family runtimes (Node, Deno, Bun) that natively load `.ts`.
- SHA-256 build cache integration. Phase 1's `Driver.cacheKey` helper is plumbed but not yet consumed; integration follows Phase 15 once `tsc` invocations dominate the wall-clock budget.
