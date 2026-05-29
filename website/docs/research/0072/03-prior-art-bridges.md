---
title: "MEP-72 Note 03: Prior-art bridges"
sidebar_position: 4
sidebar_label: "03. Prior-art bridges"
description: "Survey of prior bridges and tooling in the TypeScript / JavaScript ecosystem: dts-gen, ts-morph, api-extractor, dts-bundle-generator, swc, esbuild, Bun's bundler, Deno's deno_npm resolver, tsd, expect-type, tsc --build, Cloudflare's worker-types, the @types/node ambient declarations. What each gets right, what each requires the user to write, and what MEP-72 borrows."
---

# 03. Prior-art bridges

This note surveys the bridges and ingest tools the TypeScript / JavaScript ecosystem has produced, identifies what each gets right and what each requires the user to write, and documents what MEP-72 borrows.

## 1. dts-gen (Microsoft, deprecated 2023)

The original `dts-gen` tool ran a JavaScript runtime over a pure-JS package and observed every property access, every function call, every return type via runtime introspection, then emitted a best-effort `.d.ts`. It worked well for shallow APIs and failed silently on:

- Class instances whose state was set in constructors and not observed by the runtime sample.
- Conditional returns that depend on input.
- `this` binding in callback-style APIs.

What MEP-72 borrows: nothing. The bridge ingests the package's published `.d.ts` (not a generated one), so the dts-gen failure modes do not apply. The fallback path (DT companion types) covers the dts-gen niche.

## 2. ts-morph (David Sherret, ongoing)

`ts-morph` is a high-level wrapper around the TypeScript compiler API that exposes a more ergonomic object-oriented surface for AST manipulation. It handles symbol resolution, declaration walking, type printing, and source code transformation with less ceremony than the raw compiler API.

What MEP-72 borrows: the structural lesson that wrapping the compiler API simplifies downstream consumption. The bridge's `ts-ingest` helper does a smaller version of this: a focused walk that emits ApiSurface JSON rather than ts-morph's general-purpose AST traversal.

## 3. api-extractor (Microsoft Rush stack)

`api-extractor` consumes a TypeScript project, rolls up its `.d.ts` tree into a single API report file, and validates the public API against a checked-in baseline. It is the de facto tool for tracking API stability in large TypeScript projects (used by Microsoft's Rush, by Adobe's stack, by Google's Firebase JS SDK).

What MEP-72 borrows: the rollup-via-the-compiler-API approach. When a consumed package's `.d.ts` tree is multi-file, the bridge's `ts-ingest` helper invokes an internal rollup pass (similar to api-extractor's but simpler, since the bridge only needs ApiSurface JSON, not human-readable API reports).

## 4. dts-bundle-generator (timocov)

A simpler `.d.ts` rollup tool that produces a single `.d.ts` file from a multi-file `.d.ts` tree. Used by smaller libraries that do not want the full api-extractor ceremony.

What MEP-72 borrows: the lightweight rollup approach. The bridge uses dts-bundle-generator-style logic internally when the package's `.d.ts` tree is shallow enough that api-extractor's heavier machinery would be overkill.

## 5. swc and esbuild

Both are fast TypeScript / JavaScript bundlers written in Rust (swc) and Go (esbuild). Both have their own TypeScript type-elimination passes (they strip types at bundle time without invoking the official compiler).

What MEP-72 borrows: nothing for type ingest (swc and esbuild do not produce types; they only strip them). esbuild is the fallback bundler for the MEP-52 Phase 17 browser bundle path when `bun build` is unavailable; that is independent of MEP-72.

## 6. Bun's built-in bundler

Bun 1.1 ships its own bundler (`bun build`) and TypeScript runtime. The bundler handles ESM + CJS + JSX + TypeScript without external dependencies.

What MEP-72 borrows: the no-`node_modules`-required bundling story for the browser-target emit. MEP-52 Phase 17 already uses `bun build` as the primary bundler; MEP-72 inherits the choice.

## 7. Deno's deno_npm resolver

Deno's npm support (GA 2023) resolves npm package specifiers via a built-in resolver that downloads tarballs from the npm registry, verifies integrity, and exposes them via the `npm:` import specifier. The resolver respects `package.json` `exports` maps, peer dependencies, and the CJS-ESM interop rules.

What MEP-72 borrows: the dual-registry pattern (`npm:` and `jsr:` specifiers). The Mochi bridge's npm-registry client and JSR-registry client are independent reimplementations in Go, but the surface (the `npm:` / `jsr:` specifier syntax) is intentionally Deno-compatible so Deno-emitted Mochi binaries can use the same import strings.

## 8. tsd and expect-type

Both are TypeScript-type-test frameworks: the user writes `.test-d.ts` files asserting that certain types resolve to certain shapes, and the framework runs the assertions via the compiler API.

What MEP-72 borrows: the assertion shape. The bridge's gate suite uses tsd-style assertions to verify that the synthesised Mochi shim file produces a TS-side type matching the original ApiSurface, catching regressions in the type-mapping pass.

## 9. tsc --build (project references)

TypeScript's `--build` mode supports incremental composite builds across multiple TypeScript projects via the `references` field in `tsconfig.json`. Each referenced project produces its own `.d.ts` and `.js` artefacts; downstream projects consume them as if they were external packages.

What MEP-72 borrows: the conditional `exports` map with `types` + `import` + `require` conditions. The `TargetNpmLibrary` emit produces a `package.json` that wires all three conditions; downstream tsc + Node consumers resolve the right artefact per condition.

## 10. Cloudflare worker-types and @cloudflare/workers-types

Cloudflare's Workers runtime ships a separate `.d.ts` package describing the runtime's API surface (separate from `@types/node` because Workers is a stripped-down V8 environment without `node:fs` etc.). Vercel Edge and Deno Deploy each ship their own analogues.

What MEP-72 borrows: the edge-runtime capability model. The `[ts.capabilities]` table's `worker` flag and the bridge's edge-runtime gate (phase 17) draw on this convention.

## 11. @types/node, @types/react, DefinitelyTyped

The DT repository ships type definitions for ~10K packages whose maintainers do not (or did not) ship their own `.d.ts`. DT is a community-maintained type-definition mono-repo with strict review rules.

What MEP-72 borrows: the fallback path. When a consumed package ships no `.d.ts`, the bridge looks for a DT companion (`@types/<pkg>`) and ingests it as the secondary source. The lockfile records the source.

## 12. Pyodide / JSPyBridge (Python-JS interop)

Two examples of "run JS inside Python" bridges. They use a serialisation layer (Python pickle ↔ JSON) to cross the runtime boundary. The cost is per-call serialisation.

What MEP-72 borrows: nothing structural. The TS / JS bridge does not need a serialisation layer because Mochi-emitted JS and consumed JS share the same runtime. (Pyodide-like layers are relevant to MEP-72's sister Python bridge, not to TS.)

## 13. Cross-references

- [[01-language-surface]] — what the Mochi user sees.
- [[04-tsdoc-dts-ingest]] — the compiler API ingest details.
- [[09-esm-cjs-interop]] — the CJS-vs-ESM dual-package hazard.
