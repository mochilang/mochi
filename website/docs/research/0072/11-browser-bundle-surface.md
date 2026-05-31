---
title: "MEP-72 Note 11: Browser bundle surface"
sidebar_position: 12
sidebar_label: "11. Browser bundle"
description: "How consumed npm + JSR packages reach the browser via the MEP-52 Phase 17 browser bundle path: tree-shaking rules, the `node:` import rejection, the WebAssembly capability flag, the 50 KB size budget, the source-map convention."
---

# 11. Browser bundle surface

This note describes how consumed packages reach the browser via the MEP-52 Phase 17 `TargetBrowserBundle` path. It is informative.

## 1. The bundle pipeline

The MEP-52 Phase 17 `TargetBrowserBundle` target invokes:

```bash
bun build dist/browser/index.ts \
    --target=browser \
    --format=esm \
    --outfile=dist/bundle/index.js
```

(Fallback: `esbuild --bundle --format=esm --target=es2024 --platform=browser --tree-shaking=true`.)

For MEP-72, the bridge extends the pipeline:

1. Before invoking the bundler, the bridge walks every `[[npm-package]]` and `[[jsr-package]]` in the lockfile.
2. For each package, the bridge checks the `engines.node` and `engines.workerd` fields and any imports of `node:fs`, `node:net`, `node:child_process`, `node:worker_threads`, `node:cluster`.
3. Any incompatible package fails the build with a clear diagnostic.
4. The bundler then runs against the synthesised import-map plus the materialised `node_modules` + `jsr_cache`.

## 2. Tree-shaking rules

The bundler's tree-shaking pass requires:

- Consumed packages must declare `"sideEffects": false` in their `package.json` (or list the side-effect files explicitly).
- Consumed package's ESM exports must be analysable (no dynamic `import()` of internal modules, no `Function` constructor).

The bridge does NOT modify consumed packages' tree-shaking annotations; it propagates them as-is. The MEP-52 Phase 17 `TestPhase17BundleSize` gate (50 KB hello-world budget) catches the most common regression where a fat dep got pulled in by mistake.

## 3. The `node:` import rejection

The bridge's browser-bundle pre-flight pass refuses any package that imports:

- `node:fs`, `node:fs/promises`
- `node:net`
- `node:http`, `node:https`
- `node:child_process`
- `node:worker_threads`, `node:cluster`
- `node:os`
- `node:path` (some browser-polyfill packages provide an alternative; the bridge refuses the raw import)
- `node:crypto` (the browser has `crypto.subtle` via WebCrypto; the bridge suggests this alternative)

The user can override per package via a manual `extern fn` declaration that provides a browser-side stub.

## 4. The WebAssembly capability flag

A consumed package that uses `WebAssembly.instantiate` (e.g., `sql.js`, `wabt`, `binaryen`) declares the `wasm` capability:

```toml
[ts.capabilities]
wasm = true
```

The bridge surfaces this in the lockfile. The browser bundle path passes the `.wasm` files through to the output bundle directory (next to `index.js`); the runtime loads them via the browser's `WebAssembly.instantiateStreaming` API.

## 5. The 50 KB size budget

MEP-52 Phase 17's `TestPhase17BundleSize` enforces a 50 KB budget for the hello-world bundle. MEP-72 extends this with:

- A configurable budget per Mochi package via `mochi.toml` `[ts.publish] browser-bundle-budget = "200KB"`.
- A regression gate: if the bundle grows by more than 10% across two consecutive locks, the CI emits a warning.

The bridge does NOT enforce a hard size cap on real-world bundles (the 50 KB hello-world budget is for the tree-shaking regression canary, not for user-shipped bundles).

## 6. Source-map convention

The bundler emits source maps to `dist/bundle/index.js.map` (matching the convention from MEP-52 Phase 16's reproducible-build gate). The maps point at:

- The Mochi-emitted TS source (under `dist/browser/`).
- The consumed packages' bundled source (under `node_modules/<pkg>/`).

The maps respect MEP-52 Phase 16's SOURCE_DATE_EPOCH normalisation; absolute paths in maps are rewritten to `mochi://<pkg>/<file>` so cross-host builds produce identical maps.

## 7. The browser-runtime sub-fixture corpus

The bridge's browser-bundle gate runs against a 6-package subset of the 24-package fixture corpus (the 6 packages known to be browser-compatible without polyfills):

- `zod`
- `valibot`
- `nanoid`
- `ts-pattern`
- `effect/cjs/Schema` (the schema subset)
- `dayjs`

The other 18 fixture packages either ship CJS-only, depend on `node:fs`, or otherwise require server-side context; they are gated only against Node + Deno + Bun.

## 8. Cross-references

- [[09-esm-cjs-interop]] — the CJS rejection rule.
- [[10-runtime-target-matrix]] — what the browser supports.
- [MEP-52 Phase 17 implementation tracking](/docs/implementation/0052/phase-17-jsr-jupyter-browser) — the underlying bundle path.
