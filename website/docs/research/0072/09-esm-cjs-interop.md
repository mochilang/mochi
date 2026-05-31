---
title: "MEP-72 Note 09: ESM vs CJS interop"
sidebar_position: 10
sidebar_label: "09. ESM vs CJS"
description: "The CJS vs ESM module shape distinction in npm packages, the `exports` map conditional resolution model, the dual-package hazard, the Node 22 / Deno 2 / Bun 1.1 / browser per-runtime interop semantics, the browser bundle path's CJS rejection."
---

# 09. ESM vs CJS interop

This note describes the CJS vs ESM impedance mismatch the bridge has to handle, and the per-runtime resolution semantics. It is informative.

## 1. The two module systems

CommonJS (CJS) is Node's original module system:

```javascript
// foo.cjs (CJS)
exports.bar = function () { return 42; };

// consumer.cjs
const { bar } = require("./foo.cjs");
```

ECMAScript Modules (ESM) is the TC39-standardised module system:

```typescript
// foo.mjs (ESM)
export const bar = () => 42;

// consumer.mjs
import { bar } from "./foo.mjs";
```

A package on npm can ship CJS-only (`"main": "index.cjs"`), ESM-only (`"type": "module"` + `"main": "index.mjs"`), or dual (`"exports": { "import": "./index.mjs", "require": "./index.cjs" }`).

## 2. The `exports` map conditional resolution

The `package.json` `exports` map (Node 12+, May 2019; finalised in Node 16) lets a package author publish per-condition entry points:

```json
{
  "exports": {
    ".": {
      "types": "./dist/index.d.ts",
      "import": "./dist/index.mjs",
      "require": "./dist/index.cjs"
    },
    "./sub": {
      "types": "./dist/sub.d.ts",
      "import": "./dist/sub.mjs"
    }
  }
}
```

The conditions are evaluated in order:

- `types`: consumer is a TypeScript compiler.
- `import`: consumer is an ESM module.
- `require`: consumer is a CJS module.
- `default`: fallback.

Custom conditions (`worker`, `browser`, `node`, `deno`, `bun`) extend the model.

The bridge's `TargetNpmLibrary` emits all of `types`, `import`, `require` so downstream consumers in any environment resolve correctly.

## 3. The dual-package hazard

Some packages ship CJS + ESM duals where the CJS path and the ESM path expose different module instances (different module state). A package that maintains a singleton (e.g., a connection pool) under CJS may end up with two copies if a downstream dep imports it via CJS while the host imports it via ESM. This is the "dual package hazard" (Node.js Modules WG, 2019).

MEP-72's bridge:

- The Mochi-emitted JS is always ESM (MEP-52 phase 1's default).
- When a consumed package ships dual, the bridge picks the ESM path (the `import` condition).
- When a consumed package's ApiSurface (read from `.d.ts`) differs between the CJS and ESM paths, the bridge emits a SkipReport for the differing items. This is rare in practice (the `.d.ts` is usually the same regardless of CJS / ESM resolution).
- When a consumed transitive dep depends on a package via CJS and the host depends on it via ESM, the bridge emits a build-time warning. The user resolves by aligning the resolution path (typically by pinning the transitive dep's version to one that ships dual or ESM-only).

## 4. Per-runtime interop semantics

### Node 22 LTS

Node 22 LTS supports both CJS and ESM. The runtime auto-detects via:

- `package.json` `"type": "module"` flag (file extension fallback to `.mjs` / `.cjs`).
- The `exports` map's `import` / `require` conditions.

`import foo from "cjs-pkg"` on Node 22 LTS returns the CJS module's `module.exports` as the default import. Named imports work via the `__esModule` flag interop.

### Deno 2

Deno 2 supports both CJS (via the `npm:` specifier and built-in CJS interop) and ESM (the native module system). The runtime resolves through the `node_modules` tree (when running under Node compat mode) or through the JSR cache (when running under native Deno).

### Bun 1.1

Bun 1.1 supports both CJS and ESM natively. The runtime auto-detects per-file. Bun's CJS-to-ESM interop is faster than Node's (Bun caches the CJS module's exports as ESM-shaped on first load).

### Browser

The browser supports only ESM (via `<script type="module">` or via `import` statements). CJS-only packages cannot run on the browser without a bundler that converts CJS to ESM (Webpack, Rollup, esbuild, bun build).

MEP-52 Phase 17's `TargetBrowserBundle` path uses `bun build` (primary) or `esbuild` (fallback) to handle the conversion. The bridge surfaces CJS-only deps in the lockfile so the browser-bundle gate catches them at build time.

### Edge runtimes (Cloudflare Workers, Vercel Edge, Deno Deploy)

Each edge runtime supports ESM only. CJS packages are rejected at deploy time.

## 5. The browser bundle path's CJS rejection

When the build target is `TargetBrowserBundle` AND a consumed package is CJS-only:

1. The build fails at the bundler step with a clear diagnostic.
2. The diagnostic names the CJS-only package.
3. The diagnostic suggests `npm:<pkg>?esm` (Deno-side) or asking the upstream maintainer to ship ESM.

## 6. The `node:` import surface

Node 22 LTS, Deno 2, and Bun 1.1 expose Node built-ins under the `node:` prefix:

- `node:fs`
- `node:net`
- `node:http`
- `node:child_process`
- `node:worker_threads`
- `node:crypto`

The browser does NOT support `node:` imports. A consumed package that imports `node:fs` is browser-incompatible; the bridge surfaces this in the lockfile's `capabilities-declared` field.

The bridge's edge-runtime gate (phase 17) refuses `node:fs`, `node:net`, `node:child_process`, `node:worker_threads`, and other non-edge-supported Node built-ins.

## 7. Cross-references

- [[10-runtime-target-matrix]] — what each runtime supports.
- [[11-browser-bundle-surface]] — the browser bundle path.
- [MEP-52 phase 17 implementation tracking](/docs/implementation/0052/phase-17-jsr-jupyter-browser) — the browser bundle implementation.
