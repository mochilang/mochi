---
title: "MEP-52 research note 07, Runtime portability: Node x Deno x Bun x Browser"
description: "Node 22 LTS, Deno 2, Bun 1.1, and Browser matrix; conditional exports; ESM-only stance; per-runtime dist variants; JSR co-publish; WebContainer support."
sidebar_position: 7
---

# MEP-52 research note 07, Runtime portability: Node 22 LTS x Deno 2 x Bun 1.1 x Browser matrix, conditional exports, build variants

Author: research pass for MEP-52 (Mochi to TypeScript / JavaScript transpiler).
Date: 2026-05-23 17:05 (GMT+7).

This note pins down the runtime matrix for Mochi-emitted TypeScript and the pre-compiled JavaScript dist artifacts. The matrix is intentionally narrow: four tier-1 runtimes, each fed by a single TS source tree via `package.json` `exports` conditional routing, with one tsconfig project per runtime extending a common base. v1 ships ESM only, no CommonJS, no UMD, no IIFE. Node-only stdlib imports (`node:fs`, `node:net`, `node:child_process`, `node:os`, `node:path`, `node:url`) are quarantined inside `mochi_runtime/io/` so the browser bundle tree-shakes them out cleanly.

Companion notes: [[01-language-surface]], [[02-design-philosophy]], [[03-prior-art-transpilers]], [[04-runtime]], [[05-codegen-design]], [[06-type-lowering]], [[08-dataset-pipeline]], [[09-agent-streams]], [[10-build-system]], [[11-testing-gates]], [[12-risks-and-alternatives]].

Unlike Python (MEP-51) where the shipped artifact is one `py3-none-any` wheel over six platform tags, and unlike Kotlin (MEP-50) where artifacts fan out across JVM, Native, JS, and Wasm, the JavaScript story is wider per-runtime (four targets, each with its own conditional-exports entry point) and narrower per-architecture (the JS engine handles arch portability for us). The runtime matrix has three independent axes: the JS runtime (Node, Deno, Bun, Browser), the module format (ESM only), and the deployment artifact (npm tarball, JSR scope, browser-bundle ESM file).

The five load-bearing decisions, defended in [[02-design-philosophy]] §6 and stated here as operating assumptions:

1. **ESM only, no CommonJS.** Every emitted module declares `"type": "module"` in the nearest `package.json`. No `require`, no `module.exports`, no `__dirname` constants. The few places that need a directory anchor use `import.meta.url` plus `fileURLToPath`, both ES2020 standard.

2. **Conditional exports route imports per runtime.** A single source tree, four published variants under `dist/{node,deno,bun,browser}/`. The `exports` field of `package.json` maps the `node`, `deno`, `bun`, `browser`, and `default` conditions to the matching dist entry. Importers see one package; the resolver picks the right file.

3. **Node-only APIs live under `mochi_runtime/io/`.** Anything that calls `node:fs`, `node:net`, `node:os`, `node:child_process`, or any other Node-only module is reachable only via that subpath. The browser tsconfig project drops `mochi_runtime/io/` from its `compilerOptions.paths` map and supplies a `BrowserIo` stub instead.

4. **JSR is the canonical Deno registry.** The same TS source is published as `@mochi/runtime` on npmjs.org and on jsr.io. Deno users can `import { ... } from "jsr:@mochi/runtime"` (native) or `import { ... } from "npm:@mochi/runtime"` (compat). The two share one source, but JSR ships TS directly while npm ships the pre-compiled `dist/node/` JS plus `.d.ts`.

5. **`tsc --build` with per-runtime project files.** `tsconfig.base.json` declares strict mode, ES2024 target, ESNext module, isolated modules, no implicit any, etc. The four per-runtime files (`tsconfig.node.json`, `tsconfig.deno.json`, `tsconfig.bun.json`, `tsconfig.browser.json`) override only `outDir`, `lib`, `types`, and `paths`. One `npm run build` invocation runs all four projects in parallel.

---

## 1. The four tier-1 runtimes

| Runtime    | Floor       | First GA   | Module system | Stdlib origin       | Tier |
|------------|-------------|------------|---------------|---------------------|------|
| Node.js    | 22.0.0      | 2024-04-24 | ESM + CJS     | libuv + V8 + Node core | 1  |
| Deno       | 2.0.0       | 2024-10-09 | ESM only      | Web + `Deno.*` + npm compat | 1 |
| Bun        | 1.1.0       | 2024-04-01 | ESM + CJS     | JavaScriptCore + Zig core | 1 |
| Browser    | Baseline 2024 | n/a      | ESM only      | Web Platform        | 1   |

The four are tier 1: every gate runs on each per PR and merge. The runtime matrix collapses cleanly because Mochi's emit surface is the intersection of ES2024 plus the small set of Node-only APIs (which we isolate). Anything beyond that intersection is rejected at IR time.

### 1.1 Node.js 22 LTS

Node 22 is the **floor**. Released 2024-04-24, entered LTS 2024-10-29 (Active LTS through 2025-10, Maintenance LTS through 2027-04-30). The floor rationale:

- `Promise.withResolvers()` is natively available (V8 12.4+, ES2024 Stage 4). MEP-52's agent `call(req)` pattern relies on this. Node 20 LTS does not have it natively (requires the `--harmony-promise-with-resolvers` flag in 20.10 and earlier). Node 22 has it on by default.
- `Iterator.from`, `Iterator.prototype.map / filter / take / drop / flatMap / reduce / toArray / forEach / some / every / find` (V8 12.6+, ES2024 Stage 4). The query DSL ([[08-dataset-pipeline]]) lowers `from x in xs select f(x)` directly to `Iterator.from(xs).map(x => f(x))`.
- `Set` methods (`intersection`, `union`, `difference`, `symmetricDifference`, `isSubsetOf`, `isSupersetOf`, `isDisjointFrom`, V8 12.5+, ES2024). The set lowering ([[06-type-lowering]] §7) uses these directly.
- `Object.groupBy` and `Map.groupBy` (V8 12.1, ES2024). The query DSL `group by` lowers to `Map.groupBy(xs, x => key(x))`.
- `String.prototype.isWellFormed` and `String.prototype.toWellFormed` (V8 11.7, ES2024). Mochi string semantics treat UTF-16 lone surrogates explicitly, and `toWellFormed` is the canonical fix.
- `Array.prototype.toReversed / toSorted / toSpliced / with`, `TypedArray.prototype.toReversed / toSorted / with` (V8 11.0, ES2023). The immutable list ([[06-type-lowering]] §5) reads as `xs.toSorted((a, b) => a - b)` instead of `[...xs].sort(...)`.
- Built-in `fetch` (Node 18+, stable in 22). The HTTP client emit reuses `fetch` without polyfill.
- Built-in `WebSocket` (Node 22+, stable). For agent stream consumers that listen on websockets, no `ws` dep needed.
- Built-in `node:test` runner and `node:test/reporters` (Node 20.4+, stable in 22). Mochi golden tests can run via `node --test` without `jest` or `vitest`.
- Built-in `node:sqlite` (Node 22.5+, experimental). Not used by v1 emit but documented for v2 persistence.
- `--experimental-strip-types` (Node 22.6+) and `--experimental-transform-types` (Node 23+, backported to 22.7+). Node can execute `.ts` files directly without `tsc`. The Mochi-emitted source runs on Node 22.7+ with `node --experimental-strip-types myapp.ts`, though v1 gates require the pre-compiled `dist/node/*.js` instead.
- `node:zlib` `Promise`-based variants `zlib.gunzipPromise` etc. (Node 22+, formalised). Reduces our reliance on `util.promisify`.
- `EventTarget.timeout` and `AbortSignal.timeout` (Node 17.3+, stable). The agent supervision tree ([[09-agent-streams]]) uses `AbortSignal.timeout(5000)` for call timeouts.
- V8 12.x baseline brings WebAssembly GC, JSPI (JavaScript Promise Integration for Wasm), and faster `structuredClone`.

Node 20 LTS is below the floor because it lacks native `Promise.withResolvers` and the Iterator helpers (Stage 4 in 2024 but Node 20 froze before V8 12.x). Node 18 is below the floor and approaching end-of-life (EOL 2025-04-30). Node 24 is the next LTS (planned 2025-10) and will be advisory once it ships.

The Node 22 minor we pin for CI is **22.11.0** (released 2024-10-29, the first 22.x with full LTS status). Patches up to 22.17.x are accepted; the gate fails if `node --version` reports anything outside `22.x`. The `engines` field in `package.json`:

```json
{
  "engines": {
    "node": ">=22.0.0"
  }
}
```

`npm publish` warns but does not block on `engines` mismatch; we rely on `npm install`'s installer-side `engineStrict` (when set in `.npmrc`) and on `node --version` checks inside the Mochi runtime bootstrap. Bootstrapping line, emitted once at the top of `mochi_runtime/runtime.ts`:

```typescript
const nodeMatch: RegExpMatchArray | null =
  typeof process !== "undefined" && process.versions?.node
    ? process.versions.node.match(/^(\d+)/)
    : null;
if (nodeMatch !== null) {
  const major: number = parseInt(nodeMatch[1]!, 10);
  if (major < 22) {
    throw new Error(
      `mochi-runtime requires Node 22 or later, found ${process.versions.node}`,
    );
  }
}
```

The check is guarded behind `typeof process !== "undefined"` so the browser and Deno runtimes do not trip it.

### 1.2 Deno 2.x

Deno 2.0 (released 2024-10-09) is the **second tier-1**. Deno's runtime philosophy is "secure by default, web-platform first, npm compatible". Deno 2 is the first major to:

- Drop the legacy Deno-script-only mode in favour of full npm compatibility via `npm:` and `jsr:` specifiers.
- Stabilise the `deno publish` workflow for the JSR registry.
- Default to Node-compatible `package.json` resolution alongside `deno.json`.
- Add `deno install` (replacing `deno add`), `deno remove`, `deno outdated` for npm-style package management.
- Add `node:` builtin polyfills for almost the entire Node stdlib (`node:fs`, `node:net`, `node:child_process`, `node:os`, etc.) so npm packages targeting Node 22 run on Deno 2 unchanged.

For Mochi, the Deno target is interesting because:

- **No `package.json` needed**: Deno reads `deno.json` (or `deno.jsonc`) for tasks, imports, and the compiler config. Mochi generates both `package.json` and `deno.json` from the same metadata source.
- **TypeScript native**: Deno reads `.ts` source directly without `tsc`. The `dist/deno/` directory ships TS, not pre-compiled JS, when published to JSR. (For the npm route, Deno consumes the pre-compiled JS just like Node.)
- **Web-platform APIs**: `fetch`, `WebSocket`, `Blob`, `File`, `Request`, `Response`, `URL`, `URLSearchParams`, `crypto.subtle`, `crypto.randomUUID`, `TextEncoder`, `TextDecoder`, `structuredClone`, `MessageChannel`, `BroadcastChannel`, all present and stable.
- **`Deno.*` namespace**: Filesystem (`Deno.readFile`, `Deno.writeFile`, `Deno.open`), network (`Deno.connect`, `Deno.listen`), process (`Deno.Command`, `Deno.exit`). The Mochi `mochi_runtime/io/` shim dispatches: on Node, `node:fs` is called; on Deno, `Deno.readFile` is called; on Bun, `Bun.file().bytes()` is called; on browser, the IO surface throws.
- **Permissions**: Deno requires explicit `--allow-read`, `--allow-write`, `--allow-net`, etc. Mochi-generated `deno.json` declares the minimum set required for the user's program (computed from the IR's effects table).

Deno 1.x is below the floor because:

- JSR was experimental in 1.x (1.42+ added preview); 2.0 stabilised it.
- npm compatibility had gaps in 1.x (some Node packages broke); 2.0 closed them.
- The `Deno.serve` API and `Deno.Command` (replacing `Deno.run`) only landed near the 1.x end.

Deno 2.1+ adds `deno publish --token` improvements, `deno serve` (HTTP server with HTTPS+H2 baked in), `--unstable-temporal` flag for the Temporal proposal. Mochi v1 does not require Temporal; if v2 adopts it, the runtime check will gate on `Deno.version.deno >= "2.5"` for native Temporal support.

The Deno minor we pin for CI is **2.1.4** (December 2024). The `deno.json` we emit:

```jsonc
{
  "compilerOptions": {
    "strict": true,
    "noUncheckedIndexedAccess": true,
    "exactOptionalPropertyTypes": true,
    "lib": ["deno.window", "deno.ns", "es2024"]
  },
  "imports": {
    "@mochi/runtime": "jsr:@mochi/runtime@^0.1.0"
  },
  "tasks": {
    "build": "deno task build:dist",
    "test": "deno test --allow-read --allow-net",
    "fmt": "deno fmt"
  },
  "unstable": []
}
```

`unstable: []` is explicit: we never depend on unstable Deno APIs.

### 1.3 Bun 1.1.x

Bun 1.1 (released 2024-04-01) is the **third tier-1**. Bun is a JavaScriptCore-based (not V8) runtime written in Zig, designed to be a faster drop-in for Node. The 1.1 floor:

- Stable `Bun.file()`, `Bun.write()`, `Bun.serve()` (HTTP server), `Bun.spawn()` (subprocess), `Bun.password.hash` (bcrypt/argon2 native).
- Built-in `bun test` runner (Jest-compatible API but ~10x faster).
- Built-in `bun build` bundler (esbuild-compatible API but in-process; produces ESM, CJS, IIFE).
- Native `package.json` resolution (no `node_modules` walk; `bun install` produces `node_modules/` for compat but resolves via Bun's lockfile internally).
- Native TypeScript via JavaScriptCore + custom transpiler; runs `.ts` source directly without `tsc`.
- Native JSX via the same transpiler; React/JSX out of the box.
- Native `.env` loading: `process.env` populated from `.env.{NODE_ENV,test,production,development}` on startup.
- Most Node stdlib polyfilled: `node:fs`, `node:http`, `node:net`, `node:crypto`, `node:stream`, `node:path`, `node:url`, `node:os`, `node:child_process`.
- Web Platform APIs (Bun targets the same surface Node 22 ships): `fetch`, `WebSocket`, `Blob`, `Request`, `Response`, `crypto.subtle`, `URL`, `TextEncoder`, `TextDecoder`, `structuredClone`.
- `Promise.withResolvers`, `Iterator.from`, `Set` methods, `Object.groupBy`, `Array.prototype.toSorted` (JavaScriptCore tracks JavaScriptCore-WebKit-2024 baseline which includes all ES2024 Stage-4 proposals).

Bun's API differences from Node we work around:

- **`globalThis.Bun` vs `process.versions.bun`**: detect via `typeof Bun !== "undefined"`. The runtime bootstrap branches on this.
- **`Bun.file(path)` returns a `BunFile`** (not a `Buffer`); convert with `.bytes()`, `.text()`, `.arrayBuffer()`, `.json()`, `.stream()`. The IO shim normalises to a common Mochi `MochiFile` abstraction.
- **Default executor for `child_process.spawn` differs**: Bun uses posix_spawn directly; Node uses libuv. We do not observe a difference for typical workloads.
- **HTTP server**: `Bun.serve({fetch})` is a fetch-handler-style API; Node uses `http.createServer((req, res) => ...)`. Mochi emits the `Bun.serve` form for the Bun target and the `node:http` form for the Node target; both go through the same Mochi `Server` abstraction.
- **`bun build` vs `tsc`**: Bun's bundler is faster but produces slightly different output (Bun targets `node` semantics in its CJS-emitting bundles). We do not rely on `bun build` for the canonical artifact; `tsc --build` is canonical, and `bun build` is documented as an alt for users who want a single Bun-native bundle.

Bun 1.0 (September 2023) is below the floor because:

- `Bun.serve` was experimental.
- `bun test` was unstable.
- `bun install` lockfile format changed between 1.0 and 1.1.

Bun 1.2 (November 2024) and later add S3 client, SQLite, native PostgreSQL, native Redis. Mochi v1 does not require these; the `@mochi/runtime/io` shim covers filesystem and HTTP, and persistence is downstream.

The Bun minor we pin for CI is **1.1.40** (December 2024). The `package.json` `engines`:

```json
{
  "engines": {
    "bun": ">=1.1.0"
  }
}
```

### 1.4 Browser

The browser target is **Baseline 2024**. Baseline is a W3C-WebDX initiative that defines which web platform features are stable across the four major engines (Chromium, Firefox, Safari, plus their WebView variants on iOS and Android). Baseline 2024 captures features that have been Widely Available for at least 2.5 years; Baseline 2025 captures features available across all engines as of late 2024.

For Mochi v1, we target Baseline 2024 minus a small allowlist of newer features that we explicitly polyfill:

- **`Promise.withResolvers`**: Baseline 2024 (Chrome 119, Firefox 121, Safari 17.4, all 2024). Polyfilled for Safari < 17.4 via a 8-line helper.
- **`Iterator.from` and helpers**: NOT Baseline 2024 (Chrome 122, Firefox 131, Safari 18.4). Polyfilled via `core-js/actual/iterator` for the browser bundle. Approx 2 KB gzip.
- **`Set` methods (intersection etc.)**: Mostly Baseline 2024 (Chrome 122, Firefox 127, Safari 17.4). Polyfilled for older Safari via `core-js/actual/set`.
- **`Object.groupBy / Map.groupBy`**: Baseline 2024 (Chrome 117, Firefox 119, Safari 17.4). Polyfilled for older Safari.
- **`Array.prototype.toSorted / toReversed / toSpliced / with`**: Baseline 2024 (Chrome 110, Firefox 115, Safari 16). No polyfill needed.
- **`String.prototype.isWellFormed / toWellFormed`**: Baseline 2024 (Chrome 111, Firefox 119, Safari 17). No polyfill needed.

We treat the polyfill set as a tax we pay at bundle time, not as a load-time tax on users with modern browsers. The esbuild bundle (§5) splits into two outputs:

- `dist/browser/index.modern.js`: assumes Baseline 2024 plus the ES2024 Iterator helpers natively. ~12 KB gzip for Hello World. Loads via `<script type="module">` in modern browsers.
- `dist/browser/index.legacy.js`: bundles the core-js polyfills for the missing pieces. ~28 KB gzip. Loaded by older browsers (Safari 17.0-17.3, Firefox 125-130) via `<script type="module" nomodule>` chain or via a runtime feature check.

The Mochi-emitted HTML scaffold (when the user runs `mochi build --target=browser-page`) chooses via:

```html
<script type="module">
  const supportsIterHelpers =
    typeof globalThis.Iterator?.from === "function";
  if (supportsIterHelpers) {
    await import("./dist/browser/index.modern.js");
  } else {
    await import("./dist/browser/index.legacy.js");
  }
</script>
```

Browser support floor table (when feature X is the limiting factor):

| Engine          | Min version | Released   | Notes                                |
|-----------------|-------------|------------|--------------------------------------|
| Chromium-based  | 122         | 2024-02-20 | Edge, Chrome, Brave, Opera           |
| Firefox         | 131         | 2024-10-01 | Iterator helpers landed here         |
| Safari (desktop)| 18.4        | 2025-03-31 | Iterator helpers full ship           |
| Safari (iOS)    | 18.4        | 2025-03-31 | iOS WebView only                     |
| Samsung Internet| 27          | 2024-10-15 | Based on Chromium 127                |

For the **legacy** bundle (with polyfills), the floor drops to:

| Engine          | Min version | Released   | Notes                                |
|-----------------|-------------|------------|--------------------------------------|
| Chromium-based  | 110         | 2023-02-07 | Array.toSorted minimum               |
| Firefox         | 115         | 2023-07-04 | Array.toSorted minimum (ESR)         |
| Safari          | 16.4        | 2023-03-27 | Array.toSorted minimum               |

Older browsers (Safari 15, Firefox ESR before 115, IE11) are explicitly **not** supported. v1 gates against the floor + the polyfill set above.

## 2. The `package.json` exports conditional map

The `exports` field in `package.json` is the gatekeeper for runtime dispatch. The full v1 form:

```json
{
  "name": "@mochi/runtime",
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
      "types": "./dist/io/index.d.ts",
      "deno": "./dist/deno/io/index.js",
      "bun": "./dist/bun/io/index.js",
      "node": "./dist/node/io/index.js",
      "browser": {
        "types": "./dist/io/browser.d.ts",
        "default": "./dist/browser/io/index.js"
      },
      "default": "./dist/node/io/index.js"
    },
    "./query": {
      "types": "./dist/query/index.d.ts",
      "default": "./dist/node/query/index.js"
    },
    "./agent": {
      "types": "./dist/agent/index.d.ts",
      "default": "./dist/node/agent/index.js"
    },
    "./stream": {
      "types": "./dist/stream/index.d.ts",
      "default": "./dist/node/stream/index.js"
    },
    "./package.json": "./package.json"
  },
  "files": [
    "dist/**",
    "package.json",
    "README.md",
    "LICENSE"
  ],
  "publishConfig": {
    "access": "public",
    "provenance": true
  }
}
```

Key invariants:

- The `"types"` key always sits first inside a conditional block; Node's resolver finds the right `.d.ts` before any runtime dispatch.
- `"deno"`, `"bun"`, `"browser"`, `"node"` cover the four runtimes in priority order. `"default"` is the fallback, pointed at the Node build so that any unknown runtime (Vercel Edge runtime, Cloudflare Workers if they ever hit ESM compat, etc.) gets a sensible default.
- The subpath `./io` has a nested `"browser"` block because the browser variant ships a different `.d.ts` (the browser IO type is narrower, no filesystem methods).
- The non-IO subpaths (`./query`, `./agent`, `./stream`) only have one file per subpath because the implementations are runtime-agnostic.

The Node resolver algorithm (Node 22+):

1. Read the `exports` field of the package's `package.json`.
2. Resolve the requested subpath against the keys (`.`, `./io`, `./query`, etc.).
3. For the matched subpath, walk the conditional map in declaration order.
4. The current runtime declares its conditions via the `--conditions` CLI flag or via the implicit defaults: Node 22 declares `"node"`, `"default"`, `"import"`, `"types"`. Deno declares `"deno"`, `"default"`, `"import"`, `"types"`. Bun declares `"bun"`, `"node"`, `"default"`, `"import"`, `"types"` (note Bun also declares "node" so it matches the Node entry as a fallback if the Bun-specific entry is absent). Browser bundlers (esbuild, webpack, rollup) declare `"browser"`, `"default"`, `"import"`, `"types"`.
5. The first conditional key that matches wins. We list runtime keys before `"default"` so the runtime-specific file always wins over the fallback.

The order matters: if we put `"default"` before `"node"`, every runtime would fall through to the default before checking its specific entry. The Node docs (Node 22 documentation/packages.html §"Conditional exports") state explicitly: "the order of keys within the object is significant... earlier conditions have priority".

### 2.1 Deno's interaction with the exports map

Deno 2 reads the `exports` field directly (it does not require `deno.json`). Deno's set of declared conditions:

- `"deno"`: always declared (Deno is the runtime).
- `"node"`: declared when the package is loaded via `npm:` specifier (Deno is emulating Node).
- `"browser"`: NOT declared by default.
- `"default"`: declared.
- `"import"`: declared (Deno is ESM-only).
- `"types"`: declared during type checking.

So when a Deno program does `import { Counter } from "@mochi/runtime"`:

- If `"@mochi/runtime"` is imported via `jsr:@mochi/runtime`, Deno reads JSR's `jsr.json` (or `deno.json`), which routes to the TS source directly. No `exports` involved.
- If imported via `npm:@mochi/runtime`, Deno reads the npm `package.json`. The conditions are `["deno", "node", "default", "import", "types"]`. The `.` subpath matches the `"deno"` key (since it appears before `"node"`), so `dist/deno/index.js` is loaded.

This is why the conditional map lists `"deno"` before `"node"`: under `npm:` resolution, Deno is in the condition set but Node is also in the condition set (because Deno emulates Node); the first key wins.

### 2.2 Bun's interaction

Bun declares `["bun", "node", "default", "import", "types"]`. The `.` subpath:

- `"bun"` matches first -> `dist/bun/index.js`.

If we omit the `"bun"` entry from the map, Bun falls through to `"node"` -> `dist/node/index.js`. That works correctly because Bun is highly Node-compatible (almost every Node-targeted package runs unchanged on Bun). The `"bun"` entry exists only to ship a variant tuned for Bun-specific APIs (`Bun.file`, `Bun.serve`). For Mochi v1 we ship a separate `dist/bun/` but the differences from `dist/node/` are small (mostly IO shims).

### 2.3 Browser bundlers

esbuild, webpack 5, rollup, vite, and parcel all read the `exports` field. They declare `["browser", "default", "import"]`. The `.` subpath matches `"browser"` -> `dist/browser/index.js`.

Some bundlers (older webpack 4, older rollup 2) read the legacy `browser` field at the top level instead of `exports.browser`. We do not support those (webpack 4 is EOL; rollup 2 is EOL).

### 2.4 The `types` key positioning

TypeScript 5.0+ honours `exports` for type resolution. The `"types"` key inside a conditional block points at the `.d.ts` for that runtime. We can either:

(a) Put one `"types"` at the top of each subpath, pointing at a shared `.d.ts` that is identical across runtimes. Pros: one `.d.ts` to ship, smaller package. Cons: per-runtime type narrowing impossible.

(b) Put a separate `"types"` inside each runtime block. Pros: per-runtime narrowing (the browser `.d.ts` can omit `Buffer`, `node:fs.PathLike`, etc.). Cons: ~4x the `.d.ts` payload.

We use (a) for everything except `./io`, where (b) is necessary because the browser IO surface is genuinely narrower (no `readFile`, no `writeFile`, no `spawn`). The browser `.d.ts` declares `BrowserIo` which is a strict subset of `NodeIo` / `DenoIo` / `BunIo`.

## 3. The four-target tsconfig project layout

`tsc --build` (also written `tsc -b`) drives multi-project compilation. Each project has its own `tsconfig.<runtime>.json` extending a shared base. The layout:

```
mochi-runtime/
  tsconfig.json                    # root, lists all 4 references
  tsconfig.base.json               # strict + ES2024 + isolatedModules
  tsconfig.node.json
  tsconfig.deno.json
  tsconfig.bun.json
  tsconfig.browser.json
  src/
    index.ts                       # shared source
    io/
      index.ts                     # dispatch by globalThis runtime detection
      node.ts                      # Node-only (node:fs, etc.)
      deno.ts                      # Deno-only (Deno.readFile, etc.)
      bun.ts                       # Bun-only (Bun.file, etc.)
      browser.ts                   # browser-only (fetch, FileReader stub)
    query/
    agent/
    stream/
  dist/
    node/
    deno/
    bun/
    browser/
    *.d.ts                         # shared types
```

### 3.1 `tsconfig.base.json`

```jsonc
{
  "compilerOptions": {
    "target": "ES2024",
    "module": "ESNext",
    "moduleResolution": "Bundler",
    "resolveJsonModule": true,
    "esModuleInterop": true,
    "allowSyntheticDefaultImports": true,

    "strict": true,
    "noImplicitAny": true,
    "strictNullChecks": true,
    "strictFunctionTypes": true,
    "strictBindCallApply": true,
    "strictPropertyInitialization": true,
    "noImplicitThis": true,
    "useUnknownInCatchVariables": true,
    "alwaysStrict": true,

    "noUncheckedIndexedAccess": true,
    "exactOptionalPropertyTypes": true,
    "noImplicitOverride": true,
    "noFallthroughCasesInSwitch": true,
    "noPropertyAccessFromIndexSignature": true,
    "noUncheckedSideEffectImports": true,
    "rewriteRelativeImportExtensions": true,

    "declaration": true,
    "declarationMap": true,
    "sourceMap": true,
    "isolatedModules": true,
    "verbatimModuleSyntax": true,
    "forceConsistentCasingInFileNames": true,
    "skipLibCheck": false,

    "composite": true,
    "incremental": true
  }
}
```

Highlights:

- `"target": "ES2024"`: emit ES2024-level features (no down-leveling, no Babel-style transforms). The pre-compiled `dist/*/*.js` is ES2024 syntax. Browsers older than the Baseline 2024 floor cannot load it without polyfills.
- `"module": "ESNext"` + `"moduleResolution": "Bundler"`: ESM-only, with TypeScript 5.0+'s bundler-aware resolution. The `Bundler` mode allows omitting the `.js` extension in `.ts` source while emitting it in the `.js` output (via `rewriteRelativeImportExtensions`).
- `"rewriteRelativeImportExtensions": true` (TS 5.6+): `import "./foo.ts"` in source becomes `import "./foo.js"` in dist. This avoids the dual-source-and-dist-extension friction.
- `"isolatedModules": true`: forbids `const enum` and other features that require cross-file analysis. Critical for any tool (esbuild, swc, Bun's transpiler) that compiles file-by-file.
- `"verbatimModuleSyntax": true` (TS 5.0+): types-only imports must be `import type { Foo } from "..."`, value imports keep their syntax. Replaces the older `importsNotUsedAsValues` and `preserveValueImports`.
- `"composite": true` + `"incremental": true`: required for `tsc -b`. The build produces `.tsbuildinfo` files for incremental rebuilds.

### 3.2 `tsconfig.node.json`

```jsonc
{
  "extends": "./tsconfig.base.json",
  "compilerOptions": {
    "outDir": "./dist/node",
    "rootDir": "./src",
    "lib": ["ES2024", "DOM"],
    "types": ["node"],
    "paths": {
      "@mochi/runtime/io": ["./src/io/node.ts"],
      "@mochi/runtime/io/*": ["./src/io/node/*"]
    }
  },
  "include": ["src/**/*.ts"],
  "exclude": ["src/io/deno.ts", "src/io/bun.ts", "src/io/browser.ts"]
}
```

The `paths` map routes `@mochi/runtime/io` to the Node-specific IO file at compile time. The `exclude` list drops the other runtimes' IO files from the Node build.

`"types": ["node"]`: pulls in `@types/node` declarations. Node 22's types are at `@types/node@22.x` on npm.

### 3.3 `tsconfig.deno.json`

```jsonc
{
  "extends": "./tsconfig.base.json",
  "compilerOptions": {
    "outDir": "./dist/deno",
    "rootDir": "./src",
    "lib": ["ES2024", "DOM"],
    "types": [],
    "paths": {
      "@mochi/runtime/io": ["./src/io/deno.ts"],
      "@mochi/runtime/io/*": ["./src/io/deno/*"]
    }
  },
  "include": ["src/**/*.ts"],
  "exclude": ["src/io/node.ts", "src/io/bun.ts", "src/io/browser.ts"]
}
```

`"types": []`: Deno does not use `@types/node`. The Deno global types come from Deno's own typegen (`deno types > deno.d.ts`) which the build picks up via reference.

For JSR publishing, the same source is consumed without going through `tsc`. JSR's tooling reads the source `.ts` files directly and validates them with its own type checker (Deno's `tsc` integration).

### 3.4 `tsconfig.bun.json`

```jsonc
{
  "extends": "./tsconfig.base.json",
  "compilerOptions": {
    "outDir": "./dist/bun",
    "rootDir": "./src",
    "lib": ["ES2024", "DOM"],
    "types": ["bun-types"],
    "paths": {
      "@mochi/runtime/io": ["./src/io/bun.ts"],
      "@mochi/runtime/io/*": ["./src/io/bun/*"]
    }
  },
  "include": ["src/**/*.ts"],
  "exclude": ["src/io/node.ts", "src/io/deno.ts", "src/io/browser.ts"]
}
```

`"types": ["bun-types"]`: pulls in `bun-types` (published by Oven on npm). This declares `globalThis.Bun`, `Bun.file`, `Bun.write`, `Bun.serve`, etc.

Note that `bun-types` includes Node-compatible types as a superset; Bun is positioned as a Node-compatible runtime, so its `.d.ts` covers `node:fs`, `node:net`, etc. We do not import those directly from Bun source (the IO shim file is the only entry), but they are available for users who want to bypass the shim.

### 3.5 `tsconfig.browser.json`

```jsonc
{
  "extends": "./tsconfig.base.json",
  "compilerOptions": {
    "outDir": "./dist/browser",
    "rootDir": "./src",
    "lib": ["ES2024", "DOM", "DOM.Iterable"],
    "types": [],
    "paths": {
      "@mochi/runtime/io": ["./src/io/browser.ts"],
      "@mochi/runtime/io/*": ["./src/io/browser/*"]
    }
  },
  "include": ["src/**/*.ts"],
  "exclude": ["src/io/node.ts", "src/io/deno.ts", "src/io/bun.ts"]
}
```

`"lib": ["ES2024", "DOM", "DOM.Iterable"]`: brings in the DOM declarations (`window`, `document`, `fetch`, `WebSocket`, etc.). No Node types.

`"types": []`: empty. No ambient packages.

### 3.6 The root `tsconfig.json`

```jsonc
{
  "files": [],
  "references": [
    { "path": "./tsconfig.node.json" },
    { "path": "./tsconfig.deno.json" },
    { "path": "./tsconfig.bun.json" },
    { "path": "./tsconfig.browser.json" }
  ]
}
```

Running `tsc -b` from the package root builds all four projects in parallel (TypeScript 5.0+ added parallel build support inside `-b`). The output goes to `dist/node/`, `dist/deno/`, `dist/bun/`, `dist/browser/` respectively, plus shared `.d.ts` files at `dist/*.d.ts` (one set per project, deduplicated by the build orchestration).

`tsc -b --clean` removes all four `dist/<runtime>/` directories plus the `.tsbuildinfo` files. `tsc -b --force` rebuilds without using the incremental cache.

## 4. What's natively available per runtime

A detailed table. Y = native, P = polyfill required for browser, P* = polyfill required for some browsers below Baseline 2024 floor, N = not available.

| Feature                                | Node 22 | Deno 2 | Bun 1.1 | Browser (Baseline 2024) |
|----------------------------------------|---------|--------|---------|-------------------------|
| `Promise.withResolvers`                | Y       | Y      | Y       | Y                       |
| `Iterator.from` + helpers              | Y       | Y      | Y       | P (core-js)             |
| `Set.prototype.intersection` etc.      | Y       | Y      | Y       | P*                      |
| `Object.groupBy` / `Map.groupBy`       | Y       | Y      | Y       | P*                      |
| `Array.prototype.toSorted` etc.        | Y       | Y      | Y       | Y                       |
| `String.prototype.isWellFormed`        | Y       | Y      | Y       | Y                       |
| `structuredClone`                      | Y       | Y      | Y       | Y                       |
| `fetch`                                | Y       | Y      | Y       | Y                       |
| `WebSocket`                            | Y       | Y      | Y       | Y                       |
| `URL`, `URLSearchParams`               | Y       | Y      | Y       | Y                       |
| `crypto.subtle`                        | Y       | Y      | Y       | Y (requires HTTPS)      |
| `crypto.randomUUID`                    | Y       | Y      | Y       | Y                       |
| `TextEncoder`, `TextDecoder`           | Y       | Y      | Y       | Y                       |
| `Blob`, `File`                         | Y       | Y      | Y       | Y                       |
| `Request`, `Response`                  | Y       | Y      | Y       | Y                       |
| `AbortController`, `AbortSignal`       | Y       | Y      | Y       | Y                       |
| `AbortSignal.timeout`, `.any`          | Y       | Y      | Y       | Y                       |
| `EventTarget`                          | Y       | Y      | Y       | Y                       |
| `MessageChannel`, `BroadcastChannel`   | Y       | Y      | Y       | Y                       |
| Web Streams (`ReadableStream` etc.)    | Y       | Y      | Y       | Y                       |
| `queueMicrotask`                       | Y       | Y      | Y       | Y                       |
| `setTimeout`, `setInterval`            | Y       | Y      | Y       | Y                       |
| `performance.now`                      | Y       | Y      | Y       | Y                       |
| `console.log` (with formatting)        | Y       | Y      | Y       | Y                       |
| Temporal (ES proposal)                 | N       | N (unstable)| N  | N                       |
| `node:fs`                              | Y       | Y (compat)| Y (compat)| N                  |
| `node:net`, `node:http`                | Y       | Y (compat)| Y (compat)| N                  |
| `node:child_process`                   | Y       | Y (compat)| Y (compat)| N                  |
| `node:os`                              | Y       | Y (compat)| Y (compat)| N                  |
| `node:path`                            | Y       | Y (compat)| Y (compat)| N (use `URL`)      |
| `node:url`                             | Y       | Y (compat)| Y (compat)| N (use `URL`)      |
| `Deno.*` namespace                     | N       | Y      | N       | N                       |
| `Bun.*` namespace                      | N       | N      | Y       | N                       |
| `globalThis.window`                    | N       | N      | N       | Y                       |
| `globalThis.document`                  | N       | N      | N       | Y                       |
| FileSystem Access API                  | N       | N      | N       | Y (origin-isolated, Baseline 2024 partial) |
| IndexedDB                              | N       | N      | N       | Y                       |
| Workers (Web Workers)                  | N (use `worker_threads`)| Y | Y | Y                  |
| WebGPU                                 | N (experimental)| Y (experimental)| Y (experimental)| Y (Baseline 2025) |

Note: "Y (compat)" for Deno and Bun means the API is provided via a polyfill or shim in the runtime; behavioural fidelity is high but not 100 %. We document the known deltas in [[11-testing-gates]] §8.

The Temporal proposal (Stage 3 since 2024-10) lands in Firefox 139 (March 2025), Safari 18.5 (April 2025), and Chrome 130 (likely 2025-Q2). Mochi v1 does not depend on Temporal; the date / time emit uses `Date` plus a tiny `mochi_runtime/time.ts` wrapper that exposes `MochiInstant`, `MochiZonedDateTime`, `MochiDuration`. Once Temporal is Baseline 2026, we plan to switch.

## 5. Browser bundling via esbuild

Browser support requires a bundler because:

- Browsers cannot follow ESM imports across npm packages (no `node_modules` resolution).
- Tree-shaking removes unused exports (reduces bundle size).
- Minification (terser-like) reduces bundle size further.
- Polyfill injection adds core-js modules for sub-Baseline-2024 features.

We use **esbuild** as the canonical browser bundler. Reasons:

- Fast (1-2 orders of magnitude over webpack/rollup for medium projects).
- Native ESM output, with optional IIFE / CJS / UMD for legacy.
- Built-in tree-shaking and minification.
- Plugin API for custom resolution (rare in our case).
- Single-binary distribution; no `node_modules` for the bundler itself.
- Stable since 0.20.x (now at 0.24.x as of 2026-05).
- Used by Vite under the hood; if a user prefers Vite, the result is similar.

The browser build command (run by `mochi build --target=browser-bundle`):

```bash
esbuild src/index.ts \
  --bundle \
  --format=esm \
  --target=es2024 \
  --outfile=dist/browser/index.js \
  --minify \
  --sourcemap=external \
  --conditions=browser,default \
  --external:node:fs \
  --external:node:net \
  --external:node:child_process \
  --external:node:os \
  --external:node:path \
  --external:node:url \
  --define:globalThis.process.env.NODE_ENV='"production"' \
  --metafile=dist/browser/meta.json
```

Key flags:

- `--bundle`: walk imports, produce a single output file.
- `--format=esm`: ES module output. Loaded via `<script type="module">`.
- `--target=es2024`: emit ES2024 syntax. esbuild does not down-level past its target.
- `--conditions=browser,default`: matches the `"browser"` and `"default"` keys in `exports` maps.
- `--external:node:fs` etc.: marks these as external. The browser does not have `node:` imports; if any code path imports them, esbuild keeps the import as a bare specifier and the browser fails at runtime. The tree-shaker ensures no code path reaches these (they live behind `globalThis.process` checks that are dead-code-eliminated when `NODE_ENV` is defined). Specifically the `mochi_runtime/io/` shim has top-level branches like `if (typeof process !== "undefined" && process.versions?.node)` which esbuild eliminates given `define:process=undefined`.
- `--define:globalThis.process.env.NODE_ENV='"production"'`: inlines the value, enables DCE.
- `--metafile=...`: produces a bundle analysis JSON for `esbuild --analyze`.

Bundle sizes (Hello World + agent + 1 stream + query DSL usage):

| Target                              | Size (raw) | Size (gzip) | Notes                          |
|-------------------------------------|------------|-------------|--------------------------------|
| dist/browser/index.modern.js         | 38 KB      | 12 KB       | Baseline 2024, no polyfills    |
| dist/browser/index.legacy.js         | 92 KB      | 28 KB       | + core-js for Iterator helpers + Set methods |
| dist/browser/index.modern.min.js     | 24 KB      | 9 KB        | esbuild --minify (Hello World) |
| dist/browser/index.legacy.min.js     | 60 KB      | 20 KB       | minified legacy bundle         |

For perspective, the equivalent React + Vue + Svelte runtimes (without user code) are 45 KB / 32 KB / 4 KB gzip respectively. The Mochi runtime is competitive.

The `mochi build --target=browser-page` variant additionally:

- Generates an `index.html` with a `<script type="module">` tag.
- Generates `<link rel="modulepreload">` hints for sub-entries.
- Generates a basic `<meta name="viewport">` for mobile.
- Generates a `Content-Security-Policy` header recommendation in a comment block (we do not auto-set CSP since deployment varies).

## 6. JSR co-publish for Deno

JSR (jsr.io) is the Deno-native package registry, launched 2024-03-04 by the Deno authors. JSR's pitch:

- Source files (`.ts`) are published directly, no compilation step.
- Cross-runtime: JSR packages work on Deno, Node 22+ (via the `npm:` compatibility shim), and Bun.
- Strict semver enforcement; no `latest` floating tags.
- Built-in API docs generated from JSDoc comments.
- Provenance via Sigstore + GitHub OIDC (matches the npm Trusted Publishing model).
- Free for open source projects, including private scopes via paid plans.

The Mochi runtime publishes to both:

- `@mochi/runtime` on jsr.io (via `deno publish`).
- `@mochi/runtime` on npmjs.org (via `npm publish --provenance`).

Both packages share the same source. The JSR variant ships `.ts` directly; the npm variant ships pre-compiled `dist/*/`.

### 6.1 `jsr.json` (or `deno.json`)

JSR reads `jsr.json` or the `[publish]` table of `deno.json`. We use `deno.json`:

```jsonc
{
  "name": "@mochi/runtime",
  "version": "0.1.0",
  "exports": {
    ".": "./src/index.ts",
    "./io": "./src/io/deno.ts",
    "./query": "./src/query/index.ts",
    "./agent": "./src/agent/index.ts",
    "./stream": "./src/stream/index.ts"
  },
  "publish": {
    "include": ["src/**/*.ts", "README.md", "LICENSE"],
    "exclude": ["src/io/node.ts", "src/io/bun.ts", "src/io/browser.ts", "**/*.test.ts"]
  }
}
```

Two notable differences vs the npm `package.json` `exports` map:

1. JSR's `exports` paths point at `.ts` source, not `dist/*.js`.
2. The JSR variant excludes the Node, Bun, and Browser IO files. JSR readers only see the Deno-relevant code.

`deno publish` validates:

- All exports resolve to existing files.
- All imported modules are either local relative paths or `npm:` / `jsr:` / `node:` specifiers (no bare specifiers).
- All exported symbols have JSDoc.
- The `version` is a valid semver and is greater than the latest published version on jsr.io.

The publish workflow:

```yaml
name: jsr-publish
on:
  push:
    tags: ['v*']
permissions:
  contents: read
  id-token: write
jobs:
  publish:
    runs-on: ubuntu-22.04
    steps:
      - uses: actions/checkout@v4
      - uses: denoland/setup-deno@v2
        with:
          deno-version: v2.1.4
      - run: deno publish
```

`deno publish` uses GitHub's OIDC token (the `id-token: write` permission) to attest the publish. JSR records the GitHub repo, commit SHA, and workflow run URL on the published version page.

### 6.2 npm consumption of the JSR package

Users on Node 22 can also consume the JSR variant via the `jsr` npm tool:

```bash
npx jsr add @mochi/runtime
```

This rewrites `package.json` to add a dependency like `"@mochi/runtime": "npm:@jsr/mochi__runtime@^0.1.0"`. The `@jsr/<scope>__<name>` form is JSR's npm-compatibility shim: JSR generates a CJS+ESM npm tarball for each release and hosts it on a JSR-managed npm registry endpoint.

We document this path in the README but recommend the direct `npm:@mochi/runtime` route for npm users (simpler).

## 7. WebContainer / StackBlitz support

WebContainer is StackBlitz's in-browser Node-compatible runtime, launched 2021 and stabilised through 2023-2024. It runs an actual Node.js compiled to WebAssembly inside the browser, with a virtual filesystem and a virtual network stack. As of 2026-Q1, WebContainer supports:

- Node 18.x (latest LTS that WebContainer ships). Node 20.x in beta. Node 22.x planned for 2026-Q3.
- npm install (most packages work; native modules fail).
- `node`, `npx`, `npm`, `pnpm`, `yarn` CLIs.
- A virtual filesystem mapped to IndexedDB.
- A virtual network via the browser's `fetch` (with CORS, subject to the host page's permissions).
- No `node:child_process` (subprocess emulation is partial).
- No `node:worker_threads` natively (browser Workers are used instead).

For Mochi, WebContainer is a tier-2 target: we want Mochi-emitted code to run in StackBlitz embeds, but we do not gate against WebContainer in CI (the runtime is browser-internal and not scriptable for our test infrastructure).

What works in WebContainer with our code:

- Pure `mochi_runtime` (Hello World, agents, streams, query DSL): yes, via the `dist/node/` build.
- HTTP via `fetch`: yes (with CORS).
- WebSocket: yes (browser WS).
- Filesystem via `node:fs`: yes (IndexedDB-backed VFS).
- Subprocess via `node:child_process`: partial (only `spawn` with stdin/stdout, no PTY).
- Native modules (better-sqlite3, etc.): no (no native binary support).

What does not work:

- Anything that calls `process.cpuUsage()`, `os.networkInterfaces()`, or other host-specific syscalls.
- Anything that requires WebContainer's beta Node 22 (until they ship it).

Mochi v1's runtime does not call any of the non-working APIs, so the Hello World, agent, stream, and query examples all run inside StackBlitz embeds. This is useful for documentation: every spec note can include an embed showing the user the code running live.

The relevant embed URL pattern:

```
https://stackblitz.com/edit/mochi-hello-world?embed=1&file=src/index.ts
```

The `mochi-hello-world` template repository is published in the `mochilang` organisation and pre-configured with the four tsconfig projects, the `@mochi/runtime` dependency, and a sample `index.ts`.

## 8. Node-only API isolation under `mochi_runtime/io/`

The IO surface is the most runtime-divergent area of the runtime. Filesystem, subprocess, OS info, signals: each runtime has its own API. We isolate via a single subpath: `mochi_runtime/io/`.

The shape:

```typescript
// src/io/index.ts (the public entry)
export interface MochiIo {
  readFile(path: string): Promise<Uint8Array>;
  writeFile(path: string, data: Uint8Array): Promise<void>;
  exists(path: string): Promise<boolean>;
  remove(path: string): Promise<void>;
  mkdir(path: string, opts?: { recursive?: boolean }): Promise<void>;
  readdir(path: string): Promise<string[]>;
  stat(path: string): Promise<MochiStat>;
  spawn(cmd: string, args: readonly string[], opts?: SpawnOpts): Promise<SpawnResult>;
  cwd(): string;
  env(name: string): string | null;
  exit(code: number): never;
}

export interface MochiStat {
  readonly size: bigint;
  readonly mtime: Date;
  readonly isFile: boolean;
  readonly isDirectory: boolean;
}

export interface SpawnOpts {
  readonly stdin?: Uint8Array | "inherit" | "ignore";
  readonly env?: Readonly<Record<string, string>>;
  readonly cwd?: string;
}

export interface SpawnResult {
  readonly stdout: Uint8Array;
  readonly stderr: Uint8Array;
  readonly exitCode: number;
}
```

The four implementations:

### 8.1 Node (src/io/node.ts)

```typescript
import * as fs from "node:fs/promises";
import * as nodePath from "node:path";
import { spawn as nodeSpawn } from "node:child_process";
import { fileURLToPath } from "node:url";
import type { MochiIo, MochiStat, SpawnOpts, SpawnResult } from "./index.js";

export const io: MochiIo = {
  async readFile(path: string): Promise<Uint8Array> {
    const buf: Buffer = await fs.readFile(path);
    return new Uint8Array(buf.buffer, buf.byteOffset, buf.byteLength);
  },
  async writeFile(path: string, data: Uint8Array): Promise<void> {
    await fs.writeFile(path, data);
  },
  async exists(path: string): Promise<boolean> {
    try {
      await fs.access(path);
      return true;
    } catch {
      return false;
    }
  },
  async remove(path: string): Promise<void> {
    await fs.rm(path, { recursive: true, force: true });
  },
  async mkdir(path: string, opts?: { recursive?: boolean }): Promise<void> {
    await fs.mkdir(path, { recursive: opts?.recursive ?? false });
  },
  async readdir(path: string): Promise<string[]> {
    return fs.readdir(path);
  },
  async stat(path: string): Promise<MochiStat> {
    const s = await fs.stat(path);
    return {
      size: BigInt(s.size),
      mtime: s.mtime,
      isFile: s.isFile(),
      isDirectory: s.isDirectory(),
    };
  },
  spawn(
    cmd: string,
    args: readonly string[],
    opts?: SpawnOpts,
  ): Promise<SpawnResult> {
    return new Promise<SpawnResult>((resolve, reject) => {
      const child = nodeSpawn(cmd, [...args], {
        env: opts?.env ? { ...process.env, ...opts.env } : process.env,
        cwd: opts?.cwd,
        stdio: ["pipe", "pipe", "pipe"],
      });
      const stdoutChunks: Buffer[] = [];
      const stderrChunks: Buffer[] = [];
      child.stdout?.on("data", (chunk: Buffer) => stdoutChunks.push(chunk));
      child.stderr?.on("data", (chunk: Buffer) => stderrChunks.push(chunk));
      child.on("error", reject);
      child.on("close", (exitCode: number | null) => {
        const stdout = Buffer.concat(stdoutChunks);
        const stderr = Buffer.concat(stderrChunks);
        resolve({
          stdout: new Uint8Array(stdout.buffer, stdout.byteOffset, stdout.byteLength),
          stderr: new Uint8Array(stderr.buffer, stderr.byteOffset, stderr.byteLength),
          exitCode: exitCode ?? -1,
        });
      });
      if (opts?.stdin instanceof Uint8Array) {
        child.stdin?.end(opts.stdin);
      } else if (opts?.stdin === "ignore") {
        child.stdin?.end();
      }
    });
  },
  cwd(): string {
    return process.cwd();
  },
  env(name: string): string | null {
    return process.env[name] ?? null;
  },
  exit(code: number): never {
    process.exit(code);
  },
};
```

### 8.2 Deno (src/io/deno.ts)

```typescript
import type { MochiIo, MochiStat, SpawnOpts, SpawnResult } from "./index.ts";

export const io: MochiIo = {
  async readFile(path: string): Promise<Uint8Array> {
    return await Deno.readFile(path);
  },
  async writeFile(path: string, data: Uint8Array): Promise<void> {
    await Deno.writeFile(path, data);
  },
  async exists(path: string): Promise<boolean> {
    try {
      await Deno.stat(path);
      return true;
    } catch (err) {
      if (err instanceof Deno.errors.NotFound) return false;
      throw err;
    }
  },
  async remove(path: string): Promise<void> {
    try {
      await Deno.remove(path, { recursive: true });
    } catch (err) {
      if (!(err instanceof Deno.errors.NotFound)) throw err;
    }
  },
  async mkdir(path: string, opts?: { recursive?: boolean }): Promise<void> {
    await Deno.mkdir(path, { recursive: opts?.recursive ?? false });
  },
  async readdir(path: string): Promise<string[]> {
    const names: string[] = [];
    for await (const entry of Deno.readDir(path)) {
      names.push(entry.name);
    }
    return names;
  },
  async stat(path: string): Promise<MochiStat> {
    const s = await Deno.stat(path);
    return {
      size: BigInt(s.size),
      mtime: s.mtime ?? new Date(0),
      isFile: s.isFile,
      isDirectory: s.isDirectory,
    };
  },
  async spawn(
    cmd: string,
    args: readonly string[],
    opts?: SpawnOpts,
  ): Promise<SpawnResult> {
    const command = new Deno.Command(cmd, {
      args: [...args],
      env: opts?.env,
      cwd: opts?.cwd,
      stdin: opts?.stdin instanceof Uint8Array ? "piped" : (opts?.stdin ?? "inherit"),
      stdout: "piped",
      stderr: "piped",
    });
    const child = command.spawn();
    if (opts?.stdin instanceof Uint8Array) {
      const writer = child.stdin.getWriter();
      await writer.write(opts.stdin);
      await writer.close();
    }
    const { code, stdout, stderr } = await child.output();
    return { stdout, stderr, exitCode: code };
  },
  cwd(): string {
    return Deno.cwd();
  },
  env(name: string): string | null {
    return Deno.env.get(name) ?? null;
  },
  exit(code: number): never {
    Deno.exit(code);
  },
};
```

### 8.3 Bun (src/io/bun.ts)

```typescript
import type { MochiIo, MochiStat, SpawnOpts, SpawnResult } from "./index.ts";

export const io: MochiIo = {
  async readFile(path: string): Promise<Uint8Array> {
    return await Bun.file(path).bytes();
  },
  async writeFile(path: string, data: Uint8Array): Promise<void> {
    await Bun.write(path, data);
  },
  async exists(path: string): Promise<boolean> {
    return await Bun.file(path).exists();
  },
  async remove(path: string): Promise<void> {
    // Bun does not have a native rm; fall through to node:fs which Bun polyfills.
    const fs = await import("node:fs/promises");
    await fs.rm(path, { recursive: true, force: true });
  },
  async mkdir(path: string, opts?: { recursive?: boolean }): Promise<void> {
    const fs = await import("node:fs/promises");
    await fs.mkdir(path, { recursive: opts?.recursive ?? false });
  },
  async readdir(path: string): Promise<string[]> {
    const fs = await import("node:fs/promises");
    return fs.readdir(path);
  },
  async stat(path: string): Promise<MochiStat> {
    const file = Bun.file(path);
    const size: bigint = BigInt(file.size);
    const fs = await import("node:fs/promises");
    const s = await fs.stat(path);
    return {
      size,
      mtime: s.mtime,
      isFile: s.isFile(),
      isDirectory: s.isDirectory(),
    };
  },
  async spawn(
    cmd: string,
    args: readonly string[],
    opts?: SpawnOpts,
  ): Promise<SpawnResult> {
    const proc = Bun.spawn([cmd, ...args], {
      env: opts?.env ? { ...process.env, ...opts.env } : undefined,
      cwd: opts?.cwd,
      stdin: opts?.stdin instanceof Uint8Array ? opts.stdin : (opts?.stdin ?? "ignore"),
      stdout: "pipe",
      stderr: "pipe",
    });
    const [stdout, stderr] = await Promise.all([
      new Response(proc.stdout).bytes(),
      new Response(proc.stderr).bytes(),
    ]);
    const exitCode: number = await proc.exited;
    return { stdout, stderr, exitCode };
  },
  cwd(): string {
    return process.cwd();
  },
  env(name: string): string | null {
    return Bun.env[name] ?? process.env[name] ?? null;
  },
  exit(code: number): never {
    process.exit(code);
  },
};
```

### 8.4 Browser (src/io/browser.ts)

The browser IO is intentionally narrow: filesystem operations are unavailable; subprocess is unavailable; only `cwd`, `env`, and `exit` have meaningful (constant) implementations.

```typescript
import type { MochiIo, MochiStat, SpawnOpts, SpawnResult } from "./index.ts";

function unsupported(name: string): never {
  throw new Error(`mochi_runtime/io: ${name} is not available in the browser`);
}

export const io: MochiIo = {
  readFile(_path: string): Promise<Uint8Array> {
    unsupported("readFile");
  },
  writeFile(_path: string, _data: Uint8Array): Promise<void> {
    unsupported("writeFile");
  },
  exists(_path: string): Promise<boolean> {
    unsupported("exists");
  },
  remove(_path: string): Promise<void> {
    unsupported("remove");
  },
  mkdir(_path: string, _opts?: { recursive?: boolean }): Promise<void> {
    unsupported("mkdir");
  },
  readdir(_path: string): Promise<string[]> {
    unsupported("readdir");
  },
  stat(_path: string): Promise<MochiStat> {
    unsupported("stat");
  },
  spawn(
    _cmd: string,
    _args: readonly string[],
    _opts?: SpawnOpts,
  ): Promise<SpawnResult> {
    unsupported("spawn");
  },
  cwd(): string {
    return "/";
  },
  env(_name: string): string | null {
    return null;
  },
  exit(_code: number): never {
    throw new Error("mochi_runtime/io: exit is not available in the browser");
  },
};
```

A more user-friendly variant uses the [File System Access API](https://wicg.github.io/file-system-access/) where available (Chrome 86+, requires user gesture). The v1 default is the stub above; the user can override with `setIoImplementation(customIo)` if they want to wire up File System Access.

## 9. Per-runtime dist contents

What lives in each `dist/{runtime}/` directory after `tsc -b`:

### 9.1 dist/node/

```
dist/node/
  index.js               # entry, dispatches via runtime detection
  index.js.map           # sourcemap
  io/
    index.js
    node.js              # active IO impl for Node
  query/
    index.js
    iterator.js
    join.js
    datalog.js
  agent/
    index.js
    queue.js             # AsyncIterableQueue
    counter.js           # sample agent (in tests)
  stream/
    index.js
    cold.js
    hot.js
```

Size: ~80 KB raw, ~22 KB gzip.

### 9.2 dist/deno/

```
dist/deno/
  index.js
  index.js.map
  io/
    index.js
    deno.js              # active IO impl for Deno
  query/
    ...
  agent/
    ...
  stream/
    ...
```

Size: ~78 KB raw, ~21 KB gzip. The delta from Node is the IO file.

### 9.3 dist/bun/

```
dist/bun/
  index.js
  index.js.map
  io/
    index.js
    bun.js               # active IO impl for Bun
  query/
    ...
  agent/
    ...
  stream/
    ...
```

Size: ~79 KB raw, ~22 KB gzip.

### 9.4 dist/browser/

```
dist/browser/
  index.modern.js        # esbuild bundle (Baseline 2024)
  index.modern.js.map
  index.legacy.js        # esbuild bundle + core-js polyfills
  index.legacy.js.map
  index.min.js           # esbuild --minify
  io/                    # stub IO (browser cannot do filesystem)
    index.js
    browser.js
  query/
    ...
  agent/
    ...
  stream/
    ...
```

Size: ~38 KB raw, ~12 KB gzip for modern; ~92 KB raw, ~28 KB gzip for legacy.

The shared `dist/*.d.ts` lives at the top level of `dist/` and is referenced by all four runtime entries. Total `.d.ts` footprint: ~30 KB raw, ~6 KB gzip.

## 10. ESM-only stance: why no CommonJS

We deliberately do not ship a CommonJS build. Reasons:

- Node 22+ supports `require(esm)` (require an ESM module from CJS) when the ESM has no top-level await (Node 22.12+, stable). This closes the CJS-to-ESM friction for our case.
- Deno is ESM-only.
- Bun supports both but treats ESM as canonical.
- Modern browser bundlers (esbuild, webpack 5, rollup, vite) all default to ESM input.
- CommonJS has tree-shaking limitations (the entire module is loaded; tree-shaking is best-effort).
- Top-level `await` is ESM-only (CJS does not support it). The runtime bootstrap uses top-level `await` in one place: a feature-detection startup line that probes for `Promise.withResolvers` and falls back.
- Sourcemaps and `import.meta.url` work uniformly under ESM.

What we lose by skipping CJS:

- Compatibility with Node 16 and 18 callers that do not have `require(esm)`. Both are below our floor.
- Compatibility with old test runners (Jest 28 and earlier had ESM issues; Jest 30+ is fine).
- Compatibility with legacy webpack 4 configs. Webpack 4 is EOL.

We document the ESM-only stance in the README. Migration guidance for callers stuck on CJS:

```javascript
// instead of: const { Counter } = require("@mochi/runtime");
const { Counter } = await import("@mochi/runtime");
```

Dynamic `import()` works in any context (Node CJS, browsers, Deno, Bun) and returns an ESM module.

## 11. Reproducible builds

Reproducibility is a phase-16 gate (master gate of reproducibility-ish stuff). Two CI hosts building the same git commit must produce byte-identical `.tgz` tarballs (npm) and `.zip` archives (JSR).

Sources of non-determinism we have to control:

- **File timestamps**: `tar` records mtime per file; the npm tarball record changes if mtimes differ. Solution: set every file's mtime to `SOURCE_DATE_EPOCH` (from `git log -1 --format=%ct`) before packing.
- **Tarball entry order**: the order of files inside the tarball matters for the SHA256. Solution: sort entries by path lexicographically before writing.
- **JSON key order**: `package.json` and other JSON files emitted from build tools may serialise keys non-deterministically. Solution: pretty-print with sorted keys (`JSON.stringify(obj, Object.keys(obj).sort(), 2)`).
- **`.tsbuildinfo` files**: `tsc -b` incremental output includes timestamps. Solution: never ship `.tsbuildinfo` (excluded via `package.json` `files` allowlist).
- **Sourcemap embedding**: sourcemaps can contain absolute paths. Solution: `tsc` with `sourceRoot` set to a relative path; verify with `jq -r .sourceRoot dist/node/index.js.map`.
- **Bundler output ordering**: esbuild's tree-shaking output is deterministic given the same inputs; we verify by running twice and diffing.
- **`provenance` attestation**: the Sigstore attestation includes a timestamp from the Rekor transparency log; this is intentionally non-reproducible (it is a notarisation, not part of the artifact). We exclude it from the byte-equal check.

The reproducible-build script:

```bash
#!/bin/bash
set -euo pipefail
export SOURCE_DATE_EPOCH=$(git log -1 --format=%ct)
export TZ=UTC

# Clean
rm -rf dist/

# Build
tsc -b

# Bundle for browser
esbuild src/index.ts --bundle --format=esm --target=es2024 \
  --outfile=dist/browser/index.modern.js \
  --metafile=dist/browser/meta.json

# Pack
npm pack --pack-destination=./out

# Verify
sha256sum out/*.tgz > out/sha.txt
```

Running the script twice should produce identical `out/sha.txt`. We assert this in CI.

## 12. The `engines` field and bootstrap checks

`package.json` `engines`:

```json
{
  "engines": {
    "node": ">=22.0.0",
    "deno": ">=2.0.0",
    "bun": ">=1.1.0"
  }
}
```

`npm install` issues a warning if the running Node version mismatches; with `.npmrc` setting `engine-strict=true`, it errors. We document this in the README and recommend pinning in CI.

Bun and Deno do not honour the `engines` field directly (they read their own config files). For Deno, the floor is enforced by `deno.json`'s `compilerOptions.lib` (which references `deno.window` v2 typings). For Bun, the floor is enforced by `Bun.version` checks in the runtime bootstrap (similar to the Node check above).

Bootstrap code (in `mochi_runtime/runtime.ts`, included by every emit):

```typescript
const isNode: boolean =
  typeof process !== "undefined" && typeof process.versions?.node === "string";
const isDeno: boolean = typeof (globalThis as { Deno?: unknown }).Deno !== "undefined";
const isBun: boolean = typeof (globalThis as { Bun?: unknown }).Bun !== "undefined";
const isBrowser: boolean =
  typeof globalThis !== "undefined" &&
  typeof (globalThis as { window?: unknown }).window !== "undefined" &&
  typeof document !== "undefined";

function checkFloor(): void {
  if (isNode && !isBun) {
    const major: number = parseInt(process.versions.node.split(".")[0]!, 10);
    if (major < 22) {
      throw new Error(`mochi-runtime: Node 22 or later required (found ${process.versions.node})`);
    }
  }
  if (isDeno) {
    const deno = (globalThis as { Deno: { version: { deno: string } } }).Deno;
    const major: number = parseInt(deno.version.deno.split(".")[0]!, 10);
    if (major < 2) {
      throw new Error(`mochi-runtime: Deno 2 or later required (found ${deno.version.deno})`);
    }
  }
  if (isBun) {
    const bun = (globalThis as { Bun: { version: string } }).Bun;
    const [major, minor] = bun.version.split(".").map((s) => parseInt(s, 10));
    if (major === undefined || minor === undefined || major < 1 || (major === 1 && minor < 1)) {
      throw new Error(`mochi-runtime: Bun 1.1 or later required (found ${bun.version})`);
    }
  }
  // No floor check for browser (we cannot reliably detect engine version).
}

checkFloor();
```

The bootstrap runs once, at the first module load. Failures throw synchronously, before any user code runs.

## 13. v1 exclusions and why

The shared decisions doc commits us to Node 22 + Deno 2 + Bun 1.1 + Baseline 2024 browser. Beyond those, we explicitly exclude:

### 13.1 AWS Lambda Node 18 runtime

AWS Lambda runtimes are tied to Node LTS releases. Lambda Node 18 (introduced 2022-11) is below our floor:

- `Promise.withResolvers` not native.
- Iterator helpers not native.
- `Set` methods (intersection, union, etc.) not native.

Lambda Node 20 (introduced 2023-12) has `Promise.withResolvers` (added in Node 21.0, backported to 20.10), but Iterator helpers landed in V8 12.6 which is Node 22.x. So Lambda Node 20 is also below our floor.

Lambda Node 22 became available 2024-12-09 ("nodejs22.x" runtime identifier). This is the runtime we recommend for Lambda. We document this in the deployment section of [[10-build-system]].

For users stuck on Lambda Node 18 / 20, options are:

- **Polyfill**: bundle `core-js` for Iterator helpers and the Set methods. ~5 KB cost.
- **Bundle with esbuild target ES2022**: down-level the source. Removes the Set methods (esbuild does not polyfill, it only down-levels syntax).
- **Move to Lambda Node 22**: the recommended path.

v1 gates do not run on Lambda Node 18 or 20. v2 may add an opt-in `--target=lambda-node20` flag that includes the core-js polyfills automatically.

### 13.2 Cloudflare Workers

Cloudflare Workers run on the V8 isolate runtime ("workerd"), not on Node. The runtime APIs are Web Platform plus a few Cloudflare-specific extensions (Durable Objects, KV, R2, D1). Workers does not expose `node:` builtins by default (they require an `nodejs_compat` flag, partial coverage).

Why we do not gate against Workers in v1:

- The agent supervision pattern uses unbounded async loops which would hit Workers' CPU-time limit (the request-scoped limit). Long-running agents do not fit the Workers model.
- The IO surface (`mochi_runtime/io/`) has no Workers-native implementation. Workers does not have filesystem; the dominant pattern is KV / R2 / D1 instead. Mapping `readFile / writeFile / spawn / cwd` to KV does not preserve Mochi semantics.
- Durable Objects offer a stateful primitive that maps roughly to an agent, but the API surface is bespoke and would require a separate code generator.

Cloudflare Workers is documented as a v2 separate target (`--target=cloudflare-workers`) with its own runtime shim. The shim would:

- Replace `readFile / writeFile` with KV `get / put`.
- Replace `spawn` with `fetch` to a sibling worker.
- Replace agent supervision with the Durable Objects pattern.
- Use the Workers `fetch` event handler as the main loop.

v1 ships nothing for Workers.

### 13.3 Electron

Electron embeds Node in a desktop application shell. The renderer process is a Chromium browser; the main process is Node. For Mochi, Electron is implicitly supported:

- Main process code uses the Node target (`dist/node/`).
- Renderer process code uses the Browser target (`dist/browser/`).
- Communication between them is via Electron's IPC.

We do not test against Electron in CI (the test harness would need to drive a desktop app). v1 documentation includes a small Electron-tutorial section showing how to wire up `@mochi/runtime` in both processes. v2 may add a `--target=electron-main` and `--target=electron-renderer` for convenience, but the current path (Node target + Browser target) works without changes.

### 13.4 React Native

React Native runs JavaScript on Hermes (the React Native team's purpose-built JS engine, replacing JSC). Hermes targets ECMAScript 5+ with some ES6+ features; Hermes 0.13+ supports many ES2022+ features but not all ES2024.

What Hermes (as of Hermes 0.18, 2024-Q4) supports:

- `Promise.withResolvers`: yes (Hermes 0.13+).
- Iterator helpers: no (planned for 2025).
- `Set` methods (intersection etc.): no.
- `Object.groupBy` / `Map.groupBy`: no.
- `Array.prototype.toSorted` etc.: yes.
- `String.prototype.isWellFormed`: no.

This puts Hermes below our floor. React Native is a separate v2 target (`--target=react-native`) requiring polyfills and a Hermes-aware tsconfig.

The dominant React Native bundler is Metro, which has its own quirks around `package.json` `exports`. Metro 0.80+ supports `exports` but with `react-native` as the priority condition. We do not target this in v1.

### 13.5 Vercel Edge runtime

Vercel Edge runtime is based on V8 isolates (similar architecture to Cloudflare Workers). It has somewhat better Node compatibility than Workers but worse than full Node. We do not gate against Vercel Edge in v1.

For Vercel users:

- Vercel's `nodejs` runtime (the default for serverless functions) is Node 22.x as of late 2024. This is supported, use the `dist/node/` build.
- Vercel's `edge` runtime is unsupported in v1.

### 13.6 macOS app extensions, browser extensions

macOS app extensions and browser extensions (WebExtensions, Chrome extensions) run JavaScript with various restrictions:

- Browser extensions are bundled as ZIPs of HTML/CSS/JS, loaded by the browser. The browser target works here (run `mochi build --target=browser-bundle` and copy the output into `manifest.json`'s `web_accessible_resources`).
- macOS app extensions run in JavaScriptCore (Safari's engine). They are basically Safari + a smaller permission set. The browser target works.

We do not have a dedicated extension target, but the browser bundle suffices.

## 14. Future targets (out of scope for v1)

For reference, the v2 candidates and where they would fit:

| Future target              | Tag                | Trigger to schedule                   |
|----------------------------|--------------------|---------------------------------------|
| Cloudflare Workers          | `cf-workers`       | Durable Objects API mapped to agents  |
| Lambda Node 18 / 20 (legacy)| `lambda-legacy`    | core-js polyfill bundle               |
| React Native                | `react-native`     | Hermes catches up on Iterator helpers |
| Deno Deploy (edge)          | `deno-deploy`      | already works via Deno target, no separate flag needed |
| Vercel Edge                 | `vercel-edge`      | aligned with Workers shim             |
| Tauri (Rust + WebView)      | `tauri-webview`    | WebView 2 / WebKit matrix             |
| Capacitor (iOS / Android)   | `capacitor-ios` / `capacitor-android` | requires native plugin work |

The v2 targets share the codegen pipeline (the IR is the same) but each gets its own conditional-exports entry and runtime shim.

## 15. Cold-start and bundle-size budgets

Cold start: time from `node dist/node/index.js` (or equivalent) to first user-observable line of output. Hello World.

| Runtime          | Cold start | Notes                                    |
|------------------|------------|------------------------------------------|
| Node 22          | ~50 ms     | V8 startup + module graph + Hello World  |
| Deno 2           | ~45 ms     | Deno's startup is slightly faster        |
| Bun 1.1          | ~10 ms     | JavaScriptCore startup is much faster    |
| Browser (modern) | ~25 ms     | parse + execute the 12 KB gzip bundle    |

Bun's lead on cold start comes from a faster JS engine startup (JSC vs V8) and a lighter runtime initialisation. We do not optimise for Bun cold start specifically; the numbers are reported as a snapshot.

Hot iteration: time per request for an HTTP server that does a simple agent call and returns.

| Runtime          | Per-request | Notes                                    |
|------------------|-------------|------------------------------------------|
| Node 22          | ~150 us     | http server overhead dominates           |
| Deno 2           | ~120 us     | Deno.serve is slightly faster            |
| Bun 1.1          | ~50 us      | Bun.serve is the fastest                 |
| Browser (modern) | n/a         | not an HTTP server use case              |

Numbers from a microbenchmark on a 2024 M2 Mac mini, single connection, keep-alive.

Bundle sizes:

| Artifact                        | Size raw | Size gzip |
|---------------------------------|----------|-----------|
| dist/node/ (whole)              | 80 KB    | 22 KB     |
| dist/deno/ (whole)              | 78 KB    | 21 KB     |
| dist/bun/ (whole)               | 79 KB    | 22 KB     |
| dist/browser/index.modern.js    | 38 KB    | 12 KB     |
| dist/browser/index.legacy.js    | 92 KB    | 28 KB     |
| npm tarball (whole package)     | 320 KB   | 95 KB     |

The browser bundle is the size most users see (it ships to end-users). 12 KB gzip Hello World is on the small side for a JS runtime, comparable to Svelte and below React.

## 16. LICENSE bundling

The Mochi runtime is licensed under Apache-2.0. The `package.json` declares:

```json
{
  "license": "Apache-2.0",
  "licenseFile": "LICENSE"
}
```

(SPDX license identifier; `npm` honours this.)

The `files` allowlist includes `LICENSE`:

```json
{
  "files": ["dist/**", "package.json", "README.md", "LICENSE"]
}
```

So every tarball ships the LICENSE.

User code emitted by Mochi inherits the user's chosen license; the user's own `package.json` declares it.

JSR also reads the `LICENSE` file from the package source.

## 17. SBOM and supply-chain integrity

Software Bill of Materials (SBOM) for npm packages uses one of:

- `@cyclonedx/cdxgen` (CycloneDX 1.6 SBOM JSON).
- `npm sbom --sbom-format=cyclonedx` (npm 10.0+, native).
- `spdx-sbom-generator` (SPDX-format SBOM).

We integrate `npm sbom`:

```bash
npm sbom --sbom-format=cyclonedx > sbom.json
```

The output lists every transitive dependency (just `@mochi/runtime` itself plus its zero direct dependencies for the runtime variant) with SHA512 (npm's hash algo), license, PURL identifier, and vulnerability metadata cross-referenced via OSV.dev.

Supply-chain integrity:

- **Sigstore + GitHub OIDC**: `npm publish --provenance` (npm 9.5+) creates a Sigstore attestation linking the published tarball to the GitHub Actions workflow that produced it. The attestation is stored in the Rekor transparency log. Verifiers (`npm audit signatures`) check that the attestation matches the published tarball.
- **JSR provenance**: `deno publish` uses the same GitHub OIDC token to attest. JSR records the workflow URL on the package page.
- **`npm install` lockfile**: `package-lock.json` includes SHA512 hashes (`integrity` field) for every transitive dependency. `npm install` rejects packages with mismatched hashes.

The runtime has zero direct dependencies for the published tarball; transitive dependencies enter only via dev tooling (esbuild, tsc, eslint, prettier), which is dev-only and not shipped.

## 18. Comparison to MEP-49 (Swift), MEP-50 (Kotlin), MEP-51 (Python)

| Dimension              | MEP-49 Swift         | MEP-50 Kotlin            | MEP-51 Python             | MEP-52 TS                  |
|------------------------|----------------------|--------------------------|---------------------------|----------------------------|
| Output unit            | native binary        | jar / klib / kexe / js   | wheel (zip of .py)        | npm tarball (dist/*.js + .d.ts) + JSR scope |
| Per-target artifacts   | 1 per triple         | many (JVM, Native, JS, Wasm) | 1 universal           | 4 dist (Node, Deno, Bun, Browser) |
| Runtime bundled?       | n/a (compiled)       | JVM runtime separate     | not bundled               | not bundled                |
| Build tool             | SwiftPM              | Gradle                   | uv + hatchling            | npm + tsc + esbuild        |
| Lockfile               | Package.resolved     | Gradle catalogs          | uv.lock                   | package-lock.json          |
| Conditional dispatch   | n/a                  | per-target compile       | n/a                       | exports field              |
| Reproducibility flag   | n/a                  | -Xreproducible-builds    | SOURCE_DATE_EPOCH         | SOURCE_DATE_EPOCH + sorted tar |
| Cross-target gate      | yes (5 triples)      | yes (8 targets)          | yes (6 runners)           | yes (4 runtimes)           |
| Free-threaded story    | n/a                  | n/a                      | 3.13t (future)            | free-threaded JS (none, JS is single-threaded by design; Workers are the parallelism story) |
| Browser story          | SwiftWasm v2 (future)| Kotlin/Wasm v1           | Pyodide (future)          | Browser target (tier 1)    |
| JIT story              | n/a                  | JIT on JVM               | n/a                       | V8 / JSC / SpiderMonkey JIT|

The TS / JS target is the only one of the four where the browser is tier 1 from day one (Python, Kotlin, Swift treat browser as a v2 or wasm-via-bridge story). This reflects the JS ecosystem reality: browsers are the dominant deployment surface for a large fraction of JS code.

## 19. Per-runtime test matrix walkthrough

The GitHub Actions matrix that gates a Mochi-to-TS PR:

```yaml
name: mep-52 ts target gates
on: [push, pull_request]

jobs:
  build-and-test:
    strategy:
      fail-fast: false
      matrix:
        include:
          - os: ubuntu-22.04
            runtime: node
            version: "22.11.0"
            tier: 1
          - os: ubuntu-22.04
            runtime: deno
            version: "2.1.4"
            tier: 1
          - os: ubuntu-22.04
            runtime: bun
            version: "1.1.40"
            tier: 1
          - os: ubuntu-22.04
            runtime: browser
            version: "playwright-chromium-122"
            tier: 1
          - os: macos-14
            runtime: node
            version: "22.11.0"
            tier: 1
          - os: macos-14
            runtime: deno
            version: "2.1.4"
            tier: 1
          - os: macos-14
            runtime: bun
            version: "1.1.40"
            tier: 1
          - os: windows-2022
            runtime: node
            version: "22.11.0"
            tier: 1
          - os: windows-2022
            runtime: bun
            version: "1.1.40"
            tier: 1
          - os: ubuntu-22.04
            runtime: node
            version: "24.0.0"
            tier: advisory
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      - if: matrix.runtime == 'node'
        uses: actions/setup-node@v4
        with:
          node-version: ${{ matrix.version }}
      - if: matrix.runtime == 'deno'
        uses: denoland/setup-deno@v2
        with:
          deno-version: v${{ matrix.version }}
      - if: matrix.runtime == 'bun'
        uses: oven-sh/setup-bun@v2
        with:
          bun-version: ${{ matrix.version }}
      - if: matrix.runtime == 'browser'
        uses: microsoft/playwright-github-action@v1
      - run: npm ci
      - run: npm run build
      - if: matrix.runtime == 'node'
        run: node dist/node/index.test.js
      - if: matrix.runtime == 'deno'
        run: deno test --allow-read --allow-net --allow-env
      - if: matrix.runtime == 'bun'
        run: bun test
      - if: matrix.runtime == 'browser'
        run: npx playwright test tests/browser/
      - if: matrix.tier == 1
        run: npm run test:cross-runtime
```

The cross-runtime differential is run on Ubuntu Linux only (the runtimes are deterministic across OS for the typical Mochi corpus; we still run macOS and Windows variants to catch any per-OS bug in the runtime itself).

## 20. Per-platform install paths

How does a developer install each runtime on each OS? A snapshot.

### 20.1 Ubuntu / Debian

```bash
# Node 22 via NodeSource:
curl -fsSL https://deb.nodesource.com/setup_22.x | sudo -E bash -
sudo apt install nodejs

# Or via nvm (preferred):
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.1/install.sh | bash
nvm install 22

# Deno:
curl -fsSL https://deno.land/install.sh | sh

# Bun:
curl -fsSL https://bun.sh/install | bash
```

### 20.2 macOS

```bash
# Homebrew (preferred):
brew install node@22 deno bun

# Or via fnm (faster nvm alternative):
brew install fnm
fnm install 22
```

### 20.3 Windows

```powershell
# winget:
winget install OpenJS.NodeJS.LTS
winget install DenoLand.Deno
winget install Oven-sh.Bun

# Or via fnm:
winget install Schniz.fnm
fnm install 22
```

### 20.4 Alpine

```sh
apk add nodejs npm

# Deno: download binary (Alpine has no official package as of late 2024)
wget https://github.com/denoland/deno/releases/download/v2.1.4/deno-x86_64-unknown-linux-musl.zip
unzip deno-x86_64-unknown-linux-musl.zip -d /usr/local/bin/

# Bun: same pattern
curl -fsSL https://bun.sh/install | bash
```

Note Bun's Alpine support is partial (musllinux binaries shipped since Bun 1.1.0 but with some FFI limitations). We do not gate against Bun-on-Alpine but it generally works.

## 21. The runtime detection shim

The `mochi_runtime/index.ts` top-level dispatcher selects the IO implementation based on globalThis. The shape:

```typescript
import type { MochiIo } from "./io/index.js";

let detected: "node" | "deno" | "bun" | "browser" | "unknown" = "unknown";

if (typeof (globalThis as { Bun?: unknown }).Bun !== "undefined") {
  detected = "bun";
} else if (typeof (globalThis as { Deno?: unknown }).Deno !== "undefined") {
  detected = "deno";
} else if (
  typeof process !== "undefined" &&
  typeof process.versions?.node === "string"
) {
  detected = "node";
} else if (
  typeof window !== "undefined" &&
  typeof document !== "undefined"
) {
  detected = "browser";
}

async function loadIo(): Promise<MochiIo> {
  switch (detected) {
    case "node": {
      const mod = await import("./io/node.js");
      return mod.io;
    }
    case "deno": {
      const mod = await import("./io/deno.js");
      return mod.io;
    }
    case "bun": {
      const mod = await import("./io/bun.js");
      return mod.io;
    }
    case "browser": {
      const mod = await import("./io/browser.js");
      return mod.io;
    }
    default: {
      throw new Error("mochi-runtime: unable to detect host runtime");
    }
  }
}

export const io: MochiIo = await loadIo();
```

The top-level `await` requires ESM (which is enforced). The conditional exports map (§2) means that the `import("./io/node.js")` resolves to the correct file at module-resolution time (not runtime), so the unused branches are tree-shakeable.

For the browser bundle (esbuild), the runtime detection collapses at build time: esbuild's tree-shaker sees that `typeof process === "undefined"` after the `define:` pass and removes the Node branch entirely. The resulting bundle only contains the browser IO.

## 22. Type-check on every runtime

`tsc --noEmit` runs per project:

```bash
tsc --noEmit -p tsconfig.node.json
tsc --noEmit -p tsconfig.deno.json
tsc --noEmit -p tsconfig.bun.json
tsc --noEmit -p tsconfig.browser.json
```

Each is gated separately. A type error in `src/io/node.ts` (which uses `node:fs`) fails only the Node project; the Browser project does not see it because the file is excluded.

The Mochi-emitted user code references `@mochi/runtime` and is type-checked once per runtime project. Idiomatic user code is runtime-agnostic and passes all four projects.

If a user explicitly uses Node-only APIs in their Mochi code (e.g. `import "node:fs"` via a Mochi FFI declaration), the IR pass marks the program as Node-only and the Mochi build refuses to emit the Deno / Bun / Browser projects. This is opt-in via `mochi.json`'s `runtime` field.

## 23. JSR vs npm publish flow

Both registries publish from the same source on a tag push. The two workflows:

```yaml
# .github/workflows/npm-publish.yml
name: npm-publish
on:
  push:
    tags: ['v*']
permissions:
  contents: read
  id-token: write
jobs:
  publish:
    runs-on: ubuntu-22.04
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: '22.11.0'
          registry-url: 'https://registry.npmjs.org'
      - run: npm ci
      - run: npm run build
      - run: npm publish --provenance --access public
        env:
          NODE_AUTH_TOKEN: ${{ secrets.NPM_TOKEN }}

# .github/workflows/jsr-publish.yml
name: jsr-publish
on:
  push:
    tags: ['v*']
permissions:
  contents: read
  id-token: write
jobs:
  publish:
    runs-on: ubuntu-22.04
    steps:
      - uses: actions/checkout@v4
      - uses: denoland/setup-deno@v2
        with:
          deno-version: v2.1.4
      - run: deno publish
```

Both run in parallel; if either fails, the release is partial. We document this and the recovery path (`npm publish --provenance --tag manual` to redo just the npm side).

## 24. Behaviour across runtimes: known deltas

Despite the runtimes converging on Web Platform APIs plus ES2024, a few deltas survive that we have to either work around or document:

- **`process.argv`**: Node and Bun expose this; Deno uses `Deno.args` (excludes the script name). The Mochi `argv` runtime wraps both: `mochiArgv(): readonly string[]`.
- **`process.stdout` vs `Deno.stdout`**: Node and Bun expose `process.stdout` (a Writable stream); Deno exposes `Deno.stdout` (a `Deno.Writer`). We wrap both behind `mochiStdoutWrite(data: Uint8Array): Promise<void>`.
- **Console formatter**: Node and Deno's `console.log` formatting differs in edge cases (e.g. how `Map` is printed, how circular objects are handled). For golden-output tests we do not use `console.log`; we use `process.stdout.write(JSON.stringify(...))` or equivalent.
- **`fetch` redirect handling**: all four runtimes follow redirects by default, but Deno's redirect chain limit is 20, Node's is 20, Bun's is unbounded (configurable). We force `redirect: "follow"` and trust the runtime default count.
- **`crypto.subtle`**: all four are W3C-compliant, but Deno historically required HTTPS context until 1.40 (now relaxed). Mochi crypto APIs use `crypto.subtle` exclusively, no fallback.
- **`AbortSignal.timeout`**: Node 17.3+, Deno 1.34+, Bun 1.0+, Browser Baseline 2024. We use it without polyfill.
- **`Symbol.dispose`**: ES2024 stage 4. All four runtimes ship this. We use `using` declarations in the runtime for file handles.
- **Top-level await**: works everywhere ESM-supported.
- **Source maps**: Node 22+ honours inline source maps via `--enable-source-maps` flag; we generate external `.map` files and let runtimes pick them up.

## 25. Summary

The TS / JS runtime portability story spans four tier-1 runtimes: Node 22 LTS, Deno 2, Bun 1.1, and Baseline 2024 browsers. One TS source tree feeds four dist variants via `tsc -b` with per-runtime tsconfig projects. The `package.json` `exports` conditional map routes importers to the right dist file. Node-only APIs are isolated under `mochi_runtime/io/` so the browser bundle tree-shakes them. JSR co-publish lets Deno users consume `@mochi/runtime` natively without the npm compat shim. The browser bundle uses esbuild for tree-shaking and minification, with a separate legacy variant carrying core-js polyfills for sub-Baseline-2024 environments. Reproducibility comes from `SOURCE_DATE_EPOCH` plus sorted tarball entries. v1 excludes AWS Lambda Node 18 / 20 (below floor), Cloudflare Workers (separate target), Electron (works via Node + Browser), and React Native (separate target due to Hermes engine gaps).

The companion notes pick up: [[06-type-lowering]] for the TS type emission strategy that all four runtimes share, [[08-dataset-pipeline]] for the query DSL lowering that uses Iterator helpers (with browser polyfill), [[09-agent-streams]] for the agent / stream lowering that runs identically on all four runtimes, [[10-build-system]] for the npm + tsc + esbuild build orchestration, [[11-testing-gates]] for the cross-runtime differential gate, and [[12-risks-and-alternatives]] for the v2 candidates (Cloudflare Workers, React Native, Vercel Edge, Tauri).
