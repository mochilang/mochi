---
title: "MEP-72 Note 10: Runtime target matrix"
sidebar_position: 11
sidebar_label: "10. Runtime targets"
description: "The five JS runtimes the bridge targets: Node 22 LTS, Deno 2, Bun 1.1, browser ES2024, edge (Cloudflare Workers + Vercel Edge + Deno Deploy). What each runtime supports, what API surface differs, what the bridge promises per target, what features are deferred."
---

# 10. Runtime target matrix

This note enumerates the JS runtimes the bridge targets and the feature deltas. It is informative.

## 1. Node 22 LTS (October 2024 LTS release; maintenance support through April 2027)

Capabilities:

- Built-in `fetch` (Node 18+).
- `node:*` built-ins (fs, net, http, child_process, worker_threads, crypto, stream, util).
- ESM + CJS interop.
- `--experimental-vm-modules` for advanced module patterns (rarely used by bridge).
- Native `WebSocket` (Node 22+).
- `crypto.subtle` for WebCrypto.

Limitations:

- No `Deno.*` global.
- No browser APIs (no `window`, no `document`, no `localStorage`).
- No `Bun.*` global.

Bridge behaviour: Node 22 LTS is the default `runtime = "node22"` target. The `[ts.capabilities]` table maps directly to the `node:*` import surface.

## 2. Deno 2 (October 2024 release)

Capabilities:

- Built-in `fetch`, `WebSocket`, `crypto.subtle`.
- `Deno.*` global (Deno-specific APIs like `Deno.readFile`, `Deno.serve`, `Deno.test`).
- ESM-only by default; CJS via `npm:` specifier and `node:` prefix.
- Native TypeScript without compilation (Deno's V8 has its own TS layer).
- Import maps via `deno.json`'s `imports` field.
- JSR support via `jsr:` specifier (built-in).

Limitations:

- No `module.exports` syntax (ESM-only at the file level).
- `node:fs` works but via Deno's polyfill (subtle differences).
- The `Deno.*` global is not on Node / Bun.

Bridge behaviour: `runtime = "deno2"` produces emit that uses Deno-native APIs where they differ from Node (`Deno.serve` instead of `http.createServer`). JSR consumes are native.

## 3. Bun 1.1 (1.0 GA September 2023; 1.1 release April 2024)

Capabilities:

- Built-in `fetch`, `WebSocket`, `crypto.subtle`.
- `Bun.*` global (Bun-specific APIs like `Bun.file`, `Bun.serve`, `Bun.write`).
- ESM + CJS interop (fastest of the three native runtimes).
- Built-in TypeScript without compilation.
- Built-in JSX without compilation.
- Built-in `bun build` bundler (used by MEP-52 phase 17 for browser bundle).
- Compatible with most `node:*` built-ins via a Node.js compat layer.

Limitations:

- Newer than Node / Deno; some edge-case Node APIs are not yet implemented.
- The `Bun.*` global is not on Node / Deno.

Bridge behaviour: `runtime = "bun1.1"` produces emit that uses Bun-native APIs where they differ. The `node:*` import path works via Bun's compat layer.

## 4. Browser ES2024

Capabilities:

- ESM via `<script type="module">` or via `<link rel="modulepreload">`.
- All standard Web APIs (`window`, `document`, `fetch`, `WebSocket`, `Worker`, `IndexedDB`, `localStorage`, `crypto.subtle`).
- Import maps via `<script type="importmap">` (Chrome 89+, Firefox 108+, Safari 16.4+).
- TC39 stage 4 features (top-level await, private class fields, dynamic import).

Limitations:

- No CJS support (CJS-only packages require a bundler).
- No `node:*` built-ins (no `fs`, `net`, `child_process`, `worker_threads`).
- DOM-API surface area (different from server-side runtimes).

Bridge behaviour: `runtime = "browser"` produces emit that uses Web APIs only. The `TargetBrowserBundle` path bundles all deps into a single ESM artefact via `bun build` (primary) or `esbuild` (fallback). The bridge refuses to lock a consumed package whose `engines.node` or `engines.workerd` declares incompatibility with the browser.

## 5. Edge (Cloudflare Workers, Vercel Edge, Deno Deploy)

Capabilities:

- Subset of Web APIs (`fetch`, `Request`, `Response`, `Headers`, `crypto.subtle`, `WebSocket`).
- Some platform-specific globals (Cloudflare's `caches`, Vercel's `geolocation`, Deno Deploy's `Deno.serve`).

Limitations:

- No `node:fs` (no filesystem).
- No `node:net` (no TCP socket).
- No `node:child_process` (no process spawn).
- No `node:worker_threads` (limited worker support via the platform's own Worker primitive).
- Bundle size limits (Cloudflare Workers: 1 MB compressed; Vercel Edge: 4 MB; Deno Deploy: 10 MB).

Bridge behaviour: `runtime = "edge"` is the strictest target. The `[ts.capabilities]` table's `fs`, `proc`, `worker` flags must be `false` for an edge build. The bridge's edge-runtime gate (phase 17) cross-checks the consumed packages' import surface against the allowed edge API list.

## 6. Cross-runtime consistency

MEP-72's gate requires:

- The same Mochi source compiled for `runtime = "node22"`, `"deno2"`, and `"bun1.1"` produces identical stdout when run with the same fixture inputs (matches MEP-52 phase 1+'s "run on all three" gate).
- The `runtime = "browser"` and `"edge"` builds produce bundles that pass their respective gate suites; runtime parity with Node is not required (browser and edge have intentionally restricted APIs).

## 7. Deferred runtime targets

- **Node 20 LTS** (maintenance until April 2026): not gated because Node 22 LTS is current.
- **Older browsers (pre-ES2024)**: not gated; the ES2024 target is the floor.
- **React Native** (Hermes engine): not gated; React Native's JS engine has its own constraints.
- **Tauri (V8 / WebView2)**: not gated; would inherit the browser target.

## 8. Cross-references

- [[09-esm-cjs-interop]] — the per-runtime interop semantics.
- [[11-browser-bundle-surface]] — the browser bundle path.
- [MEP-52 §Phases](/docs/mep/mep-0052#phases) — the underlying transpiler targets.
