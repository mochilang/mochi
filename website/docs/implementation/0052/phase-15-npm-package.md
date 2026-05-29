---
title: "Phase 15. npm package build (tsc --build + npm pack)"
sidebar_position: 16
sidebar_label: "Phase 15. npm package build"
description: "MEP-52 Phase 15, full --target=npm-package pipeline; tsc --build over project references; per-runtime conditional dist; npm pack tarball; npm install from tarball into fresh dir; execute on Node 22, Deno 2, Bun 1.1, Chromium 130."
---

# Phase 15. npm package build

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 15](/docs/mep/mep-0052#phase-plan) |
| Status         | NOT STARTED |
| Started        | n/a |
| Landed         | n/a |
| Tracking issue | n/a |
| Tracking PR    | n/a |

## Gate

`TestPhase15NpmPackage`: every fixture in the Phase 1 through Phase 14 corpus (~400 fixtures cumulative by Phase 15) executes correctly via the `--target=npm-package` path: emit source, `tsc --build`, `npm pack`, `npm install <tarball>` into a fresh `/tmp/mochi-test-<n>/` directory, then `node dist/node/index.js` / `deno run dist/deno/index.js` / `bun dist/bun/index.js` / Playwright Chromium 130 on the browser bundle. The stdout from each path must `diff` clean against the vm3 recording. Secondary gates: `npm audit signatures` clean (no warnings) on every installed tarball; emitted `package.json` validates against the npm schema; the `dist/` tree has matching `.d.ts` per `.js`.

## Goal-alignment audit

Phase 15 is the first phase that produces a real installable artefact. Before Phase 15, `mochi build --target=typescript-source` writes a `.ts` source tree that the user is expected to compile themselves. After Phase 15, `mochi build --target=npm-package` produces a `.tgz` that `npm install <tarball>` installs anywhere, and the installed package runs identically on all four tier-1 runtimes. This is the gate for "Mochi can ship to npm" and the prerequisite for Phase 16 (reproducibility) and Phase 18 (Trusted Publishing).

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 15.0 | Composite `tsconfig.json` with project references for `node`, `deno`, `bun`, `browser`; `tsc --build` walks the chain in one invocation | NOT STARTED | n/a |
| 15.1 | Per-runtime tsconfig (`tsconfig.{node,deno,bun,browser}.json`) extending base; outDir per runtime; lib set per runtime | NOT STARTED | n/a |
| 15.2 | `package.json` `exports` conditional map fully populated; `types` first; `node`, `deno`, `bun`, `browser` middle; `default` last | NOT STARTED | n/a |
| 15.3 | `npm pack` invocation; tarball written to `outDir/<pkg>-<ver>.tgz`; `files` field enforces the dist whitelist | NOT STARTED | n/a |
| 15.4 | Install-from-tarball gate: `npm install <tarball>` into fresh `/tmp/<dir>/`; run on Node, Deno, Bun, Chromium | NOT STARTED | n/a |

## Sub-phase 15.0, Composite tsconfig

### Decisions made (15.0)

**Root `tsconfig.json`**:

```json
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

**`tsc --build`**: walks each project reference, builds in dependency order, caches per-project incremental state (`tsconfig.<runtime>.tsbuildinfo`). Cold build is roughly 2-5 seconds for a Phase 1 hello fixture; incremental cache hit is roughly 200 ms.

## Sub-phase 15.1, Per-runtime tsconfig

### Decisions made (15.1)

**`tsconfig.base.json`** (per MEP-52 §9):

```json
{
  "compilerOptions": {
    "strict": true,
    "noUncheckedIndexedAccess": true,
    "exactOptionalPropertyTypes": true,
    "noImplicitOverride": true,
    "noFallthroughCasesInSwitch": true,
    "noPropertyAccessFromIndexSignature": true,
    "isolatedModules": true,
    "verbatimModuleSyntax": true,
    "target": "es2024",
    "module": "esnext",
    "moduleResolution": "bundler",
    "declaration": true,
    "declarationMap": true,
    "sourceMap": true,
    "rewriteRelativeImportExtensions": true,
    "composite": true
  }
}
```

**`tsconfig.node.json`**:

```json
{
  "extends": "./tsconfig.base.json",
  "compilerOptions": { "outDir": "./dist/node", "lib": ["es2024"] },
  "include": ["src/**/*.ts"]
}
```

**`tsconfig.deno.json`**, **`tsconfig.bun.json`**: same shape, different `outDir`.

**`tsconfig.browser.json`**: `lib: ["es2024", "dom"]`. The DOM lib gives `document`, `window`, etc. without complaint. Node-only imports (`node:fs`, `node:net`) are replaced in this config's source set via the `"browser"` export condition (a per-package mapping in `package.json`).

## Sub-phase 15.2, exports conditional map

### Decisions made (15.2)

**Canonical `exports`** (per MEP-52 §2):

```json
{
  "exports": {
    ".": {
      "types": "./dist/index.d.ts",
      "deno": "./dist/deno/index.js",
      "bun":  "./dist/bun/index.js",
      "browser": "./dist/browser/index.js",
      "node": "./dist/node/index.js",
      "default": "./dist/node/index.js"
    },
    "./package.json": "./package.json"
  }
}
```

**Per-subpath entries**: when the user has multiple top-level modules, each gets its own entry. Phase 4's multi-file layout drives the subpath list.

**Why `types` first**: TypeScript's resolver picks the first matching key. Putting `types` first ensures `tsc --moduleResolution bundler` finds the `.d.ts` even when other conditions also match.

## Sub-phase 15.3, npm pack

### Decisions made (15.3)

**`npm pack`** runs in the project root after `tsc --build`. Output: `<pkg>-<ver>.tgz` (e.g., `mochi-hello-0.0.1.tgz`).

**`files` whitelist** in `package.json`:

```json
{
  "files": ["dist/", "README.md", "LICENSE"]
}
```

The whitelist excludes `src/`, `tsconfig*.json`, `.eslintrc.json`, `.prettierrc.json`, `tests/`, `node_modules/`. The published tarball is dist-only; the consumer never sees the source `.ts` files unless they were emitted into `dist/`.

**Tarball contents**: `package/package.json`, `package/README.md`, `package/LICENSE`, `package/dist/...`. npm's spec puts everything under a `package/` directory inside the tarball.

## Sub-phase 15.4, Install from tarball

### Decisions made (15.4)

**Gate harness**:

```bash
mkdir -p /tmp/mochi-test-$N
cd /tmp/mochi-test-$N
npm init -y
npm install $TARBALL_PATH
node -e "require('mochi-hello')"  # or import
```

**Per-runtime variant**:

- Node: `node -e "import('mochi-hello')"` (ESM dynamic import).
- Deno: `deno run --allow-read -A 'npm:mochi-hello'`. Deno resolves npm-installed packages via the `npm:` specifier.
- Bun: `bun run -e "import('mochi-hello')"`.
- Chromium: the test harness serves `node_modules/mochi-hello/dist/browser/index.js` via Playwright's `route` handler, loads it in a page, captures `console.log`.

**Diff vs vm3**: each runtime's captured stdout is `diff`ed against the vm3 recording. Any mismatch is a Phase 15 failure.

## Files (planned)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/emit/project.go` | All tsconfig files; root composite; per-runtime |
| `transpiler3/typescript/build/npm_pack.go` | `npm pack` subprocess and tarball-path return |
| `transpiler3/typescript/build/install_gate.go` | Tarball install + runtime execution + stdout capture |
| `transpiler3/typescript/build/phase15_test.go` | `TestPhase15NpmPackage`, runs across Phase 1-14 fixtures |

## Test set

- `TestPhase15NpmPackage`, full corpus install-and-execute gate.
- `TestPhase15Schema`, emitted `package.json` validates against `https://json.schemastore.org/package.json`.
- `TestPhase15DistTypesPaired`, every `.js` in `dist/` has a matching `.d.ts`.
- `TestPhase15FilesWhitelist`, the published tarball never contains `src/`, `tsconfig*.json`, `.eslintrc.json`.

## Deferred work

- pnpm install from tarball as a separate gate. The `package-lock.json` is pnpm-readable but the install behaviour diverges slightly; Phase 15 ships npm-only gate.
- Bun's `bun install <tarball>` as a separate gate. Bun reads `package-lock.json` but its install behaviour is slightly different.
- `npm publish --dry-run` (without provenance). Phase 18 ships the with-provenance form.
- Cyclic project references. Mochi's module graph is acyclic by language rule.
