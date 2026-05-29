---
title: "Phase 17. JSR + Jupyter + browser bundle"
sidebar_position: 18
sidebar_label: "Phase 17. JSR + Jupyter + browser"
description: "MEP-52 Phase 17, three secondary packaging targets: --target=deno-jsr (deno publish to jsr.io), --target=deno-jupyter (kernelspec under ~/.local/share/jupyter/kernels/), --target=browser-bundle (esbuild single ESM file + importmap); 25 fixtures."
---

# Phase 17. Deno JSR + Jupyter + browser bundle

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 17](/docs/mep/mep-0052#phase-plan) |
| Status         | NOT STARTED |
| Started        | n/a |
| Landed         | n/a |
| Tracking issue | n/a |
| Tracking PR    | n/a |

## Gate

`TestPhase17Targets`: 25 fixtures green across three packaging targets. (1) `--target=deno-jsr`: `deno publish --dry-run` against a verdaccio-style JSR mirror succeeds; the emitted `jsr.json` validates. (2) `--target=deno-jupyter`: kernelspec installs under `~/.local/share/jupyter/kernels/mochi-deno-<pkg>/`, `jupyter kernelspec list` reports it, a notebook cell containing the fixture source produces the recorded `expect.txt`. (3) `--target=browser-bundle`: esbuild produces a single ESM file under `dist/bundle/index.js`, Playwright Chromium 130 loads it via `<script type="module">`, captures `console.log`, diffs against vm3.

## Goal-alignment audit

Phase 17 ships the three secondary packaging targets. None of them is the npm-package path (Phase 15), but each unlocks a specific deployment surface that npm alone cannot reach. JSR is the Deno-native registry (GA September 2024) and gives Deno users a first-class `jsr:@mochi/foo` specifier instead of the `npm:` shim. Deno Jupyter (`deno jupyter --install`, GA April 2024) is the second notebook path after MEP-51's ipykernel. Browser-bundle is the only Mochi target that reaches a user who never installs a runtime, the unique value-add of MEP-52 over MEP-45 through MEP-51. All three feed off the same Phase 15 emit; only the post-emit driver differs.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 17.0 | `--target=deno-jsr`: emit `jsr.json`; invoke `deno publish --dry-run` against a local JSR mirror | NOT STARTED | n/a |
| 17.1 | `--target=deno-jupyter`: emit kernelspec JSON; install under `~/.local/share/jupyter/kernels/mochi-deno-<pkg>/`; per-cell transpile-on-receipt | NOT STARTED | n/a |
| 17.2 | `--target=browser-bundle`: invoke esbuild on `dist/browser/`; produce single tree-shaken ESM file plus importmap | NOT STARTED | n/a |
| 17.3 | Browser-side runtime stubs for `node:fs`, `node:net`, `node:path`; replaced under the `"browser"` export condition | NOT STARTED | n/a |
| 17.4 | Three-target gate harness: each fixture runs through all three paths in CI | NOT STARTED | n/a |

## Sub-phase 17.0, Deno JSR

### Decisions made (17.0)

**`jsr.json`** (emitted alongside `package.json`):

```json
{
  "name": "@mochi/hello",
  "version": "0.0.1",
  "exports": "./src/index.ts",
  "publish": {
    "include": ["src/**/*.ts", "README.md", "LICENSE"]
  }
}
```

**Why source-not-dist**: JSR transpiles TypeScript on the server and generates `.d.ts` automatically. The published artefact is `.ts` source; the `dist/` tree is npm-only.

**Dry-run**: `deno publish --dry-run --token=$JSR_TOKEN`. The local mirror (`http://localhost:8080`, a Mochi-controlled verdaccio-style JSR shim) verifies the manifest, fetches the source, simulates server-side transpile, and reports any errors without committing. CI runs the dry-run as the Phase 17 gate; the real `deno publish` lands in Phase 18.

**JSR identifier scope**: `@mochi/` is the Mochi project's reserved scope on `jsr.io`. The user's package name is `@mochi/<user-pkg>` when published under the Mochi umbrella, or `@<user-scope>/<pkg>` when the user publishes their own.

## Sub-phase 17.1, Deno Jupyter

### Decisions made (17.1)

**Kernelspec**:

```json
{
  "argv": [
    "deno",
    "jupyter",
    "--unstable",
    "--kernel",
    "{connection_file}",
    "--allow-read",
    "--allow-net",
    "--allow-env"
  ],
  "display_name": "Mochi (Deno)",
  "language": "mochi"
}
```

Installed to `~/.local/share/jupyter/kernels/mochi-deno-<pkg>/kernel.json` (Linux/macOS) or `%APPDATA%/jupyter/kernels/...` (Windows).

**Per-cell transpile-on-receipt**: the kernel wraps Deno's official Jupyter kernel. Each notebook cell is intercepted at the front-end via a custom Mochi codemirror mode that posts the cell source to a Mochi-side transpiler subprocess; the resulting `.ts` is forwarded to Deno's kernel for execution. Cell outputs (`console.log`, `Deno.display`) flow back unchanged.

**Cell-state continuity**: variables bound in cell N are visible in cell N+1, mirroring Deno's kernel behaviour. The transpiler emits each cell as a top-level statement; Deno's kernel runs them in one persistent isolate.

## Sub-phase 17.2, Browser bundle

### Decisions made (17.2)

**esbuild invocation**:

```bash
esbuild dist/browser/index.js \
  --bundle \
  --format=esm \
  --target=es2024 \
  --platform=browser \
  --tree-shaking=true \
  --minify \
  --sourcemap=external \
  --outfile=dist/bundle/index.js
```

**Output**: `dist/bundle/index.js` (single ESM file), `dist/bundle/index.js.map` (separate source map), `dist/bundle/importmap.json` (for users who prefer importmap-driven loading rather than the bundle).

**Why esbuild**: roughly 100x faster than `webpack`, tree-shakes through the `"browser"` export condition, native ESM output, sourcemap support, zero JS deps in the Mochi project (esbuild is a single Go binary).

**HTML harness** (the runtime test wraps this around the bundle):

```html
<!DOCTYPE html>
<script type="module" src="./dist/bundle/index.js"></script>
```

**Playwright capture**: launches Chromium 130, navigates to the harness, listens for `console.log`, captures the full stdout, then diffs against vm3.

## Sub-phase 17.3, Browser runtime stubs

### Decisions made (17.3)

**`@mochi/runtime` `"browser"` export condition** rewrites Node-only imports:

```json
{
  "exports": {
    "./fs": {
      "node": "./dist/node/fs/index.js",
      "deno": "./dist/deno/fs/index.js",
      "bun":  "./dist/bun/fs/index.js",
      "browser": "./dist/browser/fs-stub.js"
    }
  }
}
```

**`fs-stub.js`** throws at call time:

```typescript
export function readFile(_path: string): never {
  throw new MochiPanic("fs.readFile is not available in the browser bundle");
}
```

The emitter's browser-target reachability check (per Phase 12 §12.3 and Phase 13 §13.4) rejects at codegen any reachable call to `fs.readFile`, `net.connect`, or `path.resolve`. The stubs are a defence in depth: if a code-path slips past the reachability check, the runtime fails fast with a `MochiPanic`.

## Sub-phase 17.4, Gate harness

### Decisions made (17.4)

**One fixture, three drivers**: the Phase 17 harness picks each fixture from the Phase 1-14 corpus, then runs:

1. `mochi build --target=deno-jsr -o /tmp/jsr` then `deno publish --dry-run` against the local JSR mirror.
2. `mochi build --target=deno-jupyter -o /tmp/jup` then `jupyter nbconvert --to notebook --execute fixture.ipynb` (the harness pre-builds the notebook from the fixture source).
3. `mochi build --target=browser-bundle -o /tmp/br` then Playwright loads the bundle and captures `console.log`.

Each path diffs against vm3. Any of the three failing fails Phase 17.

## Files (planned)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/build/jsr.go` | `jsr.json` emit + `deno publish --dry-run` subprocess |
| `transpiler3/typescript/build/jupyter.go` | Kernelspec emit + `jupyter kernelspec install` |
| `transpiler3/typescript/build/browser_bundle.go` | esbuild subprocess + importmap emit |
| `runtime3/typescript/src/browser/fs-stub.ts` | Browser stubs for `fs`, `net`, `path` |
| `transpiler3/typescript/build/phase17_test.go` | `TestPhase17Targets` |
| `tests/transpiler3/typescript/fixtures/phase17-targets/` | 25 fixtures plus per-fixture HTML harness for browser |

## Test set

- `TestPhase17JSR`, 25 fixtures dry-run against local JSR mirror.
- `TestPhase17Jupyter`, 25 fixtures executed as one-cell notebooks; `deno jupyter --install --force` runs at test setup.
- `TestPhase17Browser`, 25 fixtures bundled and executed via Playwright Chromium 130.
- `TestPhase17BundleSize`, asserts the hello-world bundle is under 50 KB minified plus gzip (sanity check on tree-shaking).

## Deferred work

- Firefox and Safari runtime tests. Chromium 130 is the Phase 17 gate; FF and Safari are Phase 18 release-channel tests (manual until WebDriver-driven CI proves stable).
- Importmap-only consumption (no bundle, browser loads ESM modules directly via importmap). Works in principle on all four runtimes but the test surface multiplies; Phase 17 ships the bundle path as primary.
- JSR's `deno doc` integration (auto-generated API docs on `jsr.io`). Works out of the box; not a Phase 17 gate.
- Deno Jupyter on Windows (`%APPDATA%/jupyter/kernels/`). Linux and macOS are the Phase 17 gate; Windows lands as a Phase 17.5 followup if user demand justifies.
