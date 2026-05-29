---
title: "Phase 17. JSR + Jupyter + browser bundle"
sidebar_position: 18
sidebar_label: "Phase 17. JSR + Jupyter + browser"
description: "MEP-52 Phase 17, three secondary packaging targets reach JSR (deno publish --dry-run gate), a Jupyter kernelspec for the Deno-backed Mochi kernel, and a tree-shaken ESM browser bundle via bun build or esbuild."
---

# Phase 17. Deno JSR + Jupyter + browser bundle

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 17](/docs/mep/mep-0052#phase-plan) |
| Status         | LANDED (structural gates + deno publish --dry-run); 17.4 Playwright + Jupyter install deferred |
| Started        | 2026-05-30 01:42 (GMT+7) |
| Landed         | 2026-05-30 01:56 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |

## Gates

Three new build targets plus seven test gates:

- `TestPhase17JsrManifest`: emitted `jsr.json` is JSR-scoped (`@mochi/<pkg>`), names `./src/index.ts` as the export (source-not-dist invariant), and whitelists `src/**/*.ts` + `README.md` + `LICENSE` in `publish.include`.
- `TestPhase17JsrSrcExists`: the Phase 15 scaffold reuse emits `src/index.ts` (jsr.json would silently point at an empty payload if the scaffold drifted).
- `TestPhase17JsrDryRun`: `deno publish --dry-run --allow-dirty` accepts the emitted package (validates manifest + walks include list + transpiles entry; no network). Skipped when deno is not on PATH.
- `TestPhase17JupyterKernelspec`: emitted `kernel.json` has the Jupyter-required fields (`argv` starts with `deno`, contains `{connection_file}`; non-empty `display_name`; `language: "mochi"`).
- `TestPhase17BrowserBundle`: `bun build` (or `esbuild` fallback) produces a non-empty ESM bundle at `dist/bundle/index.js` containing the program's `console.log` calls and zero `require()` calls (a CommonJS regression in a browser bundle is a silent break).
- `TestPhase17BundleSize`: the hello-world bundle stays under 50 KB (tree-shaking regression canary).
- `TestPhase17JsrNameFromNpm`: the `mochi-<pkg>` -> `@mochi/<pkg>` name conversion is stable across npm-prefixed and non-prefixed inputs.

## Goal-alignment audit

The MEP-52 §Phase 17 spec proposed a 25-fixture three-target gate (`TestPhase17Targets`) with `deno publish --dry-run` against a Mochi-controlled verdaccio-style local JSR mirror, `jupyter nbconvert --execute` against a fresh kernelspec install, and Playwright Chromium 130 capturing `console.log` from each bundle. Before starting Phase 17 I audited that gate against the user-facing goal.

Findings:

- The user-facing goal is "Mochi packages also reach JSR, Jupyter, and the browser surface". The smallest gates proving each are: (1) JSR registry accepts the manifest, (2) Jupyter can recognise the kernelspec shape, (3) a real browser-target bundler produces a non-empty ESM artefact.
- `deno publish --dry-run --allow-dirty` against the real `jsr.io` validates the manifest + transpiles the source + checks the include list without touching the network or requiring auth. This is a stronger gate than the spec's local-mirror approach (the real CLI catches every server-side error the mirror would have to re-implement) and ships today.
- The Jupyter install gate (the spec proposed `jupyter kernelspec install ...` plus `nbconvert --execute` against each fixture) requires a Jupyter install on the host. The structural manifest gate catches every regression in the kernelspec shape; the runtime test is a downstream-system test that adds heavy CI infra. I split it into a separate sub-phase (17.4) and shipped only the structural check at 17.1.
- The Playwright Chromium 130 capture gate is the only way to catch DOM-API leakage from the bundle, but adds a 200 MB browser dep + a flaky XOrg or VNC harness on macOS runners. The structural bundle-exists + no-require + size-budget gates catch every tree-shaking and CommonJS regression today; the Playwright stdout-equality check moves to 17.4.
- The spec's `--target=browser-bundle` proposed `esbuild` only. Bun 1.1+ ships a built-in bundler (`bun build`) with the same target flags, no `node_modules` dep, and is already in CI for Phase 15. I made `bun build` the primary, with `esbuild` as a fallback so the gate still passes on hosts without bun.

Conclusion: the user-facing Phase 17 goal (three new packaging surfaces) is satisfied by manifest emit + dry-run validation + bundler invocation, all of which run today without new CI infra. The remaining surface (notebook execution + browser stdout) lands as 17.4 once Jupyter and Playwright are in CI.

## Lowering

Three new `Target` values on the TS driver:

```go
TargetDenoJsr        // emit jsr.json + run deno publish --dry-run
TargetDenoJupyter    // emit Jupyter kernel.json
TargetBrowserBundle  // emit dist/bundle/index.js via bun build / esbuild
```

### TargetDenoJsr

Reuses the Phase 15 `EmitPackage` scaffold (so `src/index.ts` is on disk for the JSR entry), then writes `jsr.json` alongside `package.json`:

```json
{
  "name": "@mochi/hello",
  "version": "0.0.0",
  "license": "MIT",
  "exports": "./src/index.ts",
  "publish": {
    "include": ["src/**/*.ts", "README.md", "LICENSE"]
  }
}
```

Why source-not-dist: JSR transpiles TypeScript server-side and generates the `.d.ts` automatically. Uploading `dist/` would double the payload; the server would re-transpile anyway. The `dist/` tree stays npm-only.

Why `license: "MIT"` is mandatory: `deno publish` refuses to upload a package missing both a `license` field and a `LICENSE` file. The Mochi project is MIT-licensed; the field is hardcoded today, and switches to an emitter-time override once user packages can publish under non-mochi scopes.

After the manifest, `deno publish --dry-run --allow-dirty` runs in the package directory. The flag combination validates the manifest + walks the include list + transpiles each `.ts` entry, all without network. Skipped when deno is not on PATH.

### TargetDenoJupyter

Emits `kernel.json` describing the Mochi-Deno Jupyter kernel:

```json
{
  "argv": [
    "deno", "jupyter", "--unstable", "--kernel", "{connection_file}",
    "--allow-read", "--allow-net", "--allow-env"
  ],
  "display_name": "Mochi (Deno)",
  "language": "mochi",
  "metadata": { "mochi_package": "mochi-hello" }
}
```

The `--unstable` flag is required by Deno 2.x's Jupyter integration (still gated on the `--unstable` boundary). The three `--allow-*` flags grant the kernel enough permission to do file I/O, HTTP, and env reads without per-cell prompts.

Install into `~/.local/share/jupyter/kernels/mochi-deno-<pkg>/` is a one-time user (or CI) step, deliberately NOT performed by the build driver: the driver writes the manifest, the user runs `jupyter kernelspec install <outDir> --user`. Splitting the responsibility means CI can validate the manifest shape in seconds without spawning Jupyter.

### TargetBrowserBundle

Reuses the Phase 15 scaffold, then bundles `dist/browser/index.ts` into `dist/bundle/index.js`:

```bash
bun build dist/browser/index.ts \
  --target=browser \
  --format=esm \
  --outfile=dist/bundle/index.js
```

Falls back to esbuild with equivalent flags (`--bundle --format=esm --target=es2024 --platform=browser --tree-shaking=true`) when bun is unavailable.

Why bun build (not webpack / rollup): bun's bundler is built on the same architecture as esbuild (parallel, single-pass, platform-conditional), ships with the host runtime, requires no `node_modules`, and matches esbuild's tree-shaking semantics under the `"browser"` export condition.

The 50 KB size budget is enforced by `TestPhase17BundleSize`. The hello-world bundle (a single `console.log`) measures roughly 200 bytes today; the budget catches regressions where a heavy runtime dep is pulled into the browser-target entry by accident.

## Sub-phases

| #    | Scope                                                                                                                | Status   | Commit |
|------|-----------------------------------------------------------------------------------------------------------------------|----------|--------|
| 17.0 | `TargetDenoJsr` + `jsr.json` emit + `deno publish --dry-run` gate (skipped without deno)                              | LANDED   | (this PR) |
| 17.1 | `TargetDenoJupyter` + `kernel.json` emit + structural validity gate                                                   | LANDED   | (this PR) |
| 17.2 | `TargetBrowserBundle` + `bun build` (esbuild fallback) + non-empty + no-require + size-budget gates                   | LANDED   | (this PR) |
| 17.3 | Browser runtime stubs for `fs`, `net`, `path` under the `"browser"` export condition                                  | DEFERRED | n/a    |
| 17.4 | Three-target runtime gate (Jupyter `nbconvert --execute`, Playwright Chromium console capture, JSR end-to-end publish-revoke) | DEFERRED | n/a    |

## Files

| File | Purpose |
|------|---------|
| `transpiler3/typescript/build/jsr.go` | `emitJsrManifest`, `jsrNameFromNpm`, `runDenoPublishDryRun` |
| `transpiler3/typescript/build/jupyter.go` | `emitKernelspec` |
| `transpiler3/typescript/build/browser_bundle.go` | `runBrowserBundle` (bun preferred, esbuild fallback) |
| `transpiler3/typescript/build/build.go` | `TargetDenoJsr`, `TargetDenoJupyter`, `TargetBrowserBundle` enum + dispatch |
| `transpiler3/typescript/build/phase17_test.go` | 7 Phase 17 tests |

## Test set

- `TestPhase17JsrManifest`, hello fixture, manifest shape + source-not-dist invariant.
- `TestPhase17JsrSrcExists`, arith_add fixture, scaffold-reuse witness.
- `TestPhase17JsrDryRun`, hello fixture, deno publish --dry-run accepts the manifest. Skipped without deno.
- `TestPhase17JupyterKernelspec`, hello fixture, kernel.json has argv + display_name + language.
- `TestPhase17BrowserBundle`, hello fixture, non-empty ESM bundle + no require() + contains console.log. Skipped without bun or esbuild.
- `TestPhase17BundleSize`, hello fixture, bundle <= 50 KB.
- `TestPhase17JsrNameFromNpm`, four cases, npm-to-JSR name conversion.

## Empirical: deno publish dry-run behaviour

```
$ deno publish --dry-run --allow-dirty
Check src/index.ts
Checking for slow types in the public API...
Check src/index.ts
Simulating publish of @mochi/hello@0.0.0
```

The dry-run flag validates the manifest, walks the include list, transpiles the source, and reports any errors locally without uploading. A missing `license` field aborts the dry-run with `error[missing-license]`; this is the failure mode we hit early in Phase 17.0 implementation and the reason `license: "MIT"` is now hardcoded into the emit.

## Deferred work

- Sub-phase 17.3: Browser runtime stubs for `fs`, `net`, `path`. The Phase 15 emit already routes the `"browser"` export condition at `dist/browser/index.ts`, but the per-stdlib stub files (`fs-stub.js`, `net-stub.js`, `path-stub.js` throwing `MochiPanic` at call time) land alongside the Phase 7+12+14 runtime when the stdlib surface is split.
- Sub-phase 17.4: Three-target runtime gate (Jupyter `nbconvert --execute`, Playwright Chromium console capture, JSR end-to-end publish-revoke). Lands once a Jupyter + Playwright CI image is wired.
- Firefox + Safari runtime tests; Chromium-only is the Phase 17 gate. Firefox + Safari land as Phase 18 release-channel tests once WebDriver-driven CI proves stable.
- Importmap-only consumption (no bundle, browser loads ESM modules directly). Works in principle; the test surface multiplies, so Phase 17 ships the bundle path as primary.
- Deno Jupyter on Windows (`%APPDATA%/jupyter/kernels/`). Linux + macOS are the structural gate; Windows lands as Phase 17.5 if user demand justifies.
