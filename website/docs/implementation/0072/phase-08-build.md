---
title: "Phase 8. Build orchestration"
sidebar_position: 10
sidebar_label: "Phase 8. Build"
description: "MEP-72 Phase 8: build orchestration. Synth workspace package.json + import-map + tsconfig from the lockfile, materialise node_modules + jsr_cache, invoke the host runtime / bundler, link the emit output."
---

# Phase 8. Build orchestration

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-72 §Phases](/docs/mep/mep-0072#phases) |
| Status         | NOT STARTED |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase8Build` in `package3/typescript/build/phase08_test.go`: subtests `synth_workspace`, `materialise_node_modules`, `materialise_jsr_cache`, `invoke_node`, `invoke_deno`, `invoke_bun`, `invoke_browser_bundle`, `cache_reuse`, `golden_corpus`. The first synthesises a workspace from a representative lockfile and asserts the rendered `package.json` + `import-map.json` + `tsconfig.json` match goldens. The next two materialise the dependency trees from the local cache (no network) and assert the file layout. The next four invoke the four runtime targets against a "hello world that calls zod" fixture and assert stdout matches the golden. The eighth asserts that a second build with no lockfile change reuses the cached workspace + dependency tree (no re-extraction). The ninth runs against all 24 fixture-corpus packages.

## Lowering decisions

The build orchestration combines four steps:

1. **Synthesise the workspace** (re-uses phase 0's renderer): from the lockfile's `[[npm-package]]` and `[[jsr-package]]` tables, build the `package.json` `dependencies` map and the `import-map.json` `imports` map. The workspace root is `target/ts/<runtime>/`.

2. **Materialise the dependency tree**: for each `[[npm-package]]`, copy the unpacked tarball from `$XDG_CACHE_HOME/mochi/ts-deps/npm/<pkg>@<version>/` into `target/ts/<runtime>/node_modules/<pkg>/`. For each `[[jsr-package]]`, copy from `$XDG_CACHE_HOME/mochi/ts-deps/jsr/@<scope>/<pkg>@<version>/` into `target/ts/<runtime>/jsr_cache/@<scope>/<pkg>@<version>/`. The copy is a content-addressed link (hard-link on POSIX, copy-on-write on macOS APFS, full copy on Windows) so multiple workspaces share the underlying tree.

3. **Invoke the host runtime / bundler**: depending on `runtime`:
   - `node22`: `node --enable-source-maps dist/index.mjs` for run; `tsc --noEmit` for type-check.
   - `deno2`: `deno run --allow-net --allow-read dist/index.ts` for run; `deno check dist/index.ts` for type-check.
   - `bun1.1`: `bun run dist/index.ts` for run; `bun build --target=node dist/index.ts` for build.
   - `browser`: `bun build --target=browser dist/browser/index.ts --outfile=dist/bundle/index.js` (primary); `esbuild --bundle --format=esm --target=es2024 --platform=browser` (fallback).
   - `edge`: same as `browser` but with `--define=process.env.NODE_ENV=production` and the edge-runtime capability gate (phase 17).

4. **Link the emit output**: the MEP-52 phase 12 emitter writes the Mochi-emitted TS source under `target/ts/<runtime>/src/`; the build orchestration links the shim files from `target/ts_shims/` so the emitted TS can import them. The link layout matches what the user's source expressed via `import ts "..." as <alias>`.

The orchestration uses Go's `os/exec` with stdout / stderr capture; failures produce a clear diagnostic naming the failing step + the failed command's stderr.

The build cache is keyed on `(workspace-manifest-hash, dep-tree-hash, runtime, target)`. A cache hit skips synthesis and materialisation; a cache miss re-runs the full pipeline. The cache directory is `$XDG_CACHE_HOME/mochi/ts-build/<hash>/`.

## Files changed

| File | Purpose |
|------|---------|
| `package3/typescript/build/orchestrate.go` | `Orchestrator`, `BuildOptions`, `Build` |
| `package3/typescript/build/materialise.go` | content-addressed link / copy from the cache to the workspace |
| `package3/typescript/build/invoke.go` | per-runtime exec wrapper |
| `package3/typescript/build/cache.go` | workspace + dep-tree hash cache |
| `package3/typescript/build/phase08_test.go` | `TestPhase8Build` sentinel |
| `package3/typescript/build/testdata/hello_zod/*` | end-to-end fixture |

## Test set

9 subtests as listed in the Gate section.

## Cross-references

- [Research note 01 §3 Manifest tables](/docs/research/0072/01-language-surface#3-the-manifest-tables) — the manifest the orchestrator synthesises.
- [Research note 10 Runtime target matrix](/docs/research/0072/10-runtime-target-matrix) — the per-runtime invocation list.
- [MEP-74 phase 9 build](/docs/implementation/0074/phase-09-build) — the sister Go-side build orchestration.
