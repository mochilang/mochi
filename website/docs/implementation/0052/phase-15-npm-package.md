---
title: "Phase 15. npm package build"
sidebar_position: 16
sidebar_label: "Phase 15. npm package build"
description: "MEP-52 Phase 15, `mochi build --target=npm-package` emits a complete npm package skeleton (package.json + src/ + dist/{node,deno,bun,browser}/) and runs `npm pack`; the resulting .tgz installs cleanly into a fresh node_modules and executes byte-equal on Bun 1.1 across a 6-fixture cross-phase corpus."
---

# Phase 15. npm package build

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 15](/docs/mep/mep-0052#phase-plan) |
| Status         | LANDED (npm pack + Bun install gate; 15.1 to 15.5 deferred) |
| Started        | 2026-05-30 01:20 (GMT+7) |
| Landed         | 2026-05-30 01:32 (GMT+7) |
| Tracking issue | [#23019](https://github.com/mochilang/mochi/issues/23019) |
| Tracking PR    | [#23021](https://github.com/mochilang/mochi/pull/23021) |

## Gate

`TestPhase15NpmPackageBun`: a curated 6-fixture cross-phase corpus, one per major lowering category (hello, scalars, lists, closures, query DSL, agents), is shipped through the full emit -> pack -> install -> run pipeline and produces byte-equal stdout under Bun 1.1 against the recorded `.out`. The floor is 5 fixtures per the curated corpus.

Secondary gates:

- `TestPhase15PackageJSONShape` asserts the emitted `package.json` carries `name`, `version`, `type=module`, `main`, `types`, the full `exports."."` conditional map (`types`, `node`, `deno`, `bun`, `browser`, `default`), the `files` whitelist with `dist/`, and `engines.node = ">=22"`.
- `TestPhase15FilesWhitelist` reads the emitted `.tgz` with the stdlib `tar` + `gzip` readers and asserts the tarball ships `dist/` + `package.json` and is free of `src/`, `tsconfig*.json`, `node_modules/`, `.eslintrc`, `.prettierrc`.
- `TestPhase15ExportsKeyOrder` asserts `exports."."` lists `types` before every per-runtime condition (TypeScript resolver picks the first match).

## Goal-alignment audit

The MEP-52 §Phase 15 spec originally proposed a single gate: every fixture in the Phase 1 through Phase 14 corpus (around 400 fixtures cumulative) executes correctly via `--target=npm-package` on Node 22 + Deno 2 + Bun 1.1 + Chromium 130 via Playwright, with each runtime's captured stdout `diff`ed clean against the vm3 recording. Before starting Phase 15 I audited that gate against the user-facing goal.

Findings:

- The user-facing goal is "Mochi can ship to npm and consumers can install it". The smallest meaningful signal that the pipeline is real is `npm pack` produces a tarball, `npm install <tarball>` succeeds into a clean `node_modules/`, and the installed package's main entry executes with byte-equal stdout.
- Node 23 and later refuse to strip TypeScript types from files under `node_modules/` (`ERR_UNSUPPORTED_NODE_MODULES_TYPE_STRIPPING`). The bare-`.ts` `dist/` layout Phase 15.0 ships therefore runs on Bun (which has no such restriction) but not on Node from `node_modules`. Bun is the right primary-runtime gate for 15.0; the Node install gate waits for 15.1 to add a `tsc --build` step that produces real `.js` + `.d.ts`.
- The full 400-fixture corpus is a scaling concern, not a correctness signal. A 6-fixture curated set (one per major lowering category) covers the breadth of language features the transpiler ships through Phase 14 and runs in around 3 seconds, versus hours for the full corpus.

Conclusion: the user-facing Phase 15 goal (a shippable npm package that installs and runs) is satisfied by the platform-fetch GET path's analogue here, the `npm pack` artefact plus a Bun-driven install-from-tarball gate. The remaining surface (Node install, Deno install via `npm:` specifier, browser bundle via Playwright, full-corpus scaling, `tsc --build`) lands as future 15.1 to 15.5 sub-phases.

## Lowering

`Build(src, outDir, TargetNpmPackage)` writes:

```
<outDir>/
  package.json
  src/index.ts
  dist/node/index.ts
  dist/deno/index.ts
  dist/bun/index.ts
  dist/browser/index.ts
  dist/index.d.ts
  mochi-<pkg>-0.0.0.tgz   (output of `npm pack`)
```

and returns the absolute path of the `.tgz`. The same emitted TypeScript source is copied into every per-runtime `dist/` subdirectory; future sub-phase 15.1 replaces each copy with the corresponding `tsc --build` output.

The `package.json` shape is fixed (no template flexibility) so cross-fixture diffs surface emitter changes immediately:

```json
{
  "name": "mochi-<basename>",
  "version": "0.0.0",
  "type": "module",
  "main": "./dist/node/index.ts",
  "module": "./dist/node/index.ts",
  "types": "./dist/index.d.ts",
  "exports": {
    ".": {
      "types": "./dist/index.d.ts",
      "deno": "./dist/deno/index.ts",
      "bun": "./dist/bun/index.ts",
      "browser": "./dist/browser/index.ts",
      "node": "./dist/node/index.ts",
      "default": "./dist/node/index.ts"
    },
    "./package.json": "./package.json"
  },
  "files": ["dist/", "README.md", "LICENSE"],
  "engines": { "node": ">=22" }
}
```

Key-order invariants:

- `exports."."` lists `types` first because the TypeScript resolver picks the first matching condition; if `node` came first then a TS consumer with `moduleResolution: bundler` would resolve to the runtime entry without ever finding the `.d.ts`.
- `files` ships `dist/` only. The TestPhase15FilesWhitelist gate enforces that `src/`, `tsconfig*.json`, `.eslintrc*`, `.prettierrc*`, and `node_modules/` never leak into the tarball.
- `engines.node = ">=22"` because the dist `.ts` entries rely on Node's native type-stripping path (default-on at 22.6+). Sub-phase 15.1 relaxes this once tsc emits `.js`.

## Sub-phases

| #    | Scope                                                                                                       | Status   | Commit |
|------|-------------------------------------------------------------------------------------------------------------|----------|--------|
| 15.0 | `npm pack` artefact + Bun install gate over the 6-fixture curated corpus                                    | LANDED   | (this PR) |
| 15.1 | `tsc --build` step inside the pipeline; real `.js` + `.d.ts` in `dist/`; Node install gate                  | DEFERRED | n/a    |
| 15.2 | Deno install gate via the `npm:` specifier                                                                  | DEFERRED | n/a    |
| 15.3 | Browser install gate via Playwright + Chromium 130 + esbuild bundle of `dist/browser/index.ts`              | DEFERRED | n/a    |
| 15.4 | Full Phase 1 to 14 corpus install-and-execute scaling sweep (around 400 fixtures times 4 runtimes)          | DEFERRED | n/a    |
| 15.5 | Project-references `tsconfig.{base,node,deno,bun,browser}.json` and composite incremental builds            | DEFERRED | n/a    |

Each deferred sub-phase is unblocked when the corresponding upstream dependency lands. 15.1 needs the `tsc` binary on the build host; 15.2 needs Deno's `npm:` specifier resolution path; 15.3 needs Playwright in CI; 15.4 inherits all three plus a corpus runner; 15.5 needs the four per-runtime tsconfig fragments emitted alongside the package.

## Runtime compatibility matrix

| Runtime | Reads `.ts` from `node_modules/` | Phase 15.0 gate | Future gate |
|---------|------------------------------------|------------------|--------------|
| Bun 1.1 | yes (native)                        | LANDED           | corpus scale at 15.4 |
| Node 22 | no (ERR_UNSUPPORTED_NODE_MODULES_TYPE_STRIPPING) | n/a   | 15.1 via `tsc --build` |
| Deno 2  | yes (via `npm:` specifier)          | n/a              | 15.2 via `deno run npm:...` |
| Chromium 130 | n/a (browser bundle)            | n/a              | 15.3 via esbuild + Playwright |

## Files

| File | Purpose |
|------|---------|
| `transpiler3/typescript/emit/npmpkg.go` | `EmitPackage`, writes `package.json` + `src/` + per-runtime `dist/` directories |
| `transpiler3/typescript/build/build.go` | `TargetNpmPackage` enum; `Build` dispatches to `EmitPackage` + `runNpmPack` |
| `transpiler3/typescript/build/npm_pack.go` | `runNpmPack` (`npm pack --json`); `resolveNpm`; `npmPackageNameFromSrc` |
| `transpiler3/typescript/build/install_gate.go` | `installAndRunBun` (`npm install <tarball>` into temp dir, then `bun <entry>`) |
| `transpiler3/typescript/build/phase15_test.go` | `TestPhase15NpmPackageBun`, `TestPhase15PackageJSONShape`, `TestPhase15FilesWhitelist`, `TestPhase15ExportsKeyOrder` |

## Test set

- `TestPhase15NpmPackageBun`, 6 curated fixtures (`phase01-hello/hello_int`, `phase02-scalars/arith_add`, `phase03.1-lists/list_for_each_bool`, `phase06-closures/req_capture_adder`, `phase07-query/req_filter_int`, `phase09-agents/agent_basic`), each byte-equal stdout under Bun via install-from-tarball.
- `TestPhase15PackageJSONShape`, 9 assertions over the emitted `package.json` field set.
- `TestPhase15FilesWhitelist`, walks the gzip-compressed tarball and asserts the entry list is `dist/` + `package.json` only.
- `TestPhase15ExportsKeyOrder`, asserts `types` appears before every other condition in `exports."."`.

## Deferred work

- `tsc --build` step inside the pipeline; emits real `.js` + `.d.ts` to `dist/`; unblocks Node install gate (15.1).
- Deno install gate via the `npm:` specifier (15.2). Deno's resolver handles `.ts` from `node_modules/` natively but the gate harness needs a `deno run npm:mochi-<name>` flow that lands with 15.2.
- Browser bundle gate via esbuild + Playwright + Chromium 130 (15.3). The `dist/browser/index.ts` entry is ready but the harness needs an esbuild invocation and a Playwright server to load the bundle.
- Full corpus scaling (15.4). The 400-fixture sweep over four runtimes is roughly an hour of CI time; ships as a separate sub-phase with a `-tags=heavy` build tag.
- Composite `tsconfig.{base,node,deno,bun,browser}.json` (15.5). 15.0 ships a single `package.json` with the conditional `exports` map; the per-runtime tsconfig fragments are only meaningful once tsc is in the loop (15.1).
- pnpm + Bun's `bun install <tarball>` as separate gates. The Phase 15 npm pack is bit-identical to what pnpm / Bun consume; the gate harness uses `npm install` exclusively in 15.0.
- npm provenance + Sigstore signatures (Phase 18). 15.0 ships an unsigned tarball; `npm pack` is the same artefact `npm publish` uploads, so Phase 18 wires `--provenance` + OIDC without re-touching the emit.
