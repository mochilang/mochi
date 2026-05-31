---
title: "Phase 10. TargetNpmLibrary emit"
sidebar_position: 12
sidebar_label: "Phase 10. TargetNpmLibrary"
description: "MEP-72 Phase 10: TargetNpmLibrary build target. Lowers a Mochi package into a publishable npm package with package.json + dist/*.mjs + dist/*.d.ts via tsc --declaration --emitDeclarationOnly."
---

# Phase 10. TargetNpmLibrary emit

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-72 §Phases](/docs/mep/mep-0072#phases) |
| Status         | NOT STARTED |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase10NpmLibrary` in `package3/typescript/library/phase10_test.go`: subtests `emit_package_json`, `emit_dist_mjs`, `emit_dts_declarations`, `exports_map_roundtrip`, `publish_dry_run`, `golden_corpus`. The first synthesises a `package.json` from a representative Mochi package's `mochi.toml`'s `[ts.publish]` section and asserts the JSON matches the golden. The second runs the MEP-52 phase 1+ emitter against `examples/hello/hello.mochi` and asserts the emitted `.mjs` runs under Node 22 and produces `Hello, World!`. The third invokes `tsc --declaration --emitDeclarationOnly` against the emitted `.mjs` and asserts the `.d.ts` round-trips through the ApiSurface ingest (phase 3) without skips. The fourth asserts the `exports` map resolves correctly for `require`, `import`, and `types` conditions. The fifth runs `npm publish --dry-run` against a representative package and asserts the printed package contents include the expected files. The sixth runs against three end-to-end Mochi packages.

## Lowering decisions

The build target is selected via:

```toml
[ts.publish]
target = "npm-library"
package-name = "@mochi/example"
version = "0.1.0"
registry = "https://registry.npmjs.org"
license = "Apache-2.0"
homepage = "https://example.com"
description = "Example Mochi package published to npm"
```

The emitter produces:

```
target/ts/npm-library/
  package.json
  README.md (copied from $MOCHI_PROJECT_ROOT/README.md)
  LICENSE (copied from $MOCHI_PROJECT_ROOT/LICENSE)
  dist/
    index.mjs       # MEP-52 phase 1+ emit, ESM
    index.cjs       # MEP-52 phase 1+ emit, CJS (when [ts.publish] dual = true)
    index.d.ts      # tsc --declaration --emitDeclarationOnly emit
    index.d.cts     # tsc --declaration --emitDeclarationOnly emit (CJS variant)
    index.mjs.map   # MEP-52 phase 16 source-map
    index.cjs.map
```

The synthesised `package.json` carries:

```json
{
  "name": "@mochi/example",
  "version": "0.1.0",
  "description": "Example Mochi package published to npm",
  "license": "Apache-2.0",
  "homepage": "https://example.com",
  "type": "module",
  "main": "./dist/index.cjs",
  "module": "./dist/index.mjs",
  "types": "./dist/index.d.ts",
  "exports": {
    ".": {
      "types": "./dist/index.d.ts",
      "import": "./dist/index.mjs",
      "require": "./dist/index.cjs",
      "default": "./dist/index.mjs"
    }
  },
  "files": ["dist", "README.md", "LICENSE"],
  "sideEffects": false,
  "publishConfig": {
    "access": "public",
    "provenance": true
  }
}
```

The `"publishConfig" "provenance": true` flag is the npm Trusted Publishing trigger; when set, `npm publish` automatically generates a Sigstore attestation.

The `.d.ts` generation uses the bundled TypeScript compiler (TS 5.6, same version as the ingest helper). The compiler runs against `target/ts/npm-library/dist/index.mjs` with `--declaration --emitDeclarationOnly --rootDir=dist --outDir=dist`. The phase asserts the output `.d.ts` round-trips through ingest (phase 3) → ApiSurface → re-emit → cross-check against the Mochi source's declared signatures. A divergence indicates a bug in the MEP-52 emitter; the gate fails loudly.

The dual CJS + ESM emit is opt-in via `dual = true`. The default is ESM-only; npm's modern ecosystem is converging on ESM (Node 22 LTS supports ESM at parity with CJS).

## Files changed

| File | Purpose |
|------|---------|
| `package3/typescript/library/npm.go` | `EmitNpmLibrary`, `RenderPackageJSON`, `RunTsc` |
| `package3/typescript/library/exports.go` | exports-map synthesiser |
| `package3/typescript/library/dts.go` | tsc invocation wrapper |
| `package3/typescript/library/phase10_test.go` | `TestPhase10NpmLibrary` sentinel |

## Test set

6 subtests as listed in the Gate section.

## Cross-references

- [MEP-72 §6 Build orchestration](/docs/mep/mep-0072#6-build-orchestration) — the target-selection model.
- [Research note 06 §1 npm publish flow](/docs/research/0072/06-npm-jsr-publish-flow#1-npm-publish-flow) — the publish-side design.
- [MEP-74 phase 11 library emit](/docs/implementation/0074/phase-11-library-emit) — the sister Go-side library-emit phase.
