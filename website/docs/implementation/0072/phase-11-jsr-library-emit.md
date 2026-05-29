---
title: "Phase 11. TargetJsrLibrary emit"
sidebar_position: 13
sidebar_label: "Phase 11. TargetJsrLibrary"
description: "MEP-72 Phase 11: TargetJsrLibrary build target. Lowers a Mochi package into a publishable JSR package with jsr.json + mod.ts + source-not-dist invariant."
---

# Phase 11. TargetJsrLibrary emit

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-72 §Phases](/docs/mep/mep-0072#phases) |
| Status         | NOT STARTED |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase11JsrLibrary` in `package3/typescript/library/phase11_test.go`: subtests `emit_jsr_json`, `emit_mod_ts`, `source_not_dist_invariant`, `deno_check`, `publish_dry_run`, `golden_corpus`. The first synthesises a `jsr.json` from `[ts.publish]` and asserts the JSON matches the golden. The second emits a `mod.ts` re-exporting the user's Mochi package's public surface and asserts the file is valid TypeScript. The third asserts NO `dist/` directory is emitted (the source-not-dist invariant). The fourth runs `deno check mod.ts` against the emitted package and asserts no type errors. The fifth runs `deno publish --dry-run` and asserts the package surface matches the golden. The sixth runs against three end-to-end Mochi packages.

## Lowering decisions

The build target is selected via:

```toml
[ts.publish]
target = "jsr-library"
package-name = "@mochi/example"
scope = "mochi"
version = "0.1.0"
registry = "https://jsr.io"
license = "Apache-2.0"
homepage = "https://example.com"
description = "Example Mochi package published to JSR"
```

The emitter produces:

```
target/ts/jsr-library/
  jsr.json
  README.md (copied from $MOCHI_PROJECT_ROOT/README.md)
  LICENSE (copied from $MOCHI_PROJECT_ROOT/LICENSE)
  mod.ts       # the source entry, exports the public surface
  *.ts         # any additional source files
```

JSR's invariant: source `.ts` only, no `dist/`, no `.d.ts`. JSR's server-side build (using the bundled `deno` toolchain) transpiles and emits `.d.ts` at publish time. The bridge emits the Mochi-source-equivalent `.ts` files directly into the target dir; the user uploads them via `deno publish`.

The synthesised `jsr.json` carries:

```json
{
  "name": "@mochi/example",
  "version": "0.1.0",
  "license": "Apache-2.0",
  "description": "Example Mochi package published to JSR",
  "exports": {
    ".": "./mod.ts"
  },
  "publish": {
    "include": ["./mod.ts", "./README.md", "./LICENSE"],
    "exclude": ["**/*.test.ts", "**/*.spec.ts"]
  }
}
```

JSR does NOT use the `exports`-map's `import`/`require`/`types` conditions (JSR is ESM-only). The `exports` field is a simple specifier-to-path map.

The bridge runs `deno check` (or `deno lint`) against the emitted `mod.ts` as a pre-publish gate; failures stop the publish.

Compared to TargetNpmLibrary, TargetJsrLibrary is simpler: no dual emit, no `.d.ts` synthesis (JSR's server does it), no `package.json`. The phase ships ~40% less Go code than phase 10.

## Files changed

| File | Purpose |
|------|---------|
| `package3/typescript/library/jsr.go` | `EmitJsrLibrary`, `RenderJsrJson`, `EmitModTs` |
| `package3/typescript/library/deno_check.go` | `deno check` invocation wrapper |
| `package3/typescript/library/phase11_test.go` | `TestPhase11JsrLibrary` sentinel |

## Test set

6 subtests as listed in the Gate section.

## Cross-references

- [Research note 06 §3 JSR publish flow](/docs/research/0072/06-npm-jsr-publish-flow#3-jsr-publish-flow) — the source-not-dist invariant rationale.
- [Research note 02 §3 Dual registry shipped at once](/docs/research/0072/02-design-philosophy#3-dual-registry-shipped-at-once) — why JSR is first-class.
