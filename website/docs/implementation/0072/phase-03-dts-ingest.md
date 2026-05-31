---
title: "Phase 3. .d.ts ingest helper"
sidebar_position: 5
sidebar_label: "Phase 3. .d.ts ingest"
description: "MEP-72 Phase 3: package3/typescript/cmd/ts-ingest helper binary. Loads consumed package's .d.ts (or source .ts for JSR) into ts.createProgram, walks the SymbolTable via ts.TypeChecker, emits ApiSurface JSON."
---

# Phase 3. .d.ts ingest helper

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-72 §Phases](/docs/mep/mep-0072#phases) |
| Status         | NOT STARTED |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase3DtsIngest` in `package3/typescript/cmd/ts-ingest/phase03_test.go`: subtests `load_program`, `walk_exports`, `resolve_types`, `emit_apisurface`, `golden_corpus`. The first invokes `ts-ingest --input=<pkg-dir> --output=<json-path>` against a minimal fixture (a single `.d.ts` file with one exported function). The second walks every export and asserts the ApiSurface JSON contains the right entries. The third resolves a representative `Promise<string>` return, a `T[]` parameter, a `Record<string, number>` shape, and a `T extends U ? X : Y` conditional. The fourth emits the JSON to disk and asserts it round-trips through the apisurface parser (phase 4). The fifth runs against all 24 fixture-corpus packages and asserts golden counts (per-package expected export count, expected SkipReport count).

## Lowering decisions

The `ts-ingest` helper is a Node-side binary, not Go. It bundles the `typescript` npm package's compiler API and is shipped as a single embedded file inside the Mochi binary. On first invocation per Mochi process, the helper is extracted to `$XDG_CACHE_HOME/mochi/ts-ingest/<sha>/ts-ingest.mjs` and run via `node --no-deprecation`.

The helper accepts:

- `--input=<dir>` — the unpacked package directory (with `package.json` + `.d.ts` tree).
- `--output=<json-path>` — the ApiSurface JSON output path.
- `--target=<runtime>` — one of `node22`, `deno2`, `bun1.1`, `browser`, `edge`. Selects the `lib` array for the synthetic tsconfig and the conditional-exports resolution.
- `--ts-version=<semver>` — the TypeScript version the helper bundled (defaults to the build-pinned version, currently 5.6).
- `--max-rss=<MB>` — soft RSS cap; the helper monitors `process.memoryUsage().rss` and exits early with a SkipReport if exceeded (default 2 GB).

The helper's flow:

1. Read `package.json`; resolve `"types"` / `"typings"` / `"exports"` to the `.d.ts` entry tree.
2. Construct a `ts.CompilerHost` with a virtual file system rooted at `--input`, configured with the synthetic tsconfig for `--target`.
3. Call `ts.createProgram({rootNames: [resolvedEntryFile], options: tsconfig, host})`.
4. Get the `TypeChecker` via `program.getTypeChecker()`.
5. Walk the root file's exports via `checker.getExportsOfModule(sourceFile.symbol)`.
6. For each exported `Symbol`, classify (function, class, interface, type-alias, const, enum, namespace) and resolve its signature.
7. For each `Type`, walk to leaves via the discriminator tree (`type.flags & ts.TypeFlags.{Number,String,...}`); record TS type IDs for cross-referencing.
8. Emit the ApiSurface JSON to `--output`.

The helper uses TypeScript 5.6's compiler API (matching MEP-52's pinned TS version). Upgrades to a newer TS major are MEP-72 sub-phase work.

When the helper encounters an unresolvable construct (conditional type with a generic parameter, mapped type beyond depth 2, ambient module wildcard), it emits a `SkipReport` entry with a clear reason; the export's slot in ApiSurface is marked `skipped`.

## Files changed

| File | Purpose |
|------|---------|
| `package3/typescript/cmd/ts-ingest/main.ts` | helper entry: arg parsing, program construction, JSON emit |
| `package3/typescript/cmd/ts-ingest/walker.ts` | export walker (symbol → ApiItem) |
| `package3/typescript/cmd/ts-ingest/resolver.ts` | type resolver (Type → ApiType) |
| `package3/typescript/cmd/ts-ingest/skipper.ts` | SkipReason classifier |
| `package3/typescript/cmd/ts-ingest/package.json` | helper's own deps (only `typescript@5.6`) |
| `package3/typescript/cmd/ts-ingest/build.sh` | bundles the helper into a single `.mjs` via `bun build` |
| `package3/typescript/cmd/ts-ingest/phase03_test.go` | Go test harness invoking the bundled helper |

The bundled helper artefact is embedded in the Mochi binary via `go:embed` at build time.

## Test set

- `TestPhase3DtsIngest/load_program`
- `TestPhase3DtsIngest/walk_exports`
- `TestPhase3DtsIngest/resolve_types`
- `TestPhase3DtsIngest/emit_apisurface`
- `TestPhase3DtsIngest/golden_corpus`

## Cross-references

- [Research note 04 ApiSurface ingest](/docs/research/0072/04-tsdoc-dts-ingest) — the ingest design.
- [MEP-72 §Risks R1](/docs/mep/mep-0072#risks) — the RSS-cap mitigation.
