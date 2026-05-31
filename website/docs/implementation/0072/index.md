---
title: MEP-72 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 72. Mochi+TS/JS package bridge"
description: "Per-phase implementation tracking for MEP-72 (Mochi+TypeScript/JavaScript package bridge). Status + commit columns capture how each phase landed on main, plus the per-target coverage matrix for host node 22 LTS, deno 2, bun 1.1, browser ES2024, and edge (Cloudflare Workers / Vercel Edge / Deno Deploy)."
---

# MEP-72 implementation tracking

Per-phase tracking for [MEP-72 Mochi and TypeScript/JavaScript package bridge](/docs/mep/mep-0072). Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main` (or, for the umbrella PR, the in-branch commit on `mep/0072-ts-package`).

A phase is LANDED only when its gate is green for every applicable target (consume direction + publish direction). Missing surfaces become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

## Phase status

| Phase | Title | Status | Commit | Tracking page |
|-------|-------|--------|--------|---------------|
| 0 | Skeleton: package3/typescript/ layout + ts-ingest helper-binary plumbing | NOT STARTED | (pending) | [phase-00](/docs/implementation/0072/phase-00-skeleton) |
| 1 | npm registry client (`registry.npmjs.org` packument + tarball + provenance reader) | NOT STARTED | (pending) | [phase-01](/docs/implementation/0072/phase-01-npm-registry) |
| 2 | JSR registry client (`jsr.io` packument + module reader + sigstore status) | NOT STARTED | (pending) | [phase-02](/docs/implementation/0072/phase-02-jsr-registry) |
| 3 | `.d.ts` ingest helper (`package3/typescript/cmd/ts-ingest` via `ts.createProgram`) | NOT STARTED | (pending) | [phase-03](/docs/implementation/0072/phase-03-dts-ingest) |
| 4 | ApiSurface JSON schema + bridge-side parser | NOT STARTED | (pending) | [phase-04](/docs/implementation/0072/phase-04-apisurface) |
| 5 | Closed type-mapping table (scalars / strings / arrays / records / unions / generics / `Promise<T>` / `AsyncIterable<T>`) | NOT STARTED | (pending) | [phase-05](/docs/implementation/0072/phase-05-typemap) |
| 6 | Mochi extern fn emitter + shim file generation | NOT STARTED | (pending) | [phase-06](/docs/implementation/0072/phase-06-extern-emit) |
| 7 | `import ts "<pkg>@<semver>" as <alias>` grammar + parser | NOT STARTED | (pending) | [phase-07](/docs/implementation/0072/phase-07-import-grammar) |
| 8 | Build orchestration: import-map synth + bundler invoke + materialise `node_modules` + `jsr_cache` | NOT STARTED | (pending) | [phase-08](/docs/implementation/0072/phase-08-build) |
| 9 | mochi.lock `[[npm-package]]` + `[[jsr-package]]` integration + `--check` mode | NOT STARTED | (pending) | [phase-09](/docs/implementation/0072/phase-09-lockfile) |
| 10 | `TargetNpmLibrary` emit (`package.json` + `dist/*.mjs` + `dist/*.d.ts` via `tsc --declaration`) | NOT STARTED | (pending) | [phase-10](/docs/implementation/0072/phase-10-npm-library-emit) |
| 11 | `TargetJsrLibrary` emit (`jsr.json` + `mod.ts` + source-not-dist invariant) | NOT STARTED | (pending) | [phase-11](/docs/implementation/0072/phase-11-jsr-library-emit) |
| 12 | npm Trusted-Publishing publish flow (`npm publish --provenance --access=public` via emitted workflow) | NOT STARTED | (pending) | [phase-12](/docs/implementation/0072/phase-12-npm-publish) |
| 13 | JSR Trusted-Publishing publish flow (`deno publish --token-source=github-actions` via emitted workflow) | NOT STARTED | (pending) | [phase-13](/docs/implementation/0072/phase-13-jsr-publish) |
| 14 | Promise / async bridge (Promise ↔ async fun, AsyncIterable ↔ stream, AbortSignal opaque extern) | NOT STARTED | (pending) | [phase-14](/docs/implementation/0072/phase-14-promise-async) |
| 15 | Monomorphisation (`[ts.monomorphise]` manifest + per-instantiation extern entries) | NOT STARTED | (pending) | [phase-15](/docs/implementation/0072/phase-15-monomorphise) |
| 16 | ESM vs CJS interop pass (exports-map conditional resolution + dual-package-hazard detection) | NOT STARTED | (pending) | [phase-16](/docs/implementation/0072/phase-16-esm-cjs) |
| 17 | Edge-runtime + Jupyter consume-side gate (Cloudflare Workers / Vercel Edge / Deno Deploy + Deno Jupyter kernel) | NOT STARTED | (pending) | [phase-17](/docs/implementation/0072/phase-17-edge-jupyter) |

## Target coverage matrix

Each phase's LANDED gate must be green for every applicable target. `n/a` cells mark targets where the phase does not apply (for example, publish-only phases on consume-only targets, or CJS-rejecting phases on browser/edge).

| Phase | host node 22 LTS (darwin-arm64) | deno 2 (linux-amd64) | bun 1.1 (linux-amd64) | browser es2024 | edge (workers / edge / deploy) |
|-------|--------|--------|--------|--------|--------|
| 0. skeleton | NOT STARTED | n/a | n/a | n/a | n/a |
| 1. npm registry client | NOT STARTED | n/a | n/a | n/a | n/a |
| 2. JSR registry client | NOT STARTED | n/a | n/a | n/a | n/a |
| 3. .d.ts ingest helper | NOT STARTED | n/a | n/a | n/a | n/a |
| 4. ApiSurface JSON | NOT STARTED | n/a | n/a | n/a | n/a |
| 5. type-mapping table | NOT STARTED | required | required | required | required |
| 6. extern emitter | NOT STARTED | required | required | required | required |
| 7. import-ts grammar | NOT STARTED | required | required | required | required |
| 8. build orchestration | NOT STARTED | required | required | required | required |
| 9. mochi.lock integration | NOT STARTED | required | required | required | required |
| 10. TargetNpmLibrary emit | NOT STARTED | required | required | n/a (no library shape in browser) | required |
| 11. TargetJsrLibrary emit | NOT STARTED | required | required | n/a | required |
| 12. npm publish | NOT STARTED | n/a (publish is host-only) | n/a | n/a | n/a |
| 13. JSR publish | NOT STARTED | n/a | n/a | n/a | n/a |
| 14. Promise / async bridge | NOT STARTED | required | required | required | required |
| 15. monomorphisation | NOT STARTED | required | required | required | required |
| 16. ESM/CJS interop | NOT STARTED | required | required | required (CJS rejected at bundler) | required (CJS rejected at deploy) |
| 17. edge + Jupyter consume | NOT STARTED | required | required | required | required |

Cell legend: `required` means the phase's gate runs against this target; `n/a` means the phase's behaviour is intentionally not exercised on this target (architectural reason in parentheses).

## Per-phase fields

Each phase tracking page documents (or will document, once the phase begins):

- **Gate**: the test or check that must pass for the phase to be LANDED on every applicable target column.
- **Files to touch**: the bridge-side files (Go) and emit-side files (TypeScript template) the phase introduces or modifies.
- **Fixtures**: which of the 24-package fixture corpus the phase validates against.
- **Skip count**: the expected SkipReport count per fixture package (golden numbers).
- **Sub-phase decomposition** (if needed): N.1, N.2, ... entries when an upstream constraint forces splitting.

## Fixture corpus

The 24-package fixture corpus (May 2026 top-25 most-imported on npm, filtered to packages that ship `.d.ts`):

typescript, zod, lodash, lodash-es, react, react-dom, vue, axios, dayjs, date-fns, valibot, drizzle-orm, prisma, hono, express, fastify, undici, nanoid, uuid, ts-pattern, immer, effect, neverthrow, ts-toolbelt.

Each phase that touches the type-mapping, extern, or runtime layer asserts golden counts against this corpus. The corpus is regenerated quarterly to track package API drift.

The browser-bundle gate (phases 11 + 17) runs against a 6-package sub-corpus known to be browser-compatible without polyfills: zod, valibot, nanoid, ts-pattern, effect/cjs/Schema, dayjs. The remaining 18 packages either ship CJS-only, depend on `node:fs`, or otherwise require server-side context; they gate against Node + Deno + Bun only.

## Implementation location

The bridge lives at `package3/typescript/` in the repo root:

```
package3/typescript/
  README.md               # pointer to MEP-72 spec
  cmd/
    ts-ingest/            # TypeScript compiler API ApiSurface emitter (phase 3)
  npmregistry/            # registry.npmjs.org client (phase 1)
  jsrregistry/            # jsr.io client (phase 2)
  apisurface/             # ApiSurface JSON parser (phase 4)
  typemap/                # closed type table (phase 5)
  emit/                   # Mochi extern fn emitter (phase 6)
  build/                  # import-map + bundler orchestration (phase 8)
  lockfile/               # `[[npm-package]]` + `[[jsr-package]]` schema + drift check (phase 9)
  library/                # TargetNpmLibrary + TargetJsrLibrary emit (phases 10, 11)
  publish/                # npm + JSR Trusted-Publishing flow wiring (phases 12, 13)
  promise/                # Promise / async bridge translation (phase 14)
  monomorphise/           # `[ts.monomorphise]` parser + renderer (phase 15)
  esm/                    # ESM/CJS interop pass + exports-map resolver (phase 16)
  edge/                   # edge-runtime gate + Jupyter consume (phase 17)
  browser/                # browser-bundle pre-flight pass (phase 11 + 17 shared)
  errors/                 # SkipReason, SkipReport, BridgeError (cross-phase)
  semver/                 # node-semver-compatible matcher (cross-phase)
```

The `package3/typescript/` location is shared with the broader MEP-57 polyglot package work (where `package3/` is the v3 package-system tree). It sits next to `package3/rust/` (MEP-73) and `package3/go/` (MEP-74), and follows the same internal structure.

## Structural simplification vs MEP-73 + MEP-74

Three architectural simplifications versus the Rust and Go bridges, captured in MEP-72 §Abstract decision #2:

- **No wrapper package on consume side.** Rust needs a `extern "C"` shim crate with a `cdylib` target; Go needs a cgo wrapper module with `c-archive` build mode and a handle pool. TypeScript needs neither. The host JS runtime is already the link layer; the Mochi-emitted JS imports the consumed package directly. The skeleton omits `wrapper/` (Rust) and `goroutine/` (Go) equivalents.
- **No async runtime singleton.** Rust needs `tokio::runtime::Runtime` constructed once per process; Go has its own scheduler inside the c-archive. JavaScript's event loop is intrinsic to every runtime target; the bridge adds zero code in `promise/` for runtime construction, only translation.
- **Mandatory transparency log is two-sided, not one-sided.** Rust uses Sigstore on top of crates.io because crates.io has no native transparency log; Go uses `sum.golang.org` as the native log. TypeScript ships against npm (provenance via Sigstore since April 2024) AND JSR (Sigstore since mid-2024). Both registries are Sigstore-mandatory in MEP-72; no fallback to long-lived tokens.

Two architectural complications versus the simpler bridges:

- **Two registries, not one.** npm and JSR are equal first-class citizens. The bridge ships both clients (`npmregistry/` + `jsrregistry/`) at phases 1 and 2; the lockfile carries two repeated tables (`[[npm-package]]` and `[[jsr-package]]`).
- **Five runtime targets, not three.** Node 22 LTS + Deno 2 + Bun 1.1 share the bulk of the consumable surface; browser ES2024 and edge (Cloudflare Workers / Vercel Edge / Deno Deploy) take a restricted subset. The per-phase target matrix marks the n/a cells explicitly.

## Status snapshot

As of 2026-05-30: spec + research bundle (12 notes) landed; implementation phases not yet started. The bridge directory `package3/typescript/` is scaffolded (README + sub-directory placeholders) but contains no code.

## Cross-references

- [MEP-72 spec](/docs/mep/mep-0072) for the normative design.
- [MEP-72 research bundle](/docs/research/0072/) for the 12-note deep-research collection.
- [MEP-52 implementation tracking](/docs/implementation/0052) for the underlying TypeScript transpiler that MEP-72 extends.
- [MEP-57 implementation tracking](/docs/implementation/0057) for the polyglot package system MEP-72 builds on.
- [MEP-73 implementation tracking](/docs/implementation/0073) for the sister Rust-bridge phase plan.
- [MEP-74 implementation tracking](/docs/implementation/0074) for the sister Go-bridge phase plan.
