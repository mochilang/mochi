---
title: "MEP-72 Note 12: Risks and alternatives"
sidebar_position: 13
sidebar_label: "12. Risks"
description: "The risk register and rejected alternatives for MEP-72: compiler-API RAM footprint, `.d.ts` quality variance, conditional-type resolution, CJS-only browser refusal, JSR attestation gap, dual-package hazard, the rejected alternatives (hand-written `.d.ts` parser, DT-as-primary, tsserver protocol, wrapper-package synthesis on consume side, long-lived NPM_TOKEN)."
---

# 12. Risks and alternatives

This note enumerates the risks the bridge accepts and the alternatives the bridge rejected. It is informative; the normative reference is [MEP-72 §Risks](/docs/mep/mep-0072#risks) and [§Alternatives considered](/docs/mep/mep-0072#alternatives-considered).

## 1. Risk register

### R1. TypeScript compiler API memory footprint

Loading a large package's `.d.ts` tree into `ts.createProgram` uses ~200 MB of RAM per program instance. AWS SDK v2 ingests at 2.4 GB peak RSS. Mitigation: sequential per package; bounded parallelism (`runtime.NumCPU() / 4`); ingest binary is a separate process so RSS is bounded.

### R2. `.d.ts` quality varies widely

Many packages ship `.d.ts` with `any` returns or unresolved generics. Mitigation: the bridge SkipReports the under-specified items; the user adds a hand-written extern fn with a refined signature.

### R3. Conditional types

`T extends U ? X : Y` is hard to resolve when the parameter is generic. Mitigation: eager resolution at bind site; SkipReport on unresolvable; monomorphisation entries for known call-site instantiations.

### R4. CJS-only browser refusal

The browser target rejects CJS-only packages. Mitigation: the bridge surfaces the package's module shape in the lockfile; the browser-bundle gate catches CJS-only deps at build time with a clear diagnostic.

### R5. JSR attestation gap

JSR Trusted Publishing GA'd mid-2024; many JSR packages predate it. Mitigation: `--sigstore-required` is opt-in; the default is warn-only.

### R6. Dual-package hazard

CJS + ESM dual packages may expose different module state to CJS and ESM consumers. Mitigation: the bridge picks ESM by default; SkipReports when the two paths differ in their ApiSurface.

### R7. TypeScript compiler-API major-version drift

TS 5.x → 6.x (projected late 2026) may break compiler-API users. Mitigation: the bridge bundles a pinned TypeScript version into the ingest helper binary; upgrades are MEP-72 sub-phase concerns.

### R8. Bun's TS implementation diverges from tsc

Bun's bundler and runtime ship their own TS implementation that diverges on edge cases. Mitigation: the official `typescript` package's compiler API is the canonical ingest authority; Bun is a runtime target but not the ingest authority.

### R9. Edge runtime API restrictions

`node:fs`, `node:net`, `node:child_process`, `node:worker_threads` are not allowed on Cloudflare Workers, Vercel Edge, Deno Deploy. Mitigation: the edge-runtime gate (phase 17) checks at lock time.

### R10. Shim file size

Large packages (lodash, AWS SDK) produce ~15K-LOC Mochi shim files. Mitigation: shim files are in `target/ts_shims/`, gitignored by default, regenerated on lock.

### R11. Deno / Node / Bun / browser runtime drift

Subtle behaviour differences (e.g., `setImmediate` is Node-only, `globalThis.process` is Node + Deno + Bun but not browser) can cause runtime-conditional behaviour in consumed packages. Mitigation: the bridge does not promise runtime-conditional resolution; the user's host runtime is the authority.

### R12. Package squatting and typosquatting

The npm registry has a known history of typosquatting attacks. Mitigation: the lockfile logs every resolved name; `mochi pkg audit --provenance` cross-checks against the registry's Sigstore attestation status.

### R13. ApiSurface ingest cost

Cold ingest of a top-25 package set takes ~30s wall-clock on darwin-arm64. Mitigation: the ApiSurface JSON is content-addressed and cached; subsequent `mochi pkg lock` runs only re-ingest packages whose tarball hash changed.

### R14. The `ts-ingest` helper binary size

The bundled TypeScript compiler + helper script is ~3 MB. Mitigation: the binary is embedded in the Mochi binary at build time; extraction to a temp file on first invocation is one-time per Mochi process.

## 2. Rejected alternatives

### A1. Hand-written `.d.ts` parser in Go

Rejected: the TypeScript grammar is the largest in mainstream production use; a passable subset is tens of thousands of LOC and immediately stale. See [[02-design-philosophy]] §1.

### A2. DefinitelyTyped (`@types/<pkg>`) as the primary type source

Rejected: DT lags upstream by months for many packages. DT accepted as fallback only.

### A3. `tsserver` protocol over stdin/stdout

Rejected: editor-shaped (request/response with state). The in-process compiler API is the correct level for batch ingest.

### A4. `tsd` or `@typescript-eslint`'s API

Rejected: both wrap the official compiler API. The bridge calls the official API directly.

### A5. Wrapper-package synthesis on consume side (mirror MEP-73 + MEP-74)

Rejected: the host JS runtime IS the link layer. No wrapper needed. See [[02-design-philosophy]] §2.

### A6. Synthesize Sigstore attestations in the Mochi shim file

Rejected: attestations are registry-side records, not source-side artefacts.

### A7. Deno's `deno info` JSON output as the ingest source

Acknowledged: works for JSR; the bridge prefers the unified TypeScript compiler API path for npm + JSR.

### A8. Long-lived `NPM_TOKEN` and `JSR_TOKEN` paths

Rejected: matches MEP-73 §A9 and MEP-74 §10. Sigstore-keyless OIDC is the mandatory publish path.

### A9. Mochi-native `.d.ts` emitter (instead of `tsc --declaration`)

Rejected for v1: tsc is the authoritative source.

### A10. TypeScript decorators as Mochi macros

Rejected: TS decorators require runtime metadata reflection; not Mochi's surface.

### A11. The `import attributes` proposal (`import zod from "zod" with { type: "json" }`)

Rejected: import attributes are for module-level metadata, not FFI-level.

### A12. Single-registry consume (npm-only, JSR later)

Rejected: JSR is already GA; the Deno stdlib is JSR-native; Hono's typed surface is JSR-first.

### A13. Hand-authored per-package type-mapping tables

Rejected: same boilerplate violation as MEP-73 §A13 and MEP-74 §13.

### A14. WASM-target Mochi consuming npm/JSR packages via dynamic import

Acknowledged: the wasm-wasip1 target consumes packages via Deno's wasm support; the bridge synthesis is identical.

## 3. Cross-references

- [MEP-72 §Risks](/docs/mep/mep-0072#risks) — normative.
- [MEP-72 §Alternatives considered](/docs/mep/mep-0072#alternatives-considered) — normative.
- [[02-design-philosophy]] — the load-bearing decisions.
