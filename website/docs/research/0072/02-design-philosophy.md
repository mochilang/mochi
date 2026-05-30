---
title: "MEP-72 Note 02: Design philosophy"
sidebar_position: 3
sidebar_label: "02. Design philosophy"
description: "The load-bearing design choices behind MEP-72: TypeScript compiler API for ingest (not hand-written parser), NO synthesised wrapper package on consume side (the structural simplification that distinguishes TS from Rust + Go bridges), no async runtime singleton (the host JS event loop is already there), dual-registry support shipped at once (not npm-first plus JSR-later), Sigstore Trusted Publishing on both registries (no NPM_TOKEN fallback)."
---

# 02. Design philosophy

This note enumerates the load-bearing choices that shape MEP-72 and why each was made. It is informative.

## 1. TypeScript compiler API as the canonical ingest source

The TypeScript team maintains the `typescript` npm package with a documented programmatic API (`ts.createProgram`, `ts.TypeChecker`, `ts.SourceFile`, `ts.Symbol`, `ts.Type`) that has been API-stable across major versions since 3.0 (2018). The same API powers `tsserver` (the editor protocol), `tsc` (the CLI compiler), `tsd` (the type-test framework), `ts-morph` (the high-level wrapper), `dts-bundle-generator` (the `.d.ts` rollup tool), `api-extractor` (the Microsoft API documentation tool), and `typedoc` (the documentation generator).

The bridge invokes this API via a small Node-side helper binary (`package3/typescript/cmd/ts-ingest/main.ts`) that:

1. Loads the consumed package's `.d.ts` files via `ts.createProgram` with the package's `tsconfig.json` (or sensible defaults).
2. Walks every exported symbol via `checker.getSymbolsInScope`.
3. Resolves each symbol's `ts.Type` via `checker.getTypeAtLocation` (or `checker.getTypeOfSymbolAtLocation` for value positions).
4. Recursively walks the type tree, emitting an ApiSurface JSON document with a stable schema.

Alternatives that were considered and rejected:

- **Hand-written `.d.ts` parser in Go.** Rejected: the TypeScript grammar is the largest language grammar in mainstream production use. A passable subset is tens of thousands of LOC and immediately stale.
- **`tsserver` protocol over stdin/stdout.** Rejected: `tsserver` is editor-shaped (request/response with state); the in-process compiler API is the correct level for a batch ingest pass.
- **DefinitelyTyped `@types/<pkg>` as primary.** Rejected: DT lags upstream by months and is unmaintained for many packages. DT is accepted as a fallback only.

## 2. No synthesised wrapper package on the consume side

This is the single largest structural departure from MEP-73 (Rust) and MEP-74 (Go). Both of those bridges synthesise a wrapper crate / package per imported dep because their host language (Rust's `cargo` toolchain or Go's `go build`) needs a closed compilation unit that exposes an `extern "C"` or `//export` C-ABI surface for the Mochi-emitted code to link against. The wrapper is the FFI boundary.

MEP-72's case is structurally different: Mochi-emitted JS code (from MEP-52) and the consumed npm / JSR package both run in the same JavaScript runtime (V8 / JavaScriptCore / SpiderMonkey / QuickJS). There is no FFI boundary. The Mochi-emitted code can `import { foo } from "<pkg>"` directly and call `foo(args)` with a direct property-access-and-call sequence at runtime. No marshaling. No wrapping. No copy.

The bridge's work on the consume side reduces to:

1. **Type-binding synthesis** (the Mochi shim file with `extern fn` declarations).
2. **Workspace orchestration** (the `package.json` `dependencies` entry pointing at the locked version, the `imports.json` import-map for Deno / browser).

The Mochi shim file is a binding, not a runtime layer. It says "this Mochi name corresponds to this TS name at this resolved registry"; the host JS runtime does the rest.

Compounding effects of this decision:

- **Audit story**: less synthesised code.
- **Compile-time cost**: zero wrapper-compile pass per dep.
- **Runtime cost**: zero per-call FFI overhead. Mochi-emitted JS calls into npm packages at the same speed as native JS-to-JS calls.

## 3. Closed type-mapping table

MEP-72 ships a fixed translation table from TypeScript types to Mochi types. The table is described in [[05-type-mapping]] §1. The rationale is the same as MEP-73 §A13 and MEP-74 §13: a closed table is auditable, predictable, and finite; an open table requires user intervention per package and re-introduces the boilerplate the bridge was designed to eliminate. Items outside the table emit a `SkipReport` entry; the user opts in via a hand-written `extern fn` override per item.

## 4. No async runtime singleton

MEP-73 needs a `tokio::runtime::Runtime` constructed lazily on first async call because Rust's `async fn` requires a runtime. MEP-74 needs a cgo handle pool because the goroutine scheduler lives inside the c-archive.

MEP-72 needs neither. Every JS runtime (Node 22 LTS, Deno 2, Bun 1.1, every modern browser, every JS-engine-based serverless edge runtime) ships with a built-in microtask queue plus an event loop. TypeScript's `Promise<T>` is the canonical async return type and is supported by every runtime out of the box. The Mochi-emitted JS code calls `await foo()` directly; the host runtime schedules the microtask through the same event loop the Mochi-emitted async code uses.

The translation table maps:

- TS `Promise<T>` → Mochi `async fun(): T`
- TS `AsyncIterable<T>` → Mochi `stream<T>`

That is the entire async-side work. No runtime singleton, no `block_on`, no thread pool, no enter-runtime ceremony.

## 5. Dual-registry support shipped at once

The TS / JS ecosystem has fragmented across npm (3.5M+ packages, the historical default, supports CJS + ESM) and jsr.io (12K+ packages, the typed-native modern alternative, supports ESM + TypeScript source publishing). MEP-72 ships both as first-class consume + publish registries from day one.

Why not ship npm-first and add JSR later:

- JSR is already GA (March 2024 launch; mid-2024 Trusted Publishing GA).
- The Deno standard library (`@std/*`) is JSR-native; ignoring JSR means Deno-shipping Mochi programs cannot use the Deno stdlib.
- The Hono framework's typed surface is JSR-first.
- Publishing to both registries from one workflow is structurally cheap (the MEP-52 Phase 18 emitted workflow already exercises both flows in the same job).

The lockfile uses two separate repeated tables (`[[npm-package]]` and `[[jsr-package]]`) because the integrity-hash formats differ (npm uses SRI `sha512-...`, JSR uses its own BLAKE3-based manifest hash). Both feed into the bridge's BLAKE3-256 primary hash.

## 6. Sigstore Trusted Publishing mandatory on both registries

Long-lived `NPM_TOKEN` and `JSR_TOKEN` paths are not supported by MEP-72's publish flow. The MEP-52 Phase 18 emitted GitHub Actions workflow:

- Acquires a GitHub OIDC token via `id-token: write` permission.
- Runs `npm publish --provenance --access=public` (which submits the OIDC token to npm's Trusted-Publishing endpoint).
- Runs `deno publish --token-source=github-actions` (which submits the OIDC token to JSR's Trusted-Publishing endpoint).
- Both registries mint short-lived publish credentials, sign the artefacts via Sigstore Fulcio, and post the attestations to the registry.
- `actions/attest-build-provenance@v2` records SLSA-style attestation for the browser bundle artefact.

The Phase 18 emitter's `TestPhase18NoLongLivedTokens` gate fails any workflow body referencing `NPM_TOKEN` or `JSR_TOKEN` secrets. MEP-72 inherits this gate without modification.

The motivation matches MEP-73 §A9 and MEP-74 §10: the xz-utils, event-stream, and 2025 npm reflected-string flood incidents trace to compromised long-lived tokens; the industry direction (npm GA April 2024, JSR GA mid-2024, PyPI PEP 740 GA late 2025, Cargo RFC #3724 accepted Q4 2025, Maven Central GA October 2024) is unambiguously toward Sigstore-keyless OIDC. A 2026 bridge that does not ship that path on day one is shipping a decade-out-of-date supply-chain story.

## 7. Cross-references

- [[03-prior-art-bridges]] — what other bridges look like.
- [[05-type-mapping]] — the closed table this philosophy underwrites.
- [[07-sigstore-npm-jsr-trusted-publishing]] — the Trusted Publishing detail.
