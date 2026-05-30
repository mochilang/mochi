---
title: "MEP-72 research bundle"
sidebar_position: 1
sidebar_label: "Overview"
description: "Twelve research notes covering the design space behind MEP-72: language surface, design philosophy, prior-art TS/JS bridges, TypeScript compiler API + .d.ts ingest, the closed type-mapping table, the dual-registry (npm + JSR) publish flow, the Sigstore-keyless Trusted Publishing on both registries, the Promise / async bridge that piggybacks on the host JS event loop, the ESM-CJS dual-package interop pass, the four-runtime target matrix (Node 22 + Deno 2 + Bun 1.1 + browser ES2024), the browser bundle surface via bun build / esbuild, plus the risks and rejected alternatives register."
---

# MEP-72 research bundle

This bundle is the informative companion to [MEP-72](/docs/mep/mep-0072). It documents the design space the TypeScript / JavaScript bridge sits in: prior art, the choices considered and rejected, the trade-offs accepted, and the open risks. The bundle is meant to be read alongside the spec, not in place of it.

## Notes

| Note | Subject |
|------|---------|
| [01. Language surface](01-language-surface.md) | The `import ts "<pkg>@<semver>" as <alias>` import shape, the `mochi.toml` `[ts-dependencies]` + `[ts]` tables, the dual-registry selector (`npm:` and `jsr:` prefixes), the CLI surface (`mochi pkg add ts`, `mochi pkg publish --to=npm` + `--to=jsr` + `--to=both`), and the per-import alias semantics. |
| [02. Design philosophy](02-design-philosophy.md) | Why a bidirectional bridge, why the TypeScript compiler API over a hand-written `.d.ts` parser, why NO synthesised wrapper package on the consume side (the key structural simplification), why no async runtime singleton is needed for JS, why dual-registry support ships at once, why Sigstore Trusted Publishing is mandatory on both registries. |
| [03. Prior-art bridges](03-prior-art-bridges.md) | dts-gen, ts-morph, api-extractor, dts-bundle-generator, swc, esbuild, Bun's bundler, Deno's `deno_npm` resolver, `tsd`, `expect-type`, `tsc --build`. What each gets right, what each requires the user to write, and what MEP-72 borrows. |
| [04. TypeScript compiler API + .d.ts ingest](04-tsdoc-dts-ingest.md) | The `ts.createProgram` flow, the `ts.TypeChecker` discriminator tree, the stability story across TS 3-5 majors, why no nightly toolchain is needed, the Node-side helper binary shape, the JSON ApiSurface schema. |
| [05. Type mapping table](05-type-mapping.md) | The complete closed translation table, the refusal cases, the generic monomorphisation rule, the `Promise<T>` and `AsyncIterable<T>` mappings, the `null | undefined` coalescing, the discriminated-union projection. |
| [06. npm + JSR publish flow](06-npm-jsr-publish-flow.md) | The npm registry's `npm publish --provenance --access=public` flow, the JSR registry's `deno publish --token-source=github-actions` flow, the dual-publish from one workflow run, the per-registry metadata requirements, the publish-side gate against verdaccio-mock + JSR-mock. |
| [07. Sigstore on npm + JSR](07-sigstore-npm-jsr-trusted-publishing.md) | The npm Sigstore attestation format, the JSR Sigstore attestation format, the OIDC token exchange model, the `actions/attest-build-provenance@v2` integration, the consumer-side verification path. |
| [08. Promise / async bridge](08-promise-async-bridge.md) | The host JS event loop, why no runtime singleton is needed (unlike Rust tokio or Go cgo handle), the `Promise<T>` ↔ Mochi `async fun` translation, the `AsyncIterable<T>` ↔ Mochi `stream<T>` translation, the microtask scheduling cost. |
| [09. ESM vs CJS interop](09-esm-cjs-interop.md) | The CJS vs ESM module shape distinction, the `exports` map conditional resolution, the dual-package hazard, the Node 22 / Deno 2 / Bun 1.1 / browser per-runtime interop semantics, the browser bundle path's CJS rejection. |
| [10. Runtime target matrix](10-runtime-target-matrix.md) | Node 22 LTS, Deno 2, Bun 1.1, browser ES2024, edge (Cloudflare Workers + Vercel Edge + Deno Deploy). What each runtime supports, what API surface differs, what the bridge promises per target. |
| [11. Browser bundle surface](11-browser-bundle-surface.md) | The `bun build` and `esbuild` paths from MEP-52 Phase 17, the tree-shaking rules for consumed packages, the `node:` import rejection on browser, the WebAssembly capability flag. |
| [12. Risks and alternatives](12-risks-and-alternatives.md) | The risk register (compiler-API RAM footprint, `.d.ts` quality variance, conditional-type resolution, CJS-only browser refusal, JSR attestation gap, dual-package hazard) and the rejected alternatives (hand-written `.d.ts` parser, DT-as-primary, tsserver protocol, wrapper-package synthesis on consume side, long-lived NPM_TOKEN). |

## Cross-references

- [MEP-72 spec](/docs/mep/mep-0072) — the normative document.
- [MEP-52](/docs/mep/mep-0052) — the TypeScript transpiler this bridge builds on.
- [MEP-57](/docs/mep/mep-0057) — the source-level package system whose manifest and lockfile the bridge extends.
- [MEP-73](/docs/mep/mep-0073) — the sister Rust bridge whose spec template MEP-72 mirrors.
- [MEP-74](/docs/mep/mep-0074) — the sister Go bridge whose spec template MEP-72 mirrors.
- [Implementation tracking](/docs/implementation/0072/) — the per-phase delivery status.
