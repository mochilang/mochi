---
title: "MEP-52 research bundle: Mochi to TypeScript / JavaScript transpiler"
description: "Twelve research notes covering language surface, design philosophy, prior art, runtime, codegen, type lowering, runtime portability, dataset pipeline, agent and stream lowering, build system, testing gates, and risks for the Mochi-to-TypeScript/JavaScript transpiler proposed in MEP-52."
sidebar_position: 52
sidebar_label: "MEP-52"
---

# MEP-52 research bundle: Mochi to TypeScript / JavaScript transpiler

Author: research pass for MEP-52 (Mochi-to-TypeScript/JavaScript transpiler).
Date: 2026-05-23 (GMT+7).

This bundle contains the twelve research notes that informed [MEP-52, the Mochi-to-TypeScript/JavaScript transpiler](/docs/mep/mep-0052). The notes are informative; the normative spec is in the MEP body.

| # | Title | Topic |
|---|-------|-------|
| 01 | [Language surface](/docs/research/0052/language-surface) | Mochi language surface mapped onto TypeScript 5.6 and ES2024 lowering obligations |
| 02 | [Design philosophy](/docs/research/0052/design-philosophy) | Why TypeScript, why ES2024 floor, why `tsc --strict` + `--noUncheckedIndexedAccess` dual gates, why AsyncIterableQueue + AbortController, why npm canonical with four-runtime conditional exports |
| 03 | [Prior art](/docs/research/0052/prior-art-transpilers) | Survey of source-to-TypeScript/JavaScript tooling: Babel, SWC, esbuild, tsc, sucrase, Bun, ts-blank-space, CoffeeScript, ReScript, Fable, Elm, PureScript, Kotlin/JS, Scala.js, GopherJS, AssemblyScript, JSII, Pyodide, and more |
| 04 | [Runtime](/docs/research/0052/runtime) | The `@mochi/runtime` npm and JSR package: collections, io, agent, stream, query, datalog, ai, fetch, JsonValue, MochiResult, Temporal polyfill, FFI dispatch across Node N-API / Deno FFI / Bun FFI |
| 05 | [Codegen design](/docs/research/0052/codegen-design) | aotir-to-TypeScript lowering via Mochi-side TS syntax tree printer, monomorphisation, closure conversion, match-to-switch-tag, source maps, prettier post-format, `tsc --noEmit` typecheck gate |
| 06 | [Type lowering](/docs/research/0052/type-lowering) | Per-type details for every Mochi type onto TypeScript 5.6: `bigint`/`number` (int monomorphisation), `string` UTF-16 vs code-point semantics, `readonly T[]` vs `T[]` variance, `Map`, `Set`, frozen classes, discriminated unions, `T \| null`, MochiResult Ok/Err, `Callable`, `AsyncIterable` |
| 07 | [Runtime portability](/docs/research/0052/runtime-portability) | Node 22 LTS x Deno 2 x Bun 1.1 x browser matrix, wheel-equivalent build variants, conditional exports map, JSR co-publish, esbuild bundle, WebContainer, v1 exclusions (Lambda Node 18, Cloudflare Workers, React Native) |
| 08 | [Dataset pipeline](/docs/research/0052/dataset-pipeline) | Query DSL lowering via ES2024 Iterator helpers + AsyncIterable, hash/merge/nested-loop joins, group-by, top-K via min-heap, Datalog semi-naive evaluation, deliberate rejection of arquero / danfojs / duckdb-wasm / polars-js |
| 09 | [Agents and streams](/docs/research/0052/agent-streams) | Mochi agents as a custom class wrapping `AsyncIterableQueue<Message>` + `AbortController` supervision tree, `Promise.withResolvers` for `call(req)`, cold/hot streams, `AggregateError` for sibling failure aggregation |
| 10 | [Build system](/docs/research/0052/build-system) | `package.json` (PEP 621-equivalent: npm spec) + `tsc --build` project references + npm publish with provenance (Sigstore + GitHub OIDC, April 2024 GA), JSR for Deno via `deno publish`, esbuild browser bundle |
| 11 | [Testing gates](/docs/research/0052/testing-gates) | Per-phase Go test gates, four-runtime matrix (Node 22 / Deno 2 / Bun 1.1 / browser via Playwright), `tsc --strict` + `--noUncheckedIndexedAccess` + `--exactOptionalPropertyTypes`, eslint + prettier fixed-point, `npm pack` + install + execute, ~400-fixture corpus by Phase 18 |
| 12 | [Risks and alternatives](/docs/research/0052/risks-and-alternatives) | 15 risks + 6 rejected alternatives (Babel, esbuild-only, JSDoc-only, Webpack, Rollup, RxJS) + 4 future candidates (WASI, ts-blank-space, Bun-native compile, Cloudflare Workers) |

## Cross-references

- [MEP-45 (C target)](/docs/mep/mep-0045)
- [MEP-46 (BEAM target)](/docs/mep/mep-0046)
- [MEP-47 (JVM target, direct bytecode)](/docs/mep/mep-0047)
- [MEP-48 (.NET target)](/docs/mep/mep-0048)
- [MEP-49 (Swift target)](/docs/mep/mep-0049)
- [MEP-50 (Kotlin target)](/docs/mep/mep-0050)
- [MEP-51 (Python target)](/docs/mep/mep-0051)
