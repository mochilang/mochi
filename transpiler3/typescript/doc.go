// Package typescript hosts the Mochi-to-TypeScript transpiler
// (MEP-52). The pipeline reuses MEP-45's aotir IR + the
// monomorphisation, match-to-decision-tree, closure-conversion,
// exhaustiveness, and sendability passes shared with MEP-46
// through MEP-51, then forks at emit: aotir → tstree → .ts
// text emitted under TS 5.6 strict mode for execution on
// Node 22 LTS, Deno 2.x, Bun 1.1.x, and ES2024-compliant
// browsers.
//
// Sub-packages:
//
//   - colour: async colour pass (Phase 1 stub, Phase 11 full)
//   - lower:  aotir.Program → tstree.SourceFile lowering
//   - tstree: TypeScript syntax-tree model with TsString printer
//   - emit:   tstree.SourceFile → .ts on disk
//   - build:  Driver.Build entry; runs the full pipeline plus
//     optional per-runtime execution (Node, Deno, Bun)
//
// Phase plan: see website/docs/implementation/0052/. The 18
// phases run from hello-world through scalars, collections,
// records, sums, closures, queries, datalog, agents, streams,
// async colour pass + MochiResult, FFI, LLM, fetch, npm package
// build, reproducibility, JSR + Jupyter + browser bundle, and
// npm Trusted Publishing.
package typescript
