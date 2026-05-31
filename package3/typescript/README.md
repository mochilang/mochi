# package3/typescript

Bidirectional TypeScript/JavaScript package bridge for Mochi. Implementation lives under this directory once phases land per [MEP-72 implementation tracking](../../website/docs/implementation/0072/index.md).

## Status

Stub. No code yet. The phase-00 skeleton (`Driver`/`Workspace` types, errors package, semver helper, ts-ingest helper plumbing) is the first implementation deliverable.

## What this is

The MEP-72 bridge sits between MEP-52 (Mochi-to-TypeScript transpiler) and MEP-57 (Mochi source-level package system). Two directions:

- **Consume**: `import ts "<pkg>@<semver>" as <alias>` in Mochi source. The bridge ingests the `.d.ts` tree (or source `.ts` for JSR) via the `package3/typescript/cmd/ts-ingest` helper (which uses the official `typescript` npm package's compiler API), lowers via a closed type table, and exposes TS items as Mochi `extern fn` declarations. NO synthesised wrapper package on the consume side: the host JS runtime IS the link layer.
- **Publish**: `mochi pkg publish --target=npm-library` (or `--target=jsr-library`). The bridge lowers the Mochi package via new `TargetNpmLibrary` + `TargetJsrLibrary` emits, then the user's CI runs the MEP-52 Phase 18 emitted GitHub Actions workflow (`npm publish --provenance` or `deno publish --token-source=github-actions`) under OIDC Trusted Publishing.

## Planned layout

```
package3/typescript/
  cmd/
    ts-ingest/      # TypeScript compiler API ApiSurface emitter (Node-side; phase 3)
  npmregistry/      # registry.npmjs.org client (phase 1)
  jsrregistry/      # jsr.io client (phase 2)
  apisurface/       # ApiSurface JSON parser (phase 4)
  typemap/          # closed type-mapping table + SkipReport (phase 5)
  emit/             # Mochi extern fn emitter (phase 6)
  build/            # workspace + bundler orchestration (phase 8)
  lockfile/         # `[[npm-package]]` + `[[jsr-package]]` schema + drift check (phase 9)
  library/          # TargetNpmLibrary + TargetJsrLibrary emit (phases 10, 11)
  publish/          # npm + JSR Trusted-Publishing flow wiring (phases 12, 13)
  promise/          # Promise / async / AsyncIterable translation (phase 14)
  monomorphise/     # `[ts.monomorphise]` parser + renderer (phase 15)
  esm/              # ESM/CJS interop + exports-map resolver (phase 16)
  edge/             # edge-runtime gate + Jupyter consume (phase 17)
  browser/          # browser-bundle pre-flight + size gate (phase 11 + 17)
  errors/           # SkipReason, SkipReport, BridgeError (cross-phase)
  semver/           # node-semver-compatible matcher (cross-phase)
```

## How MEP-72 differs from MEP-73 + MEP-74

Three architectural simplifications versus the Rust and Go bridges:

- **No wrapper package on consume side.** Rust needs an `extern "C"` shim crate with a `cdylib` target; Go needs a cgo wrapper module with `c-archive` build mode and a handle pool. TypeScript needs neither. The host JS runtime is already the link layer; the Mochi-emitted JS imports the consumed package directly. The skeleton omits the equivalents of `package3/rust/wrapper/` and `package3/go/wrapper/`.
- **No async runtime singleton.** Rust needs `tokio::runtime::Runtime` constructed once per process; Go has its own scheduler inside the c-archive. JavaScript's event loop is intrinsic to every runtime target; the bridge adds zero code for runtime construction in `promise/`, only translation.
- **Mandatory transparency log is two-sided, not one-sided.** Rust uses Sigstore on top of crates.io (no native log); Go uses `sum.golang.org` (native log). TypeScript ships against npm AND JSR (both have Sigstore Trusted Publishing). Both registries are mandatory in MEP-72; no fallback to long-lived tokens.

Two architectural complications:

- **Two registries, not one.** npm and JSR are equal first-class citizens. Both clients ship at phases 1 and 2; the lockfile carries two repeated tables.
- **Five runtime targets, not three.** Node 22 LTS + Deno 2 + Bun 1.1 share the bulk of the consumable surface; browser ES2024 and edge (Cloudflare Workers / Vercel Edge / Deno Deploy) take a restricted subset. The per-phase target matrix marks the n/a cells explicitly.

## References

- [MEP-72 spec](../../website/docs/mep/mep-0072.md) for the normative design.
- [MEP-72 research bundle](../../website/docs/research/0072/index.md) for the 12-note deep-research collection.
- [MEP-72 implementation tracking](../../website/docs/implementation/0072/index.md) for the 18-phase rollout.
