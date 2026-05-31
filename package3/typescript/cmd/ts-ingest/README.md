# ts-ingest

The Node-side helper binary that ingests a consumed package's `.d.ts` (or source `.ts` for JSR) into an ApiSurface JSON. Bundles the official `typescript` npm package's compiler API.

Lands in [MEP-72 Phase 3](../../../../website/docs/implementation/0072/phase-03-dts-ingest.md).

## Status

Stub. The phase-3 bundle (single-file `.mjs` produced via `bun build` against `main.ts` + `walker.ts` + `resolver.ts` + `skipper.ts`) is the first deliverable.

## Invocation

```
ts-ingest --input=<dir> --output=<json-path> --target=<runtime> [--ts-version=<semver>] [--max-rss=<MB>]
```

The bundled artefact is embedded in the Mochi binary via `go:embed` at build time. On first invocation per Mochi process, the helper is extracted to `$XDG_CACHE_HOME/mochi/ts-ingest/<sha>/ts-ingest.mjs` and run via `node --no-deprecation`.

## Why a Node-side helper, not a Go binding

The TypeScript compiler API is the canonical source for `.d.ts` semantics. A hand-written Go parser for the TS grammar would be (1) tens of thousands of LOC for a passable subset, (2) immediately stale (the TS team ships new syntax every release), and (3) divergent on edge cases that the official compiler defines.

The helper-binary pattern matches MEP-74's `go-ingest` (Phase 3) and MEP-73's `rustdoc-ingest` (Phase 3). Each language's bridge defers to the language's authoritative type-checker.

## References

- [Research note 04 `.d.ts` ingest](../../../../website/docs/research/0072/04-tsdoc-dts-ingest.md)
- [MEP-72 Phase 3 tracking page](../../../../website/docs/implementation/0072/phase-03-dts-ingest.md)
