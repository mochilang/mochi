---
title: "MEP-72 Note 04: TypeScript compiler API + .d.ts ingest"
sidebar_position: 5
sidebar_label: "04. compiler API ingest"
description: "How the bridge ingests TypeScript type information: the `ts.createProgram` flow, the `ts.TypeChecker` discriminator tree, the stability story across TS 3.x to 5.x majors, why no nightly toolchain is needed, the Node-side helper binary shape, the JSON ApiSurface schema."
---

# 04. TypeScript compiler API and `.d.ts` ingest

This note describes how the bridge walks a consumed package's TypeScript surface and produces the ApiSurface JSON the type-mapping pass consumes. It is informative; the normative reference is the TypeScript team's compiler-API documentation.

## 1. The `ts.createProgram` flow

The bridge's `ts-ingest` helper binary executes the following sequence:

```typescript
import * as ts from "typescript";

const program = ts.createProgram({
    rootNames: [packageEntryPoint],
    options: {
        target: ts.ScriptTarget.ES2024,
        module: ts.ModuleKind.ESNext,
        moduleResolution: ts.ModuleResolutionKind.NodeNext,
        declaration: false,
        strict: true,
        skipLibCheck: true,
    },
});

const checker = program.getTypeChecker();
const sourceFile = program.getSourceFile(packageEntryPoint);
const symbol = checker.getSymbolAtLocation(sourceFile);
const exports = checker.getExportsOfModule(symbol);

for (const exp of exports) {
    const type = checker.getTypeOfSymbolAtLocation(exp, sourceFile);
    emitApiSurfaceEntry(exp, type);
}
```

The `program.getSourceFile(packageEntryPoint)` call resolves through the package's `package.json` `types` field (npm) or `exports.types` condition; if neither is present, the bridge falls back to `<pkg>/index.d.ts`.

For JSR packages (which publish source `.ts` rather than `.d.ts`), the entry point is the `jsr.json` `exports` field; the `.ts` source is parsed directly. The compiler API handles `.ts` and `.d.ts` uniformly.

## 2. The `ts.TypeChecker` discriminator tree

Each `ts.Type` carries a `flags` field (an `ts.TypeFlags` bitfield) that the bridge dispatches on:

| `ts.TypeFlags` bit | Bridge handling |
|---|---|
| `Number`, `NumberLiteral` | Translate to Mochi `float`. |
| `BigInt`, `BigIntLiteral` | Translate to Mochi `int`. |
| `String`, `StringLiteral` | Translate to Mochi `string`. |
| `Boolean`, `BooleanLiteral` | Translate to Mochi `bool`. |
| `Void`, `Undefined`, `Null` | Translate to Mochi `nil` (`void` to `unit`). |
| `Object` + ObjectFlags `Reference` (to `Array`, `Map`, `Set`, `Promise`, etc.) | Dispatch via the generic-arg-aware sub-table in [[05-type-mapping]]. |
| `Object` + ObjectFlags `Interface` | Translate to Mochi `record` if every field is in-table. |
| `Object` + ObjectFlags `Anonymous` (object literal types) | Same as `Interface`. |
| `Object` + ObjectFlags `Class` | Translate to Mochi `extern type` (opaque handle). |
| `Union` (string-literal-discriminated) | Translate to Mochi sum type. |
| `Union` (`T | null` / `T | undefined`) | Translate to Mochi `T?`. |
| `Union` (other) | SkipReport. |
| `Intersection` | Best-effort merge if no overlapping non-identical members; otherwise SkipReport. |
| `Conditional` | Eagerly resolve at bind site; SkipReport if unresolvable. |
| `Index`, `IndexedAccess` | Eagerly resolve; SkipReport if unresolvable. |
| `TypeParameter` (unmonomorphised) | SkipReport unless the manifest's `[ts.monomorphise]` lists the type. |

For each translatable item, the bridge emits an ApiSurface entry; for each skipped item, the bridge emits a SkipReport entry naming the symbol and the reason.

## 3. The Node-side helper binary

The `ts-ingest` helper is a standalone Node / Bun script bundled at Mochi build time via `bun build --target=node --format=esm --minify` into a single `.js` file embedded in the Mochi binary. At runtime:

1. The bridge extracts the bundled helper to a temp file.
2. Invokes `node <helper>.js <pkg-dir>` (or `bun <helper>.js <pkg-dir>` if bun is available; bun is faster but optional).
3. Reads the helper's stdout (JSON-formatted ApiSurface).
4. Deletes the temp file.

The helper has a single dep: the `typescript` npm package (version pinned at Mochi build time). The helper is ~400 LOC including the discriminator walk and the JSON serialisation; it does not import any other npm packages.

The helper's behaviour is deterministic: given the same input `.d.ts` tree and the same TypeScript version, the output JSON is byte-identical (the bridge sorts symbol order alphabetically by name in the emit).

## 4. Stability story across TypeScript majors

TypeScript's compiler API has been API-stable since version 3.0 (2018). Major versions (3.x → 4.x → 5.x; 6.x projected late 2026) have not removed any of the APIs the bridge uses:

- `ts.createProgram`
- `ts.TypeChecker.getTypeAtLocation`
- `ts.TypeChecker.getTypeOfSymbolAtLocation`
- `ts.TypeChecker.getExportsOfModule`
- `ts.TypeChecker.symbolToString`
- `ts.TypeChecker.typeToString`

New features (e.g., `satisfies` operator in 4.9, `using` declarations in 5.2, `const` type parameters in 5.0) add new `ts.TypeFlags` bits the bridge can dispatch on; the bridge falls back to SkipReport for unrecognised flags, so a newer TypeScript version producing types the bridge does not yet understand is a known, recoverable failure mode (the user gets a clear SkipReport and can override with `extern fn`).

## 5. The ApiSurface JSON schema

```json
{
  "schema-version": "1",
  "package": {
    "name": "zod",
    "version": "3.23.8",
    "registry": "npm"
  },
  "exports": [
    {
      "name": "string",
      "kind": "function",
      "signature": {
        "params": [],
        "return": { "kind": "named", "name": "ZodString" }
      }
    },
    {
      "name": "object",
      "kind": "function",
      "signature": { "...": "..." }
    },
    {
      "name": "ZodString",
      "kind": "class",
      "methods": [
        { "name": "min", "signature": { "...": "..." } },
        { "name": "max", "signature": { "...": "..." } }
      ]
    }
  ],
  "skip-report": [
    {
      "name": "infer",
      "reason": "TypeFlags.TypeParameter without monomorphise entry"
    }
  ]
}
```

The schema is versioned (the `schema-version` field). Schema upgrades are append-only; the bridge accepts older schemas via a compatibility shim.

## 6. Performance characteristics

Measured on darwin-arm64, Node 22.11.0, May 2026:

| Package | `.d.ts` LOC | Ingest wall-clock | RSS peak |
|---------|-------------|-------------------|----------|
| `zod@3.23.8` | 3,200 | 0.8 s | 130 MB |
| `lodash@4.17.21` (DT companion) | 2,800 | 0.6 s | 110 MB |
| `react@18.3.1` (DT companion) | 6,500 | 1.4 s | 180 MB |
| `typescript@5.6.3` (self-ingest) | 50,000 | 8.2 s | 510 MB |
| `aws-sdk@2.x` (DT companion) | 280,000 | 95 s | 2.4 GB |

The TypeScript-itself self-ingest is the worst small case; AWS SDK is the worst large case. The bridge parallelises across packages with a soft RSS budget (sequential within a package; up to `runtime.NumCPU() / 4` packages in flight).

## 7. Failure modes

- **Package ships no `.d.ts` and no DT companion**: SkipReport for the whole package; the user must hand-author the `extern fn` declarations.
- **Package's `.d.ts` references a peer dep**: the bridge installs the peer dep into the workspace before running the ingest.
- **Package's `.d.ts` uses a TS feature the bridge does not yet understand**: SkipReport per item.

## 8. Cross-references

- [[02-design-philosophy]] §1 — why the compiler API.
- [[05-type-mapping]] — what the bridge does with the ApiSurface JSON.
- [MEP-72 §Specification §1](/docs/mep/mep-0072) — the pipeline overview.
