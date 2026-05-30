---
title: "Phase 16. ESM vs CJS interop"
sidebar_position: 18
sidebar_label: "Phase 16. ESM/CJS"
description: "MEP-72 Phase 16: ESM vs CJS interop pass. Resolves the `exports`-map conditional resolution, detects the dual-package hazard, refuses CJS-only packages on browser target."
---

# Phase 16. ESM vs CJS interop

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-72 §Phases](/docs/mep/mep-0072#phases) |
| Status         | NOT STARTED |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase16EsmCjs` in `package3/typescript/esm/phase16_test.go`: subtests `exports_map_resolve_node22`, `exports_map_resolve_deno2`, `exports_map_resolve_bun11`, `exports_map_resolve_browser`, `cjs_only_browser_refusal`, `dual_package_hazard_detect`, `node_prefix_imports`, `golden_corpus`. The first four resolve the same `exports` map under each runtime's condition set and assert the picked entry-point file matches the golden. The fifth runs the browser-bundle pre-flight pass against a CJS-only package (`lodash`) and asserts the bridge refuses with a clear diagnostic. The sixth feeds a representative dual-package fixture (`react@18` ships dual; the ESM and CJS paths share state) and asserts the bridge emits a build-time warning. The seventh runs the `node:fs`-import detector against `node:fs`, `node:net`, `node:child_process` and asserts the lockfile records the capabilities. The eighth runs all 24 fixture packages.

## Lowering decisions

The `package.json` `exports` map (Node 12+, May 2019; finalised in Node 16) is the canonical conditional resolution mechanism. The bridge implements the resolution algorithm verbatim from the Node.js docs (https://nodejs.org/api/packages.html#conditional-exports), with the following condition orders:

- **Node 22 LTS**: `["types", "node", "import", "require", "default"]` (ESM by default; CJS via `require`).
- **Deno 2**: `["types", "deno", "import", "default"]` (Deno-specific condition takes priority; no CJS).
- **Bun 1.1**: `["types", "bun", "import", "require", "default"]` (Bun-specific condition takes priority).
- **Browser**: `["types", "browser", "import", "default"]` (no `node` condition).
- **Edge** (Cloudflare Workers, Vercel Edge, Deno Deploy): `["types", "workerd"/"edge-light"/"deno", "import", "default"]`.

The resolver:

```go
func ResolveExports(exports any, condition []string) (string, error) { ... }
```

walks the exports tree (which may be a string, an object with conditions, or a nested map of subpaths) and picks the first condition that matches the given runtime's order.

The dual-package hazard detector compares the ApiSurface ingest result for the ESM path and the CJS path; if they differ in signature or in exported items, the bridge records a `SkipDualPackageHazard` for the differing items and emits a build-time warning.

The CJS-only-browser refusal: when `runtime = "browser"` and the resolved entry-point is a `.cjs` file (or the package's `"type": "module"` flag is absent AND there is no `.mjs` path), the bridge refuses with:

```
mochi build: package "lodash@4.17.21" is CJS-only and cannot be consumed by the browser target.
  Consider switching to "lodash-es@4.17.21" (the ESM-shipped lodash variant) or
  asking the upstream maintainer to ship ESM.
```

The `node:` import detector runs across the consumed package's source tree (after extraction). It uses a regex pass for `import\s+.*\s+from\s+['"]node:(\w+)['"]` and `require\(['"]node:(\w+)['"]\)`. Matches populate the lockfile's `capabilities-declared` field.

## Files changed

| File | Purpose |
|------|---------|
| `package3/typescript/esm/exports.go` | `ResolveExports`, condition-order tables |
| `package3/typescript/esm/dual_hazard.go` | dual-package hazard detector |
| `package3/typescript/esm/browser_refuse.go` | CJS-only-browser refusal |
| `package3/typescript/esm/node_prefix.go` | `node:*` import detector |
| `package3/typescript/esm/phase16_test.go` | `TestPhase16EsmCjs` sentinel |

## Test set

8 subtests as listed in the Gate section.

## Cross-references

- [Research note 09 ESM vs CJS interop](/docs/research/0072/09-esm-cjs-interop) — the full design.
- [Research note 11 §3 The `node:` import rejection](/docs/research/0072/11-browser-bundle-surface#3-the-node-import-rejection) — the browser-bundle CJS-rejection.
