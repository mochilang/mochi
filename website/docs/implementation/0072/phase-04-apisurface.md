---
title: "Phase 4. ApiSurface JSON"
sidebar_position: 6
sidebar_label: "Phase 4. ApiSurface"
description: "MEP-72 Phase 4: ApiSurface JSON schema + bridge-side parser. Canonical wire format between the ts-ingest helper (Node) and the bridge (Go)."
---

# Phase 4. ApiSurface JSON

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-72 §Phases](/docs/mep/mep-0072#phases) |
| Status         | NOT STARTED |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase4ApiSurface` in `package3/typescript/apisurface/phase04_test.go`: subtests `schema_version`, `parse_function`, `parse_class`, `parse_interface`, `parse_type_alias`, `parse_namespace`, `parse_generic`, `parse_promise`, `parse_async_iterable`, `parse_union`, `parse_intersection`, `parse_mapped`, `parse_conditional`, `roundtrip_corpus`. The first asserts the parser rejects an unknown schema-version. The next 12 each parse a representative ApiSurface JSON fragment and assert the resulting `ApiSurface` Go struct contains the expected entries. The last round-trips all 24 fixture-corpus packages: ingest produces JSON, parser produces `ApiSurface`, re-serialiser produces JSON identical to the ingest output (byte-for-byte, modulo canonical-JSON normalisation).

## Lowering decisions

The schema is versioned: `"schema-version": "1"` (an integer string). The parser refuses unknown versions; the ingest helper writes the version it was built against.

The schema has six top-level sections:

```json
{
  "schema-version": "1",
  "package": {
    "name": "zod",
    "version": "3.22.4",
    "module-shape": "esm",
    "exports-map-resolved": "./dist/index.mjs"
  },
  "imports": [
    {"specifier": "node:crypto", "kind": "builtin"},
    {"specifier": "fast-deep-equal", "kind": "external", "version": "3.1.3"}
  ],
  "items": [...],
  "types": {...},
  "skipped": [...],
  "capabilities": ["net", "fs"]
}
```

The `items` array is ordered (insertion order from the export walker; matches `.d.ts` declaration order in practice). Each item carries `{kind, name, signature, type-id, jsdoc?, deprecated?}` where `signature` is a string formatted by the helper (`(x: T) => Promise<U>`) and `type-id` is a stable integer pointer into `types`.

The `types` map is a dictionary from `type-id` to a normalised representation. The map is closed: every type the helper emits resolves to one of `{scalar, array, tuple, record, object, union, intersection, function, generic, promise, async-iterable, conditional, mapped, opaque, skip}`. The `opaque` kind covers references to types declared outside the consumed package (Node built-ins, browser DOM types, etc.); these become `extern type` declarations on the Mochi side.

The `skipped` array records every export the helper could not lower, with `{name, reason, location}`. The phase-5 type-mapping table reads this array verbatim and propagates the SkipReports into Mochi's `SkipReport` mechanism.

The `capabilities` array records which `node:*` built-ins the package imports (transitively visible via the `imports` list); phase 9 propagates this into the lockfile and phase 17 cross-checks against the edge-runtime allowed list.

The parser uses Go's `encoding/json` with strict mode (`d.DisallowUnknownFields()`) so a forward-incompatible schema version is caught early. Unknown items in the `kind` discriminator produce a clear error rather than silently dropping.

## Files changed

| File | Purpose |
|------|---------|
| `package3/typescript/apisurface/apisurface.go` | `ApiSurface`, `Package`, `Import`, `Item`, `Type`, `Skip` plus the closed type discriminator |
| `package3/typescript/apisurface/parser.go` | `Parse([]byte) (*ApiSurface, error)` |
| `package3/typescript/apisurface/serialiser.go` | `Serialise(*ApiSurface) ([]byte, error)` (canonical-JSON for lockfile-cache key stability) |
| `package3/typescript/apisurface/phase04_test.go` | `TestPhase4ApiSurface` sentinel |
| `package3/typescript/apisurface/testdata/*.json` | per-fixture canonical ApiSurface JSON (committed as golden) |

## Test set

- `TestPhase4ApiSurface/schema_version`
- `TestPhase4ApiSurface/parse_function`
- `TestPhase4ApiSurface/parse_class`
- `TestPhase4ApiSurface/parse_interface`
- `TestPhase4ApiSurface/parse_type_alias`
- `TestPhase4ApiSurface/parse_namespace`
- `TestPhase4ApiSurface/parse_generic`
- `TestPhase4ApiSurface/parse_promise`
- `TestPhase4ApiSurface/parse_async_iterable`
- `TestPhase4ApiSurface/parse_union`
- `TestPhase4ApiSurface/parse_intersection`
- `TestPhase4ApiSurface/parse_mapped`
- `TestPhase4ApiSurface/parse_conditional`
- `TestPhase4ApiSurface/roundtrip_corpus`

## Cross-references

- [Research note 04 §3 ApiSurface schema](/docs/research/0072/04-tsdoc-dts-ingest) — the wire format this phase parses.
- [MEP-74 phase 4 ApiSurface](/docs/implementation/0074/phase-04-apisurface) — the sister Go-side ApiSurface phase.
