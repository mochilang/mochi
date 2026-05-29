---
title: "Phase 15. SBOM + provenance"
sidebar_position: 16
sidebar_label: "Phase 15. SBOM + provenance"
description: "MEP-57 Phase 15 — CycloneDX 1.6, SPDX 3.0, in-toto attestation emission per target. SLSA Build L3 provenance shape. Per-target SBOM validation."
---

# Phase 15. SBOM + provenance

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 15](/docs/mep/mep-0057#phase-15-sbom) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase15SBOM`: emitted CycloneDX 1.6 validates under the NIST CycloneDX schema; SPDX 3.0 validates under spdx-tools. In-toto attestation is a valid SLSA Build L3 statement per the SLSA verifier.

Pass criteria:

1. CycloneDX 1.6 conformance. The emitted document validates against `cyclonedx.org/schema/bom-1.6.schema.json`; every locked package appears with `purl`, `name`, `version`, `hashes` (BLAKE3 + SHA-256), `licenses`, `supplier`.
2. SPDX 3.0 conformance. Validates against the SPDX 3.0 JSON-LD schema; relationships graph (`DEPENDS_ON`, `CONTAINS`) matches the lockfile tree.
3. In-toto Statement. The Statement validates under `in-toto-attestation` Go library; predicate is SLSA Provenance v1 (already used in Phase 13.2 for the signature payload).
4. CBOM (Capability BOM). The CycloneDX `components[].properties` carries each component's capabilities list, namespaced under `mochi:capability:*`.
5. Per-target SBOM. Phase 14 fan-out runs Phase 15 once per target; each artefact carries its target-specific SBOM (e.g., npm SBOM includes Node runtime deps, PyPI SBOM includes wheel build deps).
6. Bundling. Each tarball contains `.mochi-pkg/sbom.cdx.json` and `.mochi-pkg/sbom.spdx.json` at canonical paths; consumers can `mochi pkg sbom show @mochi/strings` to extract.
7. SBOM is signed. The SBOM file is content-addressed (its SHA-256 included in the in-toto Statement); the Sigstore bundle (Phase 13) covers the SBOM by transitivity through the Statement.

## Goal-alignment audit

SBOM and provenance are the compliance surface for consumers in regulated industries (post-EO 14028, OpenSSF). Without them, Mochi packages cannot enter many supply chains. The user-facing goal moved: "My company's compliance scanner reads my Mochi package's SBOM and recognises the dep graph".

Two formats (CycloneDX + SPDX) because the ecosystem is split: NIST-driven federal procurement prefers SPDX; commercial supply-chain tooling (Snyk, GitHub) prefers CycloneDX. Emitting both is cheap (same source data, different serialisation); refusing to pick avoids long-term lock-in.

The CBOM extension (Capability BOM, research note 12 §A.12) is Mochi's contribution to the SBOM ecosystem: the capability set is structurally similar to a permission manifest, and including it in the SBOM makes capability-aware scanning possible without inventing a new format.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 15.0 | CycloneDX 1.6 emitter from resolved tree | NOT STARTED | — |
| 15.1 | SPDX 3.0 emitter | NOT STARTED | — |
| 15.2 | Capability set included in SBOM (CBOM, per NodeShield 2025) | NOT STARTED | — |
| 15.3 | In-toto Statement v1 + SLSA Build L3 predicate | NOT STARTED | — |
| 15.4 | Per-target SBOM at fan-out | NOT STARTED | — |
| 15.5 | SBOM bundled into `.mochi-pkg/sbom.{cdx,spdx}.json` | NOT STARTED | — |
| 15.6 | Validation against NIST CycloneDX schema; SPDX 3.0 validator | NOT STARTED | — |
| 15.7 | `mochi pkg sbom show / verify / convert` CLI | NOT STARTED | — |

## Sub-phase 15.0 — CycloneDX 1.6 emitter

```go
// pkg/pkgsbom/cyclonedx/emit.go
type BOM struct {
    BOMFormat     string       `json:"bomFormat"`
    SpecVersion   string       `json:"specVersion"`
    SerialNumber  string       `json:"serialNumber"`     // urn:uuid:...
    Version       int          `json:"version"`           // monotonic
    Metadata      Metadata     `json:"metadata"`
    Components    []Component  `json:"components"`
    Dependencies  []Dependency `json:"dependencies"`
}

type Component struct {
    BOMRef       string             `json:"bom-ref"`
    Type         string             `json:"type"`           // "library", "application"
    Name         string             `json:"name"`
    Version      string             `json:"version"`
    Description  string             `json:"description,omitempty"`
    Hashes       []Hash             `json:"hashes,omitempty"`
    Licenses     []License          `json:"licenses,omitempty"`
    Purl         string             `json:"purl"`           // pkg:mochi/@mochi/json@1.2.5
    Supplier     Supplier           `json:"supplier,omitempty"`
    Properties   []Property         `json:"properties,omitempty"`  // CBOM lives here
}

func Emit(lock *Lockfile, m *Manifest, sde time.Time) *BOM {
    bom := &BOM{
        BOMFormat: "CycloneDX",
        SpecVersion: "1.6",
        SerialNumber: deterministicSerial(m, lock),
        Version: 1,
        Metadata: Metadata{
            Timestamp: sde.UTC().Format(time.RFC3339),
            Tools: []Tool{{Vendor: "mochi-lang.org", Name: "mochi", Version: runtimeVersion}},
            Component: rootComponent(m),
        },
    }
    for _, p := range lock.Packages {
        bom.Components = append(bom.Components, componentFromLock(p))
    }
    bom.Dependencies = buildDepGraph(lock)
    return bom
}

// deterministicSerial derives the urn:uuid SerialNumber from the manifest
// + lockfile so two SBOM emits of the same input produce the same URN. The
// input is `package.name + "@" + version + ":" + ManifestHash + ":" +
// LockfileHash`; the output is BLAKE3-derived UUIDv8 per RFC 4122 §5.8
// (vendor-specified namespaced UUID). Required for Phase 17 reproducibility.
func deterministicSerial(m *Manifest, lock *Lockfile) string {
    h := blake3.Sum256([]byte(m.Package.Name + "@" + m.Package.Version +
        ":" + manifestHash(m) + ":" + lockfileHash(lock)))
    return "urn:uuid:" + uuidv8FromHash(h[:])
}

func componentFromLock(p LockedPackage) Component {
    return Component{
        BOMRef: p.Name + "@" + p.Version,
        Type: "library",
        Name: p.Name, Version: p.Version,
        Hashes: []Hash{
            {Alg: "BLAKE3", Content: p.BLAKE3},
            {Alg: "SHA-256", Content: p.SHA256},
        },
        Purl: fmt.Sprintf("pkg:mochi/%s@%s", url.PathEscape(p.Name), p.Version),
        Properties: capProperties(p.Capabilities),
    }
}
```

PURL spec: `pkg:mochi/<scope>/<name>@<version>` (a new PURL type for Mochi, registered with the PURL spec maintainers; pending acceptance see research note 12 §A.12).

## Sub-phase 15.1 — SPDX 3.0 emitter

SPDX 3.0 uses JSON-LD with `@context`:

```go
// pkg/pkgsbom/spdx/emit.go
type Document struct {
    Context        []string                  `json:"@context"`
    Type           string                    `json:"type"`             // "spdxDocument"
    SpdxID         string                    `json:"spdxId"`
    CreationInfo   CreationInfo              `json:"creationInfo"`
    Element        []map[string]any          `json:"element"`          // Package + Relationship
    RootElement    []string                  `json:"rootElement"`
}

func Emit(lock *Lockfile, m *Manifest, sde time.Time) *Document {
    doc := &Document{
        Context: []string{"https://spdx.org/rdf/3.0.0/spdx-context.jsonld"},
        Type: "SpdxDocument",
        SpdxID: "SPDXRef-DOCUMENT",
        CreationInfo: CreationInfo{
            Created: sde.UTC().Format(time.RFC3339),
            Tool: []string{"Tool-mochi-" + runtimeVersion},
        },
    }
    rootSpdxID := emitPackage(doc, m.Package)
    doc.RootElement = []string{rootSpdxID}
    for _, p := range lock.Packages {
        pid := emitPackage(doc, p)
        emitRelationship(doc, rootSpdxID, "DEPENDS_ON", pid)
    }
    return doc
}
```

License field uses SPDX expression syntax (already in `mochi.toml`); validators check the expression is parseable.

## Sub-phase 15.2 — CBOM extension

Each component's `properties` carries the capability set:

```json
{
  "bom-ref": "@mochi/http@1.1.7",
  "name": "@mochi/http",
  "version": "1.1.7",
  "properties": [
    {"name": "mochi:capability:requires", "value": "net.dial"},
    {"name": "mochi:capability:optional", "value": "clock"}
  ]
}
```

This is the CBOM format referenced by NodeShield 2025; one property per capability allows multiset semantics without inventing a sub-schema. SPDX equivalent: the `Element` of type `spdx:Annotation` with namespace `mochi:capability:*`.

## Sub-phase 15.3 — In-toto Statement + SLSA Build L3

Reused from Phase 13.2; the Phase 15 contribution is shaping the predicate's `resolvedDependencies` so every entry has both a `pkg:mochi/...` PURL and the BLAKE3 digest, satisfying SLSA Build L3's "complete dependency tree" requirement.

```go
type ResolvedDep struct {
    URI    string            `json:"uri"`     // pkg:mochi/...
    Digest map[string]string `json:"digest"`  // {"blake3":"...","sha256":"..."}
}

func BuildResolvedDeps(lock *Lockfile) []ResolvedDep {
    out := make([]ResolvedDep, 0, len(lock.Packages))
    for _, p := range lock.Packages {
        out = append(out, ResolvedDep{
            URI: purlOf(p.Name, p.Version),
            Digest: map[string]string{"blake3": p.BLAKE3, "sha256": p.SHA256},
        })
    }
    sort.Slice(out, func(i, j int) bool { return out[i].URI < out[j].URI })
    return out
}
```

The SLSA verifier (`slsa-verifier`) runs as part of the test; passing the verifier proves Build L3 conformance.

## Sub-phase 15.4 — Per-target SBOM

Phase 14's fan-out runs Phase 15 per target. Each target's SBOM contains:

- The Mochi-side resolved tree (same across targets).
- Target-specific runtime deps (npm: Node + the runtime shim; PyPI: hatchling + the runtime shim; etc.).
- Target-specific build tooling versions.

```go
func EmitForTarget(lock *Lockfile, m *Manifest, target string) *BOM {
    base := Emit(lock, m)
    extra := targetSpecificDeps(target, m)
    base.Components = append(base.Components, extra...)
    base.Dependencies = mergeDepGraph(base.Dependencies, targetDeps(target))
    return base
}
```

The npm `package.json` `mochi.sbom` field references the bundled SBOM path.

## Sub-phase 15.5 — Bundling

```
<tarball>/
  mochi.toml
  src/
  LICENSE
  .mochi-pkg/
    sbom.cdx.json        # CycloneDX 1.6
    sbom.spdx.json       # SPDX 3.0
    provenance.intoto.jsonl  # in-toto Statement (also signed in Phase 13)
    .integrity           # BLAKE3+SHA256 of each file above
```

Files are deterministic: the CycloneDX `metadata.timestamp` and SPDX
`creationInfo.created` are populated from `pkgrepro.SourceDateEpoch()`
([phase 17 §17.0](./phase-17-repro#sub-phase-170--source_date_epoch));
CycloneDX `serialNumber` uses `deterministicSerial` (BLAKE3-derived UUIDv8
of manifest + lock hash, see §15.0); component arrays are sorted by name;
JSON object keys emit in declared order via the `encoding/json` `omitempty`
+ reflection ordering pinned to Go 1.24. The SBOM's reproducibility is
asserted by `TestPhase15Reproducible`: emit twice, byte-compare.

## Sub-phase 15.6 — Validation

The CI test runs each emitter and validates with the ecosystem tool:

```go
// tests/pkgsystem/sbom/cdx_test.go
func TestPhase15CycloneDXValidate(t *testing.T) {
    bom := emitCycloneDX(testLock, testManifest)
    schema := loadJSONSchema("schemas/bom-1.6.schema.json")
    if errs := schema.ValidateBytes(jsonMarshal(bom)); len(errs) > 0 {
        t.Fatalf("schema violations: %v", errs)
    }
}

// tests/pkgsystem/sbom/spdx_test.go
func TestPhase15SPDXValidate(t *testing.T) {
    doc := emitSPDX(testLock, testManifest)
    cmd := exec.Command("spdx-tools", "validate", "-")
    cmd.Stdin = bytes.NewReader(jsonMarshal(doc))
    require.NoError(t, cmd.Run())
}
```

`spdx-tools` and the SLSA verifier are vendored in `tests/vendor/` to avoid network at CI time.

## Sub-phase 15.7 — `mochi pkg sbom` CLI

```
mochi pkg sbom show                    # print this package's SBOM (cdx by default)
mochi pkg sbom show --format=spdx
mochi pkg sbom show @mochi/strings     # extract bundled SBOM from cache
mochi pkg sbom verify @mochi/strings   # validate against schema; verify hashes
mochi pkg sbom convert --to=spdx <cdx.json>   # cross-format conversion
mochi pkg sbom diff <bom-a.json> <bom-b.json>  # human-readable component delta
```

The `sbom verify` exit code feeds into `mochi pkg audit --supply-chain` (Phase 16).

## Files changed

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkgsbom/cyclonedx/emit.go` | CycloneDX 1.6 | Owner |
| `pkg/pkgsbom/cyclonedx/types.go` | Schema types | Owner |
| `pkg/pkgsbom/spdx/emit.go` | SPDX 3.0 | Owner |
| `pkg/pkgsbom/cbom/cbom.go` | Capability BOM properties | Owner |
| `pkg/pkgsbom/intoto/builder.go` | SLSA predicate builder | Owner |
| `pkg/pkgsbom/bundle/writer.go` | `.mochi-pkg/` writer | Owner |
| `pkg/pkgsbom/validate/cdx.go` | Schema validation | Owner |
| `pkg/pkgsbom/validate/spdx.go` | SPDX validator wrapper | Owner |
| `cmd/mochi/sbom.go` | `mochi pkg sbom ...` handler | Owner |
| `tests/pkgsystem/sbom/cdx-roundtrip/*` | CycloneDX golden + validate | Owner |
| `tests/pkgsystem/sbom/spdx-roundtrip/*` | SPDX golden + validate | Owner |
| `tests/pkgsystem/sbom/intoto/*` | SLSA verifier passes | Owner |
| `tests/pkgsystem/sbom/cbom/*` | Capability properties present | Owner |

## Error code surface

| Code | Trigger |
|------|---------|
| `M057_SBOM_E001` | Emitted BOM fails schema validation. |
| `M057_SBOM_E002` | Bundled SBOM hash does not match in-toto Statement subject. |
| `M057_SBOM_E003` | Conversion lost required field. |
| `M057_SBOM_E004` | PURL malformed (invalid scope or version). |

## Test set

- `TestPhase15CycloneDXValidate` — schema OK.
- `TestPhase15SPDXValidate` — schema OK.
- `TestPhase15CBOMProperties` — capabilities appear as properties.
- `TestPhase15SLSAVerifier` — slsa-verifier passes the in-toto Statement.
- `TestPhase15PerTarget` — npm SBOM contains npm runtime deps.
- `TestPhase15Bundle` — `.mochi-pkg/sbom.*.json` present in tarball.
- `TestPhase15Determinism` — twice-emitted SBOM byte-identical.
- `TestPhase15Convert` — `cdx -> spdx -> cdx` preserves component set.

## Open questions

- Whether the PURL type `pkg:mochi/...` is registered with the PURL spec; current plan: file the PR with `package-url/purl-spec`, ship with `pkg:generic/...` fallback if not yet merged at v1 GA.
- Whether to support VEX (Vulnerability Exploitability eXchange) inside the SBOM; current plan: yes at v1.1, after Phase 16 advisory wiring is stable.
- Whether to emit a SWID tag for Windows compliance; deferred to v2.

## Cross-references

- SBOM tooling immaturity risk: [research note 12 §A.12](/docs/research/0057/risks-and-alternatives).
- Trusted publishing in-toto context: [research note 09 §13](/docs/research/0057/trusted-publishing).
- SLSA Build L3 predicate shape: [phase 13 §13.2](./phase-13-sigstore#sub-phase-132--in-toto-statement-v1--slsa-build-l3).
- Capability source: [phase 10 §10.4](./phase-10-capabilities#sub-phase-104--lockfile-aggregator).
