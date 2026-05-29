---
title: "Phase 1. Manifest format"
sidebar_position: 2
sidebar_label: "Phase 1. Manifest format"
description: "MEP-57 Phase 1 — `mochi.toml` parser, schema validation, semver range parsing, canonical writer, error code surface. Three Mochi-only sections (`[capabilities]`, `[targets]`, `[provenance]`) gain working parsers."
---

# Phase 1. Manifest format

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 1](/docs/mep/mep-0057#phase-1-manifest) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase1Manifest`: round-trip every fixture under `tests/pkgsystem/manifest/` (parse, then canonical write, then parse again, assert structural equality). Every error code `M057_MANIFEST_E001` through `M057_MANIFEST_E012` has at least one positive (rejected) and one negative (accepted) fixture.

Pass criteria:

1. Round-trip stability. For each `tests/pkgsystem/manifest/positive/*.toml`, the byte sequence emitted by `pkgmanifest.WriteFile` after parse is structurally equal to the input on second parse (Manifest struct DeepEqual). Whitespace and ordering are normalised by the writer, so a second emit must be byte-identical to the first emit.
2. Error-code coverage. For each `M057_MANIFEST_E<NNN>` (E001 through E012), at least one fixture in `tests/pkgsystem/manifest/negative/eNNN/*.toml` raises that exact code, and the harness asserts `errors.Is(err, manifesterr.E<NNN>)`.
3. Semver range coverage. `tests/pkgsystem/manifest/semver/cases.json` lists 200+ range/version pairs from the npm and Cargo test corpora; `pkgmanifest.SemverReq.Matches(version)` returns the documented bool for each.
4. Schema closure. Any top-level table not in the closed set (see `[package]`, `[dependencies]`, `[dev-dependencies]`, `[features]`, `[targets]`, `[capabilities]`, `[provenance]`, `[workspace]`, `[registry]`) raises `M057_MANIFEST_E012`. The closed-set table appears in `pkg/pkgmanifest/schema.go`.
5. Workspace inheritance. Fixtures under `tests/pkgsystem/manifest/workspace/` exercise the `workspace = true` form: the parser resolves the inheritance against the workspace root and returns a fully expanded manifest. Unresolved inheritance raises `M057_MANIFEST_E009`.

## Goal-alignment audit

The manifest is the human-edit surface. Without a robust parser, every other phase is wedged. The user-facing goal moved is "I can write a `mochi.toml` and Mochi parses it predictably with helpful errors on malformed input".

Why this phase is load-bearing for everything that follows:

- Phase 2 (local resolution) reads the manifest to dispatch `import` statements between path-form and registry-form.
- Phase 3 (workspaces) extends the manifest with `[workspace]` and `workspace = true` inheritance, and requires the Phase 1 parser to be extensible.
- Phase 4 (lockfile) emits a lockfile that references the manifest's dep set; the lockfile is rejected if the dep set drifts from the manifest.
- Phase 5 (solver) takes the resolved manifest as its root incompatibility input.
- Phase 10 (capabilities) consumes `[capabilities]` declarations.
- Phase 12 (publish) reads `[provenance]` and merges Sigstore output back into the manifest of the published artifact.

A bug in Phase 1 surfaces as a downstream confusion in Phase 2-12; the cost of cutting corners here is multiplied 11 times. The acceptance bar (round-trip stability + closed-set rejection + 12 error codes) is therefore higher than would be normal for a parser of this size.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 1.0 | TOML parse via `pelletier/go-toml/v2`; struct mapping | NOT STARTED | — |
| 1.1 | Semver range grammar (`^`, `~`, `>=`, `=`, `*`, bare) | NOT STARTED | — |
| 1.2 | Schema validation (closed top-level keys, error codes E001-E012) | NOT STARTED | — |
| 1.3 | `[capabilities]`, `[targets]`, `[provenance]` sub-parsers | NOT STARTED | — |
| 1.4 | Canonical writer (sorted keys, lowercase hex, LF endings) | NOT STARTED | — |
| 1.5 | Workspace inheritance resolution (`workspace = true`) | NOT STARTED | — |
| 1.6 | Round-trip fixture corpus and error-code fixture corpus | NOT STARTED | — |
| 1.7 | Edition dispatch (`package.edition = "2026"` switch) | NOT STARTED | — |

## Sub-phase 1.0 — TOML parse and struct mapping

Use `github.com/pelletier/go-toml/v2` v2.2 or later. The struct shape lives in `pkg/pkgmanifest/manifest.go` (defined in Phase 0). Phase 1 wires the parser:

```go
// pkg/pkgmanifest/parse.go
package pkgmanifest

import (
    "bytes"
    "fmt"
    "os"

    "github.com/pelletier/go-toml/v2"
)

func ParseFile(path string) (*Manifest, error) {
    buf, err := os.ReadFile(path)
    if err != nil {
        return nil, fmt.Errorf("%w: %s: %v", ErrManifestE001, path, err)
    }
    return ParseBytes(buf, path)
}

func ParseBytes(buf []byte, originPath string) (*Manifest, error) {
    var m Manifest
    dec := toml.NewDecoder(bytes.NewReader(buf))
    dec.DisallowUnknownFields() // closed top-level schema
    if err := dec.Decode(&m); err != nil {
        return nil, wrapTOMLErr(err, originPath)
    }
    m.OriginPath = originPath
    if err := Validate(&m); err != nil {
        return nil, err
    }
    return &m, nil
}
```

Edge cases:

- BOM. UTF-8 BOM at start of file is stripped before TOML parse.
- CRLF. Windows-edited files use CRLF; the parser must accept them. The writer always emits LF.
- Comments. `pelletier/go-toml/v2` discards comments; round-trip stability is defined against the post-parse Manifest struct, not the byte-level source.

## Sub-phase 1.1 — Semver range grammar

Grammar (per research note 04 §3):

```
range = comparator (',' comparator)*
comparator = ('^' | '~' | '<' | '<=' | '>' | '>=' | '=' )? version
           | '*'                                                       ; any
version = digits '.' digits '.' digits ('-' prerelease)? ('+' build)?
```

Default operator if none given: `^`. So `"1.2.3"` means `"^1.2.3"`. This matches Cargo and npm; uv adopted the same.

Types:

```go
// pkg/pkgmanifest/semver.go
type Version struct {
    Major, Minor, Patch uint64
    Pre   string
    Build string
}

type Operator int
const (
    OpExact Operator = iota
    OpCaret
    OpTilde
    OpLt
    OpLte
    OpGt
    OpGte
    OpAny
)

type Comparator struct {
    Op      Operator
    Version Version
}

type Range struct {
    Comparators []Comparator // implicit AND
}

func (r Range) Matches(v Version) bool { /* ... */ }
func ParseRange(s string) (Range, error) { /* ... */ }
```

Cases the parser must cover:

| Input         | Meaning                            |
|---------------|------------------------------------|
| `"1.2.3"`     | `^1.2.3` -> `>=1.2.3, <2.0.0`      |
| `"^1.2.3"`    | `>=1.2.3, <2.0.0`                  |
| `"^0.2.3"`    | `>=0.2.3, <0.3.0` (0.x is special) |
| `"^0.0.3"`    | `>=0.0.3, <0.0.4`                  |
| `"~1.2.3"`    | `>=1.2.3, <1.3.0`                  |
| `"~1.2"`      | `>=1.2.0, <1.3.0`                  |
| `"~1"`        | `>=1.0.0, <2.0.0`                  |
| `">=1.2.3"`   | `>=1.2.3`                          |
| `"=1.2.3"`    | `=1.2.3`                           |
| `"*"`         | any                                |
| `">=1.2, <2"` | intersection (parses as two comparators) |
| `"1.0.0-rc.1"`| exact pre-release pin              |

Pre-release ordering follows semver 2.0.0 (dot-separated alphanumeric identifiers; numeric < alphanumeric).

## Sub-phase 1.2 — Schema validation

The closed top-level set, defined in `pkg/pkgmanifest/schema.go`:

```go
var allowedTopLevel = map[string]struct{}{
    "package": {}, "dependencies": {}, "dev-dependencies": {},
    "features": {}, "targets": {}, "capabilities": {},
    "provenance": {}, "workspace": {}, "registry": {},
}
```

`Validate` walks the parsed manifest and raises specific error codes:

```go
// pkg/pkgmanifest/validate.go
func Validate(m *Manifest) error {
    if err := validatePackage(m.Package); err != nil { return err }   // E002, E003, E004, E005
    for name, dep := range m.Dependencies {
        if err := validateDep(name, dep); err != nil { return err }  // E006
    }
    if err := validateCapabilities(m.Capabilities); err != nil { return err }  // E007
    if err := validateTargets(m.Targets); err != nil { return err }            // E008
    if err := validateFeatures(m); err != nil { return err }                   // E010
    if err := validateOverridePaths(m); err != nil { return err }              // E011
    return nil
}
```

Error code table (mirrors research note 04 §10):

| Code | Trigger |
|------|---------|
| `M057_MANIFEST_E001` | TOML did not parse. |
| `M057_MANIFEST_E002` | Required key absent (e.g. `package.name`, `package.version`, `package.edition`, `package.mochi`). |
| `M057_MANIFEST_E003` | `package.name` violates the scope/name regex (`[a-z][a-z0-9_-]*` or `@scope/[a-z][a-z0-9_-]*`; scope <= 39 chars, name <= 64). |
| `M057_MANIFEST_E004` | `package.version` is not semver 2.0.0. |
| `M057_MANIFEST_E005` | `package.license` is not a valid SPDX 3.x expression. |
| `M057_MANIFEST_E006` | A dependency version range is not parseable. |
| `M057_MANIFEST_E007` | A capability is not in the closed set. |
| `M057_MANIFEST_E008` | A target is not in the closed set. |
| `M057_MANIFEST_E009` | A `workspace = true` dep is not resolved against `[workspace.dependencies]`. |
| `M057_MANIFEST_E010` | A feature references a non-existent dep. |
| `M057_MANIFEST_E011` | An override path does not exist on disk. |
| `M057_MANIFEST_E012` | Unknown top-level table. |

SPDX license validation uses the `github.com/CycloneDX/cyclonedx-go` SPDX expression parser (or equivalent). Compound expressions (`Apache-2.0 OR MIT`) are valid. `LicenseRef-<id>` for custom licenses is valid.

## Sub-phase 1.3 — Three Mochi-only sub-parsers

### `[capabilities]`

Closed capability set (from research note 10 §1): `fs.read`, `fs.write`, `net.dial`, `net.listen`, `env`, `ffi`, `clock`, `random`, `proc.spawn`.

```go
var allowedCaps = map[string]struct{}{
    "fs.read": {}, "fs.write": {}, "net.dial": {}, "net.listen": {},
    "env": {}, "ffi": {}, "clock": {}, "random": {}, "proc.spawn": {},
}

func validateCapabilities(c Capabilities) error {
    for _, cap := range append(append([]string(nil), c.Required...), c.Optional...) {
        if _, ok := allowedCaps[cap]; !ok {
            return fmt.Errorf("%w: %q", ErrManifestE007, cap)
        }
    }
    return nil
}
```

### `[targets]`

Closed target set (from research note 11): `c`, `beam`, `jvm`, `dotnet`, `swift`, `kotlin`, `python`, `typescript`, `rust`. Adding a target requires a follow-on MEP.

```go
var allowedTargets = map[string]struct{}{
    "c": {}, "beam": {}, "jvm": {}, "dotnet": {}, "swift": {},
    "kotlin": {}, "python": {}, "typescript": {}, "rust": {},
}
```

Per-target struct:

```go
type TargetSpec struct {
    Entrypoint string            `toml:"entrypoint"`
    FFI        []string          `toml:"ffi,omitempty"`
    Overrides  map[string]string `toml:"overrides,omitempty"`     // src -> override path
    Dependencies map[string]Dep `toml:"dependencies,omitempty"`   // per-target dep overrides
}
```

FFI semantics by target (from research note 11):

- `jvm`: Maven coordinates (`groupId:artifactId:version`).
- `python`: PyPI distribution names with PEP 440 specifiers (`httpx>=0.27,<1`).
- `typescript`: npm package names with semver ranges (`react@^18`).
- `dotnet`: NuGet ids (`Newtonsoft.Json/13.0.3`).
- `swift`: SwiftPM URLs with version.
- `kotlin`: same as JVM.
- `rust`: crates.io names with semver.
- `c`: pkg-config names (`libpng>=1.6`).
- `beam`: Hex names (`jason ~> 1.4`).

### `[provenance]`

Phase 1 only parses; the publish pipeline (Phase 12-13) writes it. The
declarative `[provenance]` table from `mochi.toml` is the `Provenance` type
declared in Phase 0 §0.0 (Publisher, Repository, Workflow, SourceDate). The
signed attestation produced by publish lives in a sibling type so the two
cannot be confused at compile time:

```go
type Attestation struct {
    SigstoreBundle string `toml:"sigstore_bundle,omitempty"`
    OIDCIssuer     string `toml:"oidc_issuer,omitempty"`
    OIDCSubject    string `toml:"oidc_subject,omitempty"`
    BuildTimestamp string `toml:"build_timestamp,omitempty"`
    SourceCommit   string `toml:"source_commit,omitempty"`
    BuildTool      string `toml:"build_tool,omitempty"`
}
```

The parser does not validate these (publish-pipeline rewrites them); validation happens at consumer-side verify (Phase 13).

## Sub-phase 1.4 — Canonical writer

Stability rules:

1. Tables emitted in fixed order: `[package]`, `[dependencies]`, `[dev-dependencies]`, `[features]`, `[targets]`, `[targets.<name>]` (alphabetic by name), `[capabilities]`, `[provenance]`, `[workspace]`, `[workspace.dependencies]`, `[registry]`, `[[registry.alternate]]`.
2. Keys within a table emitted in fixed schema order (see research note 04 §2 for `[package]`). Dependency keys emitted in alphabetic order by dep name.
3. Strings emitted double-quoted. Multi-line strings forbidden (one-line manifest convention).
4. Numbers emitted decimal. Booleans `true` / `false`.
5. Arrays emitted with single space after comma. Inline tables emitted with single space inside braces.
6. LF line endings. Trailing newline.

```go
// pkg/pkgmanifest/write.go
func WriteFile(path string, m *Manifest) error {
    var buf bytes.Buffer
    if err := writeCanonical(&buf, m); err != nil { return err }
    return os.WriteFile(path, buf.Bytes(), 0644)
}
```

The canonical writer is its own implementation, not `toml.NewEncoder`. The encoder emits in struct field order, but Mochi imposes additional rules (e.g. alphabetic dep order) that the round-trip test enforces.

## Sub-phase 1.5 — Workspace inheritance resolution

When a member's manifest has `dep = { workspace = true }`, the parser walks up the directory tree until it finds a manifest with a `[workspace]` table (or a `mochi.workspace.toml` umbrella). It substitutes the entry with the corresponding entry from `[workspace.dependencies]`.

```go
// pkg/pkgmanifest/workspace.go
func ResolveInheritance(m *Manifest, workspaceRoot *Manifest) error {
    for name, dep := range m.Dependencies {
        if dep.Workspace {
            wsDep, ok := workspaceRoot.WorkspaceDependencies[name]
            if !ok {
                return fmt.Errorf("%w: %q", ErrManifestE009, name)
            }
            // copy version, but local fields (optional, features) win
            merged := wsDep
            if len(dep.Features) > 0 { merged.Features = dep.Features }
            if dep.Optional { merged.Optional = true }
            m.Dependencies[name] = merged
        }
    }
    return nil
}
```

The workspace root is discovered by walking parent directories from the manifest path until a `mochi.workspace.toml` or a `mochi.toml` with `[workspace]` is found. Phase 3 owns the discovery algorithm; Phase 1 owns the merge step.

## Sub-phase 1.6 — Fixture corpus

Layout:

```
tests/pkgsystem/manifest/
  positive/
    tiny-lib.toml
    cli-with-ffi.toml
    workspace-root.toml
    workspace-member.toml
    multi-target.toml
    capability-heavy.toml
    golden/                       # parsed-struct JSON for each .toml
      tiny-lib.json
      ...
  negative/
    e001/                         # invalid TOML
      truncated.toml
      bad-quoting.toml
    e002/                         # missing required key
      no-name.toml
      no-version.toml
    ...
    e012/                         # unknown top-level
      stray-table.toml
  semver/
    cases.json                    # [{range, version, matches}] tuples
  workspace/
    root/mochi.workspace.toml
    root/packages/a/mochi.toml
    root/packages/b/mochi.toml
    expected.json
```

The golden corpus uses canonical JSON (sorted keys, no whitespace) for stability across Go versions.

## Sub-phase 1.7 — Edition dispatch

The manifest carries no explicit schema version; the parser dispatches on `package.edition`:

```go
// pkg/pkgmanifest/parse.go
switch m.Package.Edition {
case "2026":
    return parseEdition2026(buf)
default:
    return fmt.Errorf("%w: unknown edition %q", ErrManifestE002, m.Package.Edition)
}
```

Future editions add or remove keys; the parser keeps editions for at least three years per the Mochi compatibility window (see research note 04 §11).

Forward compatibility: unknown sub-keys (inside known tables) emit warnings on the `Manifest.Warnings` slice, not errors. Unknown top-level tables are errors (closed schema at the root).

## Files changed

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkgmanifest/parse.go` | `ParseFile` / `ParseBytes` entry points | Owner |
| `pkg/pkgmanifest/validate.go` | `Validate` with E001-E012 error codes | Owner |
| `pkg/pkgmanifest/semver.go` | Range / Comparator / Version / `ParseRange` / `Matches` | Owner |
| `pkg/pkgmanifest/schema.go` | Closed top-level / target / capability sets | Owner |
| `pkg/pkgmanifest/write.go` | Canonical TOML writer | Owner |
| `pkg/pkgmanifest/workspace.go` | `ResolveInheritance` (extended by Phase 3) | Owner |
| `pkg/pkgmanifest/edition.go` | Edition dispatch | Owner |
| `pkg/pkgmanifest/errors.go` | Sentinel error vars (`ErrManifestE001` etc.) | Owner |
| `tests/pkgsystem/manifest/positive/*.toml` | Round-trip corpus | Owner |
| `tests/pkgsystem/manifest/positive/golden/*.json` | Expected parse output | Owner |
| `tests/pkgsystem/manifest/negative/eNNN/*.toml` | Per-error-code fixtures | Owner |
| `tests/pkgsystem/manifest/semver/cases.json` | Range matching corpus | Owner |
| `tests/pkgsystem/manifest/workspace/*` | Inheritance fixtures | Owner |
| `pkg/pkgmanifest/manifest_test.go` | `TestPhase1Manifest` driver | Owner |

## Test set

- `TestPhase1Manifest` — round-trip and error-code coverage.
- `TestPhase1Semver` — `cases.json` matching.
- `TestPhase1Workspace` — inheritance resolution.
- `TestPhase1Canonical` — emit twice and compare byte-identical.

## Open questions

- Whether to expose the `Manifest` struct directly to the CLI or via a `pkg/pkgmanifest/api` opaque interface; chosen path TBD at sub-phase 1.0 review.
- Whether to accept TOML 1.1 inline-table-newline syntax (rejected by go-toml/v2 today); revisit when the parser library upgrades.
- Whether SPDX validation requires the full expression grammar at v1 or only the simple-id case; current plan is full expression grammar to avoid a migration later.

## Cross-references

- Manifest schema: [research note 04](/docs/research/0057/manifest-format).
- TOML rationale: [research note 02 §1](/docs/research/0057/design-philosophy).
- Capability set: [research note 10 §1](/docs/research/0057/capability-model).
- Target set: [research note 11 §1](/docs/research/0057/polyglot-fanout).
- Workspace shape: [research note 04 §8](/docs/research/0057/manifest-format).
