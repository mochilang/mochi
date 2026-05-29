---
title: "Phase 16. Advisory database + audit"
sidebar_position: 17
sidebar_label: "Phase 16. Advisory + audit"
description: "MEP-57 Phase 16 — RustSec-style YAML advisory feed, severity scoring, EOL flags, `mochi pkg audit` command, `mochi pkg audit fix` automated bumps, lockfile-aware vulnerability scan."
---

# Phase 16. Advisory database + `mochi pkg audit`

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 16](/docs/mep/mep-0057#phase-16-advisory) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase16Audit`: an injected advisory file matching a fixture's lockfile causes `mochi pkg audit` to exit non-zero and name the affected packages with CVSS and EOL flag; `mochi pkg audit fix` proposes the minimal bump that lifts the lockfile out of the affected range.

Pass criteria:

1. Advisory parse. A RustSec-shape YAML advisory parses into `Advisory{ID, Package, Range, Severity, Description, References, Withdrawn, EOL}`.
2. Lockfile cross-check. Every locked package + version is matched against advisories whose range covers the version; matches sorted by `(Severity desc, Package asc)`.
3. Exit code mapping. `mochi pkg audit` returns: 0 = clean; 1 = vulnerability matched; 2 = network / cache failure. `--fail-on=critical` overrides to fail only on `Severity >= Critical`.
4. `mochi pkg audit fix`. Finds the minimal version bump per affected package that exits the advisory range AND satisfies all other resolver constraints; outputs a TOML diff against `mochi.toml`.
5. Advisory feed refresh. `mochi pkg audit` first refreshes the cached feed via ETag conditional GET; offline mode (`--offline` or no network) uses the cached feed and warns about staleness.
6. Workspace ignore. `[workspace.audit] ignore = ["MCHI-2026-0001"]` suppresses a specific advisory (with reason in the suppression source) so CI can pass while a fix lands.
7. EOL warning. A package marked EOL emits a `WARN` (exit 0) unless `--fail-on=eol` is passed.

## Goal-alignment audit

Advisory tooling is the *defensive* surface for consumers, mirroring `cargo audit` and `npm audit`. Without it, the registry is a black box for known vulnerabilities. The user-facing goal moved: "I run `mochi pkg audit` in CI; if any transitive dep has an open advisory my build breaks before it ships".

The decision to use RustSec's YAML shape (research note 12 §A.13) rather than OSV's JSON: RustSec has a curated, low-noise advisory database with strong editorial process; OSV aggregates from many feeds with variable quality. Mochi reuses the schema (so OSV-to-Mochi conversion is mechanical) but mirrors RustSec's editorial discipline.

The `mochi pkg audit fix` automation is the answer to "advisories are noise if nobody acts on them". A proposed bump that the user can review and apply with `mochi pkg lock --upgrade <pkg>` shortens the fix loop from days to minutes.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 16.0 | Advisory schema (YAML, RustSec-shape) with CVSS, ranges, EOL | NOT STARTED | — |
| 16.1 | Advisory feed: sparse HTTPS index at `advisories.mochi.dev` | NOT STARTED | — |
| 16.2 | Local advisory cache and refresh | NOT STARTED | — |
| 16.3 | `mochi pkg audit`: lockfile cross-check, exit code mapping | NOT STARTED | — |
| 16.4 | `mochi pkg audit --json` machine-readable output | NOT STARTED | — |
| 16.5 | `mochi pkg audit fix` minimal-bump suggestion via solver | NOT STARTED | — |
| 16.6 | Advisory ignore list per workspace | NOT STARTED | — |
| 16.7 | EOL flag (unmaintained package) reporting | NOT STARTED | — |
| 16.8 | OSV import path for cross-ecosystem advisories | NOT STARTED | — |

## Sub-phase 16.0 — Advisory schema

```yaml
# advisories/MCHI-2026-0001.yaml
id: MCHI-2026-0001
package: "@mochi/http"
title: "Buffer overflow in HTTP response parser"
description: |
  Versions 1.0.0 through 1.2.5 of @mochi/http parse Content-Length without
  validating against actual body size, allowing a malformed response to
  corrupt the parser's internal buffer.
date: 2026-01-15
severity: high          # info | low | medium | high | critical
cvss: "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H"
affected:
  - range: ">=1.0.0, <1.2.6"
patched:
  - "1.2.6"
unaffected: []
references:
  - url: "https://github.com/mochilang/http/security/advisories/GHSA-..."
  - url: "https://github.com/mochilang/http/commit/abc123"
aliases:
  - "CVE-2026-12345"
  - "GHSA-xxxx-xxxx-xxxx"
keywords: ["dos", "buffer-overflow"]
withdrawn: null
eol: false
```

Parser:

```go
// pkg/pkgadvisory/schema.go
type Advisory struct {
    ID          string
    Package     string
    Title       string
    Description string
    Date        time.Time
    Severity    Severity   // enum
    CVSS        string     // raw vector
    CVSSScore   float64    // derived 0-10
    Affected    []Range
    Patched     []string
    References  []string
    Aliases     []string
    Keywords    []string
    Withdrawn   *time.Time
    EOL         bool
}

type Severity int
const (
    SeverityInfo Severity = iota
    SeverityLow
    SeverityMedium
    SeverityHigh
    SeverityCritical
)

func ParseFile(path string) (*Advisory, error) { /* YAML decode + validate */ }
```

Schema validated against `advisory.schema.json` shipped in repo.

## Sub-phase 16.1 — Advisory feed

Distribution mirrors the package sparse index:

```
https://advisories.mochi.dev/
  index.jsonl                              # one line per advisory: {id, package, date, sha}
  by-id/<id>.yaml                          # individual advisory file
  by-package/<bucket>/<scope>/<name>.jsonl # advisories targeting this package
```

The `index.jsonl` is the authoritative file: a SHA-256 of each advisory file is included so the cache can detect drift.

Fetch:

```go
// pkg/pkgadvisory/feed.go
type Feed struct {
    Endpoint string
    cache    *Cache
}

func (f *Feed) Refresh(ctx context.Context) error {
    /* conditional GET on index.jsonl with ETag */
    /* compare cached SHA to upstream SHA per advisory; fetch only changed */
    /* atomic rename of the cache dir on success */
}

func (f *Feed) AdvisoriesFor(pkg string) ([]Advisory, error) {
    return f.cache.LoadByPackage(pkg)
}
```

Refresh frequency: once per `mochi pkg audit`, with a 24-hour grace period so back-to-back CI runs hit the local cache.

## Sub-phase 16.2 — Cache

Canonical root `$MOCHI_HOME` (see [phase 0
§conventions](./phase-00-skeleton#files-changed)):

```
$MOCHI_HOME/advisories/
  index.jsonl
  by-id/MCHI-2026-0001.yaml
  by-package/<bucket>/<scope>/<name>.jsonl
  .last-refresh
```

`.last-refresh` is a timestamp file; refresh is skipped if newer than 24h unless `--refresh` is passed.

## Sub-phase 16.3 — `mochi pkg audit`

```go
func cmdAudit(c *cli.Context) error {
    if err := feed.Refresh(c.Context); err != nil && !c.Bool("offline") {
        warnf("advisory refresh failed: %v (using cached)", err)
    }
    lock, _ := pkglock.ParseFile("mochi.lock")
    var hits []Hit
    for _, p := range lock.Packages {
        for _, adv := range feed.AdvisoriesFor(p.Name) {
            if adv.Affected.Contains(p.Version) && !ignored(adv.ID) {
                hits = append(hits, Hit{Pkg: p, Advisory: adv})
            }
        }
    }
    sortHits(hits)
    failOn := parseFailOn(c.String("fail-on"))
    return renderAndExit(hits, failOn)
}
```

Render (TTY):

```
Found 2 advisories:

  HIGH    MCHI-2026-0001    @mochi/http 1.2.5    (CVSS 9.1)
    Buffer overflow in HTTP response parser
    Fixed in: 1.2.6
    https://github.com/mochilang/http/security/advisories/GHSA-...

  MEDIUM  MCHI-2026-0008    @mochi/log  0.3.2    (CVSS 5.4)
    Format-string injection in trace output
    Fixed in: 0.3.3, 0.4.0
    https://github.com/mochilang/log/security/advisories/GHSA-...

Run `mochi pkg audit fix` to see suggested bumps.
```

Exit code 1 because at least one Hit at HIGH was found.

## Sub-phase 16.4 — JSON output

```json
{
  "tool": "mochi-audit",
  "version": "0.7.0",
  "lockfile": "mochi.lock",
  "feed_refreshed": "2026-05-29T07:00:01Z",
  "hits": [
    {
      "advisory_id": "MCHI-2026-0001",
      "package": "@mochi/http",
      "version": "1.2.5",
      "severity": "high",
      "cvss_score": 9.1,
      "patched_versions": ["1.2.6"],
      "references": ["https://..."]
    }
  ],
  "exit_code": 1
}
```

Consumable by GitHub Code Scanning (SARIF) via a separate `mochi pkg audit --format=sarif` flag (deferred to v1.1).

## Sub-phase 16.5 — `mochi pkg audit fix`

Runs the solver once per affected package with an additional constraint: the affected version range is *excluded*. The smallest valid version that satisfies the rest of the manifest is the suggestion.

```go
func cmdAuditFix(c *cli.Context) error {
    lock, _ := pkglock.ParseFile("mochi.lock")
    m, _ := pkgmanifest.ParseFile("mochi.toml")
    hits := runAudit(lock)
    var bumps []Bump
    for _, hit := range hits {
        bump := suggestBump(m, lock, hit)
        if bump != nil { bumps = append(bumps, *bump) }
    }
    renderBumps(bumps)
    return nil
}

func suggestBump(m *Manifest, lock *Lockfile, hit Hit) *Bump {
    exclusion := pkgmanifest.RangeExclude(hit.Advisory.Affected)
    sol, err := pkgsolver.SolveWithExclusion(m, lock, hit.Pkg.Name, exclusion)
    if err != nil { return nil }
    newVersion := sol.PackageVersion(hit.Pkg.Name)
    return &Bump{
        Pkg: hit.Pkg.Name, From: hit.Pkg.Version, To: newVersion,
        ManifestDiff: computeDiff(m, hit.Pkg.Name, newVersion),
    }
}
```

Output:

```
Suggested fixes:

  @mochi/http: 1.2.5 -> 1.2.6
    [dependencies]
    - "@mochi/http" = "^1.0"
    + "@mochi/http" = "^1.2.6"

  @mochi/log: 0.3.2 -> 0.3.3
    [dependencies]
    - "@mochi/log"  = "^0.3"
    + "@mochi/log"  = "^0.3.3"

Apply with: mochi pkg lock --upgrade @mochi/http --upgrade @mochi/log
```

## Sub-phase 16.6 — Ignore list

`mochi.toml` (or workspace root):

```toml
[workspace.audit]
ignore = ["MCHI-2026-0001"]
ignore-reasons = {"MCHI-2026-0001" = "Patched upstream PR #234 lands 2026-06"}
```

A bare ignore without a reason emits a warning (`audit ignore lacks reason`). The reason is purely for human reviewers; the audit tool does not parse it.

## Sub-phase 16.7 — EOL flag

A package's manifest may declare:

```toml
[package]
maintenance = "eol"        # or "active" | "passive"
maintenance-since = 2025-12-31
```

EOL advisories use the `eol: true` flag:

```yaml
id: MCHI-EOL-2025-0042
package: "@mochi/oldlib"
date: 2025-12-31
eol: true
description: "Package unmaintained; upstream archived."
```

Reported in audit with `EOL` severity badge; default exit code 0 unless `--fail-on=eol`.

## Sub-phase 16.8 — OSV import

A periodic job in the advisory repo imports advisories from OSV.dev for any package that also exists in the polyglot ecosystems (e.g., a CVE filed against the npm-published `@mochi/strings`). The import script maps OSV ranges to Mochi ranges; the resulting advisory carries `aliases: [GHSA-...]` so dedup works.

Implementation lives outside the Mochi binary (it is an external Go script that pushes YAML files to `advisories.mochi.dev`). Mochi's client just reads the YAML.

## Files changed

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkgadvisory/schema.go` | YAML schema + parser (extended by Phase 18 offline snapshot) | Owner |
| `pkg/pkgadvisory/severity.go` | CVSS parsing | Owner |
| `pkg/pkgadvisory/feed.go` | Sparse-index style feed client | Owner |
| `pkg/pkgadvisory/cache.go` | Local cache | Owner |
| `pkg/pkgadvisory/match.go` | Range match against lockfile | Owner |
| `pkg/pkgadvisory/audit.go` | Audit core | Owner |
| `pkg/pkgadvisory/fix.go` | Bump suggestion via solver | Owner |
| `cmd/mochi/audit.go` | `mochi pkg audit` advisory subcommand | Extends (Phase 9) |
| `tests/pkgsystem/audit/cve-hit/*` | Match a known advisory | Owner |
| `tests/pkgsystem/audit/cve-fix/*` | Bump suggestion | Owner |
| `tests/pkgsystem/audit/eol/*` | EOL warning | Owner |
| `tests/pkgsystem/audit/ignore/*` | Ignore list suppresses | Owner |
| `tests/pkgsystem/audit/offline/*` | Offline mode uses cache | Owner |

## Error code surface

| Code | Trigger |
|------|---------|
| `M057_ADV_E001` | Vulnerability found at or above `--fail-on` threshold (exit code 1). |
| `M057_ADV_E002` | Advisory feed refresh failed AND no cache. |
| `M057_ADV_E003` | Advisory YAML schema invalid. |
| `M057_ADV_E004` | `[workspace.audit] ignore` references unknown ID. |

CVSS-vector parse failure surfaces as `M057_ADV_E003` (schema invalid)
since the CVSS string is a schema field; phase 16 does not declare a
separate code. See the [error registry](./errors) for owners.

## Test set

- `TestPhase16Parse` — YAML parses.
- `TestPhase16RangeMatch` — locked 1.2.5 matched by `>=1.0.0, <1.2.6`.
- `TestPhase16Audit` — hits sorted by severity.
- `TestPhase16FailOn` — `--fail-on=critical` ignores high.
- `TestPhase16Fix` — bump suggestion satisfies other constraints.
- `TestPhase16Offline` — cache used; warning emitted.
- `TestPhase16Ignore` — suppressed advisory does not fail.
- `TestPhase16JSON` — `--json` output validates against schema.
- `TestPhase16EOL` — EOL flag rendered, exit 0 by default.

## Open questions

- Whether to support per-target advisories (e.g., a CVE that only affects the npm-target output but not the python-target output); current plan: yes, optional `targets: ["typescript"]` field on the advisory.
- Whether to integrate with GitHub Security Advisories directly (so issuance is one PR to the advisory repo + auto-create GH advisory); deferred to v1.1.
- Whether `audit fix` should auto-apply (no review); current plan: no, always print the diff; user runs `mochi pkg lock --upgrade`.

## Cross-references

- Advisory model rationale: [research note 12 §A.13](/docs/research/0057/risks-and-alternatives).
- RustSec inheritance: [research note 03 §1](/docs/research/0057/prior-art-registries).
- Solver `SolveWithExclusion`: [phase 5 §5.7](./phase-05-pubgrub).
- Lockfile cross-check: [phase 4 §4.5](./phase-04-lockfile).
