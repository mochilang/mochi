---
title: "Phase 5. PubGrub solver core"
sidebar_position: 6
sidebar_label: "Phase 5. PubGrub solver core"
description: "MEP-57 Phase 5 — PubGrub algorithm: incompatibility derivation, unit propagation, decision making, conflict-driven backtracking. Go port of the published Dart reference passing the standard PubGrub fixture corpus."
---

# Phase 5. PubGrub solver core

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 5](/docs/mep/mep-0057#phase-5-pubgrub-solver) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase5PubgrubCore`: solver produces byte-identical output to the Dart `package_resolver` reference on the published PubGrub fixture suite. Solver terminates within 60s wall-clock on every fixture; pathological fixtures abort with `M057_SOLVER_E001`.

Pass criteria:

1. Dart reference parity. Every fixture under `tests/pkgsystem/solver/dart-reference/` resolves to the same `(name, version)` set as the reference. The comparison is over the sorted resolved tree; the order of internal decisions does not need to match, only the final partial solution.
2. uv regression parity. Every fixture under `tests/pkgsystem/solver/uv-regression/` (ported from the uv issue tracker) resolves correctly. These are real-world cases that broke earlier solvers.
3. Mochi-specific cases. Capability constraints, target constraints, compiler version constraints, and workspace inheritance all behave per the cases under `tests/pkgsystem/solver/mochi-specific/`. Multi-version opt-in is exercised by the `allow-multi-version/` sub-corpus.
4. Termination guarantee. Pathological fixtures (designed to exercise the worst case) abort within the 60s wall-clock budget with `M057_SOLVER_E001`. The harness uses `runtime.SetCPUProfileRate` to detect spin-loops.
5. Determinism. Each fixture runs three times with `mochi pkg lock` against the same mock registry snapshot and produces the same lockfile bytes (research note 05 §9).

## Goal-alignment audit

The solver is the algorithmic core. Without correct PubGrub, every other phase produces a wrong tree. The user-facing goal moved: "When my dep ranges conflict, `mochi pkg lock` tells me exactly which two packages disagree, in human-readable terms".

PubGrub's load-bearing property over MVS / SAT / ASP is that conflicts are explanations, not opaque UNSAT verdicts (research note 05 §1). The Phase 5 acceptance bar specifically excludes UX (left to Phase 6) but includes the data structures that make Phase 6 possible: derivation graph, decision stack, incompatibility list.

Negative scope: Phase 5 does not generate the human-readable explanation; that is Phase 6. Phase 5 just ensures the *internals* are correctly structured (the derivation graph is walkable backwards, incompatibilities carry causes, decisions carry the responsible incompatibility).

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 5.0 | `Range` type + `intersect`, `union`, `complement` algebra | NOT STARTED | — |
| 5.1 | `Term` (positive / negative range constraints) | NOT STARTED | — |
| 5.2 | `Incompatibility` data structure + storage | NOT STARTED | — |
| 5.3 | Unit propagation loop | NOT STARTED | — |
| 5.4 | Decision making (highest-compatible-version) | NOT STARTED | — |
| 5.5 | Conflict resolution (backward derivation walk) | NOT STARTED | — |
| 5.6 | Backtracking | NOT STARTED | — |
| 5.7 | Mock registry interface for fixtures | NOT STARTED | — |
| 5.8 | Dart reference fixture port | NOT STARTED | — |
| 5.9 | uv issue tracker regression fixture port | NOT STARTED | — |
| 5.10 | Mochi extensions: capability / target / compiler / multi-version | NOT STARTED | — |
| 5.11 | Termination watchdog (60s wall-clock + spin detection) | NOT STARTED | — |

## Sub-phase 5.0 — Range algebra

From research note 05 §3:

```go
// pkg/pkgsolver/pubgrub/range.go
package pubgrub

type Range struct {
    Lower           *pkgmanifest.Version
    LowerInc        bool
    Upper           *pkgmanifest.Version
    UpperInc        bool
    AdmitPrerelease bool
}

var RangeAny = Range{}
var RangeEmpty = Range{Lower: zeroVersion, Upper: zeroVersion, LowerInc: false, UpperInc: false}

func (r Range) Contains(v pkgmanifest.Version) bool { /* ... */ }
func (r Range) IsEmpty() bool { /* ... */ }

func (r Range) Intersect(o Range) Range { /* lower = max(lows), upper = min(highs) */ }
func (r Range) Union(o Range) Range     { /* requires overlap or adjacency; else returns ambiguous */ }
func (r Range) Complement() []Range     { /* up to two ranges: (-inf, lower) and (upper, +inf) */ }
```

The algebra must handle:

- Open vs closed bounds (`>=` vs `>`).
- nil bounds (open at infinity).
- Prerelease admission flag (research note 05 §4): default false, prereleases excluded unless a constraint explicitly mentions one.
- Empty range (no versions match).

Property test (via `go test -fuzz`): for random `Range` pairs A, B and a random `Version` V: `A.Contains(V) && B.Contains(V) == A.Intersect(B).Contains(V)`. Same for `Union` (when defined) and `Complement`.

## Sub-phase 5.1 — Term

A term is a (PackageKey, Range) pair, with a `Positive` bool indicating whether the term asserts that constraint or its negation:

```go
type Term struct {
    Pkg      PackageKey
    Range    Range
    Positive bool
}

type PackageKey struct {
    Scope string // empty for unscoped
    Name  string
}

func (t Term) Negate() Term { return Term{t.Pkg, t.Range, !t.Positive} }
func (t Term) Satisfies(assignment Assignment) bool { /* ... */ }
```

Three relations between two terms over the same package:

- *Satisfies*: T1 satisfies T2 if every version satisfying T1 also satisfies T2.
- *Contradicts*: T1 contradicts T2 if no version satisfies both.
- *Almost*: neither; useful for the propagation loop.

## Sub-phase 5.2 — Incompatibility

An incompatibility is a set of Terms known to be unsatisfiable jointly:

```go
type Incompatibility struct {
    Terms []Term
    Cause Cause
}

type Cause interface{ isCause() }

type CauseRoot struct{}                    // user manifest
type CauseDep struct{ Pkg PackageKey; Version pkgmanifest.Version }
type CauseConflict struct{ Cause1, Cause2 *Incompatibility }
type CauseDeriv struct{ From *Incompatibility }
type CauseCapability struct {
    Pkg      PackageKey
    Version  pkgmanifest.Version
    Required []string  // candidate's `[capabilities].required`
    Allowed  []string  // consumer's pin (DepPin().Capabilities)
    Extra    []string  // Required minus Allowed, pre-computed for Phase 6.6
}
type CauseTarget struct{ Pkg PackageKey; Missing []string }
type CauseCompiler struct{ Pkg PackageKey; Want string; Have string }
```

`CauseConflict` is the chain that Phase 6 walks to produce human-readable explanations.

## Sub-phase 5.3 — Unit propagation

Research note 05 §2:

```go
func (s *Solver) propagate(changed PackageKey) error {
    queue := []PackageKey{changed}
    for len(queue) > 0 {
        pkg := pop(&queue)
        for _, inc := range s.incompatsByPackage(pkg) {
            switch s.relation(inc) {
            case AlmostSatisfied:
                term := inc.Terms[indexOfAlmost(inc)]
                s.derive(term.Negate(), inc)        // add to partial solution
                queue = append(queue, term.Pkg)
            case Satisfied:
                // conflict
                return s.conflict(inc)
            case Contradicted:
                // not applicable, skip
            }
        }
    }
    return nil
}
```

The propagation loop iterates until no further derivation is possible or a conflict surfaces.

## Sub-phase 5.4 — Decision making

```go
func (s *Solver) decide() (PackageKey, bool, error) {
    pkg := s.nextUndecidedPackage()  // lexicographic order
    if pkg == nil { return PackageKey{}, true /*done*/, nil }
    versions, err := s.registry.Versions(pkg.String())
    if err != nil { return *pkg, false, err }
    sort.Sort(sort.Reverse(byVersion(versions)))
    constraint := s.constraintFor(pkg)
    for _, v := range versions {
        if !constraint.Contains(v) { continue }
        if !s.passesMochiExtensions(pkg, v) { continue }
        s.assign(Decision{Pkg: *pkg, Version: v})
        manifest, _ := s.registry.Manifest(pkg.String(), v)
        for name, dep := range manifest.Dependencies {
            s.addIncompat(depIncompat(*pkg, v, name, dep))
        }
        return *pkg, false, nil
    }
    // no version satisfies; add incompatibility
    s.addIncompat(noVersionIncompat(*pkg, constraint))
    return *pkg, false, nil
}
```

Decision policy (research note 05 §4): highest version that satisfies the partial solution's constraint. Tiebreak: no-build-metadata wins.

Prerelease admission: only if any constraint explicitly mentions a prerelease (e.g. `^1.0.0-rc.1`).

## Sub-phase 5.5 — Conflict resolution

The heart of PubGrub. From research note 05 §6:

```go
func (s *Solver) conflictResolution(inc *Incompatibility) (*Incompatibility, int, error) {
    for {
        // Walk backwards along the derivation graph
        mostRecent, prev := s.findMostRecentSatisfier(inc)
        if mostRecent.IsDecision() {
            // backtrack to before this decision
            return inc, prev.Level, nil
        }
        // mostRecent is a derivation; build a new incompat by resolving inc with mostRecent's cause
        inc = resolveIncompats(inc, mostRecent.Cause)
        if isRoot(inc) {
            return inc, 0, ErrUnsat
        }
    }
}
```

The output is a new incompatibility and the decision level to backtrack to. The new incompatibility is added to the global list and will unit-propagate at the prior level.

## Sub-phase 5.6 — Backtracking

```go
func (s *Solver) backtrackTo(level int) {
    for s.currentLevel > level {
        s.popAssignment()
    }
}
```

Assignments above `level` are discarded; the partial solution shrinks. Incompatibilities are never discarded (they accumulate; that's how the solver makes progress).

## Sub-phase 5.7 — Mock registry

Test fixtures use a mock registry that reads from a JSON file:

```go
// pkg/pkgsolver/pubgrub/mockregistry/mock.go
type MockRegistry struct {
    Packages map[string]MockPackage
}

type MockPackage struct {
    Versions  []pkgmanifest.Version
    Manifests map[string]*pkgmanifest.Manifest // version -> manifest
}

func LoadMockRegistry(path string) (*MockRegistry, error) { /* ... */ }
func (r *MockRegistry) Versions(pkg string) ([]pkgmanifest.Version, error) { /* ... */ }
func (r *MockRegistry) Manifest(pkg string, v pkgmanifest.Version) (*pkgmanifest.Manifest, error) { /* ... */ }
```

JSON layout (one entry per package):

```json
{
  "@mochi/json": {
    "versions": ["1.0.0", "1.1.0", "1.2.0", "1.2.5"],
    "manifests": {
      "1.2.5": {
        "package": {"name": "@mochi/json", "version": "1.2.5", ...},
        "dependencies": {"@mochi/strings": "^0.4"}
      }
    }
  }
}
```

## Sub-phase 5.8 — Dart reference fixture port

The Dart `package_resolver` ships a fixture suite (under `test/version_solver_test.dart`). Each fixture is a synthetic registry + manifest + expected outcome. Port them to `tests/pkgsystem/solver/dart-reference/<case-name>/`:

```
tests/pkgsystem/solver/dart-reference/
  no-conflicts/
    manifest.toml
    registry.json
    expected.json
  simple-conflict/
    manifest.toml
    registry.json
    expected.json
  ...
```

Categories from the Dart suite (50+ cases): no-conflicts, simple-conflicts, transitive-conflicts, backjumping, prerelease, optional-dependencies, dev-only.

Each fixture asserts the final resolved tree (sorted by name) matches `expected.json`.

## Sub-phase 5.9 — uv regression fixture port

uv's issue tracker has dozens of reproducers for solver bugs (loops, wrong backtracking, prerelease admission). Port at least 20 of the most-cited cases to `tests/pkgsystem/solver/uv-regression/<issue-N>/`:

- Issue #1234: prerelease admission with multiple constraints.
- Issue #2345: backtracking past optional deps.
- Issue #3456: feature graph expansion before propagation.

Each fixture cites the upstream issue number in a `README.md` header.

## Sub-phase 5.10 — Mochi-specific extensions

### Capability constraints (research note 05 §5.1)

```go
func (s *Solver) passesCapabilityCheck(pkg PackageKey, v pkgmanifest.Version) bool {
    m, _ := s.registry.Manifest(pkg.String(), v)
    pinned := s.rootCapabilityPin(pkg)
    if pinned == nil { return true }
    for _, cap := range m.Capabilities.Required {
        if !contains(pinned, cap) {
            s.addIncompat(&Incompatibility{
                Terms: []Term{{Pkg: pkg, Range: pointRange(v), Positive: true}},
                Cause: CauseCapability{
                    Pkg: pkg, Version: v,
                    Required: m.Capabilities.Required,
                    Allowed:  pinned,
                    Extra:    diff(m.Capabilities.Required, pinned),
                },
            })
            return false
        }
    }
    return true
}
```

### Target constraints (research note 05 §5.2)

For consumer with `[targets] = ["python", "typescript"]`, a candidate version is rejected if its `[targets]` set is not a superset. The incompatibility carries `CauseTarget` for Phase 6 to explain.

### Compiler version constraint (research note 05 §5.3)

A candidate's `package.mochi` range is checked against the running compiler version. Mismatches add `CauseCompiler` incompatibilities.

### Multi-version opt-in (research note 05 §5.4)

When `[workspace.allow-multi-version] = ["@scope/name"]`:

```go
func (s *Solver) splitForMultiVersion(pkg PackageKey) []PackageKey {
    if !s.allowsMultiVersion(pkg) { return []PackageKey{pkg} }
    return []PackageKey{
        {Scope: pkg.Scope, Name: pkg.Name + "#1"},  // major 1
        {Scope: pkg.Scope, Name: pkg.Name + "#2"},  // major 2
    }
}
```

Each major resolves independently. The lockfile records both entries.

## Sub-phase 5.11 — Termination watchdog

```go
func (s *Solver) Solve(ctx context.Context) (*Solution, error) {
    deadline := time.Now().Add(60 * time.Second)
    iterations := 0
    for {
        if time.Now().After(deadline) {
            return nil, fmt.Errorf("%w: exceeded 60s wall-clock", ErrSolverE001)
        }
        if iterations++; iterations > 1_000_000 {
            return nil, fmt.Errorf("%w: exceeded 1M iterations (likely a loop)", ErrSolverE001)
        }
        // propagate / decide / conflict / backtrack
    }
}
```

PubGrub's correctness guarantee says it always terminates on finite registries; the watchdog guards against bugs in our implementation, not against the algorithm.

## Files changed

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkgsolver/pubgrub/range.go` | `Range` and algebra | Owner |
| `pkg/pkgsolver/pubgrub/term.go` | `Term` | Owner |
| `pkg/pkgsolver/pubgrub/incompat.go` | `Incompatibility` + causes (extended by Phase 10) | Owner |
| `pkg/pkgsolver/pubgrub/propagate.go` | Unit propagation | Owner |
| `pkg/pkgsolver/pubgrub/decide.go` | Decision making | Owner |
| `pkg/pkgsolver/pubgrub/conflict.go` | Conflict resolution | Owner |
| `pkg/pkgsolver/pubgrub/backtrack.go` | Backtracking | Owner |
| `pkg/pkgsolver/pubgrub/mochi_ext.go` | Capability / target / compiler / multi-version | Owner |
| `pkg/pkgsolver/pubgrub/solver.go` | `Solve(ctx)` entry point + watchdog | Owner |
| `pkg/pkgsolver/pubgrub/mockregistry/mock.go` | Mock registry | Owner |
| `tests/pkgsystem/solver/dart-reference/*` | Dart suite port | Owner |
| `tests/pkgsystem/solver/uv-regression/*` | uv issue suite port | Owner |
| `tests/pkgsystem/solver/mochi-specific/*` | Capability / target / compiler / multi-version | Owner |

## Error code surface

Sources (see [error registry](./errors)). Verbal aliases from early
drafts are renamed to the canonical `M057_SOLVER_E<NNN>` form:

| Code | Trigger |
|------|---------|
| `M057_SOLVER_E001` | Solver exceeded wall-clock or iteration limit. |
| `M057_SOLVER_E002` | No solution exists. Phase 6 produces the explanation. |
| `M057_SOLVER_E003` | Candidate version requires capabilities not in consumer pin. |
| `M057_SOLVER_E004` | Candidate version's target set is not a superset. |
| `M057_SOLVER_E005` | Candidate version's `mochi` range excludes running compiler. |

## Test set

- `TestPhase5DartReference` — Dart parity.
- `TestPhase5UvRegression` — uv parity.
- `TestPhase5MochiSpecific` — extensions.
- `TestPhase5Range` — algebra fuzz.
- `TestPhase5Determinism` — three runs produce same lockfile.
- `TestPhase5Termination` — 60s budget enforced.

## Performance targets

From research note 05 §10:

- 200-package graph warm: ~500ms.
- 200-package graph cold (with mock registry latency): ~2s.
- Subsequent `mochi pkg lock --check`: ~10ms (cache hit).

Critical-path optimisations deferred to v2 if v1 targets are met:

- Lazy manifest fetch.
- Range intersection caching.
- Parallel registry fetch.

## Open questions

- Whether to expose the iteration count via `--debug-solver`; current plan: yes, gated behind `MOCHI_DEBUG=1`.
- Whether the multi-version split should produce distinct package keys (`@scope/name#1`) or carry the major in the cause chain only; current plan: distinct keys, simpler downstream.

## Cross-references

- Algorithm walk: [research note 05](/docs/research/0057/solver-design).
- Rationale: [research note 02 §2](/docs/research/0057/design-philosophy).
- Capability constraint behaviour: [research note 10 §5](/docs/research/0057/capability-model).
- Lockfile output: [research note 06](/docs/research/0057/lockfile-format).
- Solver risks (loops, perf): [research note 12 §3](/docs/research/0057/risks-and-alternatives).
