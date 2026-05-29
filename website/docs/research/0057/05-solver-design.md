---
title: "Solver design: PubGrub-derived conflict-driven backtracking"
description: "PubGrub algorithm walk: incompatibility derivation, unit propagation, decision making, conflict resolution, why output generation, Mochi-specific extensions for capability constraints and target constraints, determinism, performance targets, test corpus."
sidebar_position: 5
---

# 05. Solver design: PubGrub-derived conflict-driven backtracking

**Status**: research note. **Date**: 2026-05-29 (GMT+7).
**Mirrors**: deployed to `/docs/research/0057/solver-design`.

This note specifies the version solver Mochi ships in `pkg/pkgsolver/pubgrub/`. The algorithmic family choice is in [02-design-philosophy](./02-design-philosophy) §2; the comparison with other resolvers is in [03-prior-art-registries](./03-prior-art-registries).

## 1. Why PubGrub, again, with implementation context

A package solver receives a manifest and a registry view; it must produce a single concrete version per resolved package such that every declared range is satisfied. The hard cases are conflicts: when no assignment satisfies every constraint, the solver must report *why*.

PubGrub's distinguishing property is **explanation as a first-class output**. Every backtrack records an "incompatibility" (a set of assignments known to be unsatisfiable), and a chain of incompatibilities leading from the user's root manifest to the conflict is the explanation.

The published reference implementation is Natalie Weizenbaum's Dart `package_resolver` (2018), refined by:

- uv's PubGrub (Rust, 2024).
- Cargo's `pubgrub-rs` (Rust, started 2021, GA path via RFC #3796 2024).
- Pkl's solver (Apple, 2024, Kotlin).

For Mochi we re-implement in Go to keep the static-binary single-CLI shape. The implementation lives at `pkg/pkgsolver/pubgrub/`.

## 2. Algorithm sketch (no jargon)

The solver maintains:

- A *partial solution*: a mapping from package name to a chosen version, accumulated as decisions.
- A list of *incompatibilities*: each incompatibility is a set of `(name, term)` pairs known to be unsatisfiable together.

The loop:

1. *Unit propagation*: walk incompatibilities; if all but one term of an incompatibility is already in the partial solution, the remaining term must be negated. Add its negation to the partial solution as a *derivation* (not a decision).
2. *Decision making*: pick the highest version that is not contradicted by the partial solution.
3. *Conflict resolution*: if a chosen version transitively requires an incompatibility, walk the derivation graph backward to find the responsible *prior decision*, record a new incompatibility that excludes that decision, and backtrack.
4. *Done*: when no unsatisfied incompatibility exists and all required packages have a decision, the partial solution is the resolved tree.

The decision and derivation distinction is what enables "why" output: at any conflict, the derivation graph terminates in a chain of decisions, each of which can be reported as "X was chosen because of dep on Y".

## 3. Term shape

A *term* in PubGrub is a positive or negative *range constraint*:

- Positive: "`@scope/name` must satisfy `^1.2`".
- Negative: "`@scope/name` must not satisfy `^1.5`".

Ranges are intervals on the semver number line, with prerelease handling. Mochi's range type is:

```go
type Range struct {
    Lower    *Version  // nil = open at -inf
    LowerInc bool
    Upper    *Version  // nil = open at +inf
    UpperInc bool
    // Pre-release admission is opt-in. Default false: prereleases not selected
    // unless explicitly requested in the constraint.
    AdmitPrerelease bool
}
```

Range arithmetic operations (`intersect`, `union`, `complement`) are implemented on `Range` and are the algebraic primitives PubGrub needs.

## 4. Decision making policy

When the solver must choose a version, it asks the registry for the package's available versions and picks the *highest* version compatible with the partial solution. This matches npm and Cargo behaviour and contrasts with MVS (which picks the lowest).

For pre-release versions: included only if a constraint explicitly mentions a prerelease (e.g. `^1.0.0-rc.1` admits prereleases for that range). Otherwise prereleases are excluded.

Tiebreak: when two versions tie on semver compare, the one with no build metadata wins. (Build metadata is informational per semver 2.0.)

## 5. Mochi-specific extensions to PubGrub

### 5.1 Capability constraints

A Mochi dep can pin a capability subset:

```toml
"@mochi/json" = { version = "^1.2", capabilities = ["fs.read"] }
```

The solver, when considering version `1.2.5` of `@mochi/json`, fetches that version's manifest and checks its `[capabilities] required`. If the required set is not a subset of the consumer's allowed set, the version is rejected as if it violated a range constraint, and a `cap_excluded` incompatibility is recorded:

```
@mochi/json 1.2.5 forbidden by capability constraint
@mochi/json 1.2.4 -> required net.dial; consumer allows {fs.read}; rejected
```

The "why" output surfaces this:

```
Because consumer pins @mochi/json with capabilities=["fs.read"]
  and @mochi/json 1.2.{0..7} all require net.dial,
  no version of @mochi/json in ^1.2 satisfies the constraint.
Fix: add "net.dial" to capabilities, or pin a different version range.
```

This is the supply-chain check that catches "this dep silently grew network access".

### 5.2 Target constraints

Each `[targets]` opt-in restricts the dep set: a package can only be selected if it supports all targets the consumer compiles to. Encoded as a constraint per resolution.

For consumer with `[targets] = ["python", "typescript"]`:

- Candidate `@scope/dep@1.0` declares `[targets] = ["typescript", "jvm"]`. Misses `python`. Rejected with `target_excluded`.
- Candidate `@scope/dep@2.0` declares `[targets] = ["python", "typescript", "jvm"]`. Accepted.

### 5.3 Mochi compiler version constraint

`package.mochi = ">=0.7, <1.0"` is a constraint on the *compiler* version. The solver checks each candidate's `mochi` range against the consumer's compiler version; mismatches are `compiler_excluded`.

### 5.4 Multi-version opt-in

When the solver concludes there is no single-version-per-name solution, it normally fails. If the user has set `[workspace.allow-multi-version] = ["@scope/name"]`, the solver instead splits the package into independent resolution problems, one per semver-major. Each major resolves independently; the final tree has both major versions present.

## 6. Algorithm pseudocode

```
function solve(root_manifest, registry):
    incompats = []
    solution  = {}        # name -> assignment
    next      = root_manifest.dependencies as initial constraints

    add_incompat(NotRoot)   # forces a decision at root

    while True:
        unit_propagate(incompats, solution)
        if no_conflict and all_constraints_satisfied(solution):
            return solution
        if conflict:
            new_incompat = conflict_resolution(solution, incompats)
            if new_incompat == [] (i.e. UNSAT at root):
                return error(explain(new_incompat))
            add_incompat(new_incompat)
            backtrack_to(level where new_incompat unit propagates)
            continue
        pkg = next_undecided(solution, constraints)
        version = pick_highest_compatible(pkg, solution, registry)
        if version == None:
            add_incompat(no_version_available(pkg))
            continue
        decide(pkg, version, solution)
        for dep in fetch_deps(pkg, version, registry):
            add_dep_constraint(dep)
```

`conflict_resolution` is the heart of PubGrub: walk derivation backward to the prior decision, build a new incompatibility, and use it both for backtracking and for the explanation chain.

The full pseudo-code with conflict-resolution details is approximately 200 lines and is given in the Dart `package_resolver` source; our Go implementation tracks it closely.

## 7. "Why" output

`mochi why @scope/name` walks the derivation graph for the solved tree and produces a tree like:

```
@scope/name 1.5.0
└── because @org/thing 2.3.1 → @scope/name ^1.5
    └── because @org/thing 2.3.1 was the highest matching ^2.0 in [dependencies]
```

`mochi lock` on a UNSAT manifest produces:

```
error: cannot find a satisfying version for the manifest

  Because @org/foo 1.0.0 requires @lib/x ^2.0
    and @org/bar 1.5.0 requires @lib/x ^3.0,
    no version of @lib/x satisfies both.

  Because @org/foo is depended on by your manifest
    and @org/bar is depended on by your manifest,
    your manifest cannot be resolved.

Fix options:
  - relax the @org/foo dep range to allow a version that uses @lib/x ^3.x
  - relax the @org/bar dep range to allow a version that uses @lib/x ^2.x
  - allow multiple major versions of @lib/x via:
      [workspace.allow-multi-version]
      members = ["@lib/x"]
```

The "Fix options" block is post-processing on the failed-incompatibility chain. Heuristics: find the leaf packages in the chain (those that have no derived constraint), suggest relaxing their range; suggest multi-version if both incompatibilities target distinct semver-majors.

## 8. Registry interface

The solver consumes a registry interface:

```go
type Registry interface {
    Versions(pkg string) ([]Version, error)
    Manifest(pkg string, version Version) (*Manifest, error)
}
```

`Versions` is a fast (~1 HTTPS GET) call; `Manifest` is per-version. The solver caches both inside its run.

For network resilience: a fetch failure for `Manifest` is treated as an incompatibility ("no metadata for this version, cannot consider"), so the solver does not loop on a flaky registry. The user-facing error includes the underlying transport error.

## 9. Determinism

The solver is deterministic given the same manifest and the same registry snapshot. Determinism is enforced by:

- Ordering: package names are processed in lexicographic order. Within a package, candidate versions are processed in descending semver order.
- Tiebreak rules are total.
- No reliance on iteration order of maps internally; the solver sorts before iterating.

A flaky registry can produce different outputs on different runs; that's why the lockfile is the contract. `mochi lock --check` re-runs the solver against the *exact* registry snapshot recorded in the lock and verifies the result is byte-identical.

## 10. Performance

Mochi's solver is not a hot path; build time dominates. Order-of-magnitude expectations from comparable PubGrub implementations:

- uv (Rust): 50k-package PyPI graph resolves in ~250ms cold (network), ~50ms warm.
- Cargo (Rust, classic SAT): ~100-500ms for 200-package graphs warm.
- Pub (Dart, original): ~200ms for typical Flutter apps.

A Go implementation will be ~2-3x slower than the Rust ones for the algorithm itself; for a 200-package graph we target ~500ms warm, ~2s cold. The lockfile cache renders subsequent runs trivial (~10ms).

Critical-path optimisations (kept for v2 if needed):

- Lazy manifest fetch: only fetch deps when the solver actually considers the version.
- Range intersection caching: memoise across the resolution.
- Parallel registry fetch: pre-fetch deps in the background while the current decision is being made.

v1 ships without these and measures; if 2s cold is acceptable for typical projects, optimisation waits.

## 11. Test corpus

`tests/pkgsystem/solver/` has three families of fixtures:

1. **PubGrub reference fixtures**: ports of the Dart reference test suite. Each fixture is a synthetic registry plus a manifest plus an expected outcome.
2. **Real-world regressions**: import the uv issue tracker's reproducer cases and the Cargo `pubgrub-rs` test set.
3. **Mochi-specific**: capability conflicts, target-set mismatches, workspace inheritance, multi-version splits.

Each fixture runs in under 10ms on CI; the full suite is ~5s.

## 12. Algorithm correctness

PubGrub is *complete* (always terminates) for finite registry snapshots. Termination proof: each iteration either decides a package, derives a constraint, or records a new incompatibility; the partial solution monotonically progresses, and incompatibilities are bounded by the size of the dep graph times the registry view.

Soundness: the partial solution at termination satisfies every constraint by construction (unit propagation enforces this), and incompatibilities only forbid genuinely unsatisfiable subsets.

The standard verification for Mochi is differential: run our Go implementation on the PubGrub reference fixtures and assert byte-identical output to the Dart reference.

## 13. CLI surface

```
mochi lock                # resolve and write lockfile
mochi lock --check        # verify lockfile against manifest, no write
mochi lock --refresh      # force a registry refetch even if cache is warm
mochi tree                # render resolved tree
mochi why <pkg>           # explain why <pkg> is in the tree
mochi add <pkg>[@<req>]   # add a dep + resolve + write lock
mochi remove <pkg>        # drop a dep + resolve + write lock
mochi update [<pkg>]      # bump within semver + write lock
```

`mochi why` is the explanation surface for *successful* resolutions; the solver's "Fix options" block is for *failed* resolutions.

## 14. Cross-references

- Algorithm choice rationale: [02-design-philosophy](./02-design-philosophy) §2.
- Implementations in survey: [03-prior-art-registries](./03-prior-art-registries) §1, §6.
- Manifest schema input: [04-manifest-format](./04-manifest-format).
- Lockfile output: [06-lockfile-format](./06-lockfile-format).
- Registry interface: [07-registry-index](./07-registry-index).
- Capability constraint behaviour: [10-capability-model](./10-capability-model) §5.
- Solver risks (loops, perf): [12-risks-and-alternatives](./12-risks-and-alternatives) §3.
