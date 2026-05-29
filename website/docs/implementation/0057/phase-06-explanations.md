---
title: "Phase 6. Solver explanations + mochi pkg why"
sidebar_position: 7
sidebar_label: "Phase 6. Solver explanations"
description: "MEP-57 Phase 6 — PubGrub conflict explanation generation, `mochi pkg why <pkg>` derivation tree walk, `Fix options` post-processor surfacing relax-this-range hints."
---

# Phase 6. Solver explanations + `mochi pkg why`

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 6](/docs/mep/mep-0057#phase-6-explanations) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase6Explanations`: every UNSAT fixture in `tests/pkgsystem/solver-unsat/` produces an explanation whose top three lines match the golden, plus at least one actionable `Fix options:` line. Every SAT fixture's `mochi pkg why <pkg>` output matches its golden tree.

Pass criteria:

1. UNSAT prose. For every fixture under `tests/pkgsystem/solver-unsat/<name>/`, the first three lines of the explanation match `expected-explanation.txt`'s first three lines. The full explanation does not have to be byte-identical (formatting tweaks are allowed), but each line in the golden must appear somewhere in the actual output.
2. Fix options. At least one `Fix options:` line is emitted. The fixture set explicitly covers: relax-leaf-range, allow-multi-version, swap-target, add-capability.
3. `mochi pkg why <pkg>` output. For SAT fixtures, the tree printed by `mochi pkg why <pkg>` matches `expected-why.txt` byte-for-byte (the tree is deterministic).
4. `mochi pkg tree` output. For SAT fixtures, `mochi pkg tree` matches `expected-tree.txt`.
5. Stability. Run each fixture three times; outputs must be byte-identical (no maps iterated, no time stamps embedded).

## Goal-alignment audit

Explanations are the load-bearing user value of PubGrub over SAT. Without them, the solver is just a faster opaque resolver. The user-facing goal moved: "When `mochi pkg lock` fails, the message tells me which two of my deps disagree and how to relax".

Phase 5 built the derivation graph; Phase 6 walks it and renders prose. The derivation graph carries `Cause` records (`CauseRoot`, `CauseDep`, `CauseConflict`, `CauseCapability`, `CauseTarget`, `CauseCompiler`); the renderer dispatches on the cause kind.

The "Fix options" post-processor is the difference between a frustrated user closing the terminal and a successful relax-or-retry. It is a heuristic over the unsat chain: find leaf packages (no derived constraint), suggest relaxing them; detect distinct-semver-major chains, suggest multi-version.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 6.0 | Derivation graph capture during conflict resolution | NOT STARTED | — |
| 6.1 | UNSAT explanation: chain of incompatibilities to human prose | NOT STARTED | — |
| 6.2 | `Fix options:` post-processor (relax leaf ranges, suggest multi-version) | NOT STARTED | — |
| 6.3 | `mochi pkg why <pkg>` for SAT cases: walk the dep tree | NOT STARTED | — |
| 6.4 | `mochi pkg tree` rendering | NOT STARTED | — |
| 6.5 | Output stability: same input -> byte-identical explanation | NOT STARTED | — |
| 6.6 | Cause-specific renderers (capability, target, compiler) | NOT STARTED | — |
| 6.7 | TTY color + plain-text mode | NOT STARTED | — |

## Sub-phase 6.0 — Derivation graph capture

Phase 5 records each `Incompatibility.Cause`. Phase 6 walks them in reverse:

```go
// pkg/pkgsolver/pubgrub/explain.go
type ExplanationStep struct {
    Incompat *Incompatibility
    Prose    string
    Children []*ExplanationStep
}

func BuildExplanation(rootIncompat *Incompatibility) *ExplanationStep {
    return buildStep(rootIncompat, map[*Incompatibility]*ExplanationStep{})
}

func buildStep(inc *Incompatibility, memo map[*Incompatibility]*ExplanationStep) *ExplanationStep {
    if step, ok := memo[inc]; ok { return step }
    step := &ExplanationStep{Incompat: inc, Prose: renderCause(inc)}
    memo[inc] = step
    switch c := inc.Cause.(type) {
    case CauseConflict:
        step.Children = []*ExplanationStep{
            buildStep(c.Cause1, memo),
            buildStep(c.Cause2, memo),
        }
    }
    return step
}
```

Memoisation prevents quadratic blowup; large dep graphs can produce many incompatibilities derived from the same intermediate one.

## Sub-phase 6.1 — UNSAT prose

The renderer walks the derivation tree depth-first and emits one paragraph per incompatibility. Each paragraph follows the "Because A and B, C" template (research note 05 §7):

```
Because @org/foo 1.0.0 requires @lib/x ^2.0
  and @org/bar 1.5.0 requires @lib/x ^3.0,
  no version of @lib/x satisfies both.

Because @org/foo is depended on by your manifest
  and @org/bar is depended on by your manifest,
  your manifest cannot be resolved.
```

Renderer (pseudocode):

```go
func RenderUnsat(step *ExplanationStep, w *bytes.Buffer) {
    if len(step.Children) == 0 {
        fmt.Fprintf(w, "%s\n\n", step.Prose)
        return
    }
    for _, c := range step.Children {
        RenderUnsat(c, w)
    }
    fmt.Fprintf(w, "Because %s\n  and %s,\n  %s.\n\n",
        step.Children[0].Prose, step.Children[1].Prose, step.Prose)
}
```

Prose for the most common causes:

| Cause | Prose template |
|-------|----------------|
| `CauseRoot` | "your manifest depends on `<pkg>`" |
| `CauseDep` | "`<parent>` `<version>` requires `<dep>` `<range>`" |
| `CauseCapability` | "consumer pins `<pkg>` with capabilities=`<allowed>`, but version requires `<required>`" |
| `CauseTarget` | "consumer compiles to `<targets>`, but `<pkg>` `<version>` supports only `<supported>`" |
| `CauseCompiler` | "this mochi is `<have>`, but `<pkg>` `<version>` requires `<want>`" |
| `CauseConflict` | "no version of `<pkg>` satisfies both" |

## Sub-phase 6.2 — Fix options post-processor

After the prose, append a "Fix options:" section. Heuristics over the UNSAT chain:

```go
type FixOption struct {
    Kind   string  // "relax-range", "allow-multi-version", "swap-target", "add-capability"
    Hint   string  // human-readable hint
    Diff   string  // suggested manifest diff (where applicable)
}

func ComputeFixOptions(chain []*Incompatibility) []FixOption {
    var out []FixOption
    out = append(out, suggestRelaxLeafRanges(chain)...)
    out = append(out, suggestMultiVersion(chain)...)
    out = append(out, suggestSwapTarget(chain)...)
    out = append(out, suggestAddCapability(chain)...)
    sort.SliceStable(out, func(i, j int) bool { return out[i].Kind < out[j].Kind })
    return out
}
```

### relax-range

Find leaf packages in the chain (those carrying `CauseRoot`). Their range is the consumer's input; relaxing it might unblock. Suggested diff:

```
Fix option: relax the @org/foo dep range to allow a version that uses @lib/x ^3.x

  [dependencies]
  - "@org/foo" = "^1.0"
  + "@org/foo" = "^1.0, <1.2"   # or "^2"
```

### allow-multi-version

Detect chains where two incompatibilities target distinct semver-majors of the same package. Suggest:

```
Fix option: allow multiple major versions of @lib/x via:

  [workspace.allow-multi-version]
  members = ["@lib/x"]
```

### swap-target

If the chain terminates in `CauseTarget`, suggest removing the offending target:

```
Fix option: drop the python target if you don't need it:

  [targets]
  - python     = { entrypoint = "src/main.mochi" }
```

### add-capability

If the chain terminates in `CauseCapability`, suggest adding the missing cap to the consumer pin:

```
Fix option: add "net.dial" to the capability pin for @mochi/json:

  [dependencies]
  - "@mochi/json" = { version = "^1.2", capabilities = ["fs.read"] }
  + "@mochi/json" = { version = "^1.2", capabilities = ["fs.read", "net.dial"] }
```

## Sub-phase 6.3 — `mochi pkg why <pkg>`

For SAT cases, `mochi pkg why @scope/name` walks the resolved tree:

```go
func PrintWhy(lock *pkglock.Lockfile, target PackageKey) (string, error) {
    var buf bytes.Buffer
    fmt.Fprintf(&buf, "%s %s\n", target.String(), lock.Lookup(target).Version)
    paths := findIncomingPaths(lock, target)  // each path goes from root to target
    for i, p := range paths {
        renderWhyPath(&buf, p, i == len(paths)-1)
    }
    return buf.String(), nil
}
```

Sample output:

```
@scope/name 1.5.0
└── because @org/thing 2.3.1 -> @scope/name ^1.5
    └── because your manifest [dependencies] -> @org/thing ^2.0
```

Multiple paths (diamond dep) are rendered as separate sub-trees under the same root.

## Sub-phase 6.4 — `mochi pkg tree` rendering

Walk the lockfile, render as an ASCII tree:

```
my-app 0.1.0
├── @mochi/json 1.2.5
│   └── @mochi/strings 0.4.7
└── @mochi/strings 0.4.7
```

The tree is deterministic: alphabetic by package name. Repeated sub-trees are collapsed and annotated:

```
my-app 0.1.0
├── @mochi/json 1.2.5
│   └── @mochi/strings 0.4.7
└── @mochi/strings 0.4.7  (*)
(*) shown above
```

Flags:

- `--depth N` truncates after N levels.
- `--target=python` shows only the per-platform subset.
- `--invert` flips the direction (consumers of a given package).

## Sub-phase 6.5 — Output stability

Three runs of `mochi pkg lock` against the same fixture produce byte-identical explanations. Required because:

- The fixture suite asserts byte-identity.
- Users want to grep / diff explanations across CI runs.

Sources of non-determinism eliminated:

- Map iteration order: convert to sorted slice before iterating.
- Time stamps: explanations carry no timestamp.
- Random tiebreaks: there are none; tiebreaks are total.

## Sub-phase 6.6 — Cause-specific renderers

The capability cause has its own renderer because the heuristic for "add this cap" needs the cap name and the package:

```go
func renderCauseCapability(c CauseCapability) string {
    return fmt.Sprintf(
        "consumer pins %s with capabilities=%v, but %s %s requires %v",
        c.Pkg, c.Allowed, c.Pkg, c.Version, c.Required,
    )
}
```

Same for target and compiler:

```go
func renderCauseTarget(c CauseTarget) string {
    return fmt.Sprintf("consumer compiles to %v, but %s supports only %v",
        c.ConsumerTargets, c.Pkg, c.Supported)
}

func renderCauseCompiler(c CauseCompiler) string {
    return fmt.Sprintf("this mochi is %s, but %s requires %s", c.Have, c.Pkg, c.Want)
}
```

## Sub-phase 6.7 — TTY color + plain-text

When stdout is a TTY (`golang.org/x/term.IsTerminal`), colorise:

- Package names: cyan.
- Versions: yellow.
- Cause keywords (`Because`, `Fix option`): bold.

Plain-text mode (non-TTY or `--no-color`): no ANSI escapes; the test harness uses plain-text mode for byte comparison.

## Files changed

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkgsolver/pubgrub/explain.go` | `BuildExplanation` | Owner |
| `pkg/pkgsolver/pubgrub/render.go` | UNSAT prose renderer | Owner |
| `pkg/pkgsolver/pubgrub/fixoptions.go` | `ComputeFixOptions` | Owner |
| `pkg/pkgwhy/why.go` | `PrintWhy` | Owner |
| `pkg/pkgwhy/tree.go` | `mochi pkg tree` core | Owner |
| `pkg/pkgwhy/color.go` | TTY detection | Owner |
| `cmd/mochi/why.go` | `mochi pkg why <pkg>` handler | Owner |
| `cmd/mochi/tree.go` | `mochi pkg tree` handler | Owner |
| `tests/pkgsystem/solver-unsat/*` | UNSAT fixtures | Owner |
| `tests/pkgsystem/solver-why/*` | SAT `why` fixtures | Owner |
| `tests/pkgsystem/solver-tree/*` | `tree` fixtures | Owner |

## Fix-option coverage matrix

| Fixture | Causes in chain | Expected Fix option |
|---------|-----------------|---------------------|
| `relax-leaf-range/` | CauseDep -> CauseRoot | relax-range |
| `allow-multi-version/` | Two CauseDep with distinct majors | allow-multi-version |
| `swap-target/` | CauseTarget on a leaf | swap-target |
| `add-capability/` | CauseCapability on a leaf | add-capability |
| `compiler-mismatch/` | CauseCompiler | upgrade-mochi |

## Test set

- `TestPhase6Unsat` — UNSAT fixtures match golden.
- `TestPhase6FixOptions` — every fixture emits at least one fix option of the expected kind.
- `TestPhase6Why` — SAT fixtures match `expected-why.txt`.
- `TestPhase6Tree` — `expected-tree.txt`.
- `TestPhase6Stability` — three runs identical.

## Open questions

- Whether to render the full chain or just the leaves; current plan: full chain by default, `--brief` for leaves only.
- Whether to emit JSON for editor consumption; current plan: `--format=json` flag, deferred to v1.1 if not blocking LSP.

## Cross-references

- "Why" output details: [research note 05 §7](/docs/research/0057/solver-design).
- Explanation quality measurement: 2025 IEEE Software paper cited in [research note 02 §2](/docs/research/0057/design-philosophy).
- Cause taxonomy: [research note 05 §5](/docs/research/0057/solver-design).
