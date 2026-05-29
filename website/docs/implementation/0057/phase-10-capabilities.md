---
title: "Phase 10. Capability model"
sidebar_position: 11
sidebar_label: "Phase 10. Capabilities"
description: "MEP-57 Phase 10 — closed 9-capability set, publisher declarations, consumer pinning, lockfile capability annotation, per-target enforcement, monotonicity policy, xz-style anomaly detection."
---

# Phase 10. Capability model

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 10](/docs/mep/mep-0057#phase-10-capabilities) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase10Capabilities`: a manifest that declares `[capabilities] required = ["fs.read"]` and is pinned by a consumer with `capabilities = ["fs.read"]` resolves; a version bump that silently adds `net.dial` is rejected with `M057_CAP_E001` until the consumer pin is widened; the lockfile records `capabilities-seen` exactly matching the union of the resolved tree.

Pass criteria:

1. Closed set. The set of capability names is exactly `{fs.read, fs.write, net.dial, net.listen, env, ffi, clock, random, proc.spawn}`. Any other name in a manifest's `[capabilities]` block raises `M057_CAP_E005` at parse time.
2. Publisher declaration. `[capabilities] required = [...]` (mandatory subset) and `[capabilities] optional = [...]` (subset gated behind features) are parsed and propagated to the resolved tree.
3. Consumer pin. A consumer dep entry `"@scope/name" = { version = "^1.2", capabilities = ["fs.read"] }` is the explicit allow-list; resolution fails if a candidate version's `required` exceeds the pin.
4. Lockfile annotation. The lockfile's top-level `[capabilities-seen]` is the sorted union of every locked package's `required`; per-package the lockfile carries `capabilities = ["fs.read", ...]` matching that package's resolved `required`.
5. Monotonicity. A patch bump that adds a new `required` capability is rejected by `mochi pkg audit capabilities` with `M057_CAP_E002`. A minor bump may add. A major bump may grow or shrink.
6. xz-pattern check. A minor bump that adds `ffi` while no prior version in the same major declared `ffi` is flagged as a high-risk diff in `mochi pkg audit capabilities --suspicious`.
7. Target-runtime enforcement. The polyglot fan-out (Phase 14) maps capabilities to runtime gates; this phase emits the `.caps.json` sidecar used by emitters.

## Goal-alignment audit

Capabilities are the user-facing answer to "what can this package do to my machine, and did the answer change between versions?". Without a closed set, the typing question becomes an open vocabulary problem (the npm "permissions" debate has failed for a decade). The closed nine were chosen in research note 10 §1 as the minimum that distinguishes ambient authority categories every host runtime can enforce.

Phase 10 is intentionally pre-Sigstore (Phase 13) because the capability check is independent of provenance. Even a fully-signed legitimate update that silently grows ambient authority should be visible. The xz-utils 2024 incident was a signed, attributed update; the social attack happened before the build, not at distribution time. Capability monotonicity puts a tripwire on the post-build artefact.

The "consumer pin" semantics mirror Deno's per-permission grants and Wasm component model imports: the consumer's threat model is what matters, not the publisher's claimed maxima. A consumer who only uses an HTTP client's parser does not need to grant `net.dial` (testing harnesses exist).

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 10.0 | `pkg/pkgcap`: closed set, parse, validate | NOT STARTED | — |
| 10.1 | Manifest `[capabilities]` required/optional parsing | NOT STARTED | — |
| 10.2 | Consumer pin parsing in dep tables | NOT STARTED | — |
| 10.3 | Solver integration: `CauseCapability` from Phase 5 | NOT STARTED | — |
| 10.4 | Lockfile `capabilities-seen` aggregator | NOT STARTED | — |
| 10.5 | `mochi pkg audit capabilities` monotonicity check | NOT STARTED | — |
| 10.6 | `mochi pkg why-capability <cap>` | NOT STARTED | — |
| 10.7 | xz-pattern anomaly detector | NOT STARTED | — |
| 10.8 | `.caps.json` sidecar for polyglot emitters | NOT STARTED | — |

## Sub-phase 10.0 — `pkg/pkgcap` package

```go
// pkg/pkgcap/cap.go
package pkgcap

type Capability string

const (
    FsRead    Capability = "fs.read"
    FsWrite   Capability = "fs.write"
    NetDial   Capability = "net.dial"
    NetListen Capability = "net.listen"
    Env       Capability = "env"
    FFI       Capability = "ffi"
    Clock     Capability = "clock"
    Random    Capability = "random"
    ProcSpawn Capability = "proc.spawn"
)

var ClosedSet = map[Capability]bool{
    FsRead: true, FsWrite: true, NetDial: true, NetListen: true,
    Env: true, FFI: true, Clock: true, Random: true, ProcSpawn: true,
}

func Validate(c Capability) error {
    if !ClosedSet[c] {
        return fmt.Errorf("%w: %q is not in the closed capability set", ErrCapE005, c)
    }
    return nil
}

type Set map[Capability]struct{}

func (s Set) Add(c Capability) { s[c] = struct{}{} }
func (s Set) Has(c Capability) bool { _, ok := s[c]; return ok }
func (s Set) Sorted() []Capability {
    out := make([]Capability, 0, len(s))
    for c := range s { out = append(out, c) }
    sort.Slice(out, func(i, j int) bool { return out[i] < out[j] })
    return out
}
func (s Set) Diff(other Set) (added, removed []Capability) { /* set diff */ }
func (s Set) Subset(of Set) bool { /* every elem of s is in of */ }
```

A `Set` is preferred to a slice everywhere because the union/diff/subset operations dominate downstream code.

## Sub-phase 10.1 — Manifest declaration parsing

From research note 10 §2 and research note 04 §6:

```toml
[capabilities]
required = ["fs.read", "net.dial"]                 # always needed
optional = ["proc.spawn"]                          # only behind a feature
optional_features = { spawn-shell = ["proc.spawn"] }
```

Parser:

Phase 0 §0.0 declared `Capabilities` with `Required []string` and `Optional []string`. Phase 10 narrows the field type to `pkgcap.Set` (with `.Subset()`, `.Difference()`, `.Sorted()` methods) and adds `OptionalFeatures`. The TOML tags and field names are unchanged so lockfiles written under Phase 0 continue to parse.

```go
// pkg/pkgmanifest/capabilities.go
type Capabilities struct {
    Required         pkgcap.Set                `toml:"required,omitempty"`
    Optional         pkgcap.Set                `toml:"optional,omitempty"`
    OptionalFeatures map[string]pkgcap.Set     `toml:"optional_features,omitempty"`
}

func parseCapabilities(t *toml.Tree) (*Capabilities, error) {
    out := &Capabilities{
        Required: pkgcap.Set{}, Optional: pkgcap.Set{},
        OptionalFeatures: map[string]pkgcap.Set{},
    }
    for _, raw := range t.GetArray("required") {
        c := pkgcap.Capability(raw.(string))
        if err := pkgcap.Validate(c); err != nil { return nil, err }
        out.Required.Add(c)
    }
    /* ... same for optional ... */
    /* ... optional_features keys are feature names, values are []Capability ... */
    return out, nil
}
```

Constraint enforced at parse: every cap in `optional_features` values must also appear in `optional` (research note 10 §2.3).

## Sub-phase 10.2 — Consumer pin

A dep table may carry an explicit `capabilities` list. This is the consumer's allow-list:

```toml
[dependencies]
"@mochi/http"  = { version = "^1.0", capabilities = ["net.dial"] }
"@mochi/json"  = "^1.2"                                 # no pin = grant all required
```

```go
// pkg/pkgmanifest/dep.go
type Dep struct {
    Range        pkgmanifest.Range
    Features     []string
    Capabilities pkgcap.Set    // nil if not pinned
    Optional     bool
}
```

Semantics:

- `Capabilities == nil`: the consumer accepts whatever `required` the resolved version declares (warning still flagged on monotonicity violations).
- `Capabilities != nil`: the resolved version's `required` MUST be a subset of the pin; otherwise the solver raises `CauseCapability` (Phase 5 §C.1).

## Sub-phase 10.3 — Solver integration

Phase 5's solver already emits `CauseCapability` when checking a candidate. The check function:

```go
// pkg/pkgsolver/pubgrub/check_capability.go
func capabilityCheck(consumer *Manifest, candidate *Manifest) *CauseCapability {
    pin := consumer.DepPin(candidate.Name)
    if pin.Capabilities == nil { return nil }       // not pinned, accept
    required := candidate.Capabilities.Required
    if required.Subset(pin.Capabilities) { return nil }
    extra := required.Difference(pin.Capabilities)
    return &CauseCapability{
        Pkg: candidate.Name, Version: candidate.Version,
        Allowed:  pin.Capabilities.Sorted(),
        Required: required.Sorted(),
        Extra:    extra.Sorted(),
    }
}
```

The renderer (Phase 6 §6.6) translates this into prose:

```
consumer pins @mochi/http with capabilities=[net.dial], but @mochi/http 1.2.0
requires [net.dial, fs.write]
```

## Sub-phase 10.4 — Lockfile aggregator

After solving, walk the resolved tree, union every `required`, write to the lockfile:

```go
// pkg/pkglock/capability_seen.go
func ComputeCapabilitiesSeen(packages []LockedPackage) pkgcap.Set {
    out := pkgcap.Set{}
    for _, p := range packages {
        for _, c := range p.Capabilities { out.Add(c) }
    }
    return out
}
```

Lockfile representation:

```toml
[capabilities-seen]
all = ["fs.read", "net.dial"]
```

And per-package:

```toml
[[package]]
name = "@mochi/http"
version = "1.1.7"
capabilities = ["net.dial"]
```

A diff of `capabilities-seen` between two `mochi pkg lock` runs is the primary review signal for capability change in a PR (research note 10 §4).

## Sub-phase 10.5 — `mochi pkg audit capabilities`

Monotonicity rules (research note 10 §5):

- Patch (`X.Y.Z` to `X.Y.Z+1`): MUST NOT add to `required`. May remove. (Removal is fine because old consumers' pins are wider, not narrower.)
- Minor (`X.Y.0` to `X.Y+1.0`): MAY add or remove `required`.
- Major (`X.0.0` to `X+1.0.0`): unrestricted.

```go
// pkg/pkgcap/audit.go
type Violation struct {
    Pkg          string
    From, To     string  // versions
    Kind         string  // "patch-add", "minor-suspicious", "missing-cap-from-pin"
    Caps         []pkgcap.Capability
    Severity     string  // "error", "warn"
}

func AuditMonotonicity(history []ManifestSnapshot) []Violation {
    var out []Violation
    for i := 1; i < len(history); i++ {
        prev, curr := history[i-1], history[i]
        added, _ := curr.Capabilities.Required.Diff(prev.Capabilities.Required)
        switch bump := semverBumpKind(prev.Version, curr.Version); bump {
        case "patch":
            if len(added) > 0 {
                out = append(out, Violation{
                    Pkg: curr.Name, From: prev.Version, To: curr.Version,
                    Kind: "patch-add", Caps: added, Severity: "error",
                })
            }
        case "minor":
            // Allowed but flagged for review if `--suspicious`.
        }
    }
    return out
}
```

The command:

```
mochi pkg audit capabilities                 # check current lock against publication history
mochi pkg audit capabilities --suspicious    # also emit warnings for minor bumps that add caps
mochi pkg audit capabilities --since=v0.5    # only inspect changes since a baseline
```

A `patch-add` violation exits non-zero with `M057_CAP_E002`.

## Sub-phase 10.6 — `mochi pkg why-capability <cap>`

For audit: "I see `fs.write` in `capabilities-seen`; which package needs it?"

```go
func WhyCapability(lock *pkglock.Lockfile, cap pkgcap.Capability) []string {
    var pkgs []string
    for _, p := range lock.Packages {
        for _, c := range p.Capabilities {
            if c == cap { pkgs = append(pkgs, p.Name + "@" + p.Version) }
        }
    }
    sort.Strings(pkgs)
    return pkgs
}
```

Output:

```
fs.write is required by:
  @mochi/log@1.2.5
  @mochi/sqlite@0.4.0
```

The tree walk is then available via `mochi pkg why @mochi/log` (Phase 6 §6.3) to see who introduced the dep.

## Sub-phase 10.7 — xz-pattern anomaly detector

The xz-utils CVE-2024-3094 pattern: a previously-clean package gains a new ambient-authority capability in a quiet release. The detector flags releases where:

- A minor bump adds `ffi` or `proc.spawn` (highest-blast capabilities).
- A patch bump's tarball gains a file matching `build*`, `m4/*`, or `configure*` patterns (build-time script surface).
- The maintainer set changes within 30 days of the release (cross-check with registry maintainer history; deferred to Phase 13).

```go
type Suspicious struct {
    Pkg, Version string
    Reason       string
    Evidence     map[string]any
}

func DetectXzPattern(prev, curr ManifestSnapshot, prevTarball, currTarball Files) []Suspicious {
    var out []Suspicious
    added, _ := curr.Capabilities.Required.Diff(prev.Capabilities.Required)
    for _, c := range added {
        if c == pkgcap.FFI || c == pkgcap.ProcSpawn {
            out = append(out, Suspicious{
                Pkg: curr.Name, Version: curr.Version,
                Reason: "high-blast-capability-added-in-minor",
                Evidence: map[string]any{"cap": c},
            })
        }
    }
    if buildFilesAdded(prevTarball, currTarball) && semverBumpKind(prev.Version, curr.Version) == "patch" {
        out = append(out, Suspicious{
            Pkg: curr.Name, Version: curr.Version,
            Reason: "build-script-files-added-in-patch",
            Evidence: map[string]any{"files": diffFiles(prevTarball, currTarball)},
        })
    }
    return out
}
```

Output appears under `mochi pkg audit capabilities --suspicious` and `mochi pkg audit --supply-chain` (Phase 16).

## Sub-phase 10.8 — `.caps.json` sidecar for emitters

The polyglot emitters (Phase 14) need a per-package capability map to translate into target-runtime gates:

```json
{
  "package": "@mochi/http",
  "version": "1.1.7",
  "required": ["net.dial"],
  "optional": ["clock"]
}
```

The file is written next to the compiled artefact in each target's cache. The mapping per target:

| Capability  | TypeScript (Deno)         | Python (3.12 shim)              | Wasm component                | VM3 trace tag       |
|-------------|---------------------------|----------------------------------|--------------------------------|---------------------|
| `fs.read`   | `--allow-read`            | seatbelt: `os.open` denylist OFF | imports `wasi:filesystem.read` | `cap:fs.read`       |
| `fs.write`  | `--allow-write`           | seatbelt: `os.open` write OFF    | imports `wasi:filesystem.write`| `cap:fs.write`      |
| `net.dial`  | `--allow-net=outbound`    | seatbelt: `socket.connect` OFF   | imports `wasi:sockets.dial`    | `cap:net.dial`      |
| `net.listen`| `--allow-net=listen`      | seatbelt: `socket.bind` OFF      | imports `wasi:sockets.listen`  | `cap:net.listen`    |
| `env`       | `--allow-env`             | seatbelt: `os.environ` ALLOW     | imports `wasi:cli.environment` | `cap:env`           |
| `ffi`       | `--allow-ffi`             | (denied, error at import time)   | (denied, no component import)  | `cap:ffi`           |
| `clock`     | (default)                 | (default)                        | imports `wasi:clocks`          | `cap:clock`         |
| `random`    | (default)                 | (default)                        | imports `wasi:random`          | `cap:random`        |
| `proc.spawn`| `--allow-run`             | seatbelt: `subprocess` ALLOW     | (denied, no component for now) | `cap:proc.spawn`    |

The VM3 trace tag is used in `mochi run --trace-capabilities`, which logs every capability-touched call site for forensic analysis.

## Files changed

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkgcap/cap.go` | Closed set + `Set` type | Owner |
| `pkg/pkgcap/audit.go` | Monotonicity check | Owner |
| `pkg/pkgcap/suspicious.go` | xz-pattern detector | Owner |
| `pkg/pkgmanifest/capabilities.go` | `[capabilities]` parsing | Owner |
| `pkg/pkgmanifest/dep.go` | Consumer `capabilities = [...]` pin | Owner |
| `pkg/pkgsolver/pubgrub/check_capability.go` | Solver integration | Owner |
| `pkg/pkglock/capability_seen.go` | Lockfile aggregation | Owner |
| `cmd/mochi/audit_capabilities.go` | `mochi pkg audit capabilities` handler | Owner |
| `cmd/mochi/why_capability.go` | `mochi pkg why-capability` handler | Owner |
| `pkg/pkgemit/capsfile.go` | `.caps.json` sidecar writer | Owner |
| `tests/pkgsystem/capabilities/closed-set/*` | Reject open names | Owner |
| `tests/pkgsystem/capabilities/pin-narrows/*` | Solver rejects over-broad versions | Owner |
| `tests/pkgsystem/capabilities/patch-add/*` | Audit catches monotonicity break | Owner |
| `tests/pkgsystem/capabilities/xz-pattern/*` | Suspicious detector fires | Owner |

## Error code surface

| Code | Trigger |
|------|---------|
| `M057_CAP_E001` | Consumer pin does not cover the resolved version's `required`. |
| `M057_CAP_E002` | Patch bump added a `required` capability (monotonicity break). |
| `M057_CAP_E003` | Lockfile `capabilities-seen` does not match the recomputed union (drift). |
| `M057_CAP_E004` | `optional_features` references a cap not in `optional`. |
| `M057_CAP_E005` | Capability name outside the closed set. |

## Test set

- `TestPhase10ClosedSet` — `fs.exec` (made-up) raises E005.
- `TestPhase10PinSubset` — pin narrower than `required` raises E001 in solver.
- `TestPhase10LockSeen` — `capabilities-seen` matches union.
- `TestPhase10Monotonicity` — patch adds `net.dial`, audit reports E002.
- `TestPhase10MinorWarn` — minor adds `ffi`, `--suspicious` flags it.
- `TestPhase10WhyCapability` — output lists every package that requires the cap.
- `TestPhase10CapsFile` — `.caps.json` emitted with correct content.

## Open questions

- Whether `clock` and `random` should be in the closed set at all (both are present in research note 10 §1; the argument is that deterministic-replay tools want to see them as effects). Decision: keep both. The reproducibility surface that consumes them lives in [Phase 17](./phase-17-repro): `SOURCE_DATE_EPOCH` shadows `clock` so reproducible builds are unaffected; deterministic-RNG seeding for `random` is tracked under Phase 17 open question 4.
- Whether to support a per-target capability override (e.g., `net.dial` allowed on the server target but denied on the wasm target); current plan: per-target overrides live in the consumer pin, not the publisher declaration.
- Whether the suspicious detector should fail the build or only warn; current plan: warn by default, `--suspicious-as-error` opts in.

## Cross-references

- Capability model: [research note 10](/docs/research/0057/capability-model).
- Consumer pin rationale: [research note 10 §3](/docs/research/0057/capability-model).
- Monotonicity policy: [research note 10 §5](/docs/research/0057/capability-model).
- Solver `CauseCapability`: [research note 05 §5](/docs/research/0057/solver-design).
- Lockfile annotation: [research note 06 §4](/docs/research/0057/lockfile-format).
