---
title: "Phase 3. Workspaces"
sidebar_position: 4
sidebar_label: "Phase 3. Workspaces"
description: "MEP-57 Phase 3 — `mochi.workspace.toml` umbrella, member discovery via globs, cross-member import short-circuit, workspace dependency inheritance via `workspace = true`."
---

# Phase 3. Workspaces

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 3](/docs/mep/mep-0057#phase-3-workspaces) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase3Workspaces`: every workspace fixture in `tests/pkgsystem/workspace/` resolves cross-member imports without registry calls; `[workspace.dependencies]` inheritance produces the expected effective dep set per member.

Pass criteria:

1. Member glob expansion. `[workspace] members = ["packages/*", "tools/cli"]` resolves to the on-disk member set, with `exclude = [...]` honoured. The set is computed via `filepath.Glob` (literal-glob) and explicit-paths; the result is sorted by path.
2. Cross-member import short-circuit. A member importing `@my/parser` (another member's published name) resolves to the on-disk source under `packages/parser/`; no registry call is made; the lockfile records `source = { kind = "workspace", path = "packages/parser" }`.
3. Inheritance. A member declaring `dep = { workspace = true }` reads the version range from `[workspace.dependencies]` and produces the same effective `Dep` struct as if the member had restated the range inline.
4. Target propagation. `[workspace.targets] default = ["typescript", "python"]` is the default `[targets]` set for any member without its own `[targets]` block.
5. CLI. `mochi workspace ls` lists members; `mochi workspace add <path>` appends to `members`; `mochi workspace remove <name>` removes by name. Modifications go through the canonical writer.
6. Umbrella-only detection. A directory with `mochi.workspace.toml` and no `[package]` table is treated as an umbrella (no package compiled at the root); a `mochi.toml` with `[workspace]` is treated as a combined member-and-root.

## Goal-alignment audit

Workspaces are the user-facing primitive for multi-package monorepos. Without them, every member would re-state its deps and re-resolve independently, which is the painful state npm spent 2017-2023 in. The user-facing goal moved: "I can split a single Mochi codebase into multiple publishable packages and they all share one lockfile and one set of pinned dep versions". Cargo's `workspace = true` inheritance is the proven pattern; uv (2024) adopted the same. MEP-57 follows.

Lockfile-per-workspace (not per-member) is the crucial decision: it forces every member's dep graph to agree, which catches "package A depends on lodash 4.17, package B depends on lodash 4.20" drift at lock time rather than at runtime.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 3.0 | `mochi.workspace.toml` parser; member glob expansion | NOT STARTED | — |
| 3.1 | Member discovery (every `mochi.toml` under members) | NOT STARTED | — |
| 3.2 | Cross-member import short-circuit (no registry call) | NOT STARTED | — |
| 3.3 | `[workspace.dependencies]` inheritance (`workspace = true`) | NOT STARTED | — |
| 3.4 | `[workspace.targets]` default propagation | NOT STARTED | — |
| 3.5 | `mochi workspace add / remove / list` commands | NOT STARTED | — |
| 3.6 | Umbrella-only vs combined-root detection | NOT STARTED | — |
| 3.7 | Cycle detection across members | NOT STARTED | — |

## Sub-phase 3.0 — Parser and glob expansion

`mochi.workspace.toml` is a synonym for `mochi.toml` with an empty `[package]` and a populated `[workspace]`. The parser maps it to the same `Manifest` struct:

```go
// pkg/pkgmanifest/workspace.go (extends Phase 1 stub)
type Workspace struct {
    Members        []string                  `toml:"members,omitempty"`
    Exclude        []string                  `toml:"exclude,omitempty"`
    DefaultTarget  []string                  `toml:"default-target,omitempty"`
    Dependencies   map[string]Dep            `toml:"dependencies,omitempty"`
    Targets        map[string][]string       `toml:"targets,omitempty"` // default = [...]
    AllowMultiVer  []string                  `toml:"allow-multi-version,omitempty"`
}
```

Glob expansion uses Go's `filepath.Glob` with a wrapper that:

- Normalises Windows separators to `/`.
- Rejects glob patterns with leading `/` (absolute paths outside the workspace).
- Rejects `..` segments.
- Returns paths relative to the workspace root.

```go
func ExpandMembers(rootDir string, ws Workspace) ([]string, error) {
    var members []string
    for _, pattern := range ws.Members {
        matched, err := safeGlob(rootDir, pattern)
        if err != nil { return nil, err }
        for _, m := range matched {
            if isExcluded(m, ws.Exclude) { continue }
            if !fileExists(filepath.Join(rootDir, m, "mochi.toml")) { continue }
            members = append(members, m)
        }
    }
    sort.Strings(members)
    return dedup(members), nil
}
```

Exclude entries are literal paths or simple globs; they are evaluated after member expansion.

## Sub-phase 3.1 — Member discovery

For each expanded member path, load and validate the member's `mochi.toml`:

```go
type WorkspaceState struct {
    Root        string                        // absolute path of workspace root
    Manifest    *Manifest                     // the workspace umbrella
    Members     map[string]*MemberState       // keyed by member name
    NameToPath  map[string]string             // for cross-member resolve
}

type MemberState struct {
    Path     string                           // relative to root
    Manifest *Manifest                        // member's manifest
}

func LoadWorkspace(rootDir string) (*WorkspaceState, error) {
    rootManifest, err := loadUmbrella(rootDir)
    if err != nil { return nil, err }
    memberPaths, err := ExpandMembers(rootDir, rootManifest.Workspace)
    if err != nil { return nil, err }
    ws := &WorkspaceState{Root: rootDir, Manifest: rootManifest,
                          Members: map[string]*MemberState{}, NameToPath: map[string]string{}}
    for _, p := range memberPaths {
        m, err := ParseFile(filepath.Join(rootDir, p, "mochi.toml"))
        if err != nil { return nil, err }
        if other, dup := ws.NameToPath[m.Package.Name]; dup {
            return nil, fmt.Errorf("%w: %q in %s and %s", ErrDuplicateMember,
                                   m.Package.Name, other, p)
        }
        ws.Members[m.Package.Name] = &MemberState{Path: p, Manifest: m}
        ws.NameToPath[m.Package.Name] = p
    }
    return ws, nil
}
```

Edge cases:

- Empty workspace (`members = []`) is allowed; the umbrella is a degenerate case.
- A member with the same `package.name` as another member raises `M057_WORKSPACE_E001`.
- A member listed under `members` whose directory has no `mochi.toml` is silently skipped (matches Cargo).

## Sub-phase 3.2 — Cross-member short-circuit

When the resolver classifies a scoped import that names a workspace member, it short-circuits to the local source tree:

```go
func (r *PkgResolver) Resolve(c Classification, ctx *Context) (ResolvedModule, error) {
    if ctx.Workspace != nil {
        memberPath, isMember := ctx.Workspace.NameToPath[memberKey(c.Scope, c.Name)]
        if isMember {
            return ResolvedModule{
                SourceRoot: filepath.Join(ctx.Workspace.Root, memberPath),
                Version:    ctx.Workspace.Members[memberKey(c.Scope, c.Name)].Manifest.Package.Version,
                Source:     LockSource{Kind: "workspace", Path: memberPath},
            }, nil
        }
    }
    // fall through to lockfile + cache lookup (Phase 2)
    return r.resolveFromCache(c, ctx)
}
```

Lockfile records:

```toml
[[package]]
name    = "@my/parser"
version = "0.1.0"
[package.source]
kind = "workspace"
path = "packages/parser"
```

No BLAKE3 / SHA-256 is recorded for workspace sources; the lockfile verifier skips integrity for `kind = "workspace"` (research note 06 §3).

## Sub-phase 3.3 — Inheritance

Member dep `dep = { workspace = true }` is resolved by reading the entry from the umbrella's `[workspace.dependencies]`:

```go
func ResolveMemberDeps(ws *WorkspaceState, member *MemberState) error {
    for name, dep := range member.Manifest.Dependencies {
        if !dep.Workspace { continue }
        wsDep, ok := ws.Manifest.Workspace.Dependencies[name]
        if !ok {
            return fmt.Errorf("%w: %q in member %q",
                ErrManifestE009, name, member.Manifest.Package.Name)
        }
        merged := wsDep
        if len(dep.Features) > 0 { merged.Features = unionStrings(wsDep.Features, dep.Features) }
        if dep.Optional { merged.Optional = true }
        if dep.DefaultFeatures != nil { merged.DefaultFeatures = dep.DefaultFeatures }
        member.Manifest.Dependencies[name] = merged
    }
    return nil
}
```

Merge rules:

- `version` comes from workspace.
- `features` are unioned (member can opt in to additional features without restating the version).
- `optional` is OR'd (member can mark optional independently).
- `default-features` member wins.
- `path`, `git`, `registry`, `branch`, `tag`, `rev` from the workspace win; the member cannot override them with `workspace = true` (use plain form instead).

## Sub-phase 3.4 — Target propagation

`[workspace.targets] default = [...]` is the per-member fallback when a member has no `[targets]` of its own:

```go
func ApplyTargetDefaults(ws *WorkspaceState) {
    defaults := ws.Manifest.Workspace.Targets["default"]
    if len(defaults) == 0 { return }
    for _, m := range ws.Members {
        if len(m.Manifest.Targets.Supports) == 0 {
            m.Manifest.Targets.Supports = defaults
        }
    }
}
```

The member can still narrow the set explicitly; the workspace default only fires when the member is silent.

## Sub-phase 3.5 — CLI

Subcommands added to `cmd/mochi/pkg.go` (stubbed in Phase 0):

```
mochi workspace ls               # list members with their paths and versions
mochi workspace add <path>       # add a path to [workspace] members
mochi workspace remove <name>    # remove the member named <name>
mochi workspace check            # validate workspace integrity (member set, name uniqueness, inheritance)
```

`mochi workspace add` modifies `mochi.workspace.toml` via the canonical writer (Phase 1). The writer preserves the canonical key order; the new entry is appended to `members` and the file is rewritten.

## Sub-phase 3.6 — Umbrella detection

The workspace root can be one of two shapes (research note 04 §8):

| File present                 | `[workspace]` table | Interpretation                          |
|------------------------------|---------------------|-----------------------------------------|
| `mochi.workspace.toml` only  | required            | Umbrella only; root is not a package    |
| `mochi.toml` only            | required            | Combined: root is also a member         |
| both                         | -                   | `M057_RESOLVE_E010`               |

In the combined case the root manifest's `[package]` produces a member; its sources live in the workspace root directory.

```go
func detectShape(rootDir string) (Shape, error) {
    hasUmbrella := fileExists(filepath.Join(rootDir, "mochi.workspace.toml"))
    hasManifest := fileExists(filepath.Join(rootDir, "mochi.toml"))
    switch {
    case hasUmbrella && hasManifest:
        return ShapeAmbiguous, fmt.Errorf("%w: both files present", ErrAmbiguousManifest)
    case hasUmbrella:
        return ShapeUmbrellaOnly, nil
    case hasManifest:
        return ShapeCombined, nil
    }
    return ShapeNone, nil
}
```

## Sub-phase 3.7 — Cycle detection

Cross-member imports form a DAG. The check phase verifies it:

```go
func DetectCycles(ws *WorkspaceState) error {
    g := buildMemberGraph(ws)
    if cycle, ok := tarjanFindCycle(g); ok {
        return fmt.Errorf("%w: %s", ErrWorkspaceCycle, formatCycle(cycle))
    }
    return nil
}
```

Cycles raise `M057_WORKSPACE_E003` with the full cycle path in the error message.

## Files changed

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkgmanifest/workspace.go` | Workspace struct + glob expansion | Extends (Phase 1) |
| `pkg/pkgworkspace/state.go` | WorkspaceState + member discovery | Owner |
| `pkg/pkgworkspace/inherit.go` | `ResolveMemberDeps` | Owner |
| `pkg/pkgworkspace/cycle.go` | DAG cycle check | Owner |
| `pkg/pkgresolve/resolve.go` | Cross-member short-circuit hook | Extends (Phase 2) |
| `cmd/mochi/workspace.go` | `mochi pkg workspace ...` handlers | Owner |
| `tests/pkgsystem/workspace/two-member/*` | A imports B fixture | Owner |
| `tests/pkgsystem/workspace/inherited-deps/*` | Inheritance fixture | Owner |
| `tests/pkgsystem/workspace/glob-members/*` | Glob expansion fixture | Owner |
| `tests/pkgsystem/workspace/exclude/*` | Exclude fixture | Owner |
| `tests/pkgsystem/workspace/cycle/*` | Cycle detection fixture | Owner |

## Error code surface

Sources (see [error registry](./errors)). Verbal aliases from early
drafts are renamed to the canonical `M057_WORKSPACE_E<NNN>` form:

| Code | Trigger |
|------|---------|
| `M057_WORKSPACE_E001` | Two members share `package.name`. |
| `M057_RESOLVE_E010` | Both `mochi.toml` and `mochi.workspace.toml` at the root (shared with Phase 2). |
| `M057_WORKSPACE_E003` | Cross-member imports form a cycle. |
| `M057_WORKSPACE_E004` | Member glob escapes workspace root. |
| `M057_MANIFEST_E009` | Member's `workspace = true` dep is not in `[workspace.dependencies]`. |

## Fixtures

- `tests/pkgsystem/workspace/two-member/`: minimal A imports B case.
- `tests/pkgsystem/workspace/inherited-deps/`: members reference `{ workspace = true }`.
- `tests/pkgsystem/workspace/glob-members/`: `members = ["packages/*"]` expansion.
- `tests/pkgsystem/workspace/exclude/`: `exclude` lists honoured.
- `tests/pkgsystem/workspace/cycle/`: A imports B imports A; should fail.
- `tests/pkgsystem/workspace/combined/`: `mochi.toml` with `[workspace]` at root.
- `tests/pkgsystem/workspace/umbrella-only/`: `mochi.workspace.toml` only.

## Test set

- `TestPhase3GlobExpansion` — glob and exclude rules.
- `TestPhase3CrossMember` — short-circuit imports.
- `TestPhase3Inheritance` — workspace dep merge.
- `TestPhase3CycleDetection` — failure on cyclic member graphs.
- `TestPhase3ShapeDetection` — umbrella vs combined vs ambiguous.

## Open questions

- Whether to support nested workspaces (a member that is itself a workspace root); current plan: reject with `M057_WORKSPACE_E005` at v1.
- Whether `workspace = "name"` (named workspace reference) is useful for cross-repo workspaces; deferred to v2.

## Cross-references

- Workspace surface: [research note 01 §4](/docs/research/0057/language-surface).
- Manifest workspace block: [research note 04 §8](/docs/research/0057/manifest-format).
- Lockfile workspace source kind: [research note 06 §3](/docs/research/0057/lockfile-format).
- Cargo workspace inheritance precedent: [research note 03](/docs/research/0057/prior-art-registries).
