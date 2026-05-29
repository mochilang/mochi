---
title: "Language surface: package imports, workspaces, capabilities"
description: "How MEP-57 extends Mochi's existing import statement with scoped package specifiers, workspaces, capability declarations, and polyglot target opt-in, while keeping path-form imports and FFI imports intact."
sidebar_position: 1
---

# 01. Language surface: package imports, workspaces, capabilities

**Status**: research note. **Date**: 2026-05-29 (GMT+7).
**Mirrors**: deployed to `/docs/research/0057/language-surface`.

This note maps the existing Mochi import surface (file-relative path plus FFI tags) onto the new package layer introduced by MEP-57. It is the language-surface companion to the MEP body; see [02-design-philosophy](./02-design-philosophy) for the "why" and [04-manifest-format](./04-manifest-format) for the manifest schema details.

## 1. The existing surface (pre MEP-57)

As of 2026-Q2 Mochi has exactly four import shapes, all parsed by `parser/ast.go:751-758`:

```mochi
// 1. file-relative source import (canonical)
import "./util/text" as txt

// 2. directory-form (resolves to <dir>/main.mochi)
import "./examples/hello-mochi" as hello

// 3. FFI: Go module path
import go "fmt" as fmt

// 4. FFI: Python module
import python "datetime" as dt

// 5. FFI: TypeScript / JavaScript module (MEP-52, May 2026)
import typescript "@std/path" as path
```

There is no manifest. There is no versioning. There is no registry. `mochi run hello.mochi` opens the source tree at the current directory and walks parent directories for a `go.mod` (legacy bootstrap; see `runtime/mod/mod.go:30-83`). The resolver builds a flat module graph by repeatedly parsing imports until fixpoint. Cycles trigger a parse error. Diamond resolution (the same target imported by two distinct entry points) deduplicates by absolute path.

The only "external dependency" path today is `mochi get`, which delegates to `go mod tidy` for any `import go "..."` declaration. It does not move a single byte of Mochi source. There is no `mochi.toml`, `mochi.lock`, `mochi_modules/`, or registry endpoint anywhere in the system.

This is enough for examples, tutorials, and the test corpus. It is not enough for: shipping a reusable Mochi library, depending on someone else's Mochi library, pinning a version, auditing a dependency graph, or signing a release. MEP-57 fills those gaps.

## 2. The MEP-57 additions: zero new tokens, two new specifier shapes

MEP-57 adds **zero** new keywords to the language and **zero** new AST node kinds. The single change at the parse layer is that the existing `ImportStmt.Path` string is reinterpreted by the resolver depending on its leading character class:

| First character | Specifier shape                               | Resolver path                                 |
|-----------------|-----------------------------------------------|-----------------------------------------------|
| `.` or `/`      | file-relative path (existing)                 | `runtime/mod/mod.go` walk-and-join (unchanged)|
| `@`             | scoped package: `@scope/name[@req]`           | `pkg/pkgmanifest` + `pkg/pkgsolver` + `pkg/pkgindex` |
| `[a-z]`         | unscoped package: `name[@req]`                | same as scoped, scope inferred from registry  |
| (lang tag)      | FFI: `go|python|typescript|rust "..."`        | per-target FFI driver (unchanged)             |

Examples of the new shapes:

```mochi
// scoped package, manifest must list it in [dependencies]
import "@mochi/strings@^0.4" as str

// scoped package, version pinned in manifest, no version in source
import "@mochi/json" as json

// unscoped package
import "datalog@^1" as dl

// workspace member (resolved by mochi.workspace.toml)
import "@my/internal-utils" as utils
```

The version specifier `@<req>` is **optional in source** if the package is pinned in `mochi.toml`. The version specifier in source is checked against the manifest declaration at parse time; if they disagree, the parser raises `M057_VERSION_MISMATCH` with both the in-source spec and the in-manifest spec. This matches Cargo's behaviour (you can write `use serde::Serialize;` without restating the version because `Cargo.toml` already pins it).

The version specifier grammar follows npm + Cargo semver semantics:

```
spec     = "^" version          ; compatible, default
         | "~" version          ; patch-only
         | ">=" version         ; minimum
         | "=" version          ; exact
         | version              ; treated as "^"
version  = digits "." digits "." digits ( "-" prerelease )? ( "+" build )?
```

Path-form imports are unchanged. FFI-tagged imports are unchanged. The change is purely additive: anything that parsed before MEP-57 still parses after MEP-57, and resolves to the same module.

### 2.1 Resolver dispatch

The resolver is a single function `pkgresolve.Resolve(spec, ctx) → ResolvedModule` that branches on specifier shape:

```go
func Resolve(spec string, ctx *Context) (ResolvedModule, error) {
    switch classify(spec) {
    case PathRelative:
        return ctx.PathResolver.Resolve(spec) // existing path walk
    case ScopedPackage, UnscopedPackage:
        return ctx.PkgResolver.Resolve(spec) // new pkg pipeline
    case FFIGo, FFIPython, FFITS, FFIRust:
        return ctx.FFIResolver.Resolve(spec) // existing FFI dispatch
    }
}
```

The classifier is a 12-line function over the first character class plus FFI tag presence. There is no ambiguity: a leading dot or slash always means path, a leading at-sign or alphanumeric without an FFI tag always means package, an FFI tag always means FFI. Mixed shapes (e.g. `import "./util@^1" as x`) raise `M057_AMBIGUOUS_SPEC`.

### 2.2 Aliases stay required

Existing Mochi requires every import to carry an `as Alias` clause. MEP-57 keeps this requirement. There is no implicit name binding from the package name. This avoids the Python-style `from X import *` discovery cost and matches the "every named binding is explicit at the import site" property of the existing language. It also keeps diamond-resolved packages addressable under distinct aliases when the version graph forces two copies (see §5 below).

## 3. Manifest-driven resolution

When a Mochi compilation starts, the resolver first looks for the *root manifest*. The discovery rule mirrors Cargo and uv:

1. Start at the entry point's directory.
2. Walk up looking for `mochi.workspace.toml`. If found, that directory is the workspace root.
3. Otherwise walk up looking for `mochi.toml`. The first directory containing it is the package root.
4. If neither is found, the entry point runs in *manifest-less mode*: path-form and FFI imports work, scoped imports raise `M057_NO_MANIFEST`.

This rule means existing examples (`examples/v0.7/hello-mochi/`) keep running unchanged; no `mochi.toml` is required for them. It also means a single-file script in `/tmp` still runs, because no scoped imports trigger.

Once the manifest is found, the resolver loads `[dependencies]`, `[dev-dependencies]`, and (if present) the workspace's member manifests. Scoped imports are resolved against the resolved tree under `~/.cache/mochi/registry/<scope>/<name>/<version>/` (the global content-addressed cache; see [08-content-addressed-store](./08-content-addressed-store)).

### 3.1 The resolved tree

The resolved tree is the output of `pkg/pkgsolver/pubgrub`. It is a directed acyclic graph keyed by `(scope, name)` with at most one version per key (semver-compatible diamond merge) or up to two versions per key (semver-incompatible split, requires user opt-in via `[workspace.allow-multi-version]`). The default is one version per key; MEP-57's solver fails closed on incompatible diamonds and tells the user how to fix it.

### 3.2 Capabilities at the import site

A consumer's `mochi.toml` can pin the capability set a dependency is allowed to use:

```toml
[dependencies]
"@mochi/strings" = { version = "^0.4", capabilities = ["fs.read"] }
```

This is enforced at solver time: if `@mochi/strings@0.4.2` declares `[capabilities] required = ["fs.read", "net.dial"]` and the consumer pins only `["fs.read"]`, the solver fails with `M057_CAPABILITY_DENIED` and reports the missing capability. This matches Deno's per-permission flag granularity and Pony's reference capability checks at module boundaries.

The full capability discussion lives in [10-capability-model](./10-capability-model).

## 4. Workspaces

A workspace is a collection of packages that share a lockfile and a target set. The workspace root contains `mochi.workspace.toml`:

```toml
[workspace]
members = ["packages/*", "tools/cli"]
exclude = ["tools/cli/vendor"]

[workspace.dependencies]
# Shared version pins applied to every member that depends on them.
"@mochi/strings" = "^0.4"
"@mochi/json"    = "^1.2"

[workspace.targets]
default = ["typescript", "python", "jvm"]
```

A member `packages/parser/mochi.toml` then refers to shared deps by:

```toml
[dependencies]
"@mochi/strings" = { workspace = true }
"@mochi/json"    = { workspace = true }
```

This matches the Cargo `workspace = true` inheritance pattern and uv's workspace dependencies. Inheritance avoids the "every package re-states the version" pain point that npm workspaces had through 2023.

Cross-member imports use the member's published name even though no version is published yet:

```mochi
import "@my/parser" as parser  // resolves to packages/parser
```

Workspace members do not need a registry lookup; the workspace resolver short-circuits them to the local source tree. The lockfile records them with `source = "workspace"` (see [06-lockfile-format](./06-lockfile-format) §3).

## 5. Diamond resolution and multi-version opt-in

Mochi's solver enforces single-version-per-name as the default. When two transitive dependents disagree on a version range, three outcomes are possible:

1. **Compatible**: the ranges intersect; the solver picks the highest compatible version.
2. **Adjacent**: the ranges are semver-adjacent (one wants `^1.5`, the other wants `^1.7`); the solver picks `1.7` and emits a `M057_RANGE_RAISE` advisory.
3. **Incompatible**: the ranges are disjoint (one wants `^1.x`, the other wants `^2.x`); the solver fails by default. The user can opt in to multi-version resolution via `[workspace.allow-multi-version] = ["@scope/name"]` in the manifest.

When multi-version is allowed for a name, both versions are resolved and both end up in the resolved tree under distinct paths. The compiler treats them as distinct types (no implicit cross-version conversion). This matches Cargo's behaviour for major-version splits and avoids npm's "two versions of the same module" silent confusion that has caused real-world type-identity bugs since 2014.

## 6. Polyglot target opt-in

A package declares which transpiler targets it supports:

```toml
[targets]
typescript = { entrypoint = "src/main.mochi" }
python     = { entrypoint = "src/main.mochi" }
jvm        = { entrypoint = "src/main.mochi", ffi = ["java.util"] }
```

If a consumer compiles to the TypeScript target and depends on a package whose `[targets]` set excludes `typescript`, the resolver fails at lock time with `M057_TARGET_UNSUPPORTED`. The error message lists the supported targets so the user can either swap targets or open an issue against the dep.

Per-target overrides (FFI declarations, runtime-specific code) live under `targets.<name>.overrides`:

```toml
[targets.python.overrides]
"src/clock.mochi" = "src/clock_python.mochi"
```

At compile time the resolver substitutes the overridden file. This matches the cross-compile patterns of Swift's `#if os(...)` and Rust's `[target.'cfg(...)']` blocks without polluting the source with per-target conditionals.

## 7. Worked example: walking the resolver

A two-package workspace with a single registered dep:

```
my-workspace/
├── mochi.workspace.toml
├── mochi.lock
├── packages/
│   ├── parser/
│   │   ├── mochi.toml
│   │   └── src/main.mochi
│   └── codegen/
│       ├── mochi.toml
│       └── src/main.mochi
```

`packages/codegen/src/main.mochi`:

```mochi
import "@my/parser"   as parser
import "@mochi/strings@^0.4" as str

fun emit(ast: Ast) -> string {
    let header = str.repeat("=", 40)
    return header + "\n" + parser.show(ast)
}
```

Resolution sequence (logged by `mochi --trace-resolve`):

1. classify `"@my/parser"` → ScopedPackage
2. workspace lookup: `@my/parser` is in `mochi.workspace.toml` member list → short-circuit, no registry call, type-check against `packages/parser/src/main.mochi`
3. classify `"@mochi/strings@^0.4"` → ScopedPackage with version spec
4. workspace lookup: `@mochi/strings` is **not** a member → registry path
5. lockfile lookup: `mochi.lock` pins `@mochi/strings = "0.4.7"` → use locked version
6. cache lookup: `~/.cache/mochi/registry/mochi/strings/0.4.7/` exists → use cached source
7. integrity check: BLAKE3 of cached source matches lockfile pin → proceed
8. type-check against cached source

A cold cache changes step 6 to a registry fetch:

```
HEAD https://index.mochi.dev/mochi/strings  → ETag verify
GET  https://index.mochi.dev/mochi/strings  → version manifest line-delimited JSON
GET  https://blobs.mochi.dev/<blake3-hex>   → tarball
verify BLAKE3 matches manifest               → ok
extract to cache                             → ok
verify Sigstore bundle                       → ok
```

## 8. Backwards compatibility matrix

| Pre-MEP-57 program shape          | Post-MEP-57 behaviour                                                                |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Single file, no manifest          | Unchanged. Runs.                                                                     |
| Path imports only, no manifest    | Unchanged. Runs.                                                                     |
| FFI imports, no manifest          | Unchanged. Runs. `mochi get` still delegates to `go mod tidy`.                       |
| Path imports + `go.mod` discovery | Unchanged. The `go.mod` walk still works for FFI bootstrap.                          |
| Examples in `examples/v0.7/`      | Unchanged. None of them declare scoped imports.                                      |
| Scoped imports, no manifest       | Raise `M057_NO_MANIFEST` with a "did you forget `mochi init`?" hint.                 |
| Scoped imports + manifest         | Resolve through the new pipeline.                                                    |
| Mixed shapes in one file          | Mixed shapes are fine: each resolver handles its own subset.                         |

## 9. Editor and tooling surface

The new specifier shapes need editor support in three places:

1. **Syntax highlight**: scoped specifiers are tagged as `string.literal.package.mochi` so themes can colour them distinctly. Path specifiers stay `string.literal.path.mochi`.
2. **Hover info**: hovering over a scoped specifier shows the resolved version (from lockfile) and registry URL.
3. **Go-to-definition**: jumps to the cached source under `~/.cache/mochi/registry/...`.

LSP additions are minimal: the existing `textDocument/definition`, `textDocument/hover`, and `textDocument/documentLink` are reused. The language server learns to read `mochi.toml` and `mochi.lock` and to dispatch on specifier shape.

The CLI surface (`mochi new`, `mochi add`, `mochi tree`, `mochi why`, ...) is in [MEP-57](/docs/mep/mep-0057) §10.

## 10. Cross-references

- Manifest schema details: [04-manifest-format](./04-manifest-format)
- Solver behaviour and "why" output: [05-solver-design](./05-solver-design)
- Lockfile schema and serialisation: [06-lockfile-format](./06-lockfile-format)
- Capability declarations enforcement per target: [10-capability-model](./10-capability-model)
- Polyglot fan-out mechanics: [11-polyglot-fanout](./11-polyglot-fanout)
- Rejected alternatives (Unison hash-as-name): [12-risks-and-alternatives](./12-risks-and-alternatives) §6
