---
title: "01. Language surface"
sidebar_position: 2
sidebar_label: "01. Language surface"
description: "The `import go \"<module>@<semver>\" as <alias>` import form, the `[go-dependencies]` / `[go]` / `[go.publish]` / `[go.capabilities]` / `[go.private]` manifest tables, the CLI subcommands (`mochi pkg add go`, `mochi pkg lock`, `mochi pkg publish --to=go+git+...`, `mochi pkg sync go`), and the per-import alias resolution rule."
---

# 01. Language surface

This note covers the user-visible surface MEP-74 introduces: the import syntax (extending MEP-54 phase 10's existing `go` keyword), the manifest tables, and the CLI subcommands. Everything below is observable through `mochi --help` and `mochi.toml` schema validation; the user does not need to read the rest of the bundle to use the bridge.

## Import syntax

The Mochi grammar's `ImportStmt` production (MEP-1) already accepts a `Lang` token between `import` and the string literal; MEP-54 phase 10 wired the `go` alternative for bare-name imports. MEP-74 extends the `<spec>` form without introducing a new keyword:

```
ImportStmt := "import" Lang? StringLit "as" Ident ("auto")?
Lang := "go" | "python" | "typescript" | "rust"
```

The string literal for `Lang == "go"` is one of:

| Form | Resolution |
|------|------------|
| `<module-path>` | Bare module path (e.g., `github.com/spf13/cobra`). Resolves through `[go-dependencies]` plus `mochi.lock`. |
| `<module-path>@<semver-req>` | Explicit constraint (`v1.8.0`, `^v1.8`, `~v1.8.0`, `>=v1.0 <v2.0`, `latest`). |
| `<module-path>@<pseudo-version>` | Pseudo-version `v0.0.0-YYYYMMDDhhmmss-shortrev` (Go's canonical untagged-commit form). |
| `<module-path>@git+<url>` | Git source override (used for forks). |
| `<module-path>@git+<url>#<rev>` | Git source pinned to revision or tag. |
| `<module-path>@path+<rel-path>` | Path source, relative to the manifest. |

The first form, the bare module path, is the same shape MEP-54 phase 10 accepts today; MEP-74 keeps that compatibility and adds the `@<...>` suffix forms as opt-in. A Mochi program written against MEP-54 phase 10 continues to parse unchanged.

Example surface:

```mochi
import go "github.com/spf13/cobra@^v1.8" as cobra
import go "github.com/sirupsen/logrus" as log
import go "go.uber.org/zap@v1.27.0" as zap
import go "encoding/json" as json

fn main() {
    let cmd = cobra.new_command()
    cmd.set_use("hello")
    cmd.set_short("say hello")
    cmd.set_run(fun(args: list<string>) {
        log.info("hello, world")
        let bytes = json.marshal({"k": "v"})
        log.info(bytes.to_string())
    })
    cmd.execute()
}
```

The `<alias>` introduces a Mochi namespace bound at the import site. Symbol lookup `<alias>.<item>` resolves to the synthesised `extern fn` declaration the bridge generated for the Go module's exported item.

### Stdlib paths

Bare stdlib paths (`encoding/json`, `net/http`, `strings`, `bytes`, `time`) are admitted without an `@<version>` suffix. The Go stdlib version is implicit: it ships with the Go toolchain, so the resolved version is the Go toolchain version (recorded in the lockfile as `version = "stdlib@<go-version>"`). Stdlib imports skip the sum.golang.org cross-check.

### Name renaming

Go's exported items are PascalCase by Go style; Mochi's idiom is snake_case for functions and PascalCase for types. The bridge auto-applies the following rename pass:

- `cobra.NewCommand` (Go func) → `cobra.new_command` (Mochi)
- `cobra.Command` (Go type) → `cobra.Command` (preserved; type names match)
- `cobra.Command.SetUse` (Go method) → `cobra.Command.set_use` (snake_case method)
- `json.MarshalIndent` (Go func) → `json.marshal_indent`

The emitted `extern fn` carries a `from go "<module>.<OrigName>"` clause that records the round-trip:

```mochi
extern fn cobra.new_command(): cobra.Command from go "github.com/spf13/cobra.NewCommand"
```

This lets `mochi pkg sync go` regenerate the shim file without reading the user's source.

The `auto` modifier (already accepted for `import go ... auto` since MEP-54 phase 10) is admitted unchanged. With `auto`, every public top-level item of the module is bound at file scope rather than under the alias namespace.

## Manifest: `[go-dependencies]`

This table is the user-facing dependency declaration. It uses Go's `require` grammar verbatim:

```toml
[go-dependencies]
"github.com/spf13/cobra" = "^v1.8.0"
"github.com/sirupsen/logrus" = { version = "^v1.9", build-tags = ["json_logging"] }
"go.uber.org/zap" = "^v1.27"
"gopkg.in/yaml.v3" = "^v3.0.1"
"my-local-module" = { path = "../my-module" }
"my-git-fork" = { git = "https://github.com/example/my-fork", rev = "abc123def4" }
```

The grammar:

- A bare string is shorthand for `{ version = "..." }`.
- The table form admits `version`, `build-tags`, `path`, `git`, `branch`, `rev`, `tag`, `replace` (for `replace` directives), and `indirect = true` (rare; bridges to Go's indirect requirements).
- Module paths must validate against `golang.org/x/mod/module.CheckPath`.
- Semver constraints follow the standard Mochi MEP-57 grammar (^, ~, >=, &lt;, =).
- Cyclic dependencies are rejected at lock time.

The user does not write a separate `go.mod`. The bridge synthesises the workspace `go.mod` at build time, populating `require` from `[go-dependencies]` and pinning the exact resolved version from `mochi.lock`.

## Manifest: `[go]`

```toml
[go]
go-version = "1.23"
goroutine-bridge = { default-buffer = 1, max-handles = 4096 }
monomorphise = [
    { item = "encoding/json.Unmarshal", T = "MyStruct" },
    { item = "sync.Pool", T = "MyStruct" },
]
build-tags = ["json_logging", "go_purego"]
```

| Key | Default | Meaning |
|-----|---------|---------|
| `go-version` | `"1.21"` | Go toolchain floor declared in the wrapper module's `go.mod`. |
| `goroutine-bridge.default-buffer` | `1` | Default channel buffer for Go `chan T` exposed as Mochi `stream<T>`. |
| `goroutine-bridge.max-handles` | `4096` | Soft upper bound on live cgo handles before the wrapper rejects new ones. |
| `monomorphise` | `[]` | Explicit generic instantiations. Each entry binds one `<item>` at one `<T>`. |
| `build-tags` | `[]` | Build tags the wrapper compilation respects. |

The `monomorphise` table is the only path to import a generic-parameter Go item (Go gained generics in 1.18, March 2022). The bridge does not auto-monomorphise.

## Manifest: `[go.publish]`

```toml
[go.publish]
canonical-import-path = "github.com/example/my-mochi-lib"
go-version-floor = "1.21"
license = "Apache-2.0 OR MIT"
cgo-export = true
```

| Key | Default | Meaning |
|-----|---------|---------|
| `canonical-import-path` | unset | The module path declared in the published `go.mod`. REQUIRED for `mochi pkg publish --to=go+git+...`. |
| `go-version-floor` | `"1.21"` | The `go <version>` directive value written into the published `go.mod`. |
| `license` | `""` | SPDX licence expression. Drives the LICENSE file and the `doc.go` first line. |
| `cgo-export` | `false` | Whether to emit `_cgo_export.h` for non-Go consumers. |

## Manifest: `[go.capabilities]`

```toml
[go.capabilities]
net = true
fs = false
proc = false
cgo = false
unsafe = false
```

These capability flags are a strict refinement of MEP-57's `[capabilities]` table. The bridge walks the Go dep graph at lock time, computes the union of capability marks across every reachable module (via a curated capability database; the same db that MEP-73 uses, extended with the Go-stdlib-induced cap classification), and asserts that the union is a subset of the user's `[go.capabilities]` declaration.

## Manifest: `[go.private]`

```toml
[go.private]
modules = ["corp.example.com/**"]
sumdb-skip = ["corp.example.com/**"]
```

| Key | Default | Meaning |
|-----|---------|---------|
| `modules` | `[]` | Glob patterns for which the bridge bypasses `proxy.golang.org` and reaches the upstream git repo directly. |
| `sumdb-skip` | `modules` value | Glob patterns for which the bridge skips the `sum.golang.org` cross-check. |

These mirror the `GOPRIVATE` / `GONOSUMCHECK` env vars Go uses for the same purpose.

## CLI surface

The `mochi pkg` subcommand gains four new operations and extends two existing ones:

### `mochi pkg add go <module>[@<semver>]`

```
$ mochi pkg add go github.com/spf13/cobra@^v1.8 --build-tags=json_logging
Added "github.com/spf13/cobra" = { version = "^v1.8", build-tags = ["json_logging"] } to [go-dependencies]
Running mochi pkg lock ...
Resolved 12 Go modules (cobra + 11 transitive)
Cross-verified against sum.golang.org (12/12 records present in tree)
Wrote mochi.lock (+12 [[go-package]] entries)
```

Equivalent to manually editing `mochi.toml` plus running `mochi pkg lock`. Idempotent.

### `mochi pkg lock`

Walks `[go-dependencies]`, runs the resolver against the Go module proxy, downloads each module .zip to the content-addressed cache, cross-checks against sum.golang.org, runs `go-ingest` on each, synthesises the wrapper for each, and writes a `[[go-package]]` entry per dep into `mochi.lock`.

### `mochi pkg lock --check`

Reads `mochi.lock`, recomputes every `zip-blake3`, `zip-h1`, `api-surface-sha256`, `wrapper-sha256`, cross-checks `sumdb-record-hash` against sum.golang.org's current tree, and exits non-zero on any mismatch. CI-enforced reproducibility gate.

### `mochi pkg lock --sumdb-consistency`

Additionally fetches a Merkle consistency proof from sum.golang.org for every public module, asserting that the lock-time tree leaf is still present in the current tree. Optional because of the per-dep RTT cost.

### `mochi pkg publish --to=go+git+<repo-url>@<tag> [--dry-run] [--cosign-sign]`

- Builds the package via `Driver.Build` with `target = TargetGoLibrary`.
- Runs `go vet ./...` + `gofmt -l ./...` and rejects on diagnostics.
- Runs `go build ./...` against the emitted module.
- Tags the canonical-import-path repo at HEAD with the requested semver tag.
- With `--cosign-sign`: constructs a cosign signature over the tag's commit SHA via the CI OIDC token, attaches it as a sibling git tag `<tag>.sig`.
- Pushes the tag (and optionally `<tag>.sig`) to the canonical-import-path remote.
- With `--dry-run`: stops before the push.
- Optionally pings `proxy.golang.org/<module>/@v/<tag>.info` to warm the module proxy.

### `mochi pkg publish --to=go+goproxy+<url>`

Alternative: uploads the module .zip to a private GOPROXY-compatible endpoint instead of git-tagging.

### `mochi pkg sync go`

Re-runs the wrapper synthesiser from the existing `mochi.lock`, without re-resolving versions. Used after manual edits to the synthesised shim file, or after a bridge upgrade that changes the wrapper format.

## Per-import alias resolution

The alias `<alias>` introduced by `import go "<spec>" as <alias>` participates in normal Mochi name resolution. The bridge generates a shim file at `<workdir>/go_wrap/<module>/shim.mochi` containing a corpus of `extern fn` and `extern type` declarations like:

```mochi
extern type Command
extern fn new_command(): Command from go "github.com/spf13/cobra.NewCommand"
extern fn (cmd: Command) set_use(use: string) from go "github.com/spf13/cobra.Command.SetUse"
extern fn (cmd: Command) execute(): Result<unit> from go "github.com/spf13/cobra.Command.Execute"
```

The import `import go "github.com/spf13/cobra@^v1.8" as cobra` becomes (post-resolution) `import "./go_wrap/github_com_spf13_cobra/shim.mochi" as cobra`. The synthesised shim is read by the parser exactly as a hand-written `.mochi` file would be. The shim file is regenerated on every `mochi pkg lock` and is gitignored by default.

Users who need to edit the synthesised bindings should override individual items in their own source via:

```mochi
import go "github.com/spf13/cobra@^v1.8" as cobra_auto
extern fn cobra_new_command(): cobra_auto.Command from go "github.com/spf13/cobra.NewCommand" custom
```

The `custom` modifier keeps the override stable across `mochi pkg sync go` runs.

## Cross-references

- [[02-design-philosophy]] for the rationale.
- [[04-go-doc-ast-ingest]] for how the public surface is discovered.
- [[05-type-mapping]] for the closed translation table the shim file uses.
- [[06-go-module-publish-flow]] for the `mochi pkg publish` path.
- [[10-interface-and-method-set]] for how Go interfaces become Mochi extern types.
- [MEP-74 §4](/docs/mep/mep-0074#4-surface-syntax-import-go--as-alias-extended) for the normative syntax.
- [MEP-57](/docs/mep/mep-0057) for the broader `mochi.toml` + `mochi.lock` model this extends.
