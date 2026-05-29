---
title: "01. Language surface"
sidebar_position: 2
sidebar_label: "01. Language surface"
description: "The `import rust \"<crate>@<semver>\" as <alias>` import form, the `[rust-dependencies]` / `[rust]` / `[rust.publish]` / `[rust.capabilities]` manifest tables, the CLI subcommands (`mochi pkg add rust`, `mochi pkg lock`, `mochi pkg publish --to=crates.io`, `mochi pkg sync rust`), and the per-import alias resolution rule."
---

# 01. Language surface

This note covers the user-visible surface MEP-73 introduces: the import syntax, the manifest tables, and the CLI subcommands. Everything below is observable through `mochi --help` and `mochi.toml` schema validation; the user does not need to read the rest of the bundle to use the bridge.

## Import syntax

The Mochi grammar's `ImportStmt` production (MEP-1) accepts a `Lang` token between `import` and the string literal:

```
ImportStmt := "import" Lang? StringLit "as" Ident ("auto")?
Lang := "go" | "python" | "typescript" | "rust"
```

MEP-73 adds `rust` as the fourth alternative. The string literal is one of:

| Form | Resolution |
|------|------------|
| `<crate-name>` | Bare name. Resolves through `[rust-dependencies]` plus `mochi.lock`. The lockfile records the picked version. |
| `<crate-name>@<semver-req>` | Explicit constraint (`^1.42`, `~0.4`, `>=1.0, <2.0`, `=1.2.3`). Must be compatible with `[rust-dependencies]`. |
| `<crate-name>@git+<url>` | Git source, branch default. |
| `<crate-name>@git+<url>#<rev>` | Git source, pinned to revision or tag. |
| `<crate-name>@path+<rel-path>` | Path source, relative to the manifest. |

Example surface:

```mochi
import rust "anyhow@^1.0" as anyhow
import rust "tokio" as tokio
import rust "serde_json" as serde_json

fn parse_user(body: string): User {
    let parsed = serde_json.from_str(body)
    if parsed.is_err() {
        anyhow.bail("malformed input")
    }
    return parsed.value()
}
```

The `<alias>` introduces a Mochi namespace bound at the import site. Symbol lookup `<alias>.<item>` resolves to the synthesised `extern fn` declaration the bridge generated for `<crate>::<item>`. Item names follow the crate's public surface verbatim (snake_case Rust names stay snake_case in Mochi; the bridge does not rename).

The `auto` modifier (already accepted for `import go ... auto`) is admitted for `import rust ... auto`. With `auto`, every public top-level item of the crate is bound at file scope rather than under the alias namespace. Default is namespaced (`alias`-prefixed lookup); `auto` is opt-in.

## Manifest: `[rust-dependencies]`

This table is the user-facing dependency declaration. It uses Cargo's `[dependencies]` grammar verbatim:

```toml
[rust-dependencies]
anyhow = "^1.0"
thiserror = "^1.0"
tokio = { version = "^1.42", features = ["rt", "macros"] }
reqwest = { version = "^0.12", features = ["json", "rustls-tls"], default-features = false }
serde = { version = "^1.0", features = ["derive"] }
serde_json = "^1.0"
my-local-crate = { path = "../my-crate" }
my-git-crate = { git = "https://github.com/example/my-crate", tag = "v0.2.0" }
```

The grammar mirrors Cargo's:

- A bare string is shorthand for `{ version = "..." }`.
- The table form admits `version`, `features`, `default-features`, `optional`, `path`, `git`, `branch`, `rev`, `tag`, `package` (rename), and `registry` (alternative registry).
- Cyclic dependencies are rejected at lock time (the same rule Cargo enforces).

The user does not write a separate `Cargo.toml`. The bridge synthesises the workspace `Cargo.toml` at build time, populating `[dependencies]` from `[rust-dependencies]` and pinning the exact resolved version from `mochi.lock`.

## Manifest: `[rust]`

```toml
[rust]
edition = "2024"
rust-version = "1.85"
runtime = { flavor = "current-thread" }
monomorphise = [
    { item = "serde_json::from_str", T = "MyStruct" },
    { item = "Vec::with_capacity", T = "MyStruct" },
]
```

| Key | Default | Meaning |
|-----|---------|---------|
| `edition` | `"2021"` | Rust edition for the wrapper crate. `"2024"` requires Rust 1.85+. |
| `rust-version` | `"1.78"` | Minimum supported Rust toolchain (matches MEP-53's floor). |
| `runtime.flavor` | `"current-thread"` | Tokio runtime kind for the async bridge. Alt: `"multi-thread"`. |
| `monomorphise` | `[]` | Explicit generic instantiations. Each entry binds one `<item>` at one `<T>`. |

The `monomorphise` table is the only path to import a generic-parameter Rust item. The bridge does not auto-monomorphise (the combinatorial explosion would be unbounded); the user enumerates the instantiations they need.

## Manifest: `[rust.publish]`

```toml
[rust.publish]
crate-type = ["rlib", "cdylib"]
cbindgen = true
publish-to = "crates.io"
```

| Key | Default | Meaning |
|-----|---------|---------|
| `crate-type` | `["rlib"]` | Cargo `[lib] crate-type` value. `rlib` for `cargo add`-able use, `cdylib` for dlopen / non-Rust consumers. |
| `cbindgen` | `false` | Whether to emit a C header alongside the cdylib. Requires cbindgen on PATH or `Driver.AutoInstallCbindgen=true`. |
| `publish-to` | `"crates.io"` | The publish target. Currently only `"crates.io"` is supported; future: `"sparse-mirror"`, `"private-registry"`. |

This table is only consulted when the user runs `mochi pkg publish --to=crates.io`. Mochi packages that do not publish to crates.io can omit it.

## Manifest: `[rust.capabilities]`

```toml
[rust.capabilities]
net = true
fs = false
proc = false
unsafe = false
```

These capability flags are a strict refinement of MEP-57's `[capabilities]` table. The bridge walks the Rust dep graph at lock time, computes the union of capability marks across every reachable crate (via a curated capability database, [[12-risks-and-alternatives]] §R6 documents the database maintenance), and asserts that the union is a subset of the user's `[rust.capabilities]` declaration. If the union exceeds the declaration, lock fails with a diagnostic naming the crate and the capability.

The four canonical capabilities for Rust deps are:

- `net`: any reachable crate opens TCP, UDP, or Unix sockets, or otherwise initiates network I/O.
- `fs`: any reachable crate reads or writes files.
- `proc`: any reachable crate calls `std::process::Command` or equivalent.
- `unsafe`: the user has hand-overridden an `extern fn` with `unsafe` semantics that the wrapper relies on.

Capabilities outside this set (clock, env, random) are inherited from MEP-57's broader `[capabilities]` table and audited there.

## CLI surface

The `mochi pkg` subcommand gains four new operations:

### `mochi pkg add rust <crate>[@<semver>]`

```
$ mochi pkg add rust tokio@^1.42 --features=rt,macros
Added tokio = { version = "^1.42", features = ["rt", "macros"] } to [rust-dependencies]
Running mochi pkg lock ...
Resolved 47 Rust packages (tokio + 46 transitive)
Wrote mochi.lock (+47 [[rust-package]] entries)
```

Equivalent to manually editing `mochi.toml` plus running `mochi pkg lock`. Idempotent if the entry already exists at a compatible version.

### `mochi pkg lock`

Walks `[rust-dependencies]`, runs the resolver against the Cargo sparse index, downloads each crate to the content-addressed cache, runs rustdoc-JSON on each, synthesises the wrapper for each, and writes a `[[rust-package]]` entry per dep into `mochi.lock`.

### `mochi pkg lock --check`

Reads `mochi.lock`, recomputes every `crate-blake3`, `rustdoc-sha256`, `wrapper-sha256`, and `capabilities-declared`, and exits non-zero on any mismatch. This is the CI-enforced reproducibility gate.

### `mochi pkg publish --to=crates.io [--dry-run] [--rust-edition=2024]`

- Builds the package via `Driver.Build` with `target = TargetRustLibrary, LibraryMode = true`.
- Runs `cargo package --no-verify --allow-dirty --target-dir <tmp>`.
- Obtains an OIDC token from the CI environment.
- Presents the token plus the `.crate` tarball to crates.io's trusted-publishing endpoint.
- Records the Rekor log entry.

The `--dry-run` flag skips upload; the signing flow is still exercised against a `sigstore-mock-fulcio` harness for testing.

### `mochi pkg sync rust`

Re-runs the wrapper synthesiser from the existing `mochi.lock`, without re-resolving versions. Used after manual edits to the synthesised shim file, or after a bridge upgrade that changes the wrapper format.

## Per-import alias resolution

The alias `<alias>` introduced by `import rust "<spec>" as <alias>` participates in normal Mochi name resolution. The bridge generates a shim file at `<workdir>/rust_wrap/<crate>/shim.mochi` containing a corpus of `extern fn` declarations like:

```mochi
extern type Runtime
extern fn new_current_thread(): Runtime from rust "tokio::runtime::Builder::new_current_thread"
extern fn block_on(rt: Runtime, fut: any): any from rust "tokio::runtime::Runtime::block_on"
```

The import `import rust "tokio" as tokio` becomes (post-resolution) `import "./rust_wrap/tokio/shim.mochi" as tokio`. The synthesised shim is read by the parser exactly as a hand-written `.mochi` file would be.

The shim file is regenerated on every `mochi pkg lock` and is gitignored by default. Users who need to edit the synthesised bindings should override individual items in their own source via:

```mochi
import rust "tokio" as tokio_auto
extern fn tokio_block_on(rt: tokio_auto.Runtime, fut: any): any from rust "tokio::runtime::Runtime::block_on" custom
```

The `custom` modifier keeps the override stable across `mochi pkg sync rust` runs.

## Cross-references

- [[02-design-philosophy]] for the rationale.
- [[04-rustdoc-json-ingest]] for how the public surface is discovered.
- [[05-type-mapping]] for the closed translation table the shim file uses.
- [[06-cargo-publish-flow]] for the `mochi pkg publish` path.
- [MEP-73 §4](/docs/mep/mep-0073#4-surface-syntax-import-rust) for the normative syntax.
- [MEP-57](/docs/mep/mep-0057) for the broader `mochi.toml` + `mochi.lock` model this extends.
