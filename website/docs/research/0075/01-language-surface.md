---
title: "01. Language surface"
sidebar_position: 2
sidebar_label: "01. Language surface"
description: "The `import php \"vendor/package@^semver\" as alias` import form, the `[php-dependencies]` / `[php]` / `[php.publish]` / `[php.capabilities]` manifest tables, the CLI subcommands (`mochi pkg add php`, `mochi pkg lock`, `mochi pkg publish --to=packagist`, `mochi pkg sync php`), and the per-import alias resolution rule."
---

# 01. Language surface

Author: research pass for MEP-75 (Mochi and PHP package bridge). Date: 2026-05-29 22:11 (GMT+7).

This note covers the user-visible surface MEP-75 introduces: the import syntax, the manifest tables, and the CLI subcommands. Everything below is observable through `mochi --help` and `mochi.toml` schema validation; the user does not need to read the rest of the bundle to use the bridge.

## Import syntax

The Mochi grammar's `ImportStmt` production (MEP-1) accepts a `Lang` token between `import` and the string literal:

```
ImportStmt := "import" Lang? StringLit "as" Ident ("auto")?
Lang := "go" | "python" | "typescript" | "rust" | "php"
```

MEP-75 adds `php` as the fifth alternative. The string literal is one of:

| Form | Resolution |
|------|------------|
| `<vendor>/<package>` | Bare name. Resolves through `[php-dependencies]` plus `mochi.lock`. |
| `<vendor>/<package>@<semver-req>` | Explicit constraint (`^7.8`, `~7.8.0`, `>=7.0 <8.0`, `7.8.1`). Must be compatible with `[php-dependencies]`. |
| `<vendor>/<package>@path+<rel-path>` | Path source, relative to the manifest. Used for local development of vendor packages. |

Example surface:

```mochi
import php "guzzlehttp/guzzle@^7.8" as guzzle
import php "ramsey/uuid" as uuid
import php "symfony/console@^7.0" as console

fn list_packages(): list<string> {
    let client = guzzle.new_client()
    let response = client.get("https://packagist.org/packages/list.json")
    return response.get_body().get_contents()
}

fn new_request_id(): string {
    return uuid.uuid4().to_string()
}
```

The `<alias>` introduces a Mochi namespace bound at the import site. Symbol lookup `<alias>.<item>` resolves to the synthesised `extern fn` declaration the bridge generated for `<vendor>/<package>`. PHP class names (PascalCase) are lowercased to `new_<classname>` for constructors and kept as-is for static factories. PHP method names (camelCase) are preserved; Mochi's naming convention is flexible enough to accept camelCase identifiers.

The `auto` modifier (accepted for `import go ... auto` and `import rust ... auto`) is admitted for `import php ... auto`. With `auto`, every public top-level class and function of the package is bound at file scope rather than under the alias namespace. Default is namespaced.

## Manifest: `[php-dependencies]`

This table is the user-facing dependency declaration. It uses Composer's `require` grammar:

```toml
[php-dependencies]
"guzzlehttp/guzzle" = "^7.8"
"symfony/console" = "^7.0"
"ramsey/uuid" = { version = "^4.7", suggest = ["ext-uuid"] }
"doctrine/orm" = "^3.1"
"my-local-package" = { path = "../my-package" }
```

The grammar mirrors Composer's:

- A bare string is shorthand for a version constraint string.
- The table form admits `version`, `suggest` (for optional extensions), and `path` (for local development).
- Cyclic dependencies are rejected at lock time (same rule Composer enforces).

The user does not write a `composer.json` for their dependencies. The bridge synthesises the vendor sandbox `composer.json` at build time from the lockfile data. The user's own package's `composer.json` is emitted by `TargetPhpLibrary` for the publish direction.

## Manifest: `[php]`

```toml
[php]
php-version = "8.4"
build-tags = []
```

| Key | Default | Meaning |
|-----|---------|---------|
| `php-version` | `"8.4"` | PHP version floor. The bridge targets PHP 8.4 (MEP-55's floor). `"8.1"` is the minimum accepted value. |
| `build-tags` | `[]` | Future extension; empty in v1. Reserved for PHP platform-specific compilation paths. |

The `monomorphise` key is reserved for future PHP generic support (PHPDoc `@template` expansion); empty in v1 because PHP's generics are PHPDoc-only and not runtime-observable by the Reflection API.

## Manifest: `[php.async]`

```toml
[php.async]
enabled = false
event-loop = "react"
```

| Key | Default | Meaning |
|-----|---------|---------|
| `enabled` | `false` | Whether to inject a ReactPHP or RevoltPHP event-loop dependency into the vendor sandbox. Off by default; most PHP packages are synchronous. |
| `event-loop` | `"react"` | Which event-loop library to inject: `"react"` for `react/event-loop ^3.0` or `"revolt"` for `revolt/event-loop ^1.0`. ReactPHP is the default because it has wider ecosystem adoption (see [[08-async-bridge]] §2). |

When `enabled = true`, the bridge adds the chosen event-loop package to the vendor sandbox and enables the async glue layer in the emitted PHP stubs. The async opt-in is scoped to the entire package, not per-import.

## Manifest: `[php.publish]`

```toml
[php.publish]
packagist-name = "my-vendor/my-package"
packagist-description = "A useful Mochi-compiled PHP library."
license = "MIT"
homepage = "https://github.com/example/my-mochi-lib"
keywords = ["mochi", "php", "library"]
psr4-namespace = "MyVendor\\MyPackage\\"
authors = [{ name = "Mochi User", email = "user@example.com" }]
```

| Key | Default | Meaning |
|-----|---------|---------|
| `packagist-name` | none | REQUIRED for `mochi pkg publish --to=packagist`. The Composer package name in `<vendor>/<package>` form. |
| `psr4-namespace` | derived | PHP root namespace for the PSR-4 `src/` tree. Derived from `packagist-name` if not set: `my-vendor/my-package` becomes `MyVendor\\MyPackage\\`. |
| `packagist-description` | package description | Short description written into `composer.json`. Defaults to `[package].description` from `mochi.toml`. |
| `license` | package license | SPDX expression. Written into `composer.json` and the `LICENSE` file. Defaults to `[package].license`. |
| `homepage` | package homepage | Written into `composer.json`. |
| `keywords` | `[]` | Written into `composer.json` as the `keywords` array. |
| `authors` | package authors | Written into `composer.json` as the `authors` array. |

This table is consulted only when `mochi pkg publish --to=packagist` is invoked.

## Manifest: `[php.capabilities]`

```toml
[php.capabilities]
net = false
fs = false
db = false
```

These capability flags are a refinement of MEP-57's `[capabilities]` table. The bridge walks the PHP dep graph at lock time, applies a static capability heuristic (net: packages that ship HTTP client classes; fs: packages that open files; db: packages that depend on PDO or Doctrine), and asserts that the heuristic result is a subset of the user's declaration.

| Flag | Meaning |
|------|---------|
| `net` | Any reachable package opens network sockets (GuzzleHTTP, Symfony HTTP Client, Guzzle). |
| `fs` | Any reachable package reads or writes files (Flysystem, league/flysystem). |
| `db` | Any reachable package opens database connections (Doctrine ORM, Doctrine DBAL, PDO wrappers). |

## CLI surface

The `mochi pkg` subcommand gains four new operations.

### `mochi pkg add php <vendor>/<package>[@<semver>]`

```
$ mochi pkg add php guzzlehttp/guzzle@^7.8
Added "guzzlehttp/guzzle" = "^7.8" to [php-dependencies]
Running mochi pkg lock ...
Resolved 12 PHP packages (guzzlehttp/guzzle + 11 transitive)
Wrote mochi.lock (+12 [[php-package]] entries)
```

Equivalent to manually editing `mochi.toml` plus running `mochi pkg lock`. Idempotent if the entry already exists at a compatible version.

### `mochi pkg lock`

Walks `[php-dependencies]`, queries the Packagist v2 sparse API for resolution, downloads each dist zip to the content-addressed cache, runs the reflection CLI on each package, synthesises the PHP glue stubs and Mochi extern declarations, and writes a `[[php-package]]` entry per dep into `mochi.lock`.

The lock also recursively resolves transitive dependencies declared in each package's `composer.json`. MEP-75 delegates transitive resolution to Composer's embedded solver (run via `composer update --no-install --dry-run --format=json` in a temp workspace) rather than re-implementing Composer's resolver in Go.

### `mochi pkg lock --check`

Reads `mochi.lock`, re-verifies `dist-sha256` and `reflection-sha256` for every `[[php-package]]` entry, and exits non-zero on any mismatch. This is the CI-enforced reproducibility gate.

### `mochi pkg publish --to=packagist [--dry-run]`

- Builds the package via `Driver.Build` with `target = TargetPhpLibrary, LibraryMode = true`.
- Validates the emitted `composer.json` schema.
- Runs `php -l` on all emitted PHP files to confirm syntax validity.
- GPG-signs and pushes the git tag.
- Produces the dist zip.
- Attaches the Sigstore attestation (via `actions/attest-build-provenance@v1` in CI).
- Pings the Packagist Update API (or relies on the GitHub App webhook if registered).

The `--dry-run` flag skips the tag push, API ping, and attestation upload.

### `mochi pkg sync php`

Re-runs the reflection CLI and shim synthesiser from the existing `mochi.lock` without re-resolving versions. Used after manual edits to the synthesised shim files or after a bridge upgrade that changes the shim format.

## Per-import alias resolution

The alias `<alias>` introduced by `import php "<spec>" as <alias>` participates in normal Mochi name resolution. The bridge generates a shim file at `<workdir>/php_shims/<vendor>_<package>/shim.mochi` containing a corpus of `extern fn` and `extern type` declarations:

```mochi
extern type Client
extern type Response

extern fn new_client(): Client from php "GuzzleHttp\Client::__construct"
extern fn client_get(c: Client, url: string): Response from php "GuzzleHttp\Client::get"
extern fn response_get_body(r: Response): string from php "GuzzleHttp\Psr7\Response::getBody"
```

The import `import php "guzzlehttp/guzzle" as guzzle` becomes (post-resolution) `import "./php_shims/guzzlehttp_guzzle/shim.mochi" as guzzle`. The shim is read by the parser exactly as a hand-written `.mochi` file would be.

The shim is regenerated on every `mochi pkg lock` and is gitignored by default. Users who need to override a synthesised binding do so with a `from php "..." custom` annotation that survives `mochi pkg sync php` regeneration.

## Cross-references

- [[02-design-philosophy]] for the rationale.
- [[04-packagist-ingest]] for how the package surface is fetched and reflected.
- [[05-type-mapping]] for the closed translation table the shim file uses.
- [[06-composer-publish-flow]] for the `mochi pkg publish` path.
- [MEP-75 §4](/docs/mep/mep-0075#4-surface-syntax-import-php) for the normative syntax.
- [MEP-57](/docs/mep/mep-0057) for the broader `mochi.toml` + `mochi.lock` model this extends.
- [MEP-55](/docs/mep/mep-0055) for the PHP lowering pipeline this bridge targets.
