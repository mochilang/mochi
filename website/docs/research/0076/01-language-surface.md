---
title: "01. Language surface"
sidebar_position: 2
sidebar_label: "01. Language surface"
description: "The `import ruby \"<gem>@<semver>\" as <alias>` import form, the `[ruby-dependencies]` / `[ruby]` / `[ruby.publish]` manifest tables, the CLI subcommands, and per-import alias resolution."
---

# 01. Language surface

This note covers the user-visible surface MEP-76 introduces: the import syntax, the manifest tables, and the CLI subcommands. Everything described here is observable through `mochi --help` and `mochi.toml` schema validation. The rest of the bundle (design rationale, RBS ingest, type mapping) is not required reading to use the bridge.

## Import syntax

The Mochi grammar's `ImportStmt` production (MEP-1) accepts a `Lang` token between `import` and the string literal:

```
ImportStmt := "import" Lang? StringLit "as" Ident ("auto")?
Lang := "go" | "python" | "typescript" | "rust" | "ruby"
```

MEP-76 adds `ruby` as the fifth alternative. The string literal is one of:

| Form | Resolution |
|------|------------|
| `<gem-name>` | Bare name. Resolves through `[ruby-dependencies]` plus `mochi.lock`. The lockfile records the picked version. |
| `<gem-name>@<semver-req>` | Explicit constraint. Supports `~>`, `>=`, `<=`, `=`, `!=` operators per RubyGems version spec syntax. |
| `<gem-name>@git+<url>#<rev>` | Git source, pinned to a commit SHA, tag, or branch. |
| `<gem-name>@path+<rel-path>` | Path source, relative to the manifest. |

Representative examples:

```mochi
import ruby "nokogiri@^1.16" as nokogiri
import ruby "redis" as redis
import ruby "sinatra@~>3.2" as sinatra
```

All three forms are valid. The `^1.16` constraint uses Mochi's semver syntax and maps to `>= 1.16, < 2` in RubyGems terms. The bare `redis` form delegates fully to `[ruby-dependencies]` and `mochi.lock`.

## Mochi code example

A complete usage showing HTML parsing with nokogiri and key-value storage with redis:

```mochi
import ruby "nokogiri@^1.16" as nokogiri
import ruby "redis@~>5.0" as redis

fn scrape_title(html: string): string {
    let doc = nokogiri.HTML(html)
    let node = doc.at_css("title")
    if node == nil {
        return ""
    }
    return node.text()
}

fn cache_title(key: string, title: string): void {
    let client = redis.new("redis://127.0.0.1:6379")
    client.set(key, title)
    client.disconnect()
}
```

The `alias.method(args)` call convention is identical to any other Mochi import. The bridge synthesises `extern fn` declarations for every method visible in the gem's RBS signature (or YARD fallback), binding them under the `nokogiri` and `redis` namespaces.

## Manifest: `[ruby-dependencies]`

This table is the user-facing dependency declaration. It mirrors Bundler's `Gemfile` semantics but is expressed in TOML:

```toml
[ruby-dependencies]
nokogiri = "~> 1.16"
redis = ">= 5.0, < 6"
sinatra = "~> 3.2"
rack = { version = "~> 3.0", require = false }
devise = { git = "https://github.com/heartcombo/devise", branch = "main" }
my-local-gem = { path = "../my-gem" }
```

Grammar details:

- A bare string value is a version requirement string passed directly to Bundler.
- The table form admits `version`, `require`, `git`, `branch`, `ref`, `tag`, `path`, `platforms`, and `source`.
- `require = false` suppresses the auto-require that Bundler normally does; the bridge does a targeted `require 'gem-name'` in the shim regardless.
- Cyclic gem dependencies are rejected by Bundler at lock time.

The user does not write a `Gemfile` manually. The bridge synthesises one from `[ruby-dependencies]` at build time, with exact versions pinned from `mochi.lock`.

## Manifest: `[ruby]`

```toml
[ruby]
ruby-version = "3.3"
bundler-version = "2.5"
runtime = "cruby"
```

| Key | Default | Meaning |
|-----|---------|---------|
| `ruby-version` | `"3.3"` | Minimum Ruby version. Passed as `RUBY_VERSION` for bundler and as the `required_ruby_version` in generated gemspec. |
| `bundler-version` | `"2.5"` | Bundler version to invoke. Used by `mochi pkg lock` when running `bundle install`. |
| `runtime` | `"cruby"` | Ruby runtime. Only `"cruby"` is supported in phase 1. `"jruby"` and `"truffleruby"` are deferred. |

## Manifest: `[ruby.publish]`

This table is consulted only when the user runs `mochi pkg publish --to=rubygems.org`. It maps directly to gemspec fields:

```toml
[ruby.publish]
gem-name = "my-mochi-gem"
summary = "A short one-line summary of this gem."
description = "A longer description of what this gem does."
homepage = "https://github.com/example/my-mochi-gem"
licenses = ["MIT"]
authors = ["Alice Example <alice@example.com>"]

[ruby.publish.metadata]
source_code_uri = "https://github.com/example/my-mochi-gem"
changelog_uri = "https://github.com/example/my-mochi-gem/CHANGELOG.md"
rubygems_mfa_required = "true"
```

| Key | Gemspec field | Notes |
|-----|---------------|-------|
| `gem-name` | `spec.name` | Required. Must be unique on rubygems.org. |
| `summary` | `spec.summary` | Required. Max 80 chars. |
| `description` | `spec.description` | Optional. Defaults to `summary`. |
| `homepage` | `spec.homepage` | Optional. |
| `licenses` | `spec.licenses` | Array of SPDX identifiers. |
| `authors` | `spec.authors` | Array of strings. |
| `metadata` | `spec.metadata` | Freeform key-value pairs for rubygems.org display. |

The bridge generates the `.gemspec` file from this table at publish time. The user never edits the `.gemspec` by hand.

## Manifest: `[ruby.capabilities]`

```toml
[ruby.capabilities]
net = true
fs = false
native_ext = false
```

These flags work analogously to `[rust.capabilities]` in MEP-73. At lock time the bridge walks the gem dependency graph and checks each gem against a curated capability database. If the union of required capabilities exceeds the declared set, `mochi pkg lock` fails with a diagnostic:

```
error: gem `nokogiri 1.16.4` requires `native_ext` but
       [ruby.capabilities] native_ext = false
       Add `native_ext = true` to mochi.toml to allow this.
```

The three canonical Ruby capabilities are:

- `net`: gem opens TCP/UDP sockets, makes HTTP requests, or otherwise initiates network I/O.
- `fs`: gem reads or writes files outside the working directory.
- `native_ext`: gem includes a C extension (extconf.rb / mkmf). Many popular gems (nokogiri, pg, mysql2, sqlite3) require this.

## CLI surface

The `mochi pkg` subcommand gains four Ruby-specific operations:

### `mochi pkg add ruby <gem>[@<ver>]`

```
$ mochi pkg add ruby nokogiri@~>1.16
Added nokogiri = "~> 1.16" to [ruby-dependencies]
Running mochi pkg lock ...
Resolved 8 gems (nokogiri + 7 transitive)
Wrote mochi.lock (+8 [[ruby-package]] entries)
```

Equivalent to manually editing `mochi.toml` and running `mochi pkg lock`. Idempotent if the entry already exists at a compatible version.

### `mochi pkg lock`

Walks `[ruby-dependencies]`, invokes Bundler to resolve and pin all versions, downloads each gem to the content-addressed cache, extracts RBS signatures (or YARD fallback), synthesises `shim.mochi` + `shim.rb` for each gem, and writes a `[[ruby-package]]` entry per dep into `mochi.lock`.

### `mochi pkg publish --to=rubygems.org [--dry-run]`

- Builds the package via `Driver.Build`.
- Generates the `.gemspec` from `[ruby.publish]`.
- Obtains an OIDC token from the CI environment (GitHub Actions `id-token: write` or equivalent).
- Presents the token plus the gem payload to rubygems.org's trusted-publishing endpoint.
- Records the Sigstore log entry.

The `--dry-run` flag skips upload; the signing flow is still exercised. See [[02-design-philosophy]] §5 for the rationale for OIDC-only publish.

### `mochi pkg sync ruby`

Re-runs the shim synthesiser from the existing `mochi.lock`, without re-resolving versions. Used after a bridge upgrade that changes the shim format, or after a manual override to shim files.

## Shim file mechanics

For each gem, the bridge generates two files under `<workdir>/ruby_wrap/<gem>/`:

**`shim.mochi`** contains `extern fn` declarations for every method the bridge translates from the gem's RBS surface:

```mochi
// Generated by mochi pkg lock. Do not edit.
// Source: nokogiri 1.16.4 (RBS from bundled sig/)

extern type HtmlDocument
extern type NodeSet
extern fn html_parse(input: string): HtmlDocument from ruby "Nokogiri::HTML"
extern fn at_css(doc: HtmlDocument, selector: string): HtmlDocument? from ruby "Nokogiri::XML::Node#at_css"
extern fn node_text(node: HtmlDocument): string from ruby "Nokogiri::XML::Node#text"
```

**`shim.rb`** is the companion Ruby file loaded by the runtime. It requires the gem and provides thin method-dispatch wrappers that the Mochi runtime calls via the Ruby bridge:

```ruby
# Generated by mochi pkg lock. Do not edit.
require 'nokogiri'

module MochiShim
  module Nokogiri
    def self.html_parse(input)
      ::Nokogiri::HTML(input)
    end

    def self.at_css(doc, selector)
      doc.at_css(selector)
    end

    def self.node_text(node)
      node.text
    end
  end
end
```

The import `import ruby "nokogiri" as nokogiri` resolves post-synthesis to `import "./ruby_wrap/nokogiri/shim.mochi" as nokogiri`. The shim files are gitignored by default and regenerated on every `mochi pkg lock`.

## The `auto` modifier

The `auto` modifier works the same as for `import go ... auto` and `import rust ... auto`:

```mochi
import ruby "nokogiri@^1.16" as nokogiri auto
```

With `auto`, every public top-level item in the shim is bound at file scope rather than under the `nokogiri.` namespace. Default is namespaced (`alias`-prefixed lookup); `auto` is opt-in and carries the usual shadowing caution.

## Per-import alias resolution

The alias participates in normal Mochi name resolution. Symbol lookup `<alias>.<item>` resolves to the synthesised `extern fn` in `shim.mochi`. Users who need to override a generated binding should use the `custom` modifier to keep the override stable across `mochi pkg sync ruby` runs:

```mochi
import ruby "redis" as redis_auto
extern fn redis_get(key: string): string? from ruby "Redis#get" custom
```

The `custom` modifier prevents `mochi pkg sync ruby` from overwriting the declaration.

## Cross-references

- [[02-design-philosophy]] for the rationale behind every decision on this surface.
- [[03-prior-art]] for how prior Ruby interop compares to this shim-based approach.
- [[04-rbs-yard-ingest]] for how the public surface is discovered and encoded into `shim.mochi`.
- [MEP-76 §4](/docs/mep/mep-0076#4-surface-syntax-import-ruby) for the normative syntax.
- [MEP-57](/docs/mep/mep-0057) for the broader `mochi.toml` + `mochi.lock` model this extends.
