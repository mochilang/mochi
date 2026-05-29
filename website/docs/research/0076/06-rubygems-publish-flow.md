---
title: "06. RubyGems publish flow"
sidebar_position: 7
sidebar_label: "06. Publish"
description: "The .gem tarball format, gemspec metadata fields, the compact index protocol used by Bundler, the rubygems.org upload API, and how mochi.toml [ruby.publish] maps to gemspec fields."
---

# 06. RubyGems publish flow

This note documents the Mochi-as-Ruby-gem publish path: how `mochi pkg publish --to=rubygems.org` lowers a Mochi package to a publishable Ruby gem and uploads it.

## The `.gem` file format

A `.gem` file is a TAR archive (not gzipped at the outer layer) containing exactly three members:

| Member | Description |
|--------|-------------|
| `metadata.gz` | The gemspec, marshalled via Ruby's `Marshal.dump` and then gzip-compressed. |
| `data.tar.gz` | All gem files (lib/, bin/, etc.) in a gzipped TAR. |
| `checksums.yaml.gz` | SHA-256, SHA-512 checksums for the two members above, gzip-compressed. |

The bridge produces the `.gem` by invoking `gem build <gemspec>` in a temporary directory containing the lowered Ruby sources. The `gem` binary must be present in `PATH`; the bridge errors early if it is not found.

## Gemspec fields

### Required

| Field | Description |
|-------|-------------|
| `name` | The gem name. Must match `[a-zA-Z0-9_-]+`. |
| `version` | Semantic version string. |
| `summary` | Short one-line description (< 80 chars). |
| `authors` | Array of author strings. |
| `files` | Array of files included in the gem. The bridge enumerates `lib/**/*`, `bin/*` if present, and the gemspec itself. |
| `require_paths` | Defaults to `["lib"]`. |
| `licenses` | SPDX licence identifier array. |

### Optional but important

| Field | Description |
|-------|-------------|
| `description` | Long description. Shown on rubygems.org gem page. |
| `homepage` | Project homepage URL. |
| `metadata` | Hash of arbitrary metadata (see `source_code_uri`, `changelog_uri`, etc.). |
| `add_runtime_dependency` | Declares a runtime gem dependency with version constraints. |
| `add_development_dependency` | Declares a development-only gem dependency. |

A minimal generated gemspec:

```ruby
Gem::Specification.new do |spec|
  spec.name          = "mochi-example"
  spec.version       = "0.1.0"
  spec.summary       = "An example Mochi package, published as a Ruby gem."
  spec.description   = "Longer description here."
  spec.authors       = ["Tam Nguyen Duc"]
  spec.licenses      = ["Apache-2.0"]
  spec.homepage      = "https://github.com/example/mochi-example"
  spec.require_paths = ["lib"]
  spec.files         = Dir["lib/**/*", "*.gemspec"]

  spec.metadata["source_code_uri"] = "https://github.com/example/mochi-example"

  spec.add_runtime_dependency "mochi-runtime", "~> 0.6"
end
```

## `mochi.toml` to gemspec mapping

| `mochi.toml` field | Gemspec field |
|-------------------|--------------|
| `[package] name` | `spec.name` |
| `[package] version` | `spec.version` |
| `[package] description` (first line) | `spec.summary` |
| `[package] description` (full) | `spec.description` |
| `[package] authors` | `spec.authors` |
| `[package] license` | `spec.licenses` (wrapped in an array) |
| `[ruby.publish] homepage` | `spec.homepage` |
| `[ruby.publish] metadata.source_code_uri` | `spec.metadata["source_code_uri"]` |
| `[ruby.publish] metadata.changelog_uri` | `spec.metadata["changelog_uri"]` |
| `[ruby-dependencies]` entries | `spec.add_runtime_dependency` calls |

Example `mochi.toml` section:

```toml
[package]
name = "mochi-example"
version = "0.1.0"
description = "An example Mochi package, published as a Ruby gem."
authors = ["Tam Nguyen Duc <tamnd87@gmail.com>"]
license = "Apache-2.0"

[ruby.publish]
homepage = "https://github.com/example/mochi-example"

[ruby.publish.metadata]
source_code_uri = "https://github.com/example/mochi-example"
changelog_uri   = "https://github.com/example/mochi-example/blob/main/CHANGELOG.md"

[ruby-dependencies]
mochi-runtime = "~> 0.6"
activesupport = ">= 7.0, < 8"
```

## RubyGems compact index protocol

Bundler uses the compact index protocol to resolve gem versions. The compact index is served at `https://index.rubygems.org/`. Key endpoints:

| Endpoint | Format |
|----------|--------|
| `GET /versions` | Newline-separated list of `<gem> <checksum>` pairs; tells Bundler which gems have changed since the last sync. |
| `GET /info/<gem>` | Compact info file; one line per version. |
| `GET /names` | Alphabetically sorted list of all gem names. |

The `GET /info/<gem>` format (one line per version):

```
---
1.0.0 |checksum:sha256hex,ruby:>= 2.5,activesupport:>= 6.0
1.1.0 |checksum:sha256hex,ruby:>= 2.7,activesupport:~> 7.0
```

Each line is: `<version> <platform>|<checksum>,<dep1>:<constraint>,<dep2>:<constraint>,...`

The bridge parses this format in `package3/ruby/index/compact.go` to resolve transitive dependencies at `mochi pkg lock` time without invoking Bundler itself. The parsed entries are written to `mochi.lock` under the `[ruby-packages]` section.

## Gem version constraint syntax

Bundler and RubyGems use their own version constraint operators:

| Operator | Meaning | mochi.lock representation |
|----------|---------|--------------------------|
| `~> 1.2` | Pessimistic: `>= 1.2, < 2.0` (patch) | `^1.2` |
| `~> 1.2.3` | Pessimistic: `>= 1.2.3, < 1.3.0` (minor) | `~1.2.3` |
| `>= 1.0` | Greater-or-equal | `>= 1.0` |
| `<= 2.0` | Less-or-equal | `<= 2.0` |
| `> 1.0` | Strictly greater | `> 1.0` |
| `< 2.0` | Strictly less | `< 2.0` |
| `= 1.2.3` | Exact pin | `= 1.2.3` |

The bridge converts between gem version constraint syntax and the mochi.lock representation during lock resolution.

## The `rubygems.org` upload API

The rubygems.org upload endpoint:

```
POST https://rubygems.org/api/v1/gems
Content-Type: application/octet-stream
Authorization: <api-key-or-oidc>

<binary .gem content>
```

The server validates the gemspec fields, deduplicates against the existing index, stores the `.gem` at `https://rubygems.org/gems/<name>-<version>.gem`, and updates the compact index.

The bridge wraps `gem push` rather than reimplementing the upload protocol directly (the same pattern MEP-73 uses for `cargo publish` and MEP-57 uses for npm):

```sh
gem push mochi-example-0.1.0.gem \
  --key <api-key-or-oidc-token> \
  --host https://rubygems.org
```

The `gem push` binary handles multipart encoding, retry logic, and error reporting. The bridge passes the auth credential via the `GEM_HOST_API_KEY` environment variable (for the legacy API key path) or via the OIDC token exchange described in [[07-oidc-trusted-publishing]].

## The `mochi-runtime` gem

The `mochi-runtime` gem (first published during MEP-56 phases 22 and 23) is already live on rubygems.org. It provides the Ruby-side runtime support for Mochi-generated Ruby code: the base classes, the record marshalling helpers, and the type-check shims. Every Mochi-published gem declares `mochi-runtime` as a runtime dependency.

The bridge pins the minimum `mochi-runtime` version based on which Mochi features the package uses. A package that uses MEP-76 Ruby imports requires `mochi-runtime >= 0.6` (the version that adds the Ruby bridge support classes).

## Publish gate

Before upload, the bridge runs a smoke-test install:

```sh
gem install mochi-example-0.1.0.gem --local
ruby -e 'require "mochi/runtime"; require "mochi_example"'
```

Both commands must exit 0. A failure aborts the publish with a diagnostic message. The gate runs in a temporary `GEM_HOME` to avoid polluting the system gems.

`--dry-run` mode runs `gem build` and the gemspec validation but skips the push. `--mock-registry=<local-geminabox>` points to a local registry (geminabox or Gemstash) for offline testing:

```sh
mochi pkg publish --to=rubygems.org \
  --dry-run \
  --mock-registry=http://localhost:9292
```

## Metadata validation

Before upload the bridge validates:

- `[package].name` matches gem name regex `[a-zA-Z0-9_\-]+`.
- `[package].version` parses as a valid gem version (dot-separated integers with optional pre-release suffix).
- `[package].license` parses as a recognised SPDX expression.
- `[package].description` is non-empty and under 10,000 characters.
- `[ruby.publish].homepage` is a valid URL if present.

A validation failure exits before the build step.

## Cross-references

- [[01-language-surface]] for the `mochi pkg publish` CLI surface.
- [[07-oidc-trusted-publishing]] for the trusted-publishing OIDC path.
- [[09-bundler-lockfile]] for the compact-index parsing that drives lock resolution.
- [MEP-56 phase 22](/docs/implementation/0056/phase-22) for the initial `mochi-runtime` gem publish.
- [MEP-76 §5](/docs/mep/mep-0076#5-cli-surface) for the normative CLI surface.
- [MEP-73 §6](/docs/research/0073/06-cargo-publish-flow) for the parallel Rust publish flow.
