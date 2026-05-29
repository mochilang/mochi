---
title: "08. Native C extension gems"
sidebar_position: 9
sidebar_label: "08. Native extensions"
description: "How the bridge handles gems with native C extensions (nokogiri, pg, sqlite3, mysql2, msgpack, etc.), the detection strategy, the pre-built binary gem path, and why pure-Ruby fallback is preferred."
---

# 08. Native C extension gems

This note documents how the MEP-76 bridge handles gems that include native C (or C++) extensions. These gems require compilation at install time and introduce platform-specific artifacts that complicate bridging.

## What a native extension gem is

A native extension gem contains an `ext/` directory with an `extconf.rb` script and one or more `.c` or `.cpp` source files. When the gem is installed via `gem install` or `bundle install`, Ruby runs `extconf.rb` (which invokes `mkmf` to produce a `Makefile`), then compiles the C sources against the current Ruby headers. The result is a `.so` (Linux), `.dylib` (macOS), or `.dll` (Windows) that Ruby loads via `require`.

Common native extension gems:

| Gem | C dependency | Purpose |
|-----|-------------|---------|
| `nokogiri` | libxml2 + libxslt | HTML/XML parsing |
| `pg` | libpq (PostgreSQL client) | PostgreSQL adapter |
| `sqlite3` | SQLite3 C library | SQLite adapter |
| `mysql2` | libmysqlclient | MySQL adapter |
| `msgpack` | msgpack-c | MessagePack serialisation |
| `fast_jsonparser` | simdjson | High-speed JSON parsing |
| `oj` | (vendored C) | Optimised JSON |
| `bcrypt-ruby` | OpenBSD bcrypt | Password hashing |

## Detection strategy

The bridge cannot run `gem install` at lock time (that would require a Ruby runtime, a native toolchain, and network access). Instead it detects native extensions by inspecting the gemspec field `spec.extensions`:

```ruby
# A native extension gem has a non-empty extensions array:
spec.extensions = ["ext/nokogiri/extconf.rb"]
```

The bridge fetches the gemspec from the compact index at lock time. If `extensions` is non-empty, the gem is flagged as native. The bridge then applies the three-strategy resolution order described below.

The detection happens in `package3/ruby/index/native.go` and runs as part of the `mochi pkg lock` dependency resolution pass.

## Three resolution strategies

### Strategy 1: pre-built binary gem

RubyGems.org hosts platform-specific binary gems alongside the source gem. The binary gem has a platform suffix in its filename:

```
nokogiri-1.16.2-x86_64-linux.gem
nokogiri-1.16.2-aarch64-linux.gem
nokogiri-1.16.2-x86_64-darwin.gem
nokogiri-1.16.2-arm64-darwin.gem
nokogiri-1.16.2-x64-mingw-ucrt.gem
```

The binary gem's `spec.platform` field is set to the target platform string. The bridge selects the binary gem matching the host platform by querying the compact index for the platform-qualified version entry:

```
# compact index info line for a binary version:
1.16.2-x86_64-linux |checksum:sha256hex,ruby:>= 2.7
```

The platform selection logic in `package3/ruby/index/compact.go` maps Go's `runtime.GOOS` / `runtime.GOARCH` to the RubyGems platform string:

| Go GOOS/GOARCH | RubyGems platform |
|---------------|------------------|
| `linux/amd64` | `x86_64-linux` |
| `linux/arm64` | `aarch64-linux` |
| `darwin/amd64` | `x86_64-darwin` |
| `darwin/arm64` | `arm64-darwin` |
| `windows/amd64` | `x64-mingw-ucrt` |

MEP-76 phase 12 implements binary gem selection.

### Strategy 2: pure-Ruby alternative

Many native extension gems have a pure-Ruby fallback gem. The bridge maintains a curated mapping in `package3/ruby/index/native.go`:

| Native gem | Pure-Ruby alternative |
|-----------|----------------------|
| `json` (C ext) | `json_pure` |
| `msgpack` | `msgpack-pure` (community) |
| `bcrypt-ruby` | (no pure alternative; skipped) |
| `psych` (YAML) | `psych` ships pure-Ruby fallback in Ruby stdlib |

When no binary gem is available for the current platform but a pure-Ruby alternative exists, the bridge substitutes the alternative and emits a warning:

```
WARN: no binary gem for pg@1.5.8 on arm64-darwin
  Substituting pure-Ruby alternative: pg_pure@0.2.1
  Note: pg_pure has known performance limitations; binary gem preferred.
```

The substitution is recorded in `mochi.lock` so that subsequent `mochi pkg lock` runs are deterministic.

### Strategy 3: skip with SkipReport

If no binary gem is available and no pure-Ruby alternative exists, the gem is skipped with a `NativeExtensionSkip` report:

```
SKIPPED: mysql2@0.5.6 (native extension, no binary gem for arm64-darwin, no pure alternative)
  To enable source compilation, add to mochi.toml:
    [ruby.native]
    allow_source_build = true
```

The skip report is written to `<workdir>/ruby_wrap/<gem>/skip_report.txt` alongside any type-mapping skips.

## The `[ruby.native]` manifest section

```toml
[ruby.native]
# Default: prefer binary gem only; skip if no binary available.
allow_source_build = false

# Platforms to resolve binary gems for.
# Default: auto-detect from build host.
platforms = ["x86_64-linux", "arm64-darwin", "x64-mingw-ucrt"]
```

When `allow_source_build = true`, the bridge attempts source compilation using the system C toolchain (`cc`, `make`). This requires:

- Ruby headers (e.g., `ruby-dev` package on Debian/Ubuntu)
- The native library development headers (e.g., `libpq-dev` for pg, `libsqlite3-dev` for sqlite3)
- `make` and a C compiler

Source builds are disabled by default because they are fragile in containerised CI environments and can produce non-reproducible binaries. The preferred path is always a binary gem.

## Nokogiri case study

nokogiri is the most widely used native extension gem. Its binary gem situation as of 2026:

```
nokogiri-1.16.2               # source gem (requires libxml2, libxslt to compile)
nokogiri-1.16.2-x86_64-linux  # pre-built, bundles libxml2 + libxslt
nokogiri-1.16.2-aarch64-linux # pre-built
nokogiri-1.16.2-x86_64-darwin # pre-built
nokogiri-1.16.2-arm64-darwin  # pre-built
nokogiri-1.16.2-x64-mingw-ucrt# pre-built (Windows)
nokogiri-1.16.2-java          # JRuby variant (not bridged in MEP-76 v1)
```

Binary gems for nokogiri bundle the C libraries statically, so no system libxml2/libxslt is required. The bridge selects the platform-specific binary gem automatically.

## RBS coverage for native extension gems

RBS availability varies significantly across native gems:

| Gem | RBS status | Source |
|-----|-----------|--------|
| `nokogiri` | Bundled in gem (added in 1.14) | `lib/nokogiri/**/*.rbs` inside the gem |
| `pg` | Partial (via gem_rbs_collection) | `gem_rbs_collection` repo; incomplete as of 2026 |
| `sqlite3` | Partial YARD docs only | No `.rbs` files; the bridge converts YARD via rbs-yard (best-effort) |
| `mysql2` | None | No RBS; bridge emits a SkipReport for all mysql2 items |
| `msgpack` | Bundled in gem (added in 1.7) | Good coverage of the core serialisation API |
| `oj` | None | No RBS; SkipReport generated |
| `bcrypt-ruby` | None | SkipReport generated; user must hand-author bindings |

For gems with no RBS coverage, the bridge generates a SkipReport for the entire gem. The gem may still be installed (if a binary is available), but no Mochi bindings are emitted.

## Cross-references

- [[04-rbs-ingest]] for how the bridge ingests RBS files from installed gems.
- [[05-type-mapping]] for how individual method signatures are translated.
- [[09-bundler-lockfile]] for how binary gem platform selection is recorded in `mochi.lock`.
- [[12-risks-and-alternatives]] for the rejected strategy of always source-compiling native extensions.
- [MEP-76 §8](/docs/mep/mep-0076#8-native-extensions) for the normative native extension handling specification.
