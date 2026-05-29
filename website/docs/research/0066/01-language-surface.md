---
title: "01. Language surface"
sidebar_position: 2
sidebar_label: "01. Language surface"
description: "The `import erlang \"<package>@<semver>\" as <alias>` import form, the `[erlang-dependencies]` / `[erlang]` / `[erlang.publish]` / `[erlang.capabilities]` manifest tables, the CLI subcommands (`mochi pkg add erlang`, `mochi pkg lock`, `mochi pkg publish --to=hex.pm`, `mochi pkg sync erlang`), and the per-import alias and sub-namespace resolution rule."
---

# 01. Language surface

This note covers the user-visible surface MEP-66 introduces: the import syntax, the manifest tables, and the CLI subcommands. Everything below is observable through `mochi --help` and `mochi.toml` schema validation; the user does not need to read the rest of the bundle to use the bridge.

## Import syntax

The Mochi grammar's `ImportStmt` production (MEP-1) accepts a `Lang` token between `import` and the string literal:

```
ImportStmt := "import" Lang? StringLit "as" Ident ("auto")?
Lang := "go" | "python" | "typescript" | "rust" | "ruby" | "erlang"
```

MEP-66 adds `erlang` as the sixth alternative. The string literal is one of:

| Form | Resolution |
|------|------------|
| `<package>` | Bare name. Resolves through `[erlang-dependencies]` plus `mochi.lock`. |
| `<package>@<constraint>` | Explicit Hex.pm version constraint (`~> 2.12`, `>= 1.0.0 and < 2.0.0`). |
| `<package>@git+<url>#<ref>` | Git source, pinned to ref. |
| `<package>@path+<rel-path>` | Path source, relative to the manifest. |
| `<app>.<module>@<package>@<constraint>` | Import a specific Erlang module from a multi-module OTP application. |

Example surface:

```mochi
import erlang "cowboy@~> 2.12" as cowboy
import erlang "hackney@~> 1.20" as hackney
import erlang "jose@~> 1.11" as jose

fn fetch(url: string): result<bytes, string> {
    let result = hackney.get(url, [], <<>>, [])
    match result {
        ok(status, _headers, ref) -> {
            let body = hackney.body(ref)
            return ok(body)
        }
        err(reason) -> return err(reason)
    }
}
```

The `<alias>` introduces a Mochi namespace. For multi-module OTP applications, each Erlang module becomes a sub-namespace: `cowboy.cowboy_req.reply(...)`, `cowboy.cowboy_router.compile(...)`. The user can also import a single module directly:

```mochi
import erlang "cowboy.cowboy_req@cowboy@~> 2.12" as cowboy_req

fn reply_ok(req: bytes, body: bytes): bytes {
    return cowboy_req.reply(200, #{}, body, req)
}
```

The `auto` modifier flattens the top-level namespace, bringing every translated symbol directly into the file scope rather than under the alias.

## Manifest: `[erlang-dependencies]`

```toml
[erlang-dependencies]
cowboy = "~> 2.12"
hackney = "~> 1.20"
jose = "~> 1.11"
telemetry = ">= 1.2.0 and < 2.0.0"
poolboy = { version = "~> 1.5", override = true }
my-local-app = { path = "../my_erlang_app" }
my-git-app = { git = "https://github.com/example/my_app", ref = "v2.0.0" }
```

The grammar mirrors Hex.pm's `rebar.config` deps syntax:

- A bare string is shorthand for a version constraint.
- The table form admits `version`, `override` (resolves version conflicts by forcing this version), `path`, and `git`/`ref`.
- Version constraints use Hex.pm's operators: `~>` (pessimistic), `>=`, `<=`, `>`, `<`, `==`, `!=`, and `and`/`or` for compound expressions.

The user does not write a separate `rebar.config`. The bridge synthesises the `rebar.config` at `mochi pkg lock` time, populating `{deps, [...]}` from `[erlang-dependencies]` and pinning the exact resolved version from `mochi.lock`.

## Manifest: `[erlang]`

```toml
[erlang]
otp-version = "27"
rebar3-version = "~> 3.23"
elixir-compat = false
port-timeout-ms = 30000
```

| Key | Default | Meaning |
|-----|---------|---------|
| `otp-version` | `"25"` | Minimum OTP version. Written to `rebar.config` as `{minimum_otp_vsn, "25"}`. OTP 25 is the floor because it ships the stable `Dbgi` chunk format and modern `erl_interface`. |
| `rebar3-version` | `"~> 3.20"` | rebar3 version constraint. Bridge checks `rebar3 --version` and fails if the constraint is not met. |
| `elixir-compat` | `false` | When `true`, the bridge also accepts Elixir-flavoured packages from Hex.pm that expose Erlang-callable module APIs. |
| `port-timeout-ms` | `30000` | Timeout for Port round-trip calls, in milliseconds. Increase for slow OTP operations (e.g., large Dialyzer runs). |

## Manifest: `[erlang.publish]`

```toml
[erlang.publish]
app-name = "my_mochi_app"
description = "A Mochi package published as an Erlang OTP application."
version = "1.0.0"
licenses = ["Apache-2.0"]
links = { "GitHub" = "https://github.com/example/my_mochi_app" }
maintainers = ["tamnd"]
files = ["ebin/**/*.beam", "src/**/*.erl", "include/**/*.hrl", "priv/**"]
build-tools = ["rebar3"]
```

All fields mirror the Hex.pm package metadata API. When present, `mochi pkg publish --to=hex.pm` reads these fields to populate the `rebar.config` hex metadata block. If the package already ships a hand-written `rebar.config` with a `{hex, [...]}` section, the generated one is skipped and the hand-written one is used instead.

## Manifest: `[erlang.capabilities]`

```toml
[erlang.capabilities]
net = true
fs = false
proc = false
dist = false
```

These flags mirror MEP-57's capability model, scoped to the Erlang bridge. The bridge audits the imported packages at lock time and asserts that the union of capability marks across all reachable OTP applications is a subset of the declared capabilities.

| Capability | Meaning |
|-----------|---------|
| `net` | Any reachable application opens TCP/UDP/Unix sockets or initiates network I/O. |
| `fs` | Any reachable application reads or writes files via `file:read/2`, `file:write/3`, or similar. |
| `proc` | Any reachable application spawns OS processes via `os:cmd/1` or `open_port({spawn,...})`. |
| `dist` | Any reachable application participates in Erlang distributed node protocol via `net_kernel` or `erlang:send/2` to a remote `{Name, Node}`. |

The `dist` capability is the gate for phase 13. Declaring `dist = true` enables the distributed Erlang bridge and registers the Mochi binary as a named Erlang node.

## CLI surface

### `mochi pkg add erlang <package>[@<constraint>]`

```
$ mochi pkg add erlang cowboy@~>2.12
Added cowboy = "~> 2.12" to [erlang-dependencies]
Running mochi pkg lock ...
Resolved 4 Erlang packages (cowboy + 3 transitive: cowlib, ranch, ssl_verify_fun)
Wrote mochi.lock (+4 [[erlang-package]] entries)
```

### `mochi pkg lock`

Walks `[erlang-dependencies]`, queries the Hex.pm HTTP API v2 for resolution, downloads each `.tar.gz` to the content-addressed cache, runs BEAM ingest + EDoc fallback, synthesises the shims, and writes `[[erlang-package]]` entries.

### `mochi pkg lock --check`

Reads `mochi.lock`, recomputes every `outer-sha256`, `inner-sha256`, `inner-sha512`, `beam-ingest-sha256`, and `shim-sha256`, and exits non-zero on any mismatch.

### `mochi pkg publish --to=hex.pm [--dry-run]`

Builds the package as an OTP application via `TargetErlangPort`, compiles with `rebar3 compile`, obtains an OIDC token, presents it to Hex.pm's trusted-publishing endpoint, and uploads. `--dry-run` skips upload.

### `mochi pkg sync erlang`

Re-runs the shim synthesiser from the existing `mochi.lock` without re-resolving versions.

## Per-import alias resolution

The alias `<alias>` from `import erlang "<spec>" as <alias>` binds against a synthesised shim file at `<workdir>/erlang_shims/<app>/shim.mochi`:

```mochi
extern type Pid
extern type Reference
extern type ErlPort

extern fn hackney__get(
    url: string,
    headers: list<[string, string]>,
    body: bytes,
    options: list<bytes>
): result<[int, list<[string, string]>, Reference], string>
  from erlang "hackney:get/4"

extern fn hackney__body(ref: Reference): result<bytes, string>
  from erlang "hackney:body/1"
```

The `import erlang "hackney" as hackney` statement becomes (post-resolution) `import "./erlang_shims/hackney/shim.mochi" as hackney`. The synthesised shim file is read by the Mochi parser exactly as a hand-written `.mochi` file would be. The shim is gitignored by default and regenerated on every `mochi pkg lock`.

## Cross-references

- [[02-design-philosophy]] for the rationale behind the bridge architecture.
- [[04-beam-typespec-ingest]] for how the public surface is discovered from `.beam` files.
- [[05-type-mapping]] for the closed translation table.
- [[06-hex-publish-flow]] for the `mochi pkg publish` path.
- [MEP-66 §5](/docs/mep/mep-0066#5-surface-syntax-import-erlang) for the normative syntax.
- [MEP-57](/docs/mep/mep-0057) for the `mochi.toml` + `mochi.lock` model this extends.
