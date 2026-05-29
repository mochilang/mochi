---
title: "06. Hex.pm publish flow"
sidebar_position: 7
sidebar_label: "06. Hex.pm publish flow"
description: "The Hex.pm HTTP API v2 package upload protocol, the outer/inner tarball structure and three-hash verification scheme, the .app.src metadata requirements, the TargetErlangPort build output, and the rebar3 hex publish integration path."
---

# 06. Hex.pm publish flow

## Hex.pm HTTP API v2

Hex.pm's API v2 (current as of 2026) uses JSON over HTTPS for metadata and raw bytes for package tarballs. The publish endpoint is:

```
POST https://hex.pm/api/packages/<name>/releases
Content-Type: application/octet-stream
Authorization: <api_key_or_oidc_token>
```

The request body is the package tarball (described below). The response is JSON with the package metadata, SHA-256, and SHA-512 of the inner tarball.

The index endpoint for fetching package metadata is:

```
GET https://hex.pm/api/packages/<name>
```

This returns JSON with all available versions, their checksums, and their dependency lists.

## Tarball structure

Hex.pm packages are nested tarballs:

```
<package>-<version>.tar          (outer, uncompressed)
├── VERSION                      (text file, content "3\n")
├── metadata.config              (Erlang term format: {package, [{key, value}, ...]})
├── contents.tar.gz              (inner, gzip-compressed)
│   ├── src/<name>.erl           (Erlang source files)
│   ├── src/<name>.app.src       (OTP application descriptor template)
│   ├── ebin/<name>.beam         (compiled BEAM files)
│   ├── ebin/<name>.app          (compiled OTP application descriptor)
│   ├── include/*.hrl            (header files)
│   ├── priv/                    (private data directory, e.g., binaries)
│   └── ...
└── CHECKSUM                     (text file with hex-encoded SHA-256 of contents.tar.gz)
```

The outer `.tar` (not `.tar.gz`) contains the inner `contents.tar.gz`. The bridge computes and verifies three hashes:

- `outer-sha256`: SHA-256 of the outer `.tar` file bytes as downloaded from Hex.pm.
- `inner-sha256`: SHA-256 of the `contents.tar.gz` bytes within the outer tarball.
- `inner-sha512`: SHA-512 of the `contents.tar.gz` bytes. Hex.pm publishes this in the API response under the `checksum` field; the bridge cross-verifies.

All three hashes are recorded in `[[erlang-package]]` in `mochi.lock`. The `mochi pkg lock --check` step recomputes all three from the cached content-addressed blob and exits non-zero on any mismatch.

## OTP application descriptor: `.app.src`

Every OTP application shipped to Hex.pm must include an `.app.src` file (the template from which `rebar3 compile` generates the `.app` descriptor). A minimal `.app.src` for a Mochi-generated Port driver application:

```erlang
{application, my_mochi_app,
 [{description, "A Mochi package published as an Erlang OTP application."},
  {vsn, "1.0.0"},
  {registered, []},
  {applications, [kernel, stdlib]},
  {env, []},
  {modules, [mochi_port_driver, mochi_shim_api]},
  {licenses, ["Apache-2.0"]},
  {links, [{"GitHub", "https://github.com/example/my_mochi_app"}]},
  {build_tools, ["rebar3"]}
 ]}.
```

The `TargetErlangPort` build target generates this file from the `[erlang.publish]` table in `mochi.toml`. The `modules` list is populated with the names of the generated Erlang modules (the Port driver shim and the API wrapper).

## `rebar.config` hex metadata

For Direction 2 (publish), the `rebar.config` includes a `hex_metadata` section:

```erlang
{hex_metadata,
 [{maintainers, [<<"tamnd">>]},
  {licenses, [<<"Apache-2.0">>]},
  {links, [{<<"GitHub">>, <<"https://github.com/example/my_mochi_app">>}]},
  {description, <<"A Mochi package published as an Erlang OTP application.">>},
  {build_tools, [<<"rebar3">>]},
  {files, [<<"ebin/**">>, <<"src/**">>, <<"priv/**">>]}
 ]}.
```

This section is read by `rebar3 hex build` to populate the `metadata.config` in the outer tarball.

## TargetErlangPort build output

`TargetErlangPort` emits a rebar3 application tree:

```
<build-dir>/erlang_port/
  rebar.config            (deps: mochi_port_runtime; hex metadata)
  src/
    my_mochi_app.app.src  (OTP application descriptor)
    mochi_port_driver.erl (gen_server + open_port/2 process)
    mochi_shim_api.erl    (public API module: one exported function per extern fn)
  priv/
    mochi_binary          (compiled Mochi native binary, copied from build output)
  ebin/                   (populated by rebar3 compile)
```

The `mochi_port_driver.erl` is the same `gen_server` pattern as the consume-direction shim (see [MEP-66 §6](/docs/mep/mep-0066#6-erlang-port-bridge-shim-emit)), but in reverse: it exposes Mochi functions to Erlang callers by spawning the Mochi binary as a Port and forwarding calls to it.

The `mochi_shim_api.erl` wraps `mochi_port_driver:call/2` with named functions and type-annotated `-spec` directives, so that Dialyzer can type-check Erlang code that calls into the Mochi library.

## rebar3 hex publish integration

The `mochi pkg publish --to=hex.pm` command:

1. Runs `rebar3 compile` in the `erlang_port/` directory to compile `.erl` to `.beam`.
2. Runs `rebar3 hex build` to produce the nested tarball.
3. Computes and records all three hashes.
4. Obtains the OIDC token (see [[07-oidc-trusted-publishing]]).
5. Posts the tarball to `https://hex.pm/api/packages/<name>/releases` with the OIDC token in the `Authorization` header.
6. Parses the response JSON to extract the server-computed `inner-sha512` and cross-verifies it against the locally computed value.
7. Records the published package coordinates in `mochi.lock` under a `[publish]` sub-table.

## Cross-references

- [[07-oidc-trusted-publishing]] for the OIDC token acquisition.
- [[09-rebar3-lockfile]] for the three-hash lockfile scheme.
- [MEP-66 §9](/docs/mep/mep-0066#9-publish-flow-direction-2) for the normative publish flow.
