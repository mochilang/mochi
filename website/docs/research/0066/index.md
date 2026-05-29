---
title: "MEP-66 research bundle"
sidebar_position: 1
sidebar_label: "Overview"
description: "Twelve research notes covering the design space behind MEP-66: language surface, design philosophy, prior art, BEAM typespec ingest, type mapping, Hex.pm publish flow, OIDC trusted publishing, OTP Port bridge protocol, rebar3 and lockfile, OTP behavior bindings, version resolution, and risks."
---

# MEP-66 research bundle

This bundle is the informative companion to [MEP-66](/docs/mep/mep-0066). It documents the design space the bridge sits in: prior art, the choices considered and rejected, the trade-offs accepted, and the open risks. The bundle is meant to be read alongside the spec, not in place of it.

## Notes

| Note | Subject |
|------|---------|
| [01. Language surface](01-language-surface.md) | The `import erlang "..."` import shape, the `mochi.toml` `[erlang-dependencies]`, `[erlang]`, and `[erlang.publish]` tables, the CLI surface (`mochi pkg add erlang`, `mochi pkg publish --to=hex.pm`), and the per-import alias/sub-namespace resolution rule. |
| [02. Design philosophy](02-design-philosophy.md) | Why a bidirectional bridge, why BEAM abstract code over Dialyzer PLT or EDoc XML, why OTP Port over NIF or C-node, why the ok/error idiom maps to `result<T, string>`, why OIDC trusted publishing is the only publish path. |
| [03. Prior-art bridges](03-prior-art.md) | Rustler (Rust→BEAM NIF), Zigler (Zig NIF), erlport (Python/Ruby via Ports), Ports vs C-nodes vs NIFs, Erlix, JInterface. What each gets right and what MEP-66 borrows. |
| [04. BEAM typespec ingest](04-beam-typespec-ingest.md) | The Dbgi/Abst chunk format, the ETF encoding, the Go-side ETF parser shape, walking `-spec` and `-type` directives, the `beam-ingest-sha256` reproducibility anchor, OTP 17 vs 20+ chunk differences. |
| [05. Type mapping](05-type-mapping.md) | The closed translation table from Dialyzer typespecs to Mochi types, the SkipReport cases, the ok/error idiom pattern recogniser, the `atom()` → `string` marshalling decision, the `pid()` and `reference()` opaque handle strategy. |
| [06. Hex.pm publish flow](06-hex-publish-flow.md) | The Hex.pm HTTP API v2 upload protocol, the package tarball structure (outer + inner tar, three hashes), the `.app.src` metadata requirements, the publish-side gate, and the `rebar3 hex publish` integration path. |
| [07. OIDC trusted publishing](07-oidc-trusted-publishing.md) | The Hex.pm trusted publishing flow (2024), the JWT claim requirements, the GitHub Actions OIDC provider endpoint, the `id-token: write` permission, and the `HEX_API_KEY` fallback prohibition rationale. |
| [08. OTP Port bridge protocol](08-port-bridge-protocol.md) | The ETF packet framing (`{packet, 4}` mode), the call/response message schema, the gen_server wrapper shape, Port process lifecycle (start/stop/crash recovery), and the latency profile of Port round-trips vs NIF calls. |
| [09. rebar3 and mochi.lock](09-rebar3-lockfile.md) | The rebar3.lock format vs `[[erlang-package]]` in `mochi.lock`, the three-hash verification scheme (outer SHA-256, inner SHA-256, inner SHA-512), the `--check` mode gate, and the rebar.config synthesis pipeline. |
| [10. OTP behavior bindings](10-otp-behaviors.md) | gen_server call/cast/info patterns, supervisor start/stop/which_children, application start/stop, and how the bridge translates the stateful gen_server surface to Mochi's stateless `extern fn` model via opaque `Pid` handles. |
| [11. Version resolution](11-version-resolution.md) | Hex.pm's version constraint operators (`~>`, `>=`, `<`, `and`), the Hex.pm HTTP API v2 compact index format, pre-release handling, the bridge's two-tier resolution (Go resolver + rebar3 fallback), and Elixir-compat package selection. |
| [12. Risks and alternatives](12-risks-and-alternatives.md) | The risk register (BEAM abstract code drift across OTP versions, EDoc coverage gaps, Port latency, rebar3 version fragility, Hex.pm API stability, ok/error pattern false positives, WASM exclusion) and rejected alternatives (NIF default, Dialyzer PLT primary, C-node bridge, HEX_API_KEY publish, full Elixir support, custom resolver). |

## Cross-references

- [MEP-66 spec](/docs/mep/mep-0066) — the normative document.
- [MEP-57](/docs/mep/mep-0057) — the source-level package system whose manifest and lockfile the bridge extends.
- [Implementation tracking](/docs/implementation/0066/) — the per-phase delivery status.
