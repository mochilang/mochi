---
title: MEP-66 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 66. Mochi+Erlang package bridge"
description: "Per-phase implementation tracking for MEP-66 (Mochi+Erlang/OTP package bridge). Status + commit columns capture how each phase landed on main."
---

# MEP-66 implementation tracking

Per-phase tracking for [MEP-66 Mochi+Erlang/OTP package bridge](/docs/mep/mep-0066). Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main` (or, for the umbrella PR, the in-branch commit on `mep/0066-erlang-package`).

A phase is LANDED only when its gate is green for every target in the runtime matrix. Missing surfaces become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

## Phase status

| Phase | Title | Status | Commit | Tracking page |
|-------|-------|--------|--------|---------------|
| 0 | Skeleton: package3/erlang/ layout + rebar3 workspace plumbing | NOT STARTED | — | [phase-00](/docs/implementation/0066/phase-00-skeleton) |
| 1 | Hex.pm API v2 client (package fetch + outer/inner tarball + SHA-256/SHA-512 verify) | NOT STARTED | — | [phase-01](/docs/implementation/0066/phase-01-hex-index) |
| 2 | BEAM abstract code ingest (Dbgi/Abst chunk ETF decoder, -spec/-type walker) | NOT STARTED | — | [phase-02](/docs/implementation/0066/phase-02-beam-ingest) |
| 3 | EDoc XML fallback ingest (for packages with no -spec directives) | NOT STARTED | — | [phase-03](/docs/implementation/0066/phase-03-edoc-fallback) |
| 4 | Dialyzer typespec-to-Mochi type mapping + SkipReport emit | NOT STARTED | — | [phase-04](/docs/implementation/0066/phase-04-type-mapping) |
| 5 | Port bridge shim emitter (shim.erl gen_server + shim.mochi extern fn corpus) | NOT STARTED | — | [phase-05](/docs/implementation/0066/phase-05-shim-emit) |
| 6 | `import erlang "..."` grammar + parser | NOT STARTED | — | [phase-06](/docs/implementation/0066/phase-06-import-grammar) |
| 7 | Build orchestration (rebar3 compile + Port process spawn + workspace setup) | NOT STARTED | — | [phase-07](/docs/implementation/0066/phase-07-build) |
| 8 | mochi.lock `[[erlang-package]]` integration + --check mode | NOT STARTED | — | [phase-08](/docs/implementation/0066/phase-08-lockfile) |
| 9 | TargetErlangPort emit (rebar3 app skeleton + mochi_port_driver.erl + priv/mochi_binary) | NOT STARTED | — | [phase-09](/docs/implementation/0066/phase-09-target-erlang-port) |
| 10 | Hex.pm trusted publishing (OIDC flow + rebar3 hex publish) | NOT STARTED | — | [phase-10](/docs/implementation/0066/phase-10-trusted-publish) |
| 11 | OTP behavior bindings (gen_server call/cast, supervisor, application) | NOT STARTED | — | [phase-11](/docs/implementation/0066/phase-11-otp-behaviors) |
| 12 | Async process bridge (OTP process spawn/receive/send/monitor via Mochi async) | NOT STARTED | — | [phase-12](/docs/implementation/0066/phase-12-async-bridge) |
| 13 | Distributed Erlang node bridge (C-node via erl_interface + `dist` capability) | NOT STARTED | — | [phase-13](/docs/implementation/0066/phase-13-dist-bridge) |

## Per-phase fields

Each phase tracking page documents (or will document, once the phase begins):

- **Gate**: the test or check that must pass for the phase to be LANDED.
- **Files to touch**: the bridge-side files (Go) and emit-side files (Erlang template) the phase introduces or modifies.
- **Fixtures**: which of the 20-package fixture corpus the phase validates against.
- **Skip count**: the expected SkipReport count per fixture package (golden numbers).
- **Sub-phase decomposition** (if needed): N.1, N.2, ... entries when an upstream constraint forces splitting.

## Fixture corpus

The 20-package fixture corpus (May 2026 top-downloaded Erlang packages on Hex.pm plus representative OTP behavior, distributed systems, and data-format cases):

cowboy, ranch, hackney, gun, jsx, jose, poolboy, lager, erlware_commons, parse_trans, meck, proper, recon, observer_cli, telemetry, opentelemetry_api, prometheus.erl, gproc, cuttlefish, uuid.

Packages `lager`, `observer_cli`, and `cuttlefish` exercise phase 3 (EDoc XML fallback). All other packages exercise phase 2 (BEAM abstract code ingest). Each phase that touches the type-mapping or shim-emit layer asserts golden SkipReport counts against this corpus.

## Runtime matrix

| Phase range | CI target | Notes |
|-------------|-----------|-------|
| 0-5 | OTP 25 + OTP 27 on ubuntu-latest | Ingest + type-mapping; no BEAM runtime needed |
| 6-9 | OTP 27 on ubuntu-latest | Runtime bridge; OTP 27 is primary target |
| 6-9 | OTP 27 on darwin-arm64 (macos-latest) | Added when Port bridge becomes testable |
| 10 | OTP 27 on ubuntu-latest | Publish; mock-hex harness only |
| 11-13 | OTP 27 on ubuntu-latest + darwin-arm64 | OTP behavior + async + distributed |

## Implementation location

The bridge lives at `package3/erlang/` in the repo root:

```
package3/erlang/
  README.md               # pointer to MEP-66 spec
  errors/                 # SkipReason + BridgeError (phase 0)
  build/                  # rebar3 workspace synth + build driver (phase 0)
  etf/                    # Erlang External Term Format encoder/decoder in Go (phase 0)
  hexsemver/              # Hex.pm-flavoured semver parser (phase 1)
  hexindex/               # Hex.pm HTTP API v2 client + content-addressed cache (phase 1)
  beamingest/             # BEAM file parser + Dbgi/Abst chunk reader (phase 2)
  edocingest/             # EDoc XML fallback parser (phase 3)
  typemap/                # closed typespec-to-Mochi table + SkipReport (phase 4)
  portemit/               # shim.erl gen_server emitter (phase 5)
  externemit/             # shim.mochi extern fn/type emitter (phase 5)
  port/                   # Go-side Port process manager (phase 7)
  cnode/                  # C-node via erl_interface (phase 13)
  publish/                # Hex.pm OIDC publish + rebar3 hex publish (phase 10)
  target/                 # TargetErlangPort rebar3 app skeleton emitter (phase 9)
  testutil/               # mock-hex harness + BEAM test fixtures (all phases)
```

## Status snapshot

As of 2026-05-29 22:46 (GMT+7): phases 0-13 NOT STARTED. The MEP spec and research bundle are written; implementation begins with phase 0 (skeleton).

## Cross-references

- [MEP-66 spec](/docs/mep/mep-0066) for the normative design.
- [MEP-66 research bundle](/docs/research/0066/) for the 12-note deep-research collection.
- [MEP-57 implementation tracking](/docs/implementation/0057) for the polyglot package system MEP-66 builds on.
