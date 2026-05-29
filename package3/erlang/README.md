# package3/erlang

Mochi+Erlang/OTP package bridge. See [MEP-66](/docs/mep/mep-0066) for the full specification and [/docs/implementation/0066/](/docs/implementation/0066/) for per-phase implementation tracking.

## Overview

The `package3/erlang/` tree implements the bidirectional bridge between Mochi and the Hex.pm ecosystem:

- **Direction 1 (consume)**: `import erlang "<package>@<semver>" as <alias>` resolves a Hex.pm package, ingests typespecs from BEAM abstract code, translates Dialyzer typespecs to Mochi types via a closed table, and emits a `shim.erl` (OTP Port gen_server) + `shim.mochi` (extern fn corpus) pair.

- **Direction 2 (publish)**: `mochi pkg publish --to=hex.pm` invokes `TargetErlangPort` to emit a rebar3 application wrapping the compiled Mochi binary as an Erlang Port driver, then publishes to Hex.pm via OIDC trusted publishing.

## Packages

| Package | Phase | Purpose |
|---------|-------|---------|
| `errors/` | 0 | `SkipReason` constants, `SkipReport` struct, `BridgeError` |
| `etf/` | 0 | Erlang External Term Format encoder/decoder (pure Go) |
| `build/` | 0 | rebar3 workspace synthesiser + build driver |
| `hexsemver/` | 1 | Hex.pm version constraint parser and comparator |
| `hexindex/` | 1 | Hex.pm HTTP API v2 client + content-addressed cache |
| `beamingest/` | 2 | BEAM file parser (IFF chunks) + Dbgi/Abst ETF reader |
| `edocingest/` | 3 | EDoc XML fallback parser |
| `typemap/` | 4 | Closed Dialyzer typespec → Mochi type translation table |
| `portemit/` | 5 | `shim.erl` gen_server emitter |
| `externemit/` | 5 | `shim.mochi` extern fn/type corpus emitter |
| `port/` | 7 | Go-side Port process manager (ETF I/O loop) |
| `target/` | 9 | `TargetErlangPort` rebar3 app skeleton emitter |
| `publish/` | 10 | Hex.pm OIDC trusted publishing client |
| `cnode/` | 13 | C-node via `erl_interface` (distributed Erlang bridge) |
| `testutil/` | — | `mock-hex` harness + BEAM golden-file fixtures |

## Dependencies

- OTP 25+ at `mochi pkg lock` time (for EDoc fallback only; BEAM ingest requires no OTP).
- OTP 27 at runtime (for the Port bridge and the `rebar3 compile` step).
- rebar3 3.20+ (the bridge enforces `[erlang].rebar3-version`).

## Status

All phases NOT STARTED. See [implementation tracking](/docs/implementation/0066/) for current status.
