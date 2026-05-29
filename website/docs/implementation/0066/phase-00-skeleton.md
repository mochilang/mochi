---
title: "Phase 0: Skeleton"
sidebar_position: 2
sidebar_label: "Phase 0. Skeleton"
description: "Phase 0 of MEP-66: package3/erlang/ directory layout, errors package, ETF encoder/decoder, rebar3 workspace synthesiser, and build driver skeleton."
---

# Phase 0: Skeleton

## Goal

Establish the `package3/erlang/` directory structure, define the `SkipReason` and `BridgeError` types, implement the Go-side ETF encoder/decoder, and write the rebar3 workspace synthesiser skeleton. No actual package fetching or type translation yet.

## Gate

`go test ./package3/erlang/...` passes with:
- ETF round-trip tests: encode then decode for all in-table types (integer, float, boolean, atom/string, binary/bytes, list, 2-tuple, 3-tuple, 4-tuple, pid, reference, nil/undefined).
- SkipReason string representation covers all 17 skip codes.
- `Build.SynthRebar3Config` produces a syntactically valid Erlang term file for a 3-package dep set.

## Files to touch

| File | Description |
|------|-------------|
| `package3/erlang/README.md` | Pointer to MEP-66 spec. |
| `package3/erlang/errors/errors.go` | `SkipReason` constants + `SkipReport` struct + `BridgeError` type. |
| `package3/erlang/errors/errors_test.go` | String representation round-trip. |
| `package3/erlang/etf/etf.go` | ETF tag constants + `Decode([]byte) (interface{}, error)` + `Encode(interface{}) ([]byte, error)`. |
| `package3/erlang/etf/etf_test.go` | Round-trip tests for all supported tag types. |
| `package3/erlang/build/driver.go` | `Driver` struct: `SynthRebar3Config`, `SynthRebar3Lock`, `RunRebar3Compile`. |
| `package3/erlang/build/driver_test.go` | `SynthRebar3Config` golden-file test. |

## SkipReason codes (phase 0)

```go
const (
    SkipAnyTerm          SkipReason = iota // any() / term()
    SkipBitstring                          // bitstring()
    SkipCharlist                           // string() (erlang charlist)
    SkipComplexUnion                       // 3+ branch union, no ok/error
    SkipEDoc                               // fell back to EDoc (no BEAM abstract code)
    SkipFunArgNotInTable                   // fun arg type not in table
    SkipIodata                             // iodata() / iolist()
    SkipNonOkErrorUnion                    // 2-branch union, not ok/error pattern
    SkipNoSpec                             // exported function has no -spec
    SkipNoTypeinfo                         // no abstract code AND no EDoc
    SkipRecursiveType                      // user type alias is recursive
    SkipRemoteType                         // remote_type not in deps
    SkipTypedMap                           // #{K := V} map type
    SkipUntypedFun                         // fun() untyped
    SkipUntypedMap                         // map() untyped
    SkipUntypedTuple                       // tuple() untyped
    SkipElixirRuntime                      // requires Elixir runtime
)
```

## ETF types supported in phase 0

```go
// etf.go (phase 0 subset)
type Atom string          // ETF ATOM_UTF8_EXT / SMALL_ATOM_UTF8_EXT
type Tuple []interface{}  // ETF SMALL_TUPLE_EXT / LARGE_TUPLE_EXT
type List []interface{}   // ETF LIST_EXT / NIL_EXT
type Pid struct {         // ETF PID_EXT / NEW_PID_EXT
    Node     Atom
    ID       uint32
    Serial   uint32
    Creation uint32
}
type Reference struct {   // ETF REFERENCE_EXT / NEW_REFERENCE_EXT
    Node     Atom
    Creation uint32
    IDs      []uint32
}
```

## rebar.config synthesis (phase 0)

```go
// build/driver.go
type DepEntry struct {
    Name    string
    Version string // exact, from mochi.lock
}

func (d *Driver) SynthRebar3Config(deps []DepEntry, otpVsn string) string
```

Output example:
```erlang
{erl_opts, [debug_info]}.

{deps, [
  {cowboy, "2.12.0"},
  {hackney, "1.20.0"},
  {jose, "1.11.0"}
]}.

{minimum_otp_vsn, "25"}.
```

## Sub-phase decomposition

Phase 0 is a single unit; no sub-phases are expected.

## Cross-references

- [Phase 1](/docs/implementation/0066/phase-01-hex-index) builds the Hex.pm client on top of this skeleton.
- [MEP-66 §1](/docs/mep/mep-0066#1-pipeline-overview) for the pipeline context.
