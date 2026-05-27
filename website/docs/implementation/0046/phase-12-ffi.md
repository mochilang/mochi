---
title: "Phase 12. FFI (Erlang)"
sidebar_position: 14
sidebar_label: "Phase 12. FFI (Erlang)"
description: "MEP-46 Phase 12. FFI (Erlang) — detailed implementation spec."
---

# Phase 12. FFI (Erlang)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-46 §Phases · Phase 12. FFI](/docs/mep/mep-0046#phase-12-ffi) |
| Status         | LANDED |
| Started        | 2026-05-27 (GMT+7) |
| Landed         | 2026-05-27 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

This phase implements Mochi's foreign function interface for the BEAM target. Unlike MEP-45's C FFI (which uses JSON-over-subprocess to interop with external processes), the BEAM FFI is a zero-overhead direct cross-module call within the same VM. The `extern "Erlang"` declaration form introduces a BEAM-specific binding that lowers to a plain `c_call` in Core Erlang, with no marshalling cost for most types.

---

## Gate

See [MEP-46 §Phases · Phase 12. FFI](/docs/mep/mep-0046) for the normative gate. All 15 fixtures must produce byte-equal output to vm3.

---

## Goal-alignment audit

FFI is the escape hatch that lets Mochi programs access the full OTP ecosystem without waiting for Mochi-native wrappers. Phase 12 fixtures cover calls to `lists`, `maps`, `string`, `crypto`, and `erlang` stdlib modules — all directly useful in user programs. The identity-marshalling design means FFI has zero overhead for all common types, making it a first-class primitive rather than a last resort.

---

## Sub-phases

### Sub-phase 12.0: extern Erlang module declarations

**Syntax**

```mochi
extern "Erlang" mod lists {
  fun nth(n: int, xs: list<int>): int
  fun sort(xs: list<int>): list<int>
}
```

This declares FFI bindings to functions in the Erlang `lists` module. The module name after `mod` is the unquoted Erlang module atom. The function signature uses Mochi types; the type checker validates call sites against these declared signatures at compile time.

**Lowering: ExternErlangDecl IR node**

The parser produces an `ExternErlangDecl` AST node containing:

- `TargetModule string` — the Erlang module name (e.g., `"lists"`).
- `Bindings []ExternErlangBinding` — each binding has a Mochi name, Erlang function name, parameter types, and return type.

The aotir lowerer records `ExternErlangDecl` in `aotir.Program.ExternErlang` (a new map field: Mochi qualified name -> `ExternErlangFunc`). When a call to an extern Erlang function appears in Mochi source:

```mochi
let x = nth(1, xs)
```

The call lowerer checks `ExternErlang`, finds the binding, and emits:

```erlang
c_call(c_atom(lists), c_atom(nth), [lowerExpr(1), lowerExpr(xs)])
```

This is a direct Erlang cross-module call. No wrapper function is generated; no marshalling code is emitted.

**Dotted module names and `as` clause**

For cases where the Mochi function name differs from the Erlang function name, or where the Erlang module differs from the `mod` alias:

```mochi
extern "Erlang" mod crypto {
  fun hash(algo: atom, data: string): string as crypto:hash
}
```

The `as module:function` clause overrides the default mapping. For simple cases the standard form works:

```mochi
extern "Erlang" mod crypto {
  fun hash(algo: atom, data: string): string
}
```

Lowers to `c_call(c_atom(crypto), c_atom(hash), [AlgoArg, DataArg])`.

**BEAM-target-specific declaration**

`extern "Erlang"` declarations are silently ignored by non-BEAM lowerers. The C lowerer in MEP-45 treats an `ExternErlangDecl` as a no-op and emits no code. A call site that references an extern Erlang function from a non-BEAM target produces a compile-time error: `extern "Erlang" binding 'lists.nth' is not available on target 'c'`.

---

### Sub-phase 12.1: Marshalling: Mochi types <-> Erlang terms

The BEAM target's key advantage over the C FFI is that most Mochi types are identity-mapped to Erlang terms: there is nothing to marshal. The marshalling table lives in `transpiler3/beam/lower/ffi.go` as `func beamFFIType(t aotir.Type) beamRepr`.

**Type mapping table**

| Mochi type | BEAM representation | Notes |
|------------|---------------------|-------|
| `int` | BEAM integer | Identity. Erlang integers are arbitrary precision. |
| `float` | BEAM float (64-bit IEEE 754) | Identity. |
| `bool` | `true` / `false` atoms | Identity. |
| `string` | BEAM binary (UTF-8) | Identity. Mochi strings on BEAM are always UTF-8 binaries. |
| `list<T>` | BEAM cons-cell list | Identity. Mochi lists are BEAM proper lists. |
| `map<K,V>` | BEAM map | Identity. Mochi maps are BEAM maps. |
| `atom` | BEAM atom | FFI-only type; see below. |
| `any` | Any BEAM term | FFI escape hatch; see below. |
| User-defined record | BEAM map with atom keys | Same as Mochi's internal record representation. |

**The `atom` FFI type**

`atom` is a type available only in `extern "Erlang"` declarations. It represents an Erlang atom literal or a runtime atom value. In Mochi source:

```mochi
extern "Erlang" mod crypto {
  fun hash(algo: atom, data: string): string
}

let digest = hash(#sha256, "hello")
```

The `#sha256` syntax (new for Phase 12) is an atom literal. It lowers to `c_atom(sha256)` directly. At call sites that need to produce atoms from string data at runtime, the lowerer emits `binary_to_existing_atom(Data, utf8)` (see design decisions for atom safety rationale).

**The `any` FFI escape hatch**

`any` in an `extern "Erlang"` signature tells the Mochi type checker to treat the argument or return value as an opaque BEAM term. The lowerer emits the term as-is with no type annotation in the generated Core Erlang:

```mochi
extern "Erlang" mod gun {
  fun open(host: string, port: int, opts: any): any
}

let conn = open("api.example.com", 443, gun_tls_opts())
```

`any` values are opaque to the Mochi type checker after they are produced; they can only be passed to other `any`-typed FFI parameters or returned from functions with `any` return types. This prevents `any` from infecting the Mochi type system beyond FFI boundaries.

**Marshalling table implementation**

```go
// transpiler3/beam/lower/ffi.go

func beamFFIType(t aotir.Type) beamRepr {
  switch t.(type) {
  case *aotir.IntType:    return beamReprIdentity
  case *aotir.FloatType:  return beamReprIdentity
  case *aotir.BoolType:   return beamReprIdentity
  case *aotir.StringType: return beamReprIdentity
  case *aotir.ListType:   return beamReprIdentity
  case *aotir.MapType:    return beamReprIdentity
  case *aotir.AtomType:   return beamReprAtom
  case *aotir.AnyType:    return beamReprAny
  default:
    panic(fmt.Sprintf("unsupported FFI type: %T", t))
  }
}
```

For `beamReprIdentity`, the lowerer passes the Core Erlang expression directly with no wrapper. For `beamReprAtom`, if the source is a string variable (not a compile-time literal), the lowerer wraps with `binary_to_existing_atom/2`.

---

### Sub-phase 12.2: Hex.pm dep declarations in mochi.toml -> rebar.config

**mochi.toml Hex dependency syntax**

A `mochi.toml` project manifest (proposed for a later MEP; referenced here for completeness) can declare Hex.pm dependencies for the BEAM target:

```toml
[beam_deps]
gun = "2.1.0"
cowboy = "2.10.0"
```

**Build driver: mochi.toml -> rebar.config generation**

The `mochi build --target beam` command reads `mochi.toml` and generates `rebar.config`:

```erlang
{deps, [
  {gun, "2.1.0"},
  {cowboy, "2.10.0"}
]}.
```

It also generates `{erl_opts, [debug_info]}` and `{profiles, [{test, [{deps, [...]}]}]}` sections. The build driver is implemented in `cmd/mochi/build_beam.go`.

**FFI declarations referencing Hex deps**

Once a Hex dep is declared, FFI can reference its modules:

```mochi
extern "Erlang" mod gun {
  fun open(host: string, port: int, opts: any): any
  fun request(conn: any, method: atom, path: string, headers: any, body: any): any
}
```

The Mochi compiler does not verify that the referenced Erlang module exists at compile time. A missing dep causes an Erlang compile error downstream when rebar3 attempts to compile the generated `.erl` files.

**Phase 12 CI scope**

Phase 12 fixtures use only OTP stdlib modules (no Hex deps) to keep CI reproducible without a Hex.pm network connection. The `mochi.toml` Hex dep feature is tested in Phase 14's fetch fixtures (which require `gun`).

---

## Test set

15 fixtures under `tests/transpiler3/beam/fixtures/phase12/`:

| # | File | Description |
|---|------|-------------|
| 01 | `ffi_lists_nth.mochi` | `lists:nth` with int list |
| 02 | `ffi_lists_sort.mochi` | `lists:sort` with int list |
| 03 | `ffi_lists_reverse.mochi` | `lists:reverse` |
| 04 | `ffi_lists_map.mochi` | `lists:map` with a Mochi fun |
| 05 | `ffi_lists_filter.mochi` | `lists:filter` |
| 06 | `ffi_maps_merge.mochi` | `maps:merge` of two Mochi maps |
| 07 | `ffi_maps_keys.mochi` | `maps:keys` returns a list |
| 08 | `ffi_string_uppercase.mochi` | `string:uppercase` on a UTF-8 binary |
| 09 | `ffi_string_split.mochi` | `string:split` |
| 10 | `ffi_crypto_hash.mochi` | `crypto:hash(sha256, Data)` via atom literal `#sha256` |
| 11 | `ffi_erlang_system_time.mochi` | `erlang:system_time(millisecond)` |
| 12 | `ffi_erlang_node.mochi` | `erlang:node()` returns current node name |
| 13 | `ffi_any_passthrough.mochi` | Pass `any`-typed value through FFI without type error |
| 14 | `ffi_atom_literal.mochi` | Atom literals `#ok`, `#error` in FFI call |
| 15 | `ffi_multi_module.mochi` | Multiple `extern "Erlang"` blocks in one file |

---

## Decisions made

**Why `extern "Erlang"` syntax rather than a generic `extern fun`**

MEP-45's `extern fun` is a C-direct FFI that uses a subprocess and JSON-over-stdin/stdout to communicate with Go, Python, or JavaScript programs. BEAM FFI is fundamentally different: it is a direct cross-module call in the same VM process, with no subprocess, no serialization, and no network. Using the same `extern fun` syntax for both would obscure this distinction and mislead users into expecting C FFI behavior (process isolation, JSON marshalling). The `extern "Erlang"` form is target-specific and makes the mechanism explicit.

**Why identity marshalling for most types**

Mochi's BEAM representation is designed to be idiomatic Erlang from the ground up: lists are cons cells, maps are BEAM maps, strings are UTF-8 binaries, booleans are atoms. This is a deliberate design choice that makes Mochi/Erlang interop cost-free for the common case. The C target's FFI requires JSON-over-subprocess precisely because C has no native representation for Mochi's high-level types. On BEAM, there is nothing to convert.

**Why `binary_to_existing_atom` for string-to-atom conversion**

Erlang's atom table is a fixed-size global table (default: 1,048,576 atoms). `binary_to_atom/1` creates a new atom entry if the string has not been seen before; under adversarial input (e.g., a web service that reads user-supplied field names and passes them as atoms), this can exhaust the atom table and crash the BEAM node. `binary_to_existing_atom/2` only succeeds if the atom is already registered, preventing atom table exhaustion from runtime data. The FFI layer never creates new atoms from runtime data; atom literals in Mochi source are compiled to `c_atom(...)` directly.

---

## Closeout notes

Sub-phase 12.1 (extern Erlang FFI) landed as `3edc11cbda` — `extern "Erlang"` declarations lower to plain `c_call` nodes with no marshalling overhead for identity-mapped types. Sub-phase 12.2 (`mochi.toml` to `rebar.config` generation) landed as `924dfd9901`, adding the build driver support for converting Hex.pm dependency declarations into a `rebar.config` file. All 15 FFI fixtures produce byte-equal output against vm3.
