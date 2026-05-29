---
title: "05. Type mapping"
sidebar_position: 6
sidebar_label: "05. Type mapping"
description: "The closed translation table from Dialyzer typespecs to Mochi types, SkipReport cases, the ok/error idiom pattern recogniser, atom() to string marshalling, pid()/reference() opaque handle strategy, and user-defined type alias expansion."
---

# 05. Type mapping

## The closed table philosophy

The Erlang typespec language (Dialyzer typespecs) is expressive: it supports recursive types, union types of arbitrary depth, parameterised type aliases, opaque types, fun types with complex arities, and type variables. Translating all of this to Mochi's type system correctly and soundly is not possible in a finite closed table. The bridge takes the same approach as MEP-73 (Rust) and MEP-76 (Ruby): define a closed table that covers the most common Erlang types found in real package APIs, and skip (with a structured `SkipReport`) anything outside the table. A user who needs an out-of-table binding writes a hand-authored `extern fn` declaration in Mochi.

The SkipReport for each skipped item records:

- The module name and function name.
- The argument position (or `return`) where the skip occurred.
- The specific type node that caused the skip (e.g., `untyped_map`, `bitstring`, `non_ok_error_union`).

## Complete translation table

| Erlang typespec | Mochi type | Notes |
|---|---|---|
| `integer()` | `int` | 64-bit signed; arbitrary-precision Erlang integers clip at ±2^63-1; SkipNote on overflow |
| `pos_integer()` | `int` | Range constraint (>0) not preserved; SkipNote |
| `non_neg_integer()` | `int` | Range constraint (≥0) not preserved; SkipNote |
| `neg_integer()` | `int` | Range constraint (<0) not preserved; SkipNote |
| `0..255` (byte range literal) | `int` | Literal integer range types are mapped to `int` with a SkipNote |
| `float()` | `float` | IEEE 754 double |
| `boolean()` | `bool` | |
| `true` (atom literal) | `bool` | Erlang `true` is just the atom `true`; bridge recognises it as boolean literal |
| `false` (atom literal) | `bool` | |
| `atom()` | `string` | All atoms become strings; ETF transport uses ATOM_UTF8_EXT |
| `binary()` | `bytes` | Arbitrary byte binary; the most common string type in modern Erlang |
| `list(T)` (T in table) | `list<T>` | |
| `[T]` (T in table) | `list<T>` | Shorthand form |
| `nonempty_list(T)` (T in table) | `list<T>` | Non-empty constraint lost; SkipNote |
| `nil` (empty list literal) | `list<T>` | Empty list is `[]` in Mochi |
| `ok` (atom literal, return pos.) | `nil` | Unit success return |
| `{ok, T}` (T in table) | mapped in ok/error pattern only | see §2 |
| `{ok, T} \| {error, atom()}` | `result<T, string>` | Canonical Erlang result |
| `{ok, T} \| {error, binary()}` | `result<T, string>` | |
| `{ok, T} \| {error, atom() \| binary()}` | `result<T, string>` | |
| `{ok, T} \| error` | `result<T, string>` | bare `error` atom treated as empty error |
| `T \| undefined` | `T?` | `undefined` atom used as optional sentinel |
| `undefined` | `nil` | |
| `{A, B}` (2-tuple, A and B in table) | `[A, B]` | Mochi pair |
| `{A, B, C}` (3-tuple, all in table) | `[A, B, C]` | Mochi triple |
| `{A, B, C, D}` (4-tuple, all in table) | `[A, B, C, D]` | Mochi quadruple |
| `pid()` | `extern type Pid` | Opaque handle to an Erlang process |
| `reference()` | `extern type Reference` | Opaque reference term |
| `port()` | `extern type ErlPort` | Opaque Erlang port (name-conflict avoided by alias) |
| `node()` | `string` | Node name serialised as string |
| `fun((A) -> B)` (A, B in table) | `fun(A): B` | Fixed-arity, in-table only |
| `fun((A, B) -> C)` (all in table) | `fun(A, B): C` | |
| `number()` | refused | Union integer\|float; SkipReport: ambiguous_number |
| `string()` | refused | Charlist (list of integers); SkipReport: erlang_charlist |
| `iodata()` | refused | Union binary\|iolist; SkipReport: iodata_union |
| `iolist()` | refused | Recursive list; SkipReport: iolist |
| `bitstring()` | refused | Non-byte-aligned; SkipReport: bitstring |
| `tuple()` (untyped) | refused | SkipReport: untyped_tuple |
| `map()` (untyped) | refused | SkipReport: untyped_map |
| `#{K := V}` (typed map) | refused | SkipReport: typed_map (no Mochi equivalent) |
| `any()` / `term()` | refused | Top type; SkipReport: any_term |
| `none()` / `no_return()` in non-return | refused | SkipReport: no_return_in_non_return |
| complex union (3+ branches, no ok/error) | refused | SkipReport: complex_union |
| complex union (2 branches, no ok/error) | refused | SkipReport: non_ok_error_union |
| `fun()` (untyped) | refused | SkipReport: untyped_fun |
| `fun((...) -> T)` with out-of-table args | refused | SkipReport: fun_arg_not_in_table |
| user type with recursive expansion | refused if cycle | SkipReport: recursive_type |

## Atom → string marshalling

Erlang atoms are interned strings: the atom `hello` is represented as the integer identifier for the string `"hello"` in the atom table. From the bridge's perspective, atoms are opaque identifiers that can be converted to strings. The ETF `ATOM_UTF8_EXT` tag carries the atom as a UTF-8 string. The bridge maps all `atom()` type annotations to Mochi `string` and serialises atoms as their UTF-8 representation.

One important consequence: the `boolean()` type in Erlang is defined as `true | false`, where `true` and `false` are atoms. The bridge recognises `boolean()` as a special case and maps it to Mochi `bool` directly, not to `string`. It also recognises the literal atom forms `{atom, _, true}` and `{atom, _, false}` as boolean literals.

## Opaque handles: Pid, Reference, ErlPort

Three Erlang types (`pid()`, `reference()`, `port()`) are opaque terms that cannot be meaningfully serialised to a Mochi-side value and back: they are runtime handles that only the BEAM VM can interpret. The bridge maps them to `extern type` declarations, which become opaque pointer-sized values in Mochi. On the wire (ETF), they are transmitted as-is using the `PID_EXT`, `REFERENCE_EXT`, and `PORT_EXT` tag bytes; the Mochi binary stores the raw ETF bytes as an opaque blob and passes them back to the Erlang side without interpretation.

This means a Mochi program can hold a `Pid` (a reference to a running gen_server process), pass it to a function like `gen_server:call/2`, and receive a reply, without the Mochi side ever knowing what is inside the `Pid` blob. The bridge ensures the blob is passed through verbatim and that no Mochi code can construct a `Pid` from scratch (construction would require knowing the BEAM VM's internal pid layout, which would break across OTP versions).

## User-defined type expansion

When the bridge encounters a `{user_type, _, TypeName, Params}` reference, it looks up `TypeName` in the module's `-type` declarations and expands the definition. Expansion is non-recursive: if expanding `TypeName` produces another `{user_type, _, ...}`, the bridge expands that too, up to a depth of 10. A cycle (type alias that eventually refers back to itself) causes a `SkipReport: recursive_type` for the function using the type.

Opaque types (`-opaque`) are never expanded; they become `extern type` declarations in the shim. This is the correct semantics: opaque types are intentionally unexpanded to preserve module-level encapsulation.

## Remote type expansion

When the bridge encounters `{remote_type, _, [{atom,_,Module}, {atom,_,TypeName}, Params]}`, it checks whether `Module` is a module in the same OTP application (or a transitive dependency in `mochi.lock`). If yes, it loads the relevant `.beam` file's abstract code and expands the type. If no, it produces a `SkipReport: remote_type_not_in_deps`.

Common remote types:
- `erlang:timestamp/0` → refused (3-tuple of integers; too complex)
- `calendar:datetime/0` → refused (nested tuple; complex)
- `inet:hostname/0` → `string` (recognised as a special case; ETF encodes as binary)
- `inet:port_number/0` → `int` (recognised as a special case; 0-65535 integer)

## SkipReport format

```go
type SkipReport struct {
    Module   string
    Function string
    Arity    int
    Position string // "arg1", "arg2", ..., "return"
    Reason   SkipReason
    Detail   string // the specific type node that caused the skip
}

type SkipReason int
const (
    SkipAnyTerm SkipReason = iota
    SkipBitstring
    SkipCharlist
    SkipComplexUnion
    SkipEDoc           // no BEAM abstract code; fell back to EDoc
    SkipIodata
    SkipNoSpec         // function exported but has no -spec
    SkipNoTypeinfo     // package has neither abstract code nor EDoc
    SkipRecursiveType
    SkipRemoteType
    SkipTypedMap
    SkipUntypedFun
    SkipUntypedMap
    SkipUntypedTuple
    // ...
)
```

## Cross-references

- [[04-beam-typespec-ingest]] for how the AST nodes are extracted.
- [[08-port-bridge-protocol]] for how in-table types are serialised over the Port.
- [MEP-66 §4](/docs/mep/mep-0066#4-dialyzer-typespec-to-mochi-type-mapping) for the normative table.
