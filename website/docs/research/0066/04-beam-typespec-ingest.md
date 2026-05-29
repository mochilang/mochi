---
title: "04. BEAM abstract code ingest"
sidebar_position: 5
sidebar_label: "04. BEAM typespec ingest"
description: "The Dbgi/Abst chunk format, ETF encoding of the abstract syntax tree, the Go-side ETF parser shape, walking -spec and -type directives from the AST, beam-ingest-sha256 reproducibility anchor, OTP 17 vs OTP 20+ chunk differences, and the EDoc XML fallback."
---

# 04. BEAM abstract code ingest

## BEAM file format overview

A compiled Erlang `.beam` file is an IFF (Interchange File Format) archive: a 4-byte `FOR1` magic, a 4-byte size field, a 4-byte `BEAM` marker, and a sequence of typed chunks. Each chunk has a 4-byte tag, a 4-byte length, and chunk-length bytes of content, padded to a 4-byte boundary.

The chunks relevant to type extraction are:

| Chunk tag | OTP versions | Content |
|-----------|-------------|---------|
| `Abst` | OTP 17-19 | Uncompressed ETF encoding of the abstract syntax tree (Erlang AST as produced by `erl_parse`) |
| `Dbgi` | OTP 20+ | Zlib-compressed ETF encoding of a debug information structure; the first element is `{debug_info_v1, erl_abstract_code, Metadata}` where `Metadata` contains the module's abstract syntax tree and compile options |
| `Attr` | All | Module attributes (`-module`, `-export`, `-author`, `-vsn`), ETF-encoded |
| `CInf` | All | Compiler info (source path, options), ETF-encoded |

The `Dbgi` chunk replaced `Abst` in OTP 20 to allow pluggable debug information backends. The default backend is `erl_abstract_code`, which stores the same AST as `Abst` but wrapped in the `debug_info_v1` container and zlib-compressed. The bridge handles both chunk types: it first looks for `Dbgi`, then falls back to `Abst`.

## ETF decoder in Go

The bridge includes a pure-Go ETF decoder in `package3/erlang/etf/`. ETF (Erlang External Term Format) is a binary serialisation format defined in the OTP documentation. The relevant tag bytes and their Go representations are:

| ETF tag byte | Erlang type | Go type |
|-------------|-------------|---------|
| `70` (`NEW_FLOAT_EXT`) | float | `float64` |
| `97` (`SMALL_INTEGER_EXT`) | integer (0-255) | `int64` |
| `98` (`INTEGER_EXT`) | integer (signed 32-bit) | `int64` |
| `100` (`ATOM_EXT`) | atom (Latin-1) | `etf.Atom` (string alias) |
| `110` (`SMALL_BIG_EXT`) | big integer | `*big.Int` |
| `104` (`SMALL_TUPLE_EXT`) | tuple (≤255 elements) | `etf.Tuple` ([]interface{}) |
| `105` (`LARGE_TUPLE_EXT`) | tuple (>255 elements) | `etf.Tuple` |
| `106` (`NIL_EXT`) | empty list `[]` | `etf.List` (nil) |
| `107` (`STRING_EXT`) | char list (uint16 len) | `etf.List` of int64 |
| `108` (`LIST_EXT`) | proper list | `etf.List` |
| `109` (`BINARY_EXT`) | binary | `[]byte` |
| `115` (`ATOM_EXT` small) | atom (≤255 bytes) | `etf.Atom` |
| `118` (`ATOM_UTF8_EXT`) | atom (UTF-8) | `etf.Atom` |
| `119` (`SMALL_ATOM_UTF8_EXT`) | atom (UTF-8, ≤255 bytes) | `etf.Atom` |

The `Dbgi` chunk is zlib-compressed (using Go's `compress/zlib`) before the ETF content. The `Abst` chunk is uncompressed ETF directly.

## Abstract syntax tree structure

After decoding, the abstract syntax tree is an Erlang list of form declarations. The bridge walks this list looking for four declaration kinds:

**`-spec` declarations** appear as tuples:
```erlang
{attribute, Line, spec, {{FunName, Arity}, [TypeList]}}
```
where `TypeList` is a list of function type specs (one per clause). The bridge reads the first clause for the primary spec and flags multi-clause specs with a `SkipNote: multi_clause_spec`.

**`-type` declarations** appear as:
```erlang
{attribute, Line, type, {TypeName, TypeDef, Vars}}
```

**`-opaque` declarations** appear identically but with the tag `opaque`. The bridge treats opaque types as type aliases that are not expanded during translation (they become `extern type` declarations in the shim).

**`-export` declarations** list the `{Name, Arity}` pairs that the module exports. The bridge intersects the spec map with the export list: only exported functions with specs are translated. Unexported functions are not bridged even if they have specs.

## Type expression AST nodes

A typespec's type expression is itself an Erlang AST term. The relevant shapes are:

```erlang
% Primitive types
{type, Line, integer, []}          % integer()
{type, Line, float, []}            % float()
{type, Line, boolean, []}          % boolean()
{type, Line, atom, []}             % atom()
{type, Line, binary, []}           % binary()
{type, Line, pid, []}              % pid()
{type, Line, reference, []}        % reference()
{type, Line, any, []}              % any()
{type, Line, term, []}             % term()
{type, Line, none, []}             % none()
{type, Line, no_return, []}        % no_return()

% Parameterised types
{type, Line, list, [ElemType]}     % list(T)
{type, Line, nonempty_list, [ElemType]}

% Tuple types
{type, Line, tuple, [T1, T2]}      % {T1, T2}
{type, Line, tuple, any}           % tuple() (untyped)

% Union type
{type, Line, union, [T1, T2, ...]} % T1 | T2 | ...

% Atom literal (for ok/error/undefined)
{atom, Line, ok}                   % atom literal ok
{atom, Line, error}                % atom literal error
{atom, Line, undefined}            % atom literal undefined

% User-defined type reference
{user_type, Line, TypeName, Params}
{remote_type, Line, [{atom,_,Module}, {atom,_,TypeName}, Params]}
```

The bridge walks this AST recursively, consulting the closed translation table at each node. A node that is not in the table causes the current top-level function spec to be added to the SkipReport with the specific failing node type identified.

## The ok/error pattern recogniser

The bridge applies a structural pattern recogniser before consulting the closed translation table for union types. When it encounters a `{type, _, union, [Branch1, Branch2]}` node (exactly 2 branches), it checks:

1. Is one branch `{type, _, tuple, [{atom, _, ok}, T]}` (the `{ok, T}` form)?
2. Is the other branch `{type, _, tuple, [{atom, _, error}, _Reason]}` (the `{error, Reason}` form)?

If both conditions are true, the union is mapped to `result<T, string>` (where T is the translated form of the `ok` branch's second element, and Reason is collapsed to `string`). If only condition 1 is true (no `{error, _}` branch), the union is mapped to `T?`. All other 2-branch unions, and all unions with 3+ branches that do not match the ok/error pattern, are refused.

## beam-ingest-sha256

The `beam-ingest-sha256` value in `[[erlang-package]]` is computed as:

```
sha256(concat(sorted_by_module_name(dbgi_bytes_for_each_module)))
```

where `dbgi_bytes_for_each_module` is the raw (compressed) bytes of the `Dbgi` (or `Abst`) chunk from each `.beam` file in the package, sorted by module name. This hash anchors the ingest result: if the package is rebuilt with a different OTP version or different compiler options, the `Dbgi` bytes change, the `beam-ingest-sha256` changes, and `mochi pkg lock --check` fails. The user must re-run `mochi pkg lock` to regenerate the shims.

## EDoc XML fallback

For packages that ship no `-spec` directives (or whose `Dbgi` chunk contains no spec attributes), the bridge falls back to EDoc XML. EDoc XML is generated by the edoc tool from source comment tags (`@spec`, `@type`). The bridge does not run `edoc` directly; instead, it looks for a pre-generated `doc/` directory or `edoc-info` file inside the package tarball. Many packages on Hex.pm include pre-generated EDoc output in their release tarball. If no EDoc output is present, the package receives a blanket `SkipReport` of kind `no_typeinfo`.

The EDoc XML parser (`package3/erlang/edocingest/`) reads the XML, walks `<module>` → `<functions>` → `<function>` elements, and extracts `@spec` type expressions from the `<spec>` child element. The type expressions are parsed with a best-effort heuristic (not a full Erlang parser) and converted to the same `etf.TypeExpr` AST nodes that the BEAM ingest produces. The closed translation table is then applied identically.

## Cross-references

- [[05-type-mapping]] for the closed table the ingest feeds into.
- [[09-rebar3-lockfile]] for how `beam-ingest-sha256` participates in the lock-check protocol.
- [MEP-66 §3](/docs/mep/mep-0066#3-lockfile-extension-erlang-package) for the normative `beam-ingest-sha256` definition.
