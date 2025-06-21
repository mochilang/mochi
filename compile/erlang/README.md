# Erlang Backend

The Erlang backend compiles Mochi programs to plain Erlang source. The emitted
module is executable via `escript` and exposes any top-level functions declared
in the source file. This backend is useful for running Mochi code in the Erlang
runtime or integrating with existing Erlang projects.

## Files

- `compiler.go` – walks the Mochi AST and generates Erlang code
- `compiler_test.go` – golden tests that execute the generated code with
  `escript`
- `tools.go` – helper that verifies the Erlang toolchain is installed

## Runtime Helpers

Generated programs embed a small runtime implemented directly inside
`compiler.go`. Functions cover printing, formatting, counting, input handling
and control flow helpers. The implementation looks like:

```erlang
mochi_print(Args) ->
        Strs = [ mochi_format(A) || A <- Args ],
        io:format("~s~n", [lists:flatten(Strs)]).

mochi_format(X) when is_integer(X) -> integer_to_list(X);
mochi_format(X) when is_float(X) -> float_to_list(X);
mochi_format(X) when is_list(X) -> X;
mochi_format(X) -> lists:flatten(io_lib:format("~p", [X])).

mochi_count(X) when is_list(X) -> length(X);
mochi_count(X) when is_map(X) -> maps:size(X);
mochi_count(X) when is_binary(X) -> byte_size(X);
mochi_count(_) -> erlang:error(badarg).
```
【F:compile/erlang/compiler.go†L632-L649】
Additional helpers implement `mochi_input/0`, `mochi_avg/1`, list iteration
(`mochi_foreach/2`), optional map/list indexing via `mochi_get/2` and a simple
`mochi_while/2` loop construct.
Generative helpers `mochi_gen_text/3`, `mochi_gen_embed/3` and
`mochi_gen_struct/4` return placeholder values.  The `mochi_fetch/2` helper now
performs a real HTTP GET request using Erlang's `httpc` module.

## Features

The backend supports fundamental language constructs:

- `for` and `while` loops with `break`/`continue`
- pattern matching with `match` and `case`
- query expressions over one or more sources with `where`, `sort`, `skip` and `take`
- list set operations `union`, `except` and `intersect`
- simple cross joins using multiple `from` clauses
- struct and union type declarations generate Erlang records
- AI `generate_text`, `generate_embed` and `generate_struct` helpers (placeholders)
- test blocks and `expect` statements
- `load` and `save` for Erlang terms or plain text files
- HTTP `fetch` using `httpc`

## Building

Use the `mochi` command to compile a source file to Erlang:

```bash
mochi build --target erlang main.mochi -o main.erl
escript main.erl            # run the program
```
【F:cmd/mochi/main.go†L507-L518】
The output module is named `main` by default and exports `main/1` along with
any user functions defined at the top level. When a source file declares a
`package`, that name is used for the generated module instead of `main`.

## Tests

The test suite exercises this backend by generating Erlang code and running it
with `escript`. Tests are tagged `slow` since they depend on the Erlang toolchain.
Run them with:

```bash
go test ./compile/erlang -tags slow
```

Golden files live under `tests/compiler/erl_simple` and `tests/compiler/erl`.
Updating them can be done with `go test -tags slow -update ./compile/erlang`.

`EnsureErlang` attempts to install Erlang via `apt-get` or Homebrew when tests are
executed:

```go
// EnsureErlang verifies that the Erlang toolchain is installed. It attempts a
// best-effort installation using apt-get on Linux or Homebrew on macOS.
func EnsureErlang() error { return ensureErlang() }
```
【F:compile/erlang/tools.go†L10-L13】

## Notes

The Erlang backend still implements only a portion of Mochi. Query
expressions without joins or grouping are translated to Erlang list
comprehensions; more complex queries return an error. Pattern matching on
union values is supported via `case` expressions. The following language
features are not yet handled:

- Joins or grouping inside queries
- Logic programming constructs and streams
- Agents and event streams
- Foreign function imports via `extern`
- Extern variables and objects
- Model declarations and dataset helpers
- Concurrency primitives like `spawn` and channels
- `generate` helpers return placeholder data
- Imports targeting languages other than Erlang
- Reflection or macro facilities

Generated Erlang favors clarity over speed, mirroring Mochi constructs
directly.
