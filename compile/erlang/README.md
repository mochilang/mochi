# Erlang Backend

The Erlang backend compiles Mochi programs to plain Erlang source. The generated module can be executed with `escript` and exposes any top-level functions defined in the source file.

## Files

- `compiler.go` – walks the Mochi AST and generates Erlang code
- `compiler_test.go` – golden tests that execute the generated code with `escript`
- `tools.go` – helper that verifies the Erlang toolchain is installed

## Runtime Helpers

Generated programs embed a small runtime implemented directly inside `compiler.go`. Helper functions handle printing, formatting, counting, input and control flow:

```erlang
mochi_print(Args) ->
        Strs = [ mochi_format(A) || A <- Args ],
        io:format("~s~n", [lists:flatten(Strs)]).

mochi_format(X) when is_integer(X) -> integer_to_list(X);
mochi_format(X) when is_float(X) -> float_to_list(X);
mochi_format(X) when is_list(X) -> X;
mochi_format(X) -> lists:flatten(io_lib:format("~p", [X])).

mochi_count(X) when is_list(X) -> length(X);
mochi_count(X) when is_map(X), maps:is_key('Items', X) -> length(maps:get('Items', X));
mochi_count(X) when is_map(X) -> maps:size(X);
mochi_count(X) when is_binary(X) -> byte_size(X);
mochi_count(_) -> erlang:error(badarg).
```
【F:compile/erlang/compiler.go†L1545-L1566】

Additional helpers provide `mochi_input/0`, `mochi_avg/1`, `mochi_foreach/2`, optional indexing via `mochi_get/2`, the `mochi_while/2` loop and placeholders like `mochi_gen_text/3`. HTTP `fetch` requests use Erlang's `httpc` module.

## Features

The backend currently supports:

- `for` and `while` loops with `break`/`continue`
- pattern matching with `match` and `case`
- query expressions over one or more sources with `where`, `sort`, `skip` and `take`
- list set operations `union`, `except` and `intersect`
- simple cross joins using multiple `from` clauses
- simple `group by` without joins or filters
- struct and union type declarations generate Erlang records
- AI `generate_text`, `generate_embed` and `generate_struct` helpers (placeholders)
- test blocks and `expect` statements
- `load` and `save` for Erlang terms or plain text files
- HTTP `fetch` using `httpc`

## Building

Compile a Mochi file to Erlang with:

```bash
mochi build --target erlang main.mochi -o main.erl
escript main.erl            # run the program
```
【F:cmd/mochi/main.go†L507-L518】
The output module is named `main` by default. When a source file declares a `package`, that name is used instead.

## Tests

The test suite generates Erlang code and runs it with `escript`. Tests are tagged `slow` because they require the Erlang toolchain:

```bash
go test ./compile/erlang -tags slow
```

Golden files live under `tests/compiler/erl_simple` and `tests/compiler/erl` and can be updated with `go test -tags slow -update ./compile/erlang`.

`EnsureErlang` attempts to install Erlang via `apt-get` or Homebrew when tests are executed:

```go
// EnsureErlang verifies that the Erlang toolchain is installed. It attempts a
// best-effort installation using apt-get on Linux or Homebrew on macOS.
func EnsureErlang() error { return ensureErlang() }
```
【F:compile/erlang/tools.go†L10-L13】

## Unsupported Features

- The Erlang backend still implements only part of Mochi. Missing features include:
- left/right/outer joins inside queries
- logic programming constructs and streams
- agents and event streams
- intent declarations
- generic type parameters
- model declarations and dataset helpers
- concurrency primitives like `spawn` and channels
- `generate` helpers return placeholder data
- imports targeting languages other than Erlang
- methods declared inside type blocks
- reflection or macro facilities

Generated Erlang favors clarity over speed, mirroring Mochi constructs directly.
