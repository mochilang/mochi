# Erlang Backend

The Erlang backend compiles Mochi programs to plain Erlang source. The generated module can be executed with `escript` and exposes any top-level functions defined in the source file.

## Files

- `compiler.go` – walks the Mochi AST and generates Erlang code
- `runtime.go` – emits runtime helper functions used by generated code
- `compiler_test.go` – golden tests that execute the generated code with `escript`
- `tools.go` – helper that verifies the Erlang toolchain is installed

## Runtime Helpers

Generated programs embed a small runtime defined in `runtime.go`. Helper functions handle printing, formatting, counting, input and control flow:

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
【F:compile/erlang/runtime.go†L5-L28】

Additional helpers provide `mochi_input/0`, `mochi_avg/1`, `mochi_foreach/2`, optional indexing via `mochi_get/2`, the `mochi_while/2` loop and placeholders like `mochi_gen_text/3`. HTTP `fetch` requests use Erlang's `httpc` module.

## Features

The backend currently supports:

- `for` and `while` loops with `break`/`continue`
- pattern matching with `match` and `case`
- query expressions over one or more sources with `where`, `sort`, `skip` and `take`
- list set operations `union`, `union all`, `except` and `intersect`
- simple cross joins using multiple `from` clauses
- simple `group by` without joins or filters
- single `left join` clauses
- struct and union type declarations generate Erlang records
- struct literals now instantiate these records with `#name{field=val}` syntax
- AI `generate_text`, `generate_embed` and `generate_struct` helpers (placeholders)
- test blocks and `expect` statements
- `load` and `save` for Erlang terms or plain text files with optional `filter`, `skip` and `take` options
- HTTP `fetch` using `httpc` with method, headers, query, body and timeout options

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

`EnsureFormatter` checks for the `erlfmt` formatter and attempts a best-effort
installation when tests are run:

```go
// EnsureFormatter verifies that the `erlfmt` tool is installed.
func EnsureFormatter() error { return ensureErlfmt() }
```
【F:compile/erlang/tools.go†L78-L135】

## Unsupported Features

- The Erlang backend still implements only part of Mochi. Missing features include:
- right/outer joins inside queries
- logic programming constructs and streams
- agents and event streams
- intent declarations
- generic type parameters
- model declarations and dataset helpers
- concurrency primitives like `spawn` and channels
- `generate` helpers return placeholder data
- imports targeting languages other than Erlang
- reflection or macro facilities
- error handling with `try`/`catch`
- match patterns for union variants accept only identifiers or `_`
- `load` and `save` support only text and Erlang term files

Generated Erlang favors clarity over speed, mirroring Mochi constructs directly.

## Erlang to Mochi Conversion

The `any2mochi` tool can translate Erlang source back to Mochi. Function
signatures are obtained from the language server and bodies are parsed for
simple assignments and return expressions.

### Supported Features

- Top-level function declarations
- Parameter and return types extracted from hover information
- Basic statement bodies with variable assignments and a final return

### Unsupported Features

- Pattern matching clauses or guards
- Complex control flow such as `case`, `try`, or list comprehensions
- Macros and preprocessor directives
