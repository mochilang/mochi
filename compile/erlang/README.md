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

## Building

Use the `mochi` command to compile a source file to Erlang:

```bash
mochi build --target erlang main.mochi -o main.erl
escript main.erl            # run the program
```
【F:cmd/mochi/main.go†L507-L518】
The output module is named `main` and exports `main/1` along with any user
functions defined at the top level.

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

The Erlang backend currently supports a subset of Mochi features. Query
expressions without joins or grouping are translated to Erlang list
comprehensions; more complex queries return an error. Indexing strings or maps
emits the `mochi_get/2` helper only when required. The generated code is designed
for readability rather than speed, mirroring Mochi constructs closely.
