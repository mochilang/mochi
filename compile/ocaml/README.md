# OCaml Backend

The OCaml backend translates a small subset of Mochi into OCaml source code.  It
was initially written to compile algorithmic examples such as the LeetCode
`two_sum` problem and focuses on lists, loops and basic IO.

## Files

- `compiler.go` – walks the Mochi AST and emits OCaml code
- `compiler_test.go` – golden tests that compile and run the generated program
- `tools.go` – helper for tests that ensures `ocamlc` is available

## Compilation

`Compiler` accumulates output in a buffer and manages indentation.  Early returns
are implemented using generated exceptions:

```go
func (c *Compiler) compileFun(fn *parser.FunStmt) error {
    ex := fmt.Sprintf("Return_%d", c.tmp)
    c.tmp++
    ...
    c.writeln("try")
    ...
    c.writeln(fmt.Sprintf("with %s v -> v", ex))
}
```
【F:compile/ocaml/compiler.go†L57-L82】

Statements handle variable declarations via `ref`s, assignments, loops and basic
conditionals:

```go
case s.Var != nil:
    name := sanitizeName(s.Var.Name)
    c.vars[name] = true
    c.writeln(fmt.Sprintf("let %s = ref %s;;", name, val))
case s.While != nil:
    return c.compileWhile(s.While, ex)
```
【F:compile/ocaml/compiler.go†L97-L129】

`compileFor` and `compileWhile` produce OCaml `for`/`while` loops while
`compileIf` emits `if ... then begin ... end` blocks.

Builtin calls such as `len` and `print` are mapped to OCaml equivalents inside
`compileCall`:

```go
case "len":
    return fmt.Sprintf("List.length %s", args[0]), nil
case "print":
    return fmt.Sprintf("print_endline (string_of_int %s)", args[0]), nil
```
【F:compile/ocaml/compiler.go†L349-L357】

Identifiers are cleaned using `sanitizeName` so generated names are valid OCaml:

```go
func sanitizeName(name string) string {
    ...
    if res == "" || !((res[0] >= 'A' && res[0] <= 'Z') ||
        (res[0] >= 'a' && res[0] <= 'z') || res[0] == '_') {
        res = "_" + res
    }
    return res
}
```
【F:compile/ocaml/compiler.go†L390-L403】

## Tools

`EnsureOCaml` attempts to install the OCaml compiler when tests run. It supports
`apt-get` on Linux and Homebrew on macOS:

```go
func EnsureOCaml() error {
    if _, err := exec.LookPath("ocamlc"); err == nil { return nil }
    switch runtime.GOOS {
    case "linux":
        cmd := exec.Command("apt-get", "install", "-y", "ocaml")
        ...
    }
    return fmt.Errorf("ocamlc not found")
}
```
【F:compile/ocaml/tools.go†L10-L47】

## Building

Generate OCaml source using `mochi build`:

```bash
mochi build --target ocaml main.mochi -o main.ml
```

The output can be compiled with `ocamlc`:

```bash
ocamlc main.ml -o main
./main
```

## Tests

The golden tests compile programs under `tests/compiler/ocaml` and a curated
subset in `tests/compiler/valid_ocaml`, then run them with `ocamlc`:

```go
if err := mlcode.EnsureOCaml(); err != nil {
    t.Skipf("ocamlc not installed: %v", err)
}
```
【F:compile/ocaml/compiler_test.go†L56-L59】

Run the tests with:

```bash
go test ./compile/ocaml -tags slow
```

These tests verify both the generated program output and the emitted `.ml` code.

## Unsupported features

The OCaml backend covers only a small slice of Mochi. Missing pieces include:

- Query expressions such as `from` / `sort by` / `select`
- Pattern matching and union types
- Anonymous functions with block bodies
- Streams, LLM helpers and the foreign function interface
