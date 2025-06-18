# F# Backend

The F# backend compiles Mochi programs into F# source code. It currently targets a small subset of the language that is sufficient for LeetCode style examples and algorithmic problems.

## Files

- `compiler.go` – main code generator which walks the AST and emits F# code
- `compiler_test.go` – golden tests that verify generated code using `dotnet fsi`
- `tools.go` – helper used by the tests to ensure the `dotnet` CLI is available

## Code Generation

The entry point is `Compile` which writes a simple program header and then translates statements in order:

```go
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
    c.buf.Reset()
    c.writeln("open System")
    ...
}
```
【F:compile/fs/compiler.go†L38-L67】

Functions are converted to `let` bindings that use exceptions to simulate early returns:

```go
func (c *Compiler) compileFunStmt(fn *parser.FunStmt) error {
    c.pushFunc(fn.Name)
    defer c.popFunc()
    exc := fmt.Sprintf("Return_%s", sanitizeName(fn.Name))
    ...
    c.writeln("try")
    ...
    c.writeln(fmt.Sprintf("with %s v -> v", exc))
    return nil
}
```
【F:compile/fs/compiler.go†L70-L94】

The compiler handles variable declarations, assignments, loops, conditionals and pattern matching. For example the `compileFor` method emits either a numeric `for` loop or a collection loop:

```go
func (c *Compiler) compileFor(f *parser.ForStmt) error {
    name := sanitizeName(f.Name)
    ...
    c.writeln(fmt.Sprintf("for %s = %s to %s - 1 do", loopVar, start, end))
    ...
    return nil
}
```
【F:compile/fs/compiler.go†L205-L239】

List literals become `[| ... |]` arrays and union types are emitted with variant constructors. Built-in helpers such as `len`, `count`, `avg` and `print` are mapped to F# equivalents inside `compileCallExpr`:

```go
case "avg":
    return fmt.Sprintf("(Array.sum %s) / %s.Length", args[0], args[0]), nil
case "print":
    return fmt.Sprintf("printfn \"%%A\" %s", args[0]), nil
```
【F:compile/fs/compiler.go†L525-L551】

## Tools

Tests call `EnsureDotnet` to install the .NET SDK if it is missing so that `dotnet fsi` can run:

```go
// EnsureDotnet verifies that the dotnet CLI is installed and attempts to
// install it if missing. It is safe to call from tests.
func EnsureDotnet() error { return ensureDotnet() }
```
【F:compile/fs/tools.go†L11-L13】

## Running Tests

The F# tests are tagged `slow` because they invoke the .NET toolchain. Run them directly with:

```bash
go test ./compile/fs -tags slow
```

This command compiles the subset programs in `tests/compiler/fs` and also runs the example solutions for LeetCode problems 1–3 to ensure the generated F# code behaves correctly.


The tests compile Mochi sources from `tests/compiler/fs` and execute them with `dotnet fsi`:

```go
func TestFSCompiler_SubsetPrograms(t *testing.T) {
    if err := fscode.EnsureDotnet(); err != nil {
        t.Skipf("dotnet not installed: %v", err)
    }
    ...
    cmd := exec.Command("dotnet", "fsi", "--quiet", file)
    out, err := cmd.CombinedOutput()
    ...
}
```
【F:compile/fs/compiler_test.go†L21-L52】

## Usage

Use `mochi build --target fs` to generate an `.fs` file:

```bash
mochi build --target fs program.mochi -o program.fs
```

The emitted code is intended to be executed with `dotnet fsi` or included in larger F# projects.

