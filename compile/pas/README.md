# Pascal Backend

The Pascal backend translates Mochi source into Object Pascal code compatible with the Free Pascal Compiler (FPC). It provides a minimal code generator primarily aimed at exercising the language core.

## Files

- `compiler.go` – walks the AST and emits Pascal code
- `compiler_test.go` – golden tests that compile and run generated programs
- `helpers.go` – utility helpers for name sanitisation and temporary variables
- `tools.go` – helper to locate or install `fpc` for the test suite

## Code generation

`Compiler.Compile` writes the program preamble, emits all function definitions and declares global variables before outputting the main body:

```go
c.writeln("program main;")
c.writeln("{$mode objfpc}")
c.writeln("uses SysUtils;")
c.writeln("")
c.writeln("type TIntArray = array of integer;")
```
【F:compile/pas/compiler.go†L24-L32】

Global variables are collected and declared in a `var` block sorted by name:

```go
vars := map[string]string{}
collectVars(prog.Statements, c.env, vars)
if len(vars) > 0 {
    c.writeln("var")
    c.indent++
    names := make([]string, 0, len(vars))
    for n := range vars {
        names = append(names, n)
    }
    sort.Strings(names)
    for _, n := range names {
        c.writeln(fmt.Sprintf("%s: %s;", sanitizeName(n), vars[n]))
    }
    c.indent--
    c.writeln("")
}
```
【F:compile/pas/compiler.go†L44-L60】

Control structures directly map to Pascal statements. For example a `for` range becomes:

```go
c.writeln(fmt.Sprintf("for %s := %s to %s - 1 do", name, start, end))
c.writeln("begin")
...
c.writeln("end;")
```
【F:compile/pas/compiler.go†L136-L178】

While loops and if/else chains follow a similar pattern using `while ... do` and `if ... then` blocks.【F:compile/pas/compiler.go†L181-L237】

Functions are emitted with parameter and return types determined by `typeRef` and include local variable collection:

```go
c.writeln(fmt.Sprintf("function %s(%s): %s;", name, strings.Join(params, "; "), retType))
...
c.writeln("begin")
...
c.writeln("end;")
```
【F:compile/pas/compiler.go†L246-L279】

Built‑in calls like `len` and `print` translate to the Pascal `Length` and `writeln` functions:

```go
case "len":
    return fmt.Sprintf("Length(%s)", argStr), nil
case "print":
    return fmt.Sprintf("writeln(%s)", argStr), nil
```
【F:compile/pas/compiler.go†L450-L456】

Names are sanitised to avoid reserved keywords using `sanitizeName` from `helpers.go`:

```go
if _, ok := pasReserved[strings.ToLower(res)]; ok {
    return "_" + res
}
```
【F:compile/pas/helpers.go†L20-L45】

## Tools

`EnsureFPC` tries to locate `fpc` and installs it via `apt` or Homebrew if missing so that tests can run automatically:

```go
if path, err := exec.LookPath("fpc"); err == nil {
    return path, nil
}
...
return "", fmt.Errorf("fpc not installed")
```
【F:compile/pas/tools.go†L10-L46】

## Building

Compile a Mochi program to Pascal using the build command:

```bash
mochi build --target pas main.mochi -o main.pas
```

`mochi build` selects the Pascal backend when the output file has a `.pas` extension as shown in the CLI implementation.【F:cmd/mochi/main.go†L736-L746】

The resulting `.pas` file can be compiled with `fpc` to produce an executable.

## Tests

Golden tests under `tests/compiler/pas` compile example programs with `fpc` and compare the output. The helper automatically installs the compiler if needed:

```go
fpc, err := pascode.EnsureFPC()
...
if out, err := exec.Command(fpc, file).CombinedOutput(); err != nil {
    return nil, fmt.Errorf("❌ fpc error: %w\n%s", err, out)
}
```
【F:compile/pas/compiler_test.go†L57-L82】

Run all tests with:

```bash
go test ./compile/pas
```
