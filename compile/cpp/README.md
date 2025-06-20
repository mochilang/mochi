# C++ Backend

The C++ backend translates a Mochi program to standard C++17 source. It is a minimal code generator intended mainly for testing and experimentation.

## Files

- `compiler.go` – walks the AST and emits C++ code
- `compiler_test.go` – golden tests that compile the generated code with `g++`
- `tools.go` – helper for locating/ installing a C++ compiler

## Code generation

`Compiler.Compile` first emits the program body, then prepends the standard
C++ headers. The final output always starts with:

```cpp
#include <bits/stdc++.h>
using namespace std;
```

This occurs when writing the generated code back to the main buffer:

```go
c.writeln("#include <bits/stdc++.h>")
c.writeln("using namespace std;")
c.writeln("")
```
【F:compile/cpp/compiler.go†L65-L72】

Type translation is straightforward. Integers become `int`, floats `double`,
booleans `bool`, strings `string`, and lists map to `std::vector` as shown in
`cppType`:

```go
func (c *Compiler) cppType(t *parser.TypeRef) string {
    if t == nil {
        return "void"
    }
    if t.Simple != nil {
        switch *t.Simple {
        case "int":
            return "int"
        case "float":
            return "double"
        case "bool":
            return "bool"
        case "string":
            return "string"
        }
        return *t.Simple
    }
    if t.Generic != nil && t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
        return "vector<" + c.cppType(t.Generic.Args[0]) + ">"
    }
    return "auto"
}
```
【F:compile/cpp/compiler.go†L75-L95】

Union type declarations are now translated using `std::variant`. Each variant
is emitted as a separate struct and the union type becomes a `using` alias. The
relevant code lives in `compileTypeDecl`:

```go
func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
    if len(t.Variants) > 0 {
        names := make([]string, len(t.Variants))
        for i, v := range t.Variants {
            names[i] = v.Name
            c.writeln(fmt.Sprintf("struct %s {", v.Name))
            // ... fields ...
            c.writeln("};")
        }
        c.writeln(fmt.Sprintf("using %s = std::variant<%s>;", t.Name, strings.Join(names, ", ")))
        return nil
    }
    // struct declarations
}
```
【F:compile/cpp/compiler.go†L179-L207】

Loops, conditionals and expressions map directly to their C++ equivalents. A
`for` range becomes a standard loop:

```go
func (c *Compiler) compileFor(f *parser.ForStmt) error {
    if f.RangeEnd != nil {
        start := c.compileExpr(f.Source)
        end := c.compileExpr(f.RangeEnd)
        c.writeln(fmt.Sprintf("for (int %s = %s; %s < %s; %s++) {", f.Name, start, f.Name, end, f.Name))
        c.indent++
        for _, st := range f.Body {
            if err := c.compileStmt(st); err != nil {
                return err
            }
        }
        c.indent--
        c.writeln("}")
        return nil
    }
    // unsupported
    return nil
}
```
【F:compile/cpp/compiler.go†L181-L196】

`print()` is detected specially to output using `std::cout` followed by
`std::endl`:

```go
func (c *Compiler) compilePrint(call *parser.CallExpr) error {
    args := make([]string, len(call.Args))
    for i, a := range call.Args {
        args[i] = c.compileExpr(a)
    }
    c.writeIndent()
    for i, a := range args {
        if i > 0 {
            c.buf.WriteString("std::cout << \" \" << ")
            c.buf.WriteString(a)
        } else {
            c.buf.WriteString("std::cout << ")
            c.buf.WriteString(a)
        }
    }
    c.buf.WriteString(" << std::endl;\n")
    return nil
}
```
【F:compile/cpp/compiler.go†L345-L361】

## Tools

The `EnsureCPP` helper checks for `g++`, `clang++` or `c++` and attempts to
install them via `apt-get` or Homebrew if missing:

```go
// EnsureCPP verifies that a C++ compiler is installed. If missing, it attempts
// to install one using apt-get on Linux or Homebrew/Xcode tools on macOS.
func EnsureCPP() (string, error) {
    if path, err := exec.LookPath("g++"); err == nil {
        return path, nil
    }
    if path, err := exec.LookPath("clang++"); err == nil {
        return path, nil
    }
    if path, err := exec.LookPath("c++"); err == nil {
        return path, nil
    }
    switch runtime.GOOS {
    case "linux":
        fmt.Println("🔧 Installing g++...")
        if _, err := exec.LookPath("apt-get"); err == nil {
            cmd := exec.Command("apt-get", "update")
            cmd.Stdout = os.Stdout
            cmd.Stderr = os.Stderr
            if err := cmd.Run(); err != nil {
                return "", err
            }
            cmd = exec.Command("apt-get", "install", "-y", "build-essential")
            cmd.Stdout = os.Stdout
            cmd.Stderr = os.Stderr
            _ = cmd.Run()
        }
    case "darwin":
        if _, err := exec.LookPath("xcode-select"); err == nil {
            fmt.Println("🔧 Installing Xcode Command Line Tools...")
            _ = exec.Command("xcode-select", "--install").Run()
        }
        if _, err := exec.LookPath("brew"); err == nil {
            fmt.Println("🍺 Installing LLVM via Homebrew...")
            _ = exec.Command("brew", "install", "llvm").Run()
        }
    }
    if path, err := exec.LookPath("g++"); err == nil {
        return path, nil
    }
    if path, err := exec.LookPath("clang++"); err == nil {
        return path, nil
    }
    if path, err := exec.LookPath("c++"); err == nil {
        return path, nil
    }
    return "", fmt.Errorf("C++ compiler not found")
}
```
【F:compile/cpp/tools.go†L10-L56】

## Running tests

Golden tests under `tests/compiler/cpp` verify the backend. Run them with the `slow` tag because they invoke an external compiler:

```bash
go test ./compile/cpp -tags slow
```

During tests the compiler is invoked similar to:

```go
dir := t.TempDir()
file := filepath.Join(dir, "prog.cpp")
bin := filepath.Join(dir, "prog")
exec.Command(cpp, file, "-std=c++17", "-o", bin)
```

【F:compile/cpp/compiler_test.go†L40-L46】

from `compiler_test.go`.

## Status

This backend covers only a subset of Mochi's features but is useful as an example for implementing other targets.

Dataset queries support `where`, `skip`, `take`, `sort by` and basic `group by` clauses.
`join` clauses remain unimplemented.

### Supported features

* Function definitions and calls
* `if`, `for` and `while` statements
* Structs, unions and pattern matching with `match`
* List and map literals with indexing and slicing
* Built‑in functions `print`, `len` and `str`
* Dataset queries with filtering, sorting, pagination and simple grouping

### Unsupported features

Some LeetCode solutions use language constructs that the C++ backend can't yet translate. Unsupported features include:

* Dataset queries with `join` clauses or advanced grouping
* Agents, streams and intents
* `generate` blocks and model definitions
* Dataset helpers such as `fetch`, `load`, `save` and SQL-style `from ...` queries
* `logic` queries for Prolog-style reasoning
* Foreign imports and `extern` declarations
* Package management, tests and `expect` blocks
* Set literals and operations
* HTTP `fetch` requests for JSON data
* Concurrency primitives like `spawn` and channels
* Reflection or macro facilities
* Generic type parameters and methods inside `type` blocks
* List operators such as `union`, `union all`, `except` and `intersect`
