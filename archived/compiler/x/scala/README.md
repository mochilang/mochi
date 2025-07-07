# Scala Backend

The Scala backend translates Mochi programs into plain Scala source code. It targets a limited subset of the language and is mainly used for experimentation and verifying features across different runtimes.

## Files

- `compiler.go` – core code generator walking the AST
- `compiler_test.go` – golden tests executing the emitted Scala code
- `helpers.go` – pattern helpers used by the compiler
- `tools.go` – utility ensuring the `scalac` compiler is available

## Compilation Flow

`Compiler.Compile` outputs type declarations and function definitions before building a `Main` object containing a `main` entry point. Remaining statements are emitted inside that method:

```go
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
        for _, s := range prog.Statements {
                if s.Type != nil {
                        if err := c.compileTypeDecl(s.Type); err != nil {
                                return nil, err
                        }
                        c.writeln("")
                }
        }

        c.writeln("object Main {")
        c.indent++
        for _, s := range prog.Statements {
                if s.Fun != nil {
                        if err := c.compileFun(s.Fun); err != nil {
                                return nil, err
                        }
                        c.writeln("")
                } else if s.Type == nil {
                        c.mainStmts = append(c.mainStmts, s)
                }
        }
        c.writeln("def main(args: Array[String]): Unit = {")
        c.indent++
        for _, s := range c.mainStmts {
                if err := c.compileStmt(s); err != nil {
                        return nil, err
                }
        }
        c.indent--
        c.writeln("}")
        c.indent--
        c.writeln("}")
        return c.buf.Bytes(), nil
}
```
【F:compile/scala/compiler.go†L24-L58】

Functions are compiled with type annotations and parameters that are mutated become local `var` bindings:

```go
func (c *Compiler) compileFun(fn *parser.FunStmt) error {
        c.writeIndent()
        c.buf.WriteString("def " + sanitizeName(fn.Name) + "(")
        for i, p := range fn.Params {
                if i > 0 {
                        c.buf.WriteString(", ")
                }
                c.buf.WriteString(fmt.Sprintf("%s: %s", sanitizeName(p.Name), scalaType(c.resolveTypeRef(p.Type))))
        }
        c.buf.WriteString(")")
        if fn.Return != nil {
                c.buf.WriteString(": " + scalaType(c.resolveTypeRef(fn.Return)))
        }
        c.buf.WriteString(" = {\n")
        c.indent++
        for _, p := range fn.Params {
                if paramMutated(fn.Body, p.Name) {
                        c.writeln(fmt.Sprintf("var %s = %s", sanitizeName(p.Name), sanitizeName(p.Name)))
                }
        }
        for _, st := range fn.Body {
                if err := c.compileStmt(st); err != nil {
                        return err
                }
        }
        c.indent--
        c.writeln("}")
        return nil
}
```
【F:compile/scala/compiler.go†L61-L88】

Loops, conditionals and expressions are translated to straightforward Scala constructs. For example `compileFor` handles both range and collection forms:

```go
func (c *Compiler) compileFor(st *parser.ForStmt) error {
        name := sanitizeName(st.Name)
        if st.RangeEnd != nil {
                start, err := c.compileExpr(st.Source)
                if err != nil {
                        return err
                }
                end, err := c.compileExpr(st.RangeEnd)
                if err != nil {
                        return err
                }
                c.writeln(fmt.Sprintf("for (%s <- %s until %s) {", name, start, end))
        } else {
                src, err := c.compileExpr(st.Source)
                if err != nil {
                        return err
                }
                c.writeln(fmt.Sprintf("for (%s <- %s) {", name, src))
        }
        c.indent++
        for _, s := range st.Body {
                if err := c.compileStmt(s); err != nil {
                        return err
                }
        }
        c.indent--
        c.writeln("}")
        return nil
}
```
【F:compile/scala/compiler.go†L214-L241】

Pattern matching is supported via `match` expressions and sealed trait type declarations. Variant types are emitted as case classes extending the trait:

```go
func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
        name := sanitizeName(t.Name)
        if len(t.Variants) > 0 {
                c.writeln("sealed trait " + name)
                for _, v := range t.Variants {
                        vname := sanitizeName(v.Name)
                        fields := make([]string, len(v.Fields))
                        for i, f := range v.Fields {
                                typ := scalaType(c.resolveTypeRef(f.Type))
                                fields[i] = fmt.Sprintf("%s: %s", sanitizeName(f.Name), typ)
                        }
                        params := strings.Join(fields, ", ")
                        c.writeln(fmt.Sprintf("case class %s(%s) extends %s", vname, params, name))
                }
                return nil
        }
        ...
}
```
【F:compile/scala/compiler.go†L559-L573】

Built‑in functions like `print`, `len`, `count`, `avg`, `input` and `str` are recognised by `compileCall` and mapped to idiomatic Scala operations:

```go
switch call.Func {
case "print":
        return fmt.Sprintf("println(%s)", argStr), nil
case "len":
        if len(args) != 1 {
                return "", fmt.Errorf("len expects 1 arg")
        }
        return fmt.Sprintf("%s.length", args[0]), nil
case "count":
        if len(args) != 1 {
                return "", fmt.Errorf("count expects 1 arg")
        }
        return fmt.Sprintf("%s.size", args[0]), nil
case "avg":
        if len(args) != 1 {
                return "", fmt.Errorf("avg expects 1 arg")
        }
        a := args[0]
        return fmt.Sprintf("(%s.sum / %s.size)", a, a), nil
case "input":
        if len(args) != 0 {
                return "", fmt.Errorf("input expects no args")
        }
        return "scala.io.StdIn.readLine()", nil
case "str":
        if len(args) == 1 {
                return args[0] + ".toString()", nil
        }
}
```
【F:compile/scala/compiler.go†L436-L463】

## Tools

The tests rely on `EnsureScala` to check for the Scala compiler and attempt installation on macOS or Linux when missing:

```go
// EnsureScala verifies that the Scala compiler is installed. If missing,
// it attempts a best-effort installation using Homebrew on macOS or apt-get on Linux.
func EnsureScala() error {
        if _, err := exec.LookPath("scalac"); err == nil {
                return nil
        }
        switch runtime.GOOS {
        case "darwin":
                if _, err := exec.LookPath("brew"); err == nil {
                        cmd := exec.Command("brew", "install", "scala")
                        cmd.Stdout = os.Stdout
                        cmd.Stderr = os.Stderr
                        return cmd.Run()
                }
                return fmt.Errorf("scalac missing; install via Homebrew or sbt")
        case "linux":
                if _, err := exec.LookPath("apt-get"); err == nil {
                        cmd := exec.Command("apt-get", "update")
                        cmd.Stdout = os.Stdout
                        cmd.Stderr = os.Stderr
                        if err := cmd.Run(); err != nil {
                                return err
                        }
                        cmd = exec.Command("apt-get", "install", "-y", "scala")
                        cmd.Stdout = os.Stdout
                        cmd.Stderr = os.Stderr
                        if err := cmd.Run(); err == nil {
                                return nil
                        }
                }
                cmd := exec.Command("bash", "-c", "curl -sS https://get.sdkman.io | bash && source $HOME/.sdkman/bin/sdkman-init.sh && sdk install scala")
                cmd.Stdout = os.Stdout
                cmd.Stderr = os.Stderr
                return cmd.Run()
        }
        return fmt.Errorf("unsupported platform: %s", runtime.GOOS)
}
```
【F:compile/scala/tools.go†L10-L45】

## Building

Compile a Mochi program to Scala with the `mochi` CLI then use `scalac` or `scala-cli` to run it:

```bash
mochi build --target scala main.mochi -o Main.scala
scalac Main.scala
scala Main
```

### Python FFI

The backend can call Python libraries through `import python "mod" as m` and
attribute access. For example `m.sqrt(9)` invokes the function using a Python
subprocess.

## Tests

Golden tests under `compile/scala` are tagged `slow` as they invoke the Scala toolchain. They compile programs in `tests/compiler/scala` as well as a subset under `tests/compiler/valid_scala`, run the resulting binaries and compare their output:

```go
func TestScalaCompiler_SubsetPrograms(t *testing.T) {
        if err := scalacode.EnsureScala(); err != nil {
                t.Skipf("scala not installed: %v", err)
        }
        golden.Run(t, "tests/compiler/valid_scala", ".mochi", ".out", func(src string) ([]byte, error) {
                ...
        })
        golden.Run(t, "tests/compiler/scala", ".mochi", ".out", func(src string) ([]byte, error) {
                ...
        })
}
```
【F:compile/scala/compiler_test.go†L20-L33】

Run the tests with:

```bash
go test ./compile/scala -tags slow
```

## Status

The Scala backend currently covers only basic Mochi features. It supports functions, loops, `match` expressions and simple built‑ins but lacks full runtime support. Dataset queries with filtering, sorting and pagination (including joins and grouping) are implemented. It is best suited for examples and for exercising the compiler architecture rather than production use.

## Unsupported features

The following pieces of Mochi are not yet handled by the Scala backend:

- concurrent primitives such as `spawn` and channels
- module system and imports (except for Python FFI)
- generic types and higher‑order functions
- logic queries
- reflection and macro facilities
- streams and agents
- error handling with `try`/`catch`
- logic programming constructs (`fact`, `rule`)
- event emission and intent handlers (`emit`, `on`)
- model declarations
- package imports and export declarations
- complex Python FFI expressions (chained calls or indexing)
- asynchronous functions (`async`/`await`)
- YAML dataset loading and saving
- set collections (`set<T>`) and related operations
- agent initialization with field values
- destructuring bindings in `let` and `var` statements
- extern type declarations

