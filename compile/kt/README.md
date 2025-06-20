# Kotlin Backend

The Kotlin backend generates Kotlin source code from Mochi programs. It is useful for running Mochi code on the JVM or integrating it with Kotlin projects.

## Files

- `compiler.go` – main code generator walking the AST
- `compiler_test.go` – golden tests executing generated code
- `helpers.go` – helpers for identifier sanitisation and pattern detection
- `tools.go` – ensures the Kotlin toolchain is installed for tests

## Compilation Flow

`Compiler.Compile` emits type and function declarations then builds a `main` function for any top-level statements:

```go
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
        for _, s := range prog.Statements {
                if s.Type != nil {
                        if err := c.compileTypeDecl(s.Type); err != nil {
                                return nil, err
                        }
                        c.writeln("")
                        continue
                }
                if s.Fun != nil {
                        if err := c.compileFun(s.Fun); err != nil {
                                return nil, err
                        }
                        c.writeln("")
                        continue
                }
                c.mainStmts = append(c.mainStmts, s)
        }
        if len(c.mainStmts) > 0 {
                c.writeln("fun main() {")
                c.indent++
                for _, s := range c.mainStmts {
                        if err := c.compileStmt(s); err != nil {
                                return nil, err
                        }
                }
                c.indent--
                c.writeln("}")
        }
        c.writeln("")
        return c.buf.Bytes(), nil
}
```

## Type Declarations

Union and struct types are converted to `sealed interface` and `data class` definitions:

```go
func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
        name := sanitizeName(t.Name)
        if len(t.Variants) > 0 {
                c.writeln(fmt.Sprintf("sealed interface %s", name))
                for _, v := range t.Variants {
                        vname := sanitizeName(v.Name)
                        fields := []string{}
                        for _, f := range v.Fields {
                                typ := ktType(c.resolveTypeRef(f.Type))
                                fields = append(fields, fmt.Sprintf("val %s: %s", sanitizeName(f.Name), typ))
                        }
                        if len(fields) == 0 {
                                c.writeln(fmt.Sprintf("data class %s() : %s", vname, name))
                        } else {
                                c.writeln(fmt.Sprintf("data class %s(%s) : %s", vname, joinArgs(fields), name))
                        }
                }
                return nil
        }
        fields := []string{}
        for _, m := range t.Members {
                if m.Field == nil {
                        continue
                }
                typ := ktType(c.resolveTypeRef(m.Field.Type))
                fields = append(fields, fmt.Sprintf("val %s: %s", sanitizeName(m.Field.Name), typ))
        }
        c.writeln(fmt.Sprintf("data class %s(%s)", name, joinArgs(fields)))
        return nil
}
```

## Dataset Queries

Query expressions are translated to chained collection operations using `filter`, `sortedBy`, `drop`, `take` and `map`:

```go
func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
        src, err := c.compileExpr(q.Source)
        if err != nil {
                return "", err
        }
        varName := sanitizeName(q.Var)
        sel, err := c.compileExpr(q.Select)
        if err != nil {
                return "", err
        }
        var where, sortKey, skip, take string
        if q.Where != nil {
                where, err = c.compileExpr(q.Where)
                if err != nil {
                        return "", err
                }
        }
        if q.Sort != nil {
                sortKey, err = c.compileExpr(q.Sort)
                if err != nil {
                        return "", err
                }
        }
        if q.Skip != nil {
                skip, err = c.compileExpr(q.Skip)
                if err != nil {
                        return "", err
                }
        }
        if q.Take != nil {
                take, err = c.compileExpr(q.Take)
                if err != nil {
                        return "", err
                }
        }
        var buf bytes.Buffer
        buf.WriteString("run {\n")
        buf.WriteString("                var res = " + src + "\n")
        if where != "" {
                buf.WriteString(fmt.Sprintf("                res = res.filter { %s -> %s }\n", varName, where))
        }
        if sortKey != "" {
                buf.WriteString(fmt.Sprintf("                res = res.sortedBy { %s -> %s }\n", varName, sortKey))
        }
        if skip != "" {
                buf.WriteString("                res = res.drop(" + skip + ")\n")
        }
        if take != "" {
                buf.WriteString("                res = res.take(" + take + ")\n")
        }
        buf.WriteString(fmt.Sprintf("                res = res.map { %s -> %s }\n", varName, sel))
        buf.WriteString("                res\n")
        buf.WriteString("        }")
        return buf.String(), nil
}
```

## Pattern Matching

`match` expressions emit a `when` block and unpack union variants as needed:

```go
func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
        target, err := c.compileExpr(m.Target)
        if err != nil {
                return "", err
        }
        var buf bytes.Buffer
        buf.WriteString("run {\n")
        buf.WriteString("                val _t = " + target + "\n")
        buf.WriteString("                when {\n")
        for i, cse := range m.Cases {
                res, err := c.compileExpr(cse.Result)
                if err != nil {
                        return "", err
                }
                if call, ok := callPattern(cse.Pattern); ok {
                        if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
                                st := ut.Variants[call.Func]
                                buf.WriteString("                        _t is " + sanitizeName(call.Func) + " -> {\n")
                                for idx, arg := range call.Args {
                                        if id, ok := identName(arg); ok && id != "_" {
                                                field := sanitizeName(st.Order[idx])
                                                buf.WriteString(fmt.Sprintf("                                val %s = _t.%s\n", sanitizeName(id), field))
                                        }
                                }
                                buf.WriteString("                                " + res + "\n")
                                buf.WriteString("                        }\n")
                                continue
                        }
                }
                if ident, ok := identName(cse.Pattern); ok {
                        if _, ok := c.env.FindUnionByVariant(ident); ok {
                                buf.WriteString("                        _t is " + sanitizeName(ident) + " -> " + res + "\n")
                                continue
                        }
                }
                if isUnderscoreExpr(cse.Pattern) {
                        buf.WriteString("                        else -> " + res + "\n")
                        continue
                }
                pat, err := c.compileExpr(cse.Pattern)
                if err != nil {
                        return "", err
                }
                buf.WriteString("                        _t == " + pat + " -> " + res + "\n")
                if i == len(m.Cases)-1 {
                        buf.WriteString("                        else -> null\n")
                }
        }
        buf.WriteString("                }\n")
        buf.WriteString("        }")
        return buf.String(), nil
}
```

## Built‑in Functions

`compilePrimary` rewrites common built‑ins to idiomatic Kotlin equivalents:

```go
if name == "print" {
        return "println(" + joinArgs(args) + ")", nil
}
if name == "len" && len(args) == 1 {
        return args[0] + ".size", nil
}
if name == "count" && len(args) == 1 {
        return args[0] + ".size", nil
}
if name == "avg" && len(args) == 1 {
        return args[0] + ".average()", nil
}
if name == "str" && len(args) == 1 {
        return args[0] + ".toString()", nil
}
if name == "input" && len(args) == 0 {
        return "readln()", nil
}
```

## Kotlin Toolchain

The tests rely on `EnsureKotlin` which installs `kotlinc` if it is missing:

```go
// EnsureKotlin verifies that the Kotlin compiler is installed. If missing,
// it attempts a best-effort installation using Homebrew on macOS or apt-get on Linux.
func EnsureKotlin() error {
        if _, err := exec.LookPath("java"); err != nil {
                switch runtime.GOOS {
                case "darwin":
                        if _, err := exec.LookPath("brew"); err == nil {
                                cmd := exec.Command("brew", "install", "openjdk")
                                cmd.Stdout = os.Stdout
                                cmd.Stderr = os.Stderr
                                _ = cmd.Run()
                        }
                case "linux":
                        if _, err := exec.LookPath("apt-get"); err == nil {
                                cmd := exec.Command("apt-get", "update")
                                cmd.Stdout = os.Stdout
                                cmd.Stderr = os.Stderr
                                if err := cmd.Run(); err == nil {
                                        cmd = exec.Command("apt-get", "install", "-y", "openjdk-17-jdk")
                                        cmd.Stdout = os.Stdout
                                        cmd.Stderr = os.Stderr
                                        _ = cmd.Run()
                                }
                        }
                }
                if err := javacode.EnsureJavac(); err != nil {
                        return err
                }
        }
        if _, err := exec.LookPath("kotlinc"); err == nil {
                return nil
        }
        switch runtime.GOOS {
        case "darwin":
                if _, err := exec.LookPath("brew"); err == nil {
                        cmd := exec.Command("brew", "install", "kotlin")
                        cmd.Stdout = os.Stdout
                        cmd.Stderr = os.Stderr
                        return cmd.Run()
                }
                return fmt.Errorf("kotlinc missing; install via Homebrew or Android Studio")
        case "linux":
                if _, err := exec.LookPath("apt-get"); err == nil {
                        cmd := exec.Command("apt-get", "update")
                        cmd.Stdout = os.Stdout
                        cmd.Stderr = os.Stderr
                        if err := cmd.Run(); err != nil {
                                return err
                        }
                        cmd = exec.Command("apt-get", "install", "-y", "kotlin")
                        cmd.Stdout = os.Stdout
                        cmd.Stderr = os.Stderr
                        if err := cmd.Run(); err == nil {
                                return nil
                        }
                }
                cmd := exec.Command("bash", "-c", "curl -sS https://get.sdkman.io | bash && source $HOME/.sdkman/bin/sdkman-init.sh && sdk install kotlin")
                cmd.Stdout = os.Stdout
                cmd.Stderr = os.Stderr
                return cmd.Run()
        }
        return fmt.Errorf("unsupported platform: %s", runtime.GOOS)
}
```

## Building

Generate Kotlin code with the build command:

```bash
mochi build --target kt main.mochi -o Main.kt
```

The produced `Main.kt` can be compiled with `kotlinc` and executed using the standard JVM tools.

## Tests

Golden tests under `tests/compiler/kt` compile and execute each program. They are tagged `slow` since they invoke the Kotlin toolchain:

```bash
go test ./compile/kt -tags slow
```

The tests automatically skip when `kotlinc` is unavailable.

## Unsupported Features

The Kotlin backend still lacks several Mochi features that other compilers
support:

- Advanced dataset queries such as joins, grouping and union operations
- Streams, agents and intent handlers
- Logic programming constructs (`fact`, `rule`, `query`)
- Package declarations and the foreign function interface
- `generate` and `fetch` expressions for LLM and HTTP integration
- External helpers like `_genText` and concurrency primitives such as `spawn` and channels


