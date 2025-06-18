# Smalltalk Backend

The Smalltalk backend translates Mochi programs into GNU Smalltalk source. It is a minimal implementation intended for experimentation and for running simple examples via the `gst` interpreter.

## Files

- `compiler.go` – main code generator
- `compiler_test.go` – golden tests executed with `gst`
- `tools.go` – helper that installs GNU Smalltalk when required

## Compilation Flow

`Compiler.Compile` creates a `Main` class, emits any function declarations and then translates remaining statements into the body of the program:

```go
// Compile generates Smalltalk code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
        c.buf.Reset()
        c.writeln("Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!")
        c.writeln("")
        for _, s := range prog.Statements {
                if s.Fun != nil {
                        if err := c.compileFun(s.Fun); err != nil {
                                return nil, err
                        }
                        c.writeln("")
                }
        }
        c.writelnNoIndent("!!")
        for _, s := range prog.Statements {
                if s.Fun != nil || s.Type != nil || s.Test != nil {
                        continue
                }
                if err := c.compileStmt(s); err != nil {
                        return nil, err
                }
        }
        return c.buf.Bytes(), nil
}
```
【F:compile/st/compiler.go†L33-L55】

## Statements and Expressions

Functions are compiled with local variable collection and emitted under the `mochi` method category:

```go
func (c *Compiler) compileFun(fn *parser.FunStmt) error {
        header := fn.Name + ": " + fn.Params[0].Name
        names := []string{fn.Params[0].Name}
        for _, p := range fn.Params[1:] {
                header += " " + p.Name + ": " + p.Name
                names = append(names, p.Name)
        }
        c.funParams[fn.Name] = names
        vars := collectVars(fn.Body)
        c.writeln("!Main class methodsFor: 'mochi'!")
        if len(vars) > 0 {
                c.writeln(header + " | " + strings.Join(vars, " ") + " |")
        } else {
                c.writeln(header)
        }
        c.indent++
        for _, st := range fn.Body {
                if err := c.compileStmt(st); err != nil {
                        return err
                }
        }
        c.indent--
        c.writelnNoIndent("!")
        return nil
}
```
【F:compile/st/compiler.go†L58-L81】

Statement translation covers variable declarations, returns, `for` loops, `if` expressions and simple expression statements:

```go
func (c *Compiler) compileStmt(s *parser.Statement) error {
        switch {
        case s.Let != nil:
                val, err := c.compileExpr(s.Let.Value)
                if err != nil {
                        return err
                }
                c.writeln(fmt.Sprintf("%s := %s.", s.Let.Name, val))
        case s.Return != nil:
                val, err := c.compileExpr(s.Return.Value)
                if err != nil {
                        return err
                }
                c.writeln("^ " + val)
        case s.For != nil:
                return c.compileFor(s.For)
        case s.If != nil:
                return c.compileIf(s.If)
        case s.Expr != nil:
                expr, err := c.compileExpr(s.Expr.Expr)
                if err != nil {
                        return err
                }
                if expr != "" {
                        c.writeln(expr)
                }
        }
        return nil
}
```
【F:compile/st/compiler.go†L114-L142】

`for` loops over ranges become numeric loops using `to:do:`. `while` loops use `whileTrue:` and `if` expressions map directly to `ifTrue:ifFalse:`:

```go
func (c *Compiler) compileFor(f *parser.ForStmt) error {
        if f.RangeEnd == nil {
                return fmt.Errorf("collection loops not supported")
        }
        start, err := c.compileExpr(f.Source)
        if err != nil {
                return err
        }
        end, err := c.compileExpr(f.RangeEnd)
        if err != nil {
                return err
        }
        c.writeln(fmt.Sprintf("%s to: %s - 1 do: [:%s |", start, end, f.Name))
        c.indent++
        for _, st := range f.Body {
                if err := c.compileStmt(st); err != nil {
                        return err
                }
        }
        c.indent--
        c.writeln("]")
        c.writeln(".")
        return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
        cond, err := c.compileExpr(stmt.Cond)
        if err != nil {
                return err
        }
        c.writeln("(" + cond + ") ifTrue: [")
        c.indent++
        for _, st := range stmt.Then {
                if err := c.compileStmt(st); err != nil {
                        return err
                }
        }
        c.indent--
        if len(stmt.Else) > 0 {
                c.writeln("] ifFalse: [")
                c.indent++
                for _, st := range stmt.Else {
                        if err := c.compileStmt(st); err != nil {
                                return err
                        }
                }
                c.indent--
                c.writeln("]")
                c.writeln(".")
        } else {
                c.writeln("]")
                c.writeln(".")
        }
        return nil
}
```
【F:compile/st/compiler.go†L148-L232】

【F:compile/st/compiler.go†L298-L333】
Expressions recurse through binary, unary and postfix helpers. Indexing lists produces `at:` calls with 1-based offsets while slice expressions use `copyFrom:to:`:

```go
func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
        expr, err := c.compilePrimary(p.Target)
        if err != nil {
                return "", err
        }
        for _, op := range p.Ops {
                if op.Index != nil {
                        if op.Index.Colon != nil {
                                start := "1"
                                if op.Index.Start != nil {
                                        s, err := c.compileExpr(op.Index.Start)
                                        if err != nil {
                                                return "", err
                                        }
                                        start = fmt.Sprintf("(%s + 1)", s)
                                }
                                end := fmt.Sprintf("%s size", expr)
                                if op.Index.End != nil {
                                        e, err := c.compileExpr(op.Index.End)
                                        if err != nil {
                                                return "", err
                                        }
                                        end = e
                                }
                                expr = fmt.Sprintf("(%s copyFrom: %s to: %s)", expr, start, end)
                        } else {
                                idx, err := c.compileExpr(op.Index.Start)
                                if err != nil {
                                        return "", err
                                }
                                expr = fmt.Sprintf("(%s at: %s + 1)", expr, idx)
                        }
                }
        }
        return expr, nil
}
```

Calls to `print` and `len` are recognised as built‑ins and converted to `Transcript` output or `size` messages respectively. Other calls dispatch to class methods on `Main`:

```go
func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
        name := call.Func
        args := make([]string, len(call.Args))
        for i, a := range call.Args {
                v, err := c.compileExpr(a)
                if err != nil {
                        return "", err
                }
                args[i] = "(" + v + ")"
        }
        switch name {
        case "print":
                if len(args) != 1 {
                        return "", fmt.Errorf("print expects 1 arg")
                }
                return fmt.Sprintf("Transcript show: %s printString; cr", args[0]), nil
        case "len":
                if len(args) != 1 {
                        return "", fmt.Errorf("len expects 1 arg")
                }
                return fmt.Sprintf("%s size", args[0]), nil
        default:
                params, ok := c.funParams[name]
                if !ok {
                        return "", fmt.Errorf("unsupported call %s", name)
                }
                parts := []string{"Main", name + ":"}
                for i, p := range params {
                        if i == 0 {
                                parts = append(parts, args[0])
                        } else {
                                parts = append(parts, fmt.Sprintf("%s: %s", p, args[i]))
                        }
                }
                return strings.Join(parts, " "), nil
        }
}
```
【F:compile/st/compiler.go†L288-L323】

Literal values are formatted according to their type:

```go
func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
        switch {
        case l.Int != nil:
                return fmt.Sprintf("%d", *l.Int), nil
        case l.Float != nil:
                return fmt.Sprintf("%f", *l.Float), nil
        case l.Bool != nil:
                if *l.Bool {
                        return "true", nil
                }
                return "false", nil
        case l.Str != nil:
                return strconv.Quote(*l.Str), nil
        }
        return "", fmt.Errorf("unknown literal")
}
```
【F:compile/st/compiler.go†L336-L351】

## Tools

`EnsureSmalltalk` checks for the `gst` executable. On Linux it first tries
`apt-get` and if that fails downloads the GNU Smalltalk 3.2.5 sources from
GitHub and builds them. The download URL can be overridden via the
`SMALLTALK_TARBALL` environment variable. On macOS it uses Homebrew:

```go
func EnsureSmalltalk() error {
        if _, err := exec.LookPath("gst"); err == nil {
                return nil
        }
        switch runtime.GOOS {
        case "linux":
                if _, err := exec.LookPath("apt-get"); err == nil {
                        fmt.Println("🔧 Installing GNU Smalltalk via apt-get...")
                        if err := run(exec.Command("apt-get", "update")); err == nil {
                                if err := run(exec.Command("apt-get", "install", "-y", "gnu-smalltalk")); err == nil {
                                        if _, err := exec.LookPath("gst"); err == nil {
                                                return nil
                                        }
                                        fmt.Println("⚠️ apt-get install failed, falling back to build from source")
                                }
                        }
                }
                if err := buildSmalltalkFromSource(); err != nil {
                        return err
                }
        case "darwin":
                if _, err := exec.LookPath("brew"); err == nil {
                        fmt.Println("🍺 Installing GNU Smalltalk via Homebrew...")
                        if err := run(exec.Command("brew", "install", "gnu-smalltalk")); err != nil {
                                return err
                        }
                } else {
                        return fmt.Errorf("brew not found")
                }
        default:
                return fmt.Errorf("unsupported OS: %s", runtime.GOOS)
        }
        if _, err := exec.LookPath("gst"); err == nil {
                return nil
        }
        return fmt.Errorf("gst not found")
}
```
【F:compile/st/tools.go†L10-L52】

## Building

Compile a program to Smalltalk using the `mochi` CLI and run it with GNU Smalltalk:

```bash
mochi build --target st example.mochi -o main.st
gst main.st
```

## Tests

The slow golden tests in `compiler_test.go` generate Smalltalk for each program under `tests/compiler/st` and execute it with `gst`:

```bash
go test ./compile/st -tags slow
```
