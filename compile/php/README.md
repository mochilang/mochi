# PHP Backend

The PHP backend emits plain PHP source code from a Mochi program. It is
minimal yet functional, providing a way to run Mochi scripts anywhere a
`php` interpreter is available.

## Files

- `compiler.go` – walks the AST and writes PHP code
- `compiler_test.go` – golden tests that execute generated programs
- `helpers.go` – utility helpers for indentation and name sanitisation
- `tools.go` – installs the `php` CLI when required for tests

## Compilation flow

`Compiler.Compile` resets the buffer, writes `<?php`, outputs all function
definitions, then emits the body of the program:

```go
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
        c.buf.Reset()
        c.writeln("<?php")
        // functions first
        for _, s := range prog.Statements {
                if s.Fun != nil {
                        if err := c.compileFun(s.Fun); err != nil {
                                return nil, err
                        }
                        c.writeln("")
                }
        }
        // main body
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
【F:compile/php/compiler.go†L23-L47】

Statements such as variable declarations, loops and conditionals are handled
in `compileStmt`:

```go
func (c *Compiler) compileStmt(s *parser.Statement) error {
        switch {
        case s.Let != nil:
                return c.compileLet(s.Let)
        case s.Var != nil:
                return c.compileVar(s.Var)
        case s.Assign != nil:
                return c.compileAssign(s.Assign)
        case s.Return != nil:
                val, err := c.compileExpr(s.Return.Value)
                if err != nil {
                        return err
                }
                c.writeln("return " + val + ";")
                return nil
        case s.For != nil:
                return c.compileFor(s.For)
        case s.While != nil:
                return c.compileWhile(s.While)
        case s.If != nil:
                return c.compileIf(s.If)
        case s.Break != nil:
                c.writeln("break;")
                return nil
        case s.Continue != nil:
                c.writeln("continue;")
                return nil
        case s.Expr != nil:
                expr, err := c.compileExpr(s.Expr.Expr)
                if err != nil {
                        return err
                }
                if expr != "" {
                        c.writeln(expr + ";")
                }
                return nil
        default:
                return nil
        }
}
```
【F:compile/php/compiler.go†L52-L91】

Loops are emitted as regular PHP `for` or `foreach` constructs. A range loop
results in code like:

```go
func (c *Compiler) compileFor(f *parser.ForStmt) error {
        name := "$" + sanitizeName(f.Name)
        if f.RangeEnd != nil {
                start, err := c.compileExpr(f.Source)
                if err != nil {
                        return err
                }
                end, err := c.compileExpr(f.RangeEnd)
                if err != nil {
                        return err
                }
                c.writeln(fmt.Sprintf("for (%s = %s; %s < %s; %s++) {", name, start, name, end, name))
        } else {
                src, err := c.compileExpr(f.Source)
                if err != nil {
                        return err
                }
                c.writeln(fmt.Sprintf("foreach ((is_string(%[1]s) ? str_split(%[1]s) : %[1]s) as %s) {", src, name))
        }
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
```
【F:compile/php/compiler.go†L154-L181】

## Built‑in functions

`compileCallExpr` recognises several standard library calls and emits idiomatic
PHP:

```go
switch name {
case "print":
        if len(args) == 0 {
                return "", fmt.Errorf("print expects at least 1 arg")
        }
        joined := strings.Join(args, " . \" \" . ")
        return fmt.Sprintf("echo %s, PHP_EOL", joined), nil
case "len":
        if len(args) != 1 {
                return "", fmt.Errorf("len expects 1 arg")
        }
        return fmt.Sprintf("count(%s)", args[0]), nil
case "str":
        if len(args) != 1 {
                return "", fmt.Errorf("str expects 1 arg")
        }
        return fmt.Sprintf("strval(%s)", args[0]), nil
case "input":
        if len(args) != 0 {
                return "", fmt.Errorf("input expects no args")
        }
        return "trim(fgets(STDIN))", nil
case "count":
        if len(args) != 1 {
                return "", fmt.Errorf("count expects 1 arg")
        }
        return fmt.Sprintf("(is_array(%[1]s) ? count(%[1]s) : strlen(%[1]s))", args[0]), nil
case "avg":
        if len(args) != 1 {
                return "", fmt.Errorf("avg expects 1 arg")
        }
        return fmt.Sprintf("(count(%[1]s) ? array_sum(%[1]s) / count(%[1]s) : 0)", args[0]), nil
default:
        return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", ")), nil
}
```
【F:compile/php/compiler.go†L350-L394】

## Helper utilities

Names are sanitised in `helpers.go` so generated variables are valid PHP
identifiers:

```go
func sanitizeName(name string) string {
        if name == "" {
                return ""
        }
        var b strings.Builder
        for i, r := range name {
                if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
                        b.WriteRune(r)
                } else {
                        b.WriteRune('_')
                }
        }
        s := b.String()
        if s == "" || !((s[0] >= 'A' && s[0] <= 'Z') || (s[0] >= 'a' && s[0] <= 'z') || s[0] == '_') {
                s = "_" + s
        }
        return s
}
```
【F:compile/php/helpers.go†L20-L36】

## PHP installation helper

`tools.go` offers `EnsurePHP` which attempts to install the PHP CLI via
`apt-get`, `apk`, or Homebrew when missing:

```go
// EnsurePHP ensures the php command is available, attempting installation if missing.
func EnsurePHP() error {
        if _, err := exec.LookPath("php"); err == nil {
                return nil
        }
        switch runtime.GOOS {
        case "linux":
                if _, err := exec.LookPath("apt-get"); err == nil {
                        cmd := exec.Command("apt-get", "update")
                        cmd.Stdout = os.Stdout
                        cmd.Stderr = os.Stderr
                        if err := cmd.Run(); err != nil {
                                return err
                        }
                        cmd = exec.Command("apt-get", "install", "-y", "php-cli")
                        cmd.Stdout = os.Stdout
                        cmd.Stderr = os.Stderr
                        if err := cmd.Run(); err == nil {
                                break
                        }
                }
        case "darwin":
                if _, err := exec.LookPath("brew"); err == nil {
                        cmd := exec.Command("brew", "install", "php")
                        cmd.Stdout = os.Stdout
                        cmd.Stderr = os.Stderr
                        if err := cmd.Run(); err == nil {
                                break
                        }
                }
        }
        if _, err := exec.LookPath("php"); err == nil {
                return nil
        }
        return fmt.Errorf("php not installed")
}
```
【F:compile/php/tools.go†L10-L52】

## Building

Use the `mochi` CLI to generate PHP:

```bash
mochi build --target php main.mochi -o main.php
php main.php
```

For example, to run the [two-sum](../../examples/leetcode/1/two-sum.mochi),
[add-two-numbers](../../examples/leetcode/2/add-two-numbers.mochi),
or [longest-substring-without-repeating-characters](../../examples/leetcode/3/longest-substring-without-repeating-characters.mochi)
LeetCode solutions you can compile and execute them directly:

```bash
mochi build --target php ../../examples/leetcode/1/two-sum.mochi -o two-sum.php
php two-sum.php

mochi build --target php ../../examples/leetcode/2/add-two-numbers.mochi -o add-two-numbers.php
php add-two-numbers.php

mochi build --target php ../../examples/leetcode/3/longest-substring-without-repeating-characters.mochi -o longest-substring.php
php longest-substring.php

mochi build --target php ../../examples/leetcode/4/median-of-two-sorted-arrays.mochi -o median.php
php median.php

mochi build --target php ../../examples/leetcode/5/longest-palindromic-substring.mochi -o palindrome.php
php palindrome.php

mochi build --target php ../../examples/leetcode/6/zigzag-conversion.mochi -o zigzag-conversion.php
php zigzag-conversion.php

mochi build --target php ../../examples/leetcode/7/reverse-integer.mochi -o reverse-integer.php
php reverse-integer.php

mochi build --target php ../../examples/leetcode/8/string-to-integer-atoi.mochi -o atoi.php
php atoi.php

mochi build --target php ../../examples/leetcode/9/palindrome-number.mochi -o palindrome-number.php
php palindrome-number.php

mochi build --target php ../../examples/leetcode/10/regular-expression-matching.mochi -o regular-expression-matching.php
php regular-expression-matching.php
```

## Tests

Golden tests under `tests/compiler/php` compile and run each program using the
system `php` interpreter. They are tagged `slow`:

```bash
go test ./compile/php -tags slow
```

The tests skip automatically if `php` is not installed and rely on
`EnsurePHP` for installation when possible.【F:compile/php/compiler_test.go†L58-L110】

## Notes

The PHP backend currently supports a subset of Mochi's features suitable for
scripts and simple utilities. Complex type handling and advanced runtime
support are not yet implemented.

### Unsupported features

The PHP compiler is intentionally lightweight. The following Mochi features are
currently **not supported**:

- modules or imports
- concurrency primitives
- generics and complex type inference
- advanced dataset queries such as joins or grouping
- skip/take query clauses
- nested functions that capture variables across scopes

