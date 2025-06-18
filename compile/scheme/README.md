# Scheme Backend

The Scheme backend translates a subset of Mochi programs to plain Scheme source code targeting the [chibi-scheme](https://github.com/ashinn/chibi-scheme) interpreter.

## Files

- `compiler.go` – walks the AST and generates Scheme code
- `compiler_test.go` – golden tests that execute the generated Scheme
- `tools.go` – helper that locates or installs `chibi-scheme`

## Implementation details

Functions use `call/cc` to implement early returns:
```scheme
func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	c.writeln(fmt.Sprintf("(define (%s %s)", sanitizeName(fn.Name), strings.Join(params, " ")))
	c.indent++
	c.writeln("(call/cc (lambda (return)")
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("))")
	c.indent--
	c.writeln(")")
	return nil
```

`for` loops translate to a recursive `let loop` form:
```scheme
func (c *Compiler) compileFor(st *parser.ForStmt) error {
	name := sanitizeName(st.Name)
	start, err := c.compileExpr(st.Source)
	if err != nil {
		return err
	}
	end, err := c.compileExpr(st.RangeEnd)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(let loop ((%s %s))", name, start))
	c.indent++
	c.writeln(fmt.Sprintf("(if (< %s %s)", name, end))
	c.indent++
	c.writeln("(begin")
	c.indent++
	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.writeln(fmt.Sprintf("(loop (+ %s 1))", name))
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln("'())")
	c.indent--
	c.writeln(")")
	return nil
```

`if` statements emit normal Scheme `if` expressions with optional else blocks:
```scheme
func (c *Compiler) compileSimpleIf(st *parser.IfStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(if %s", cond))
	c.indent++
	c.writeln("(begin")
	c.indent++
	for _, s := range st.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln(")")
	if len(st.Else) > 0 {
		c.writeln("(begin")
		c.indent++
		for _, s := range st.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln(")")
	} else {
		c.writeln("'()")
	}
	c.indent--
	c.writeln(")")
```

The compiler only recognises two builtin functions. `len` maps to `(length)` and `print` emits `(display ...)` with a newline:
```scheme
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		return fmt.Sprintf("(length %s)", args[0]), nil
        case "print":
                if len(args) == 0 {
                        return "", fmt.Errorf("print expects at least 1 arg")
                }
                parts := make([]string, 0, len(args)*2+1)
                for i, a := range args {
                        if i > 0 {
                                parts = append(parts, "(display \" \" )")
                        }
                        parts = append(parts, fmt.Sprintf("(display %s)", a))
                }
                parts = append(parts, "(newline)")
                return "(begin " + strings.Join(parts, " ") + ")", nil
        }
```

Identifiers are sanitised to valid Scheme identifiers:
```scheme

func sanitizeName(name string) string {
	if name == "" {
		return ""
	}
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('a' <= r && r <= 'z') || ('A' <= r && r <= 'Z') || ('0' <= r && r <= '9' && i > 0) {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	s := b.String()
	if s == "" {
		return "_"
	}
	if !(s[0] >= 'a' && s[0] <= 'z' || s[0] >= 'A' && s[0] <= 'Z' || s[0] == '_') {
		s = "_" + s
	}
	return s
```

## Example

Compiling a simple loop:
```mochi
for i in 1..4 {
  print(i)
}
```

Produces Scheme code like:
```scheme
(let loop ((i 1))
        (if (< i 4)
                (begin
                        (begin (display i) (newline))
                        (loop (+ i 1))
                )
        '())
)
```

Compiling a while loop:
```mochi
var i = 0
while i < 3 {
  print(i)
  i = i + 1
}
```

Produces Scheme code like:
```scheme
(define i 0)
(let loop ()
        (if (< i 3)
                (begin
                        (begin (display i) (newline))
                        (set! i (+ i 1))
                        (loop)
                )
        '())
)
```

## Building

Run the Mochi CLI to generate Scheme and execute it with chibi-scheme:
```bash
mochi build --target scheme main.mochi -o main.scm
chibi-scheme -m chibi main.scm
```

`EnsureScheme()` attempts to install chibi-scheme when missing:
```go
// EnsureScheme verifies that chibi-scheme is installed. On Linux it attempts a
// best-effort installation using apt-get, while on macOS it tries Homebrew.
func EnsureScheme() (string, error) {
	if path, err := exec.LookPath("chibi-scheme"); err == nil {
		return path, nil
	}
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "chibi-scheme")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return "", err
			}
			cmd = exec.Command("apt-get", "install", "-y", "chibi-scheme")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if path, err := exec.LookPath("chibi-scheme"); err == nil {
		return path, nil
	}
	return "", fmt.Errorf("chibi-scheme not found")
```

## Tests

`compiler_test.go` executes the LeetCode examples using the generated Scheme code:
```go
// TestSchemeCompiler_LeetCode1 runs the two-sum example.
func TestSchemeCompiler_LeetCode1(t *testing.T) {
        runLeetExample(t, 1)
}

// TestSchemeCompiler_LeetCode2 runs the add-two-numbers example.
func TestSchemeCompiler_LeetCode2(t *testing.T) {
        runLeetExample(t, 2)
}

// TestSchemeCompiler_LeetCode3 runs the longest-substring example.
func TestSchemeCompiler_LeetCode3(t *testing.T) {
        runLeetExample(t, 3)
}
```

Execute them with the `slow` tag as they invoke an external interpreter:
```bash
go test ./compile/scheme -tags slow
```

