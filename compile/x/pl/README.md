# Prolog Backend

The Prolog backend compiles Mochi programs to SWI-Prolog source code. Generated
programs can be run with `swipl` and contain a `main/0` predicate for any
top-level statements.

## Files

- `compiler.go` – entry point and helper tracking
- `statements.go` – compilation of functions and statements
- `expressions.go` – expression and predicate compilation helpers
- `compiler_test.go` – golden tests that compile and run examples with `swipl`
- `helpers.go` – helper functions for indentation and name sanitisation
- `tools.go` – `EnsureSWIPL` helper that locates or installs the interpreter

## Compilation

`Compile` writes a style check directive, outputs function definitions and then
builds a `main` predicate from the remaining statements:

```go
// Compile translates a Mochi AST into Prolog source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.writeln(":- style_check(-singleton).")
	// function declarations
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	oldBuf := c.buf
	oldIndent := c.indent
	c.buf = bytes.Buffer{}
	c.indent = oldIndent + 1
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Type != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s, "_"); err != nil {
			c.buf = oldBuf
			return nil, err
		}
	}
	b := c.buf.Bytes()
	if bytes.HasSuffix(b, []byte(",\n")) {
		b = b[:len(b)-2]
		b = append(b, '\n')
	}
	c.buf = oldBuf
	c.writeln("main :-")
	c.buf.Write(b)
	c.writeln(".")
	c.indent = oldIndent
	c.writeln(":- initialization(main, main).")
	return c.buf.Bytes(), nil
```

Functions are compiled as predicates returning their result via an extra
argument. Each body is wrapped in `catch/3` so that `return` statements can
throw a `return(Value)` term which is intercepted and assigned to the result.
If the final statement is a `return`, a fallback clause is emitted for that
value.

Statements support variable bindings, explicit returns, the `print` builtin,
`expect` assertions, `test` blocks, range `for` loops and simple `if` statements:

```go
	switch {
	case s.Let != nil:
		val, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		name := sanitizeVar(s.Let.Name)
		for _, line := range val.code {
			c.writeln(line)
		}
		c.writeln(fmt.Sprintf("%s = %s,", name, val.val))
       case s.Return != nil:
               val, err := c.compileExpr(s.Return.Value)
               if err != nil {
                       return err
               }
               for _, line := range val.code {
                       c.writeln(line)
               }
               c.writeln(fmt.Sprintf("throw(return(%s))", val.val))
	case s.Expr != nil:
                if call := s.Expr.Expr.Binary.Left.Value.Target.Call; call != nil && call.Func == "print" {
                        if len(call.Args) == 0 {
                                return fmt.Errorf("print expects at least 1 arg")
                        }
                        for i, a := range call.Args {
                                arg, err := c.compileExpr(a)
                                if err != nil {
                                        return err
                                }
                                for _, line := range arg.code {
                                        c.writeln(line)
                                }
                                c.writeln(fmt.Sprintf("write(%s),", arg.val))
                                if i < len(call.Args)-1 {
                                        c.writeln("write(' '),")
                                }
                        }
                        c.writeln("nl,")
                } else {
                        return fmt.Errorf("unsupported expression statement")
                }
	case s.For != nil:
		return c.compileFor(s.For, ret)
	case s.If != nil:
		return c.compileIf(s.If, ret)
	default:
		return fmt.Errorf("unsupported statement")
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt, ret string) error {
	if f.RangeEnd == nil {
		return fmt.Errorf("only range for supported")
	}
	start, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	end, err := c.compileExpr(f.RangeEnd)
	if err != nil {
		return err
	}
	tempEnd := c.newVar()
	for _, line := range start.code {
		c.writeln(line)
	}
	for _, line := range end.code {
		c.writeln(line)
	}
	c.writeln(fmt.Sprintf("%s is %s - 1,", tempEnd, end.val))
	loopVar := sanitizeVar(f.Name)
	c.writeln(fmt.Sprintf("forall(between(%s, %s, %s), (", start.val, tempEnd, loopVar))
	c.indent++
	for _, s := range f.Body {
		if err := c.compileStmt(s, ret); err != nil {
			return err
		}
	}
	c.writeln("true")
	c.indent--
	c.writeln(")),")
	return nil
}
```

Loops rely on `forall/2` combined with `between/3` while `if` uses `->` with a
fallback `true` branch. Expressions handle numeric operators such as addition
and subtraction, equality checks, list indexing via `nth0/3`, literals, lists
and function calls like `len`.

## Helpers

Identifier sanitisation is handled in `helpers.go`:

```go

var plReserved = map[string]bool{
	"is":    true,
	"true":  true,
	"false": true,
}

func sanitizeVar(name string) string {
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	sanitized := b.String()
	if sanitized == "" || !((sanitized[0] >= 'A' && sanitized[0] <= 'Z') || (sanitized[0] >= 'a' && sanitized[0] <= 'z') || sanitized[0] == '_') {
		sanitized = "_" + sanitized
	}
	if plReserved[sanitized] {
		sanitized = "_" + sanitized
	}
	if sanitized[0] >= 'a' && sanitized[0] <= 'z' {
		sanitized = strings.ToUpper(sanitized[:1]) + sanitized[1:]
	}
	return sanitized
}

func sanitizeAtom(name string) string {
	name = strings.ReplaceAll(name, "-", "_")
	return strings.ToLower(name)
}
```

`sanitizeVar` converts Mochi identifiers into valid Prolog variables, avoiding
reserved words and ensuring an initial capital letter. `sanitizeAtom` converts
function names to lowercase atoms.

## Tools

`EnsureSWIPL` verifies that `swipl` exists and attempts installation via
`apt-get` on Linux or Homebrew on macOS if missing. The optional `SWIPL_BIN`
environment variable can be used to point to a custom interpreter path:

```go
// EnsureSWIPL verifies that the SWI-Prolog interpreter is installed. If missing,
// it attempts a best-effort installation using apt-get on Linux or Homebrew on
// macOS.
func EnsureSWIPL() error {
        if bin := os.Getenv("SWIPL_BIN"); bin != "" {
                if _, err := os.Stat(bin); err == nil {
                        return nil
                }
        }
        if _, err := exec.LookPath("swipl"); err == nil {
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
			cmd = exec.Command("apt-get", "install", "-y", "swi-prolog")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "swi-prolog")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	}
	if _, err := exec.LookPath("swipl"); err == nil {
		return nil
	}
	return fmt.Errorf("swipl not found")
```

## Building

Compile a Mochi source file to Prolog and run it with:

```bash
mochi build --target pl main.mochi -o main.pl
swipl -q main.pl
```

## Tests

Golden tests under `compile/pl` generate code for programs in
`tests/compiler/pl` and execute them with `swipl`. They are tagged `slow` because
the Prolog interpreter is invoked:

```bash
go test ./compile/pl -tags slow
```

## Test Blocks and `expect`

The Prolog backend supports `expect` statements for assertions and `test` blocks
to group them. Each test block is compiled into a predicate named
`test_<name>` which runs the block body. Compiled programs call all generated
test predicates from `main/0`, so running the produced Prolog file executes the
tests automatically. Failed expectations raise an exception with the message
`expect failed`.

## Supported Features

The Prolog backend can compile a subset of Mochi focused on core control flow and list processing. Currently implemented features include:

- Function definitions with typed parameters and return values
- `let` and `var` declarations with assignment
- Arithmetic and comparison operators plus list concatenation with `+`
- Lists and maps with indexing, slicing and element assignment
- Struct literals using Prolog dicts with field selectors
- Membership checks using `in` on lists, strings and maps
- Built-in functions `len`, `count`, `avg`, `str` and `input`
- `print` statements
- `if` statements and expressions
- Range `for` loops and iteration over lists or strings
- List set operations `union`, `union all`, `except` and `intersect`
- `while` loops with `break` and `continue`
- Expect assertions and `test` blocks
- Logic programming facts, rules and `query` expressions
- Anonymous function literals (`fun` expressions)
- Passing functions as parameters and return values
 - Dataset queries with optional filtering, sorting, joins and limits
- Helper predicates `dataset_filter/3` and `dataset_paginate/4` for filtering
  and paginating row lists
- Data loading and persistence helpers `load_data/3` and `save_data/3` for JSON files
## Unsupported Features

The Prolog backend focuses on basic control flow and list operations. Several
Mochi language features are not yet implemented:

- Pattern matching expressions and union types
 - Dataset query grouping
- Enum declarations and variant matching
- Concurrency primitives and external helpers like `_fetch` or `_genText`
- Import declarations (currently ignored)
- Agent and stream features (`agent`, `on`, `emit`)
- Intent handlers within agents (`intent` blocks)
- Extern declarations and foreign imports
- Model declarations
- Generative AI helpers (`generate`, `model`)
- Embedding generation with `generate embedding`
- Package imports and exports

## any2mochi Converter

`tools/any2mochi` includes a Prolog converter that uses `prolog-lsp` to generate
Mochi code from existing Prolog source files. When the language server is not
available, the converter falls back to a simple regex based parser.

### Supported Features

- Extraction of predicate names and arity via document symbols.
- Parameter names retrieved from hover information when available.
- Basic parsing of predicate bodies to convert `write/1` calls and simple
  assignments.
- Regex fallback when `prolog-lsp` is missing.

### Unsupported Features

- Type information or return values.
- Directives and module metadata not reported by the language server.
- Control flow constructs such as loops or conditionals.
- Complex expressions and dataset helpers.
