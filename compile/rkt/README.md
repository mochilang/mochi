# Racket Backend

The Racket backend emits plain Racket source code from Mochi programs. It covers a
small subset of the language and is mainly used for testing and experiments.

## Files

- `compiler.go` – walks the AST and writes Racket forms
- `compiler_test.go` – golden tests that run the generated code
- `helpers.go` – small utilities such as identifier sanitising
- `tools.go` – helper used by tests to install `racket`

## Compilation process

`Compile` starts by emitting the `#lang racket` header, then generates any
function declarations followed by top-level statements:

```go
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
        c.writeln("#lang racket")
        c.writeln("")
        // function declarations first
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
                if s.Fun == nil && s.Type == nil && s.Test == nil {
                        if err := c.compileStmt(s); err != nil {
                                return nil, err
                        }
                }
        }
        return c.buf.Bytes(), nil
}
```

【F:compile/rkt/compiler.go†L24-L44】

Functions are implemented using `define` and a `let/ec` block to mimic early
returns:

```go
c.writeIndent()
c.buf.WriteString("(define (" + name)
for _, p := range fn.Params {
        c.buf.WriteString(" " + sanitizeName(p.Name))
}
c.buf.WriteString(")\n")
c.indent++
c.writeln("(let/ec return")
...
c.writeln("(return (void))") // default return when none hit
c.indent--
c.writeln(")")
c.indent--
c.writeln(")")
```

【F:compile/rkt/compiler.go†L47-L68】

Loops are lowered to Racket's `for` form. When iterating over a numeric range the
compiler emits `in-range`:

```go
if f.RangeEnd != nil {
        start, err := c.compileExpr(f.Source)
        ...
        c.writeln(fmt.Sprintf("(for ([%s (in-range %s %s)])", name, start, end))
} else {
        src, err := c.compileExpr(f.Source)
        ...
        c.writeln(fmt.Sprintf("(for ([%s %s])", name, src))
}
```

【F:compile/rkt/compiler.go†L111-L129】

Only a small set of binary operators is recognised. They map directly to
Racket primitives:

```go
switch op.Op {
case "+":
        val = fmt.Sprintf("(+ %s %s)", val, rhs)
case "-":
        val = fmt.Sprintf("(- %s %s)", val, rhs)
case "*":
        val = fmt.Sprintf("(* %s %s)", val, rhs)
case "/":
        val = fmt.Sprintf("(/ %s %s)", val, rhs)
case "==":
        val = fmt.Sprintf("(= %s %s)", val, rhs)
case "!=":
        val = fmt.Sprintf("(not (= %s %s))", val, rhs)
}
```

【F:compile/rkt/compiler.go†L183-L196】

Builtin functions `len` and `print` are rewritten to `length` and `displayln` in
`compileCallExpr`:

```go
name := sanitizeName(call.Func)
switch call.Func {
case "len":
        name = "length"
case "print":
        name = "displayln"
}
```

【F:compile/rkt/compiler.go†L277-L284】

## Building

Compile a Mochi source file to Racket and execute it with the `racket` command:

```bash
mochi build --target rkt main.mochi -o main.rkt
racket main.rkt
```

## Tests

Tests are tagged `slow` because they invoke the external `racket` tool. Run them
with:

```bash
go test ./compile/rkt -tags slow
```

`EnsureRacket` attempts a best-effort installation via `apt-get` on Linux or
Homebrew on macOS when tests are run:

```go
// EnsureRacket verifies that the Racket binary is installed. If missing,
// it attempts a best-effort installation using apt-get on Linux or
// Homebrew on macOS.
func EnsureRacket() error {
        if _, err := exec.LookPath("racket"); err == nil {
                return nil
        }
        switch runtime.GOOS {
        case "linux":
                fmt.Println("🔧 Installing Racket via apt-get...")
                ...
        case "darwin":
                fmt.Println("🍺 Installing Racket via Homebrew...")
                ...
        }
        if _, err := exec.LookPath("racket"); err == nil {
                return nil
        }
        return fmt.Errorf("racket not found")
}
```

【F:compile/rkt/tools.go†L10-L45】

## Notes

The Racket backend is intentionally minimal. Conditionals now support simple
`else` and `else if` branches, but complex expressions may still be rejected.
The generated code aims
for readability over performance and mirrors Mochi constructs closely.
Loops support `break` and `continue` using `let/ec` and named `let` constructs.
Binary operators now include `union`, `union_all`, `except` and `intersect` for
basic list set operations.
`match` expressions compile directly to Racket's `match` form for simple pattern
matching.
Dataset helpers `_fetch`, `_load` and `_save` handle basic JSON data.
The `in` operator now works with lists and strings, not just maps, and slice
assignments like `xs[1:3] = sub` are supported.
Basic dataset queries support `sort`, `skip` and `take` clauses, and simple
`struct` type declarations emit Racket structs.

Unsupported features currently include:

* Generative `generate` blocks and model definitions
* Dataset query `group` clauses and join side options
* Error handling with `try`/`catch`
* Agents, streams and intents
* Logic programming constructs (`fact`, `rule`, `query`)
* Concurrency primitives such as `spawn` and channels
* Foreign function interface via `import`
* Package management and `package` declarations
* Union type declarations
* LLM helpers like `_genText`, `_genEmbed` and `_genStruct`
* Multi-dimensional slice assignment or indexing beyond two levels
* Methods defined inside `type` blocks
* Export statements via `export`
* Extern declarations (`extern var`, `extern fun`, `extern type`, `extern object`)
