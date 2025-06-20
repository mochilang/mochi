# Lua Backend

The Lua backend translates Mochi programs into portable Lua source. It focuses on straightforward code generation so that examples can be run with any standard Lua interpreter.

## Files

- `compiler.go` – main code generator
- `compiler_test.go` – golden tests that execute generated Lua
- `helpers.go` – utilities for writing indented code and analysing the AST
- `tools.go` – ensures a Lua interpreter is available for tests

## Overview

`Compiler` keeps a buffer and emits statements while tracking indentation and helper usage:

```go
// Compiler translates a Mochi AST into Lua source code.
type Compiler struct {
        buf    bytes.Buffer
        indent int
        env    *types.Env

        loopLabels []string
        labelCount int

        tmpCount int

        helpers map[string]bool
}
```
【F:compile/lua/compiler.go†L12-L29】

`Compile` walks the program twice: first emitting all function declarations then the main body. After compiling statements it injects any helper functions required by built‑ins and finally returns the complete source:

```go
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
        c.buf.Reset()

        // Emit function declarations first.
        for _, s := range prog.Statements {
                if s.Fun != nil {
                        if err := c.compileFun(s.Fun, false); err != nil {
                                return nil, err
                        }
                        c.writeln("")
                }
        }

        // Emit test block declarations.
        for _, s := range prog.Statements {
                if s.Test != nil {
                        if err := c.compileTestBlock(s.Test); err != nil {
                                return nil, err
                        }
                        c.writeln("")
                }
        }

        // Emit main body.
        for _, s := range prog.Statements {
                if s.Fun != nil || s.Type != nil || s.Test != nil {
                        continue
                }
                if err := c.compileStmt(s); err != nil {
                        return nil, err
                }
        }

        // Invoke test blocks.
        for _, s := range prog.Statements {
                if s.Test != nil {
                        name := "test_" + sanitizeName(s.Test.Name)
                        c.writeln(name + "()")
                }
        }

        bodyBytes := append([]byte(nil), c.buf.Bytes()...)
        c.buf.Reset()
        c.emitHelpers()
        c.buf.Write(bodyBytes)
        return c.buf.Bytes(), nil
}
```
【F:compile/lua/compiler.go†L31-L80】

## Statements and Expressions

The compiler supports the usual Mochi statements such as variable declarations, assignments, conditionals and loops. `for` loops over ranges produce numeric loops while loops over collections call the `__iter` helper:

```go
func (c *Compiler) compileFor(s *parser.ForStmt) error {
        label := c.pushLoopLabel()
        name := sanitizeName(s.Name)
        if s.RangeEnd != nil {
                start, err := c.compileExpr(s.Source)
                ...
                c.writeln(fmt.Sprintf("for %s = %s, (%s)-1 do", name, start, end))
        } else {
                src, err := c.compileExpr(s.Source)
                ...
                c.helpers["iter"] = true
                c.writeln(fmt.Sprintf("for _, %s in __iter(%s) do", name, src))
        }
        ...
        c.writeln("::" + label + "::")
        c.writeln("end")
        c.popLoopLabel()
        return nil
}
```
【F:compile/lua/compiler.go†L191-L222】

Binary operations handle boolean and arithmetic operators. Membership (`in`) and division invoke helpers on demand:

```go
switch opstr {
case "&&":
        expr = fmt.Sprintf("(%s and %s)", l, r)
case "||":
        expr = fmt.Sprintf("(%s or %s)", l, r)
case "in":
        c.helpers["contains"] = true
        expr = fmt.Sprintf("__contains(%s, %s)", r, l)
case "/":
        c.helpers["div"] = true
        expr = fmt.Sprintf("__div(%s, %s)", l, r)
default:
        expr = fmt.Sprintf("(%s %s %s)", l, opstr, r)
}
```
【F:compile/lua/compiler.go†L302-L317】

Built‑in function calls are translated directly or mapped to helpers when needed:

```go
switch name {
case "print":
        return fmt.Sprintf("print(%s)", argStr), nil
case "str":
        if len(args) == 1 {
                return fmt.Sprintf("tostring(%s)", args[0]), nil
        }
        return fmt.Sprintf("tostring(%s)", argStr), nil
case "input":
        c.helpers["input"] = true
        return "__input()", nil
case "count":
        c.helpers["count"] = true
        return fmt.Sprintf("__count(%s)", argStr), nil
case "avg":
        c.helpers["avg"] = true
        return fmt.Sprintf("__avg(%s)", argStr), nil
}
```
【F:compile/lua/compiler.go†L453-L485】

Query expressions without joins compile into Lua loops. Sorting, skipping and taking are handled inline:

```go
// handle simple cross join without sort/skip/take
if len(q.Froms) > 0 && sortExpr == "" && skipExpr == "" && takeExpr == "" {
        ...
        b.WriteString("\treturn _res\n")
        b.WriteString("end)()")
        return b.String(), nil
}

b.WriteString("\tlocal items = {}\n")
... // optional sort, skip and take
b.WriteString("\tlocal _res = {}\n")
... // build result
b.WriteString("end)()")
```
【F:compile/lua/compiler.go†L530-L615】

Pattern matching on union values expands into conditional checks. Captured fields are passed through a small wrapper when necessary:

```go
if call, ok := callPattern(cs.Pattern); ok {
        if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
                st := ut.Variants[call.Func]
                cond = fmt.Sprintf("%s.__name == \"%s\"", tmp, call.Func)
                names := []string{}
                values := []string{}
                for idx, arg := range call.Args {
                        if id, ok := identName(arg); ok && id != "_" {
                                names = append(names, sanitizeName(id))
                                field := sanitizeName(st.Order[idx])
                                values = append(values, fmt.Sprintf("%s.%s", tmp, field))
                        }
                }
                if len(names) > 0 {
                        res = fmt.Sprintf("(function(%s) return %s end)(%s)", strings.Join(names, ", "), res, strings.Join(values, ", "))
                }
        }
}
```
【F:compile/lua/compiler.go†L606-L642】

## Runtime Helpers

Helpers are emitted only when referenced. `emitHelpers` writes Lua functions such as `__iter`, `__div`, `__contains`, `__input`, `__count`, `__avg`, `__index`, `__indexString` and `__slice`:

```go
if c.helpers["iter"] {
        c.writeln("function __iter(obj)")
        ...
        c.writeln("end")
        c.writeln("")
}
if c.helpers["div"] {
        c.writeln("function __div(a, b)")
        ...
        c.writeln("end")
        c.writeln("")
}
```
【F:compile/lua/compiler.go†L756-L814】

Additional helpers implement containment checks, input reading, counting, averaging, indexing and slicing:

```go
if c.helpers["contains"] {
        c.writeln("function __contains(container, item)")
        ...
}
...
if c.helpers["slice"] {
        c.writeln("function __slice(obj, i, j)")
        ...
}
```
【F:compile/lua/compiler.go†L814-L1018】

## Tooling

`EnsureLua` tries to install the Lua interpreter using `apt-get` on Linux or Homebrew on macOS when tests are run:

```go
// EnsureLua verifies that the Lua interpreter is installed. If missing,
// it attempts a best-effort installation on Linux or macOS.
func EnsureLua() error {
        if _, err := exec.LookPath("lua"); err == nil {
                return nil
        }
        switch runtime.GOOS {
        case "linux":
                if _, err := exec.LookPath("apt-get"); err == nil {
                        fmt.Println("🔧 Installing Lua...")
                        cmd := exec.Command("apt-get", "update")
                        ...
                }
        case "darwin":
                if _, err := exec.LookPath("brew"); err == nil {
                        fmt.Println("🍺 Installing Lua via Homebrew...")
                        cmd := exec.Command("brew", "install", "lua")
                        ...
                }
        }
        if _, err := exec.LookPath("lua"); err == nil {
                return nil
        }
        return fmt.Errorf("lua not found")
}
```
【F:compile/lua/tools.go†L1-L46】

## Building

Compile a Mochi file to Lua with:

```bash
mochi build --target lua main.mochi -o main.lua
lua main.lua
```

The generated source relies only on the standard Lua library plus the small helpers embedded during compilation.

Test blocks within a Mochi file are compiled into Lua functions and automatically
invoked after the main body.

## Tests

The golden tests compile programs in `tests/compiler/lua` and execute them with `lua`. They are tagged `slow` because the Lua toolchain must be present. Run them with:

```bash
go test ./compile/lua -tags slow
```

The `LeetCodeExamples` test additionally compiles and runs the first five
solutions under `examples/leetcode` to verify basic program execution.

## Notes

The Lua backend supports most core language features but skips advanced LLM helpers and external objects. Query expressions handle simple `from` clauses along with `where`, `sort`, `skip` and `take`. The emphasis is on readability and portability of the emitted Lua code.

### Unsupported Features

Some Mochi features are not yet implemented for Lua and can cause programs to
fail at runtime:

- Regular expression helpers beyond simple `match`
- Mutating lists while iterating (e.g. `insert`, `remove`)
- Query `group` clauses and joins with `left`, `right` or `outer` sides
- Local recursive functions
- HTTP `fetch` expressions
- Logic query expressions
- Foreign function interface (FFI)
- Interaction with external objects
- Error handling with `try`/`catch` blocks
- Set collections (`set<T>`) and related operations
- Model and stream declarations
- Methods declared inside `type` blocks
- Reflection and macro facilities

- Concurrency primitives such as `spawn`, `stream`, `agent` and related features
  are not available
- Module imports and package declarations

`load` and `save` support JSON, YAML and CSV formats. Other formats like JSONL
are not implemented.

Problems `6`, `10`, `23` and `27` currently do not run correctly when compiled
to Lua.
