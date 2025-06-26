# Zig Backend

The Zig backend translates Mochi programs into [Zig](https://ziglang.org/) source code. It currently
covers a small subset of the language and is mainly used for experimentation.

## Files

- `compiler.go` – main code generator
- `infer.go` – basic type inference helpers
- `compiler_test.go` – golden test that compiles the generated Zig code
- `tools.go` – helper locating the Zig compiler

## Compilation Flow

`Compiler.Compile` walks the AST, emits function declarations first and then the
`main` function for the remaining statements. The standard library import is
prepended at the end:

```go
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
        // compile functions first
        for _, s := range prog.Statements {
                if s.Fun != nil {
                        if err := c.compileFun(s.Fun); err != nil {
                                return nil, err
                        }
                        c.writeln("")
                }
        }
        // main body
        c.writeln("pub fn main() void {")
        c.indent++
        for _, s := range prog.Statements {
                if s.Fun != nil {
                        continue
                }
                if err := c.compileStmt(s, false); err != nil {
                        return nil, err
                }
        }
        c.indent--
        c.writeln("}")

        // prepend import
        body := c.buf.String()
        c.buf.Reset()
        c.writeln("const std = @import(\"std\");")
        c.writeln("")
        c.buf.WriteString(body)
        return c.buf.Bytes(), nil
}
```

【F:compile/zig/compiler.go†L36-L68】

Functions are emitted with typed parameters and the return type resolved using `zigType`:

```go
func (c *Compiler) compileFun(fn *parser.FunStmt) error {
        name := sanitizeName(fn.Name)
        params := make([]string, len(fn.Params))
        for i, p := range fn.Params {
                typ := c.zigType(p.Type)
                params[i] = fmt.Sprintf("%s: %s", sanitizeName(p.Name), typ)
        }
        ret := "void"
        if fn.Return != nil {
                ret = c.zigType(fn.Return)
        }
        c.writeln(fmt.Sprintf("fn %s(%s) %s {", name, strings.Join(params, ", "), ret))
        ...
}
```

【F:compile/zig/compiler.go†L70-L78】

## Features

### Type Mapping

`zigType` converts Mochi type references to Zig types. Lists become slices and
maps use `std.AutoHashMap`:

```go
func (c *Compiler) zigType(t *parser.TypeRef) string {
        if t == nil {
                return "i32"
        }
        if t.Generic != nil && t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
                return "[]const " + c.zigType(t.Generic.Args[0])
        }
        if t.Generic != nil && t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
                return fmt.Sprintf("std.AutoHashMap(%s, %s)", c.zigType(t.Generic.Args[0]), c.zigType(t.Generic.Args[1]))
        }
        if t.Simple == nil {
                return "i32"
        }
        switch *t.Simple {
        case "int":
                return "i32"
        case "float":
                return "f64"
        case "bool":
                return "bool"
        case "string":
                return "[]const u8"
        }
        return "i32"
}
```

【F:compile/zig/compiler.go†L204-L228】

### Type Inference

`infer.go` analyses expressions to determine their types when a declaration
lacks an explicit type. This allows the compiler to emit `var` and `const`
definitions with concrete Zig types instead of relying on `var` inference.

### Statement Handling

`compileStmt` supports variable declarations, returns, `for` loops, expression
statements and `if` chains:

```go
func (c *Compiler) compileStmt(s *parser.Statement, inFun bool) error {
        switch {
        case s.Let != nil:
                ...
        case s.Return != nil:
                ...
        case s.For != nil:
                start, err := c.compileExpr(s.For.Source, false)
                ...
                if s.For.RangeEnd != nil {
                        c.writeln(fmt.Sprintf("for (%s .. %s) |%s| {", start, end, name))
                } else {
                        c.writeln(fmt.Sprintf("for (%s) |%s| {", start, name))
                }
                ...
        case s.Expr != nil:
                v, err := c.compileExpr(s.Expr.Expr, false)
                ...
        case s.If != nil:
                return c.compileIf(s.If)
        default:
                return fmt.Errorf("unsupported statement")
        }
        return nil
}
```

【F:compile/zig/compiler.go†L115-L176】

Conditionals are expanded recursively to handle `else if` chains:

```go
func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
        cond, err := c.compileExpr(stmt.Cond, false)
        ...
        if stmt.ElseIf != nil {
                c.writeIndent()
                c.buf.WriteString("} else ")
                return c.compileIf(stmt.ElseIf)
        }
        if len(stmt.Else) > 0 {
                c.writeln("} else {")
                ...
        }
        c.writeln("}")
        return nil
}
```

【F:compile/zig/compiler.go†L182-L213】

### Functions

Named functions are emitted as Zig `fn` declarations. When a function is marked
`export` it becomes `pub fn` so it can be imported from other Zig files. Mochi
also supports anonymous function expressions which compile to inline Zig
functions and can be assigned to variables or passed as parameters.

### Built‑ins

`compileCallExpr` implements simple built‑ins like `len` and `print`:

```go
func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
        name := sanitizeName(call.Func)
        if name == "len" && len(call.Args) == 1 {
                arg, err := c.compileExpr(call.Args[0], false)
                ...
                if c.isMapExpr(call.Args[0]) {
                        return fmt.Sprintf("%s.count()", arg), nil
                }
                return fmt.Sprintf("(%s).len", arg), nil
        }
        if name == "print" && len(call.Args) == 1 {
                arg, err := c.compileExpr(call.Args[0], false)
                ...
                return fmt.Sprintf("std.debug.print(\"{}\\n\", .{%s})", arg), nil
        }
        ...
}
```

When applied to maps, `len` and `count` use the container's `count()` method.
The helpers `_map_keys` and `_map_values` build slices of all keys or values
respectively when the `keys` or `values` builtin is used.
【F:compile/zig/compiler.go†L624-L698】

### Struct Types

`compileTypeDecl` emits Zig structs for `type` declarations. Methods defined
inside the type are translated to struct methods that take `self` as the first
parameter. `compileStructLiteral` constructs values using field initializers:

```go
const Person = struct {
        name: []const u8,
        age: i32,
        fn greet(self: *Person) []const u8 {
                return _concat_string("hi ", self.name);
        },
};

let p = Person{ .name = "Ada", .age = 42 };
print(p.greet());
```

### Helpers

Variable declarations with `var` are now supported. Empty list values create a
`std.ArrayList` for dynamic appends. Assignments of the form `list = list + [x]`
translate to `list.append(x)`, and `while` statements map directly to Zig's
`while` syntax. Numeric loops using `range(start, end, step)` now lower to a
temporary counter and `while` loop for the step value. `break` and `continue`
pass through unchanged. List membership
checks using the `in` operator compile to helper functions for integer and
string lists. String and list concatenation with `+` emit calls to
`_concat_string` or `_concat_list`.

List literals are emitted as fixed-size arrays or references when used in return
expressions. Reserved words are prefixed with `_` by `sanitizeName`:

```go
var zigReserved = map[string]bool{
        "fn": true, "var": true, "const": true, "pub": true, "return": true,
        "for": true, "while": true, "if": true, "else": true,
}
```

【F:compile/zig/compiler.go†L1119-L1121】

### Test Blocks

`test` blocks are emitted as regular functions named `test_<name>` and invoked
from `main`. Expectations use a simple helper:

```zig
fn expect(cond: bool) void {
if (!cond) @panic("expect failed");
}
```


### Dataset Queries

`compileQueryExpr` lowers `from`/`where`/`select` expressions into loops that
build an in-memory slice using `std.ArrayList`. Basic pagination with `skip` and
`take` is handled by slicing the result. More advanced features like joins or
grouping remain unsupported.

```go
func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
        if len(q.Froms) > 0 || len(q.Joins) > 0 || q.Group != nil || q.Sort != nil {
                return "", fmt.Errorf("unsupported query features")
        }
        ...
}
```

【F:compile/x/zig/compiler.go†L562-L640】

## Building

Generate Zig code and build it with the Zig compiler:

```bash
mochi build --target zig main.mochi -o main.zig
zig build-exe main.zig -O ReleaseSafe -femit-bin=main
./main
```

## Tests

`compiler_test.go` runs a golden test using the real Zig compiler. It compiles several LeetCode examples and compares the output:

```go
func TestZigCompiler_LeetCode1to10(t *testing.T) {
        zigc, err := zigcode.EnsureZig()
        if err != nil {
                t.Skipf("zig compiler not installed: %v", err)
        }
        ...
        if out, err := exec.Command(zigc, "build-exe", file, "-O", "ReleaseSafe", "-femit-bin="+exe).CombinedOutput(); err != nil {
                t.Fatalf("zig build error: %v\n%s", err, out)
        }
        out, err := exec.Command(exe).CombinedOutput()
        ...
}
```

【F:compile/zig/compiler_test.go†L17-L53】

`EnsureZig` locates the Zig binary and allows the tests to skip gracefully if it
is not present:

```go
func EnsureZig() (string, error) {
        if path, err := exec.LookPath("zig"); err == nil {
                return path, nil
        }
        dir := filepath.Join(os.TempDir(), "zig-0.12.0")
        bin := filepath.Join(dir, "zig")
        if _, err := os.Stat(bin); err == nil {
                return bin, nil
        }
        // download and install Zig if missing
        // (see tools.go for full implementation)
}
```

【F:compile/zig/tools.go†L1-L17】

## Notes

This backend is intentionally minimal. It does not yet support complex types,
streams or the full Mochi standard library but demonstrates how Mochi can target
a different runtime using Zig.

### Unsupported Features

The Zig generator currently omits several language constructs needed for later
LeetCode solutions:

- advanced string slicing (step values) and indexing on assignment
- iteration over map key/value pairs (only key iteration is implemented)
- functions with multiple return values
- generic type parameters
- nested list types beyond one dimension
- union type declarations
- built-in error handling with `try`/`catch`
- set collections (`set<T>`) and reflection features
- asynchronous functions (`async`/`await`)
- the `eval` builtin function
- YAML dataset loading and saving
 - built-in helpers like `fetch` and `generate`
- logic programming constructs (`fact`, `rule`, `query`)
- concurrency features such as streams or `spawn`
- arrow function syntax (`fun(x: int): int => x + 1`)
- foreign imports and `extern` declarations
- automatic language imports (`import python "..." auto`)
- agent declarations and `on` handlers
- intent functions within agents
- extern object declarations
- stream declarations and `emit` statements
- model declarations
- package declarations and `export` statements
- destructuring bindings in `let` and `var` statements
- nested function declarations inside other functions
- methods declared inside `type` blocks
- enum type declarations
- variadic functions
- closures that capture surrounding variables
- dataset joins with outer or right side options
- agent initialization with field values

These features are not yet implemented, so programs relying on them will fail to
compile or run correctly. Most LeetCode tasks after problem 10 depend on at
least one of these constructs.

Until these are implemented the compiler can only compile the first few
problems.
