# Swift Backend

The Swift backend translates Mochi programs into Swift source code. It currently covers a minimal subset of the language and is mainly used for verifying cross-language behaviour.

## Files

- `compiler.go` – core code generator
- `compiler_test.go` – golden tests that compile and run the generated Swift code
- `helpers.go` – small helpers for indentation and block formatting
- `tools.go` – helper for installing the Swift toolchain during tests

## Supported Features

The Swift backend currently supports:

- Basic statements, loops and conditionals
- Lists and maps including negative indexing
- Inner joins and simple dataset queries
- Built‑in functions: `len`, `count`, `str`, `upper`, `lower`, `concat`, `avg`, `sum`, `min`, `max`, `now`, `json`, `input`, `abs`

## Compilation Flow

`Compiler.Compile` walks the Mochi AST, emits function declarations and wraps remaining statements in a `main` function:

```go
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
        c.useAvg = false
        c.useIndexStr = false

        var body bytes.Buffer
        oldBuf := c.buf
        c.buf = body
        c.indent = 0

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
        c.writeln("func main() {")
        c.indent++
        for _, s := range prog.Statements {
                if s.Fun != nil {
                        continue
                }
                if err := c.compileStmt(s); err != nil {
                        return nil, err
                }
        }
        c.indent--
        c.writeln("}")
        c.writeln("main()")
```
【F:compile/swift/compiler.go†L24-L55】

After emitting the body the compiler injects helper functions on demand and returns the final source:

```go
        c.writeln("import Foundation")
        if c.useAvg {
                c.writeln("func _avg<T: BinaryInteger>(_ arr: [T]) -> Double {")
                // ...
                c.writeln("}")
                c.writeln("func _avg<T: BinaryFloatingPoint>(_ arr: [T]) -> Double {")
                // ...
                c.writeln("}")
        }
        if c.useIndexStr {
                c.writeln("func _indexString(_ s: String, _ i: Int) -> String {")
                // ...
                c.writeln("}")
        }
```
【F:compile/swift/compiler.go†L60-L94】

## Type Mapping

`compileType` converts Mochi types into Swift equivalents such as `Int`, `Double` and `[T]` for lists:

```go
func (c *Compiler) compileType(t *parser.TypeRef) string {
        if t == nil {
                return "Void"
        }
        if t.Fun != nil {
                params := make([]string, len(t.Fun.Params))
                for i, p := range t.Fun.Params {
                        params[i] = c.compileType(p)
                }
                ret := c.compileType(t.Fun.Return)
                return "(" + strings.Join(params, ", ") + ") -> " + ret
        }
        if t.Simple != nil {
                switch *t.Simple {
                case "int":
                        return "Int"
                case "float":
                        return "Double"
                case "bool":
                        return "Bool"
                case "string":
                        return "String"
                default:
                        return *t.Simple
                }
        }
        if t.Generic != nil {
                if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
                        return "[" + c.compileType(t.Generic.Args[0]) + "]"
                }
                if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
                        return "[" + c.compileType(t.Generic.Args[0]) + ": " + c.compileType(t.Generic.Args[1]) + "]"
                }
        }
        return "Any"
}
```
【F:compile/swift/compiler.go†L124-L158】

## Built‑in Functions

`compilePrimary` recognises several built‑ins and maps them to Swift equivalents:

```go
switch p.Call.Func {
case "len":
        if len(args) != 1 { return "", fmt.Errorf("len expects 1 arg") }
        return fmt.Sprintf("%s.count", args[0]), nil
case "count":
        if len(args) != 1 { return "", fmt.Errorf("count expects 1 arg") }
        return fmt.Sprintf("%s.count", args[0]), nil
case "str":
        if len(args) != 1 { return "", fmt.Errorf("str expects 1 arg") }
        return fmt.Sprintf("String(%s)", args[0]), nil
case "upper":
        if len(args) != 1 { return "", fmt.Errorf("upper expects 1 arg") }
        return fmt.Sprintf("%s.uppercased()", args[0]), nil
case "lower":
        if len(args) != 1 { return "", fmt.Errorf("lower expects 1 arg") }
        return fmt.Sprintf("%s.lowercased()", args[0]), nil
case "abs":
        if len(args) != 1 { return "", fmt.Errorf("abs expects 1 arg") }
        return fmt.Sprintf("abs(%s)", args[0]), nil
case "avg":
        if len(args) != 1 { return "", fmt.Errorf("avg expects 1 arg") }
        c.useAvg = true
        return fmt.Sprintf("_avg(%s.map { Double($0) })", args[0]), nil
case "input":
        if len(args) != 0 { return "", fmt.Errorf("input expects 0 args") }
        return "readLine() ?? \"\"", nil
default:
        return fmt.Sprintf("%s(%s)", p.Call.Func, strings.Join(args, ", ")), nil
}
```
【F:compile/swift/compiler.go†L468-L497】

Negative string indexing is supported via the `_indexString` helper when a string is indexed with a potentially negative value.
List indexing now handles negative values through the generic `_index` helper.
List set operations `union`, `union all`, `except` and `intersect` use helper functions that deduplicate or combine arrays as needed.

## Swift Installation

`EnsureSwift` from `tools.go` installs the Swift toolchain if missing:

```go
// EnsureSwift verifies that the Swift toolchain is installed. If missing,
// it attempts a best-effort installation using Homebrew on macOS or apt-get on Linux.
func EnsureSwift() error {
        if _, err := exec.LookPath("swiftc"); err == nil {
                return nil
        }
        switch runtime.GOOS {
        case "darwin":
                if _, err := exec.LookPath("brew"); err == nil {
                        cmd := exec.Command("brew", "install", "swift")
                        cmd.Stdout = os.Stdout
                        cmd.Stderr = os.Stderr
                        return cmd.Run()
                }
                return fmt.Errorf("swift toolchain missing; install Xcode command line tools")
        case "linux":
                if _, err := exec.LookPath("apt-get"); err == nil {
                        cmd := exec.Command("apt-get", "update")
                        cmd.Stdout = os.Stdout
                        cmd.Stderr = os.Stderr
                        if err := cmd.Run(); err != nil {
                                return err
                        }
                        cmd = exec.Command("apt-get", "install", "-y", "swiftlang")
                        cmd.Stdout = os.Stdout
                        cmd.Stderr = os.Stderr
                        if err := cmd.Run(); err == nil {
                                return nil
                        }
                }
                cmd := exec.Command("bash", "-c", "curl -sSL https://swift.org/install.sh | bash")
                cmd.Stdout = os.Stdout
                cmd.Stderr = os.Stderr
                return cmd.Run()
        }
        return fmt.Errorf("unsupported platform: %s", runtime.GOOS)
}
```
【F:compile/swift/tools.go†L10-L45】

`Format` prettifies generated Swift code using `swift-format` when available. `EnsureSwiftFormat` can install the formatter if missing.
【F:compile/x/swift/tools.go†L61-L130】

## Building

Generate Swift code from a Mochi program with:

```bash
mochi build --target swift main.mochi -o main.swift
swiftc main.swift -o main
./main
```

`EnsureSwift` can be used by tests or scripts to install the toolchain automatically.

To experiment, try compiling the first three [LeetCode](../../examples/leetcode) examples:

```bash
mochi build --target swift ../../examples/leetcode/1/two-sum.mochi -o two-sum.swift
swiftc two-sum.swift -o two-sum
./two-sum

mochi build --target swift ../../examples/leetcode/2/add-two-numbers.mochi -o add-two-numbers.swift
swiftc add-two-numbers.swift -o add-two-numbers
./add-two-numbers

mochi build --target swift ../../examples/leetcode/3/longest-substring-without-repeating-characters.mochi -o longest.swift
swiftc longest.swift -o longest
./longest
```

## Tests

The golden tests are tagged `slow` because they compile and run the generated Swift code:

```bash
go test ./compile/swift -tags slow
```

`compiler_test.go` parses programs from `tests/compiler/swift`, compiles them, runs `swiftc`, and compares the output.

## Notes

This backend implements only a subset of Mochi. It handles basic statements, loops, conditionals, lists, maps and the built‑ins listed above. It is suitable for experimentation but not yet a full-featured Swift target.

## Unsupported Features

The Swift backend still lacks support for a number of language capabilities:

- Dataset queries with left, right or outer joins. Inner joins along with
  grouping, sorting and pagination are supported.
- Type inference for empty collections
- Streams, agents and intent handlers
 - The ``generate`` expression for LLM integration
- Package declarations and the foreign function interface
- Set collections and related operations
- Model declarations using ``model`` blocks
- ``fact`` and ``rule`` statements for logic programming
- Import statements
- Concurrency primitives like ``spawn`` and channels
- The ``eval`` builtin function
- Error handling with ``try``/``catch``
- Asynchronous ``async``/``await`` functions
 - Generic type parameters for functions or types
 - Macro or reflection facilities
 - YAML dataset loading or saving
