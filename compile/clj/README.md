# Clojure Backend

The Clojure backend translates a limited subset of Mochi into Clojure source code. It is primarily used for experiments and currently supports only basic language features such as functions, `let` bindings, loops and simple expressions.

## Files

- `compiler.go` – walks the AST and emits Clojure forms
- `helpers.go` – helper utilities (for example, sanitising identifiers)
- `tools.go` – best effort installation of the `clojure` command for tests
- `compiler_test.go` – golden tests that execute the generated code

## Implementation Notes

The `Compiler` type collects top level statements and emits them in order. Functions are defined using `(defn ...)` and returns are implemented by throwing a small exception so that nested expressions can unwind:

```go
// Compiler translates a Mochi AST into Clojure source code (limited subset).
type Compiler struct {
        buf       bytes.Buffer
        indent    int
        env       *types.Env
        mainStmts []*parser.Statement
}
```
【F:compile/clj/compiler.go†L13-L19】

When compiling a function, a `try`/`catch` block catches the special `return` exception and extracts its value:

```go
c.writeln("(try")
...
c.writeln("(catch clojure.lang.ExceptionInfo e")
c.indent++
c.writeln("(if (= (.getMessage e) \"return\")")
c.indent++
c.writeln("(:value (ex-data e))")
```
【F:compile/clj/compiler.go†L56-L71】

Variable names are made safe for Clojure via `sanitizeName` in `helpers.go`:

```go
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
}
```
【F:compile/clj/helpers.go†L1-L25】

For numeric ranges the compiler emits a `(loop ... (recur ...))` form, while sequence iteration uses `doseq`:

```go
c.writeln(fmt.Sprintf("(doseq [%s %s]", name, src))
```
【F:compile/clj/compiler.go†L182-L183】

Calls to `print` and `len` are mapped to `println` and `count` respectively:

```go
if name == "print" {
        expr = fmt.Sprintf("(println %s)", strings.Join(args, " "))
} else if name == "len" {
        expr = fmt.Sprintf("(count %s)", args[0])
}
```
【F:compile/clj/compiler.go†L286-L291】

Test blocks are compiled to functions named `test_<name>` and executed after the main program. Inside these functions `expect` statements become simple assertions via Clojure's `assert` form.

## Tooling

Tests require the Clojure CLI tool. `EnsureClojure` tries to install it using `apt-get` or Homebrew if missing:

```go
// EnsureClojure verifies that the clojure command line tool is installed.
// It attempts a best-effort installation using apt-get on Linux or Homebrew on
// macOS. Tests can call this to skip if installation fails.
func EnsureClojure() error {
        if _, err := exec.LookPath("clojure"); err == nil {
                return nil
        }
        if _, err := exec.LookPath("clj"); err == nil {
                return nil
        }
        ...
}
```
【F:compile/clj/tools.go†L10-L18】

## Running Tests

The tests are tagged `slow` because they invoke the Clojure toolchain. Run them with:

```bash
go test ./compile/clj -tags slow
```

They compare the execution output of generated programs in `tests/compiler/clj` with predefined golden files.

The test suite also compiles and runs the example LeetCode solutions in
`examples/leetcode/1` through `examples/leetcode/6` to verify that these programs
execute correctly using the Clojure backend.

## Status

The backend only implements a small subset of Mochi and is mainly a proof of concept. More advanced features such as data sets, agents or LLM helpers are not currently supported.
