# Ruby Backend

The Ruby backend translates Mochi programs into plain Ruby source. It is useful for running Mochi code in environments where Ruby is available or for experimenting with the language on other platforms.

## Files

- `compiler.go` – walks the AST and emits Ruby code
- `compiler_test.go` – golden tests that compile and execute the generated Ruby
- `helpers.go` – utilities for indentation and name sanitisation
- `tools.go` – helper to locate or install the `ruby` binary

## Built‑in functions

When compiling call expressions the generator maps several Mochi built‑ins to their Ruby equivalents:

```go
switch expr {
case "print":
    expr = fmt.Sprintf("puts([%s].join(\" \"))", argStr)
case "len", "count":
    expr = fmt.Sprintf("(%s).length", args[0])
case "str":
    expr = fmt.Sprintf("(%s).to_s", args[0])
case "upper":
    expr = fmt.Sprintf("(%s).to_s.upcase", args[0])
case "lower":
    expr = fmt.Sprintf("(%s).to_s.downcase", args[0])
case "avg":
    expr = fmt.Sprintf("((%[1]s).length > 0 ? (%[1]s).sum(0.0) / (%[1]s).length : 0)", args[0])
case "input":
    expr = "STDIN.gets.to_s.strip"
}
```
【F:compile/rb/compiler.go†L503-L526】

## Query expressions

`compileQueryExpr` handles dataset queries including filtering, sorting and pagination. Basic `group by` clauses are supported when used alone. Cross joins and single `join` clauses (including `left join`) are implemented. Unmatched records from a `left join` use `nil` for the join variable.

## Pattern matching

Match expressions are translated using a temporary variable and a chain of Ruby `if`/`elsif` checks:

```go
b.WriteString(fmt.Sprintf("\t%s = %s\n", tmp, target))
...
b.WriteString("\telse\n\t\tnil\n\tend\nend)")
```
【F:compile/rb/compiler.go†L708-L768】

When all map keys are identifiers the backend emits `OpenStruct` objects and inserts the required `require 'ostruct'` header:

```go
if c.useOpenStruct {
    c.writeln("require 'ostruct'")
    c.writeln("")
}
```
【F:compile/rb/compiler.go†L56-L58】

```go
if identOnly {
    c.useOpenStruct = true
    return "OpenStruct.new(" + strings.Join(items, ", ") + ")", nil
}
```
【F:compile/rb/compiler.go†L598-L608】

## Building

Use the `mochi` CLI to compile a program to Ruby:

```bash
mochi build --target rb main.mochi -o main.rb
ruby main.rb
```

### Example: LeetCode Two Sum

To compile and run the `two-sum` example located in `examples/leetcode/1`, use:

```bash
mochi build --target rb examples/leetcode/1/two-sum.mochi -o two-sum.rb
ruby two-sum.rb
```

The program prints the indices of the elements that sum to the target:

```
0
1
```

### Example: LeetCode Add Two Numbers

To compile and run the `add-two-numbers` example located in `examples/leetcode/2`, use:

```bash
mochi build --target rb examples/leetcode/2/add-two-numbers.mochi -o add-two-numbers.rb
ruby add-two-numbers.rb
```

This prints the resulting list digits on separate lines.

### Example: LeetCode Longest Substring Without Repeating Characters

To compile and run the `longest-substring-without-repeating-characters` example located in `examples/leetcode/3`, use:

```bash
mochi build --target rb examples/leetcode/3/longest-substring-without-repeating-characters.mochi -o longest.rb
ruby longest.rb
```

The program defines the function and unit tests but running the compiled Ruby simply evaluates without output.

### Example: LeetCode Median of Two Sorted Arrays

To compile and run the `median-of-two-sorted-arrays` example located in `examples/leetcode/4`, use:

```bash
mochi build --target rb examples/leetcode/4/median-of-two-sorted-arrays.mochi -o median.rb
ruby median.rb
```

This file only contains unit tests so there is no output when the program runs.

### Example: LeetCode Longest Palindromic Substring

To compile and run the `longest-palindromic-substring` example located in `examples/leetcode/5`, use:

```bash
mochi build --target rb examples/leetcode/5/longest-palindromic-substring.mochi -o palindrome.rb
ruby palindrome.rb
```

Like the previous example this program just evaluates the test cases.

`tools.go` provides helpers to ensure required tools are present. `EnsureRuby` installs Ruby on Linux or macOS when missing. `EnsureFormatter` checks for the `standardrb` or `rubocop` gems so generated code can be prettified:

```go
// EnsureRuby verifies that the ruby binary is installed. If missing, it
// attempts a best-effort installation using apt-get on Linux or Homebrew on
// macOS.
```
【F:compile/x/rb/tools.go†L12-L66】

```go
// EnsureFormatter checks for either standardrb or rubocop so generated code can
// be formatted.
```
【F:compile/x/rb/tools.go†L90-L118】

## Tests

The golden tests exercise the backend by compiling example programs in `tests/compiler/rb` and running them with Ruby:

```bash
go test ./compile/rb -tags slow
```

The first test is skipped when Ruby is not available and the suite falls back to comparing generated code.

## Supported Features

The Ruby backend covers a wide slice of Mochi. Implemented functionality includes:

- Function definitions and calls
- Control flow with `if`, `for` and `while`
- Structs, unions and pattern matching with `match`
- Lists and maps with indexing, slicing, membership checks and concatenation
- Iterating over map keys in `for` loops
- Dataset queries with `from`, `join`, `left join`, `right join`, `where`, `group by`, `sort`, `skip` and `take`
- Set operations such as `union`, `union all`, `except` and `intersect`
- Built‑in helpers including `fetch`, `load`, `save` and placeholder LLM generation

## Unsupported Features

- The Ruby backend does not yet implement every construct in Mochi. Missing
features include:

- Dataset joins with `join`, `left join` and `right join` are supported. `outer`
  joins remain unsupported.
- Package declarations using `package` are ignored.
- Agent and stream constructs (`agent`, `on`, `emit`) and logic programming
  features (`fact`, `rule`, `query`).
- Packages and foreign function interface declarations (`import`, `extern`).
- `model` and `stream` declarations are not compiled.
- Concurrency primitives such as `spawn` and channels.
- Reflection or macro facilities.
- Error handling with `try`/`catch`.
- Asynchronous functions (`async`/`await`).
- Generic type parameters.
- Set collections (`set<T>`) and related operations.
- Export statements on functions or types.
- Destructuring bindings in `let` and `var` statements.
- Generic methods inside `type` blocks.
- Functions with multiple return values.
- Variadic functions.
- Agent initialization with field values.


## Ruby to Mochi Converter

The `any2mochi` tool ships with a basic converter that can translate small Ruby
snippets into Mochi. It relies on the `solargraph` language server for syntax
validation before applying a light-weight transformation implemented in pure Go.

### Supported Features

- `puts` statements (with or without parentheses) are mapped to `print`
- `for` loops, `each` blocks and `n.times` loops become Mochi `for` loops
- `while` loops including membership checks using `map.key?(k)`
- Conditional blocks with `if`/`elsif`/`else`
- Simple `def` and `class` declarations
- Variable assignments and `return` statements

### Unsupported Features

- Modules and `require` statements
- Pattern matching and struct or union declarations
- Blocks other than `each`
- Exception handling and concurrency primitives
- Metaprogramming constructs
