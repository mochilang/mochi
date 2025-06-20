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
case "avg":
    expr = fmt.Sprintf("((%[1]s).length > 0 ? (%[1]s).sum(0.0) / (%[1]s).length : 0)", args[0])
case "input":
    expr = "STDIN.gets.to_s.strip"
}
```
【F:compile/rb/compiler.go†L503-L526】

## Query expressions

`compileQueryExpr` handles simple dataset queries including filtering, sorting and pagination. More advanced clauses and cross joins are not yet implemented:

```go
if len(q.Joins) > 0 || q.Group != nil {
    return "", fmt.Errorf("advanced query clauses not supported")
}
...
expr = fmt.Sprintf("(%s).map { |%s| %s }", expr, iter, sel)
```
【F:compile/rb/compiler.go†L336-L382】

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

`tools.go` provides `EnsureRuby` which attempts to install Ruby on Linux or macOS if it is missing:

```go
// EnsureRuby verifies that the ruby binary is installed. If missing, it
// attempts a best-effort installation using apt-get on Linux or Homebrew on
// macOS.
```
【F:compile/rb/tools.go†L10-L46】

## Tests

The golden tests exercise the backend by compiling example programs in `tests/compiler/rb` and running them with Ruby:

```bash
go test ./compile/rb -tags slow
```

The first test is skipped when Ruby is not available and the suite falls back to comparing generated code.

## Unsupported Features

The Ruby backend does not yet implement every construct in Mochi. Missing
features include:

- Complex dataset queries using grouping, joins or pagination when compiling
  with cross joins.
- Block-bodied anonymous functions like `fun(x: int) { ... }`.
- Agent and stream constructs (`agent`, `on`, `emit`) and logic programming
  features (`fact`, `rule`, `query`).
- External helpers for data fetching or generation such as `_fetch`, `_genText`,
  `load`, `save` and `generate`.

