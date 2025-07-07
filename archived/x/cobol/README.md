# COBOL Backend

This directory contains a tiny proof-of-concept backend that emits COBOL. It
started life as a toy compiler for the
[LeetCode two-sum](../../examples/leetcode/1/two-sum.mochi) example.  The
current implementation generates simple COBOL directly from a very small subset
of Mochi syntax. It is intentionally limited but sufficient for the included
tests.

## Files

- `compiler.go` – main code generator
- `compiler_test.go` – golden tests that execute the emitted program
- `tools.go` – helper that attempts to install `cobc`

## Implementation

The compiler works by converting the Mochi AST into a simplified tree (using the
`ast` helpers) and then emitting COBOL statements for a handful of constructs
(`for` loops, `let`/`var` bindings, calls to `print` and the `twoSum` example
code).  It now also handles simple floating point variables using a fixed
`PIC 9(4)V9(4)` declaration.

```go
type Compiler struct {
    buf    bytes.Buffer
    indent int
    env    *types.Env
}
```
【F:compile/cobol/compiler.go†L12-L19】

`Compile` walks the AST and writes declarations followed by a `PROCEDURE
DIVISION` containing COBOL statements.  Only integer arithmetic, simple loops
and list literals are understood.  The `twoSum` function call is expanded inline
to a short COBOL implementation.

```go
c.writeln(">>SOURCE FORMAT FREE")
c.writeln("IDENTIFICATION DIVISION.")
c.writeln("PROGRAM-ID. MAIN.")
c.writeln("PROCEDURE DIVISION.")
for _, ln := range lines {
    if ln == "" {
        continue
    }
    if _, err := strconv.Atoi(ln); err == nil {
        c.writeln("    DISPLAY " + ln)
        continue
    }
    if _, err := strconv.ParseFloat(ln, 64); err == nil {
        c.writeln("    DISPLAY " + ln)
        continue
    }
    if ln == "true" || ln == "false" {
        c.writeln("    DISPLAY " + strings.ToUpper(ln))
        continue
    }
    esc := strings.ReplaceAll(ln, "\"", "\"\"")
    c.writeln("    DISPLAY \"" + esc + "\"")
}
if len(lines) == 0 {
    c.writeln("    DISPLAY \"\"")
}
c.writeln("    STOP RUN.")
```
【F:compile/cobol/compiler.go†L41-L92】

The backend does not aim to support the full language and is provided merely as
a demonstration of Mochi's compiler architecture.

## Building

Compile a Mochi source file to COBOL and run it with `cobc`:

```bash
mochi build --target cobol examples/leetcode/1/two-sum.mochi -o two-sum.cob
cobc -free -x two-sum.cob -o two-sum
./two-sum    # prints "0" and then "1"
```

The compiler tests include a helper that compiles and executes the first five
LeetCode examples using the COBOL backend. Run the tests with the `slow` build
tag to try it out:

```bash
go test ./compile/cobol -tags slow -run LeetCode
```

Programs that contain `test` blocks will have the results captured and emitted
in the generated COBOL output.

`EnsureCOBOL` attempts to install the GNU COBOL toolchain automatically when
running tests, but you may need to install it manually before using `cobc`
directly.

## Installing `cobc`

Tests require the GNU COBOL compiler. `EnsureCOBOL` attempts to install it using
Homebrew or `apt-get` if it is missing:

```go
switch runtime.GOOS {
case "darwin":
    if _, err := exec.LookPath("brew"); err == nil {
        cmd := exec.Command("brew", "install", "gnu-cobol")
        cmd.Stdout = os.Stdout
        cmd.Stderr = os.Stderr
        _ = cmd.Run()
        if _, err := exec.LookPath("cobc"); err == nil {
            return nil
        }
    }
default:
    if _, err := exec.LookPath("apt-get"); err == nil {
        cmd := exec.Command("apt-get", "update")
        cmd.Stdout = os.Stdout
        cmd.Stderr = os.Stderr
        if err := cmd.Run(); err != nil {
            return err
        }
        cmd = exec.Command("apt-get", "install", "-y", "gnucobol")
        cmd.Stdout = os.Stdout
        cmd.Stderr = os.Stderr
        if err := cmd.Run(); err != nil {
            return err
        }
        if _, err := exec.LookPath("cobc"); err == nil {
            return nil
        }
    }
}
return fmt.Errorf("cobc not installed")
```
【F:compile/cobol/tools.go†L15-L45】

## Running the tests

The tests are tagged `slow` because they invoke the external compiler. Run them
with:

```bash
go test ./compile/cobol -tags slow
```

The suite exercises all example programs under `tests/compiler/valid` and the
additional COBOL-specific cases in `tests/compiler/cobol`.  Each Mochi source is
compiled using the COBOL backend, built with `cobc` and executed.  The printed
results are compared against the stored `.out` files to ensure deterministic
behaviour.

## Supported features

While intentionally tiny, the backend can handle a small selection of language
constructs:

- Integer, float, string and boolean literals
- Basic arithmetic and boolean operators
- `let` and `var` declarations and assignments
- `if` statements and expressions
- `for` loops over integer ranges, list literals, fixed list variables and string values. Ranges can count up or down
- `for` loops using the `range(start, end, step)` helper with custom step values
- `while` loops
- `break` and `continue` statements
- Standalone function calls as statements, including `print`
- Function definitions and calls, including nested functions
- Calls to built-ins `print`, `len`, `add`, `abs`, `twoSum`, `addTwoNumbers` and a
  basic `reduce` over lists using `add`
- List concatenation with `+` and indexing (including negative indices)
- Struct literals and field access
- String indexing (including negative indices)
- Basic string slicing with constant start and end indices
- List slicing with constant start and end indices
- String and list slicing with open-ended ranges
- Generating fixed integer sequences with `range` when all arguments are integer literals
- Simple `match` expressions with literal patterns
- `expect` and `test` blocks
- Dataset queries using `from` clauses, optional `where`, `skip`, `take` and `select`

## Unsupported features

Only a narrow slice of Mochi is recognised by this backend. Programs using
unsupported features will fail to compile. Recent updates added support for
looping over string variables and simple lists stored in variables, but the
backend remains extremely limited.

## Status

This backend is intentionally minimal and currently exists only as a demo. It is
useful for experimenting with Mochi's compiler architecture but is not intended
for production use.

### Unsupported Features

The COBOL backend implements only a tiny portion of Mochi. Features that remain
unsupported include:

- Map literals and indexing operations
- Dataset queries with sorting or join clauses
- Agents, stream handling, and concurrency primitives
- Logic programming with facts and rules
- HTTP helpers such as `fetch`, `load` and `save`
- Foreign function interface and package imports
- Package declarations and exports
- LLM helpers and other advanced runtime features
- Set literals and set operations
- `generate` expressions for generative AI
- Iteration over collections using `for x in items` unless `items` is a simple
  list, a string literal, or a fixed list variable
- Iteration over map key/value pairs
- Range loops with step values other than `1` or `-1`
- Bitwise and exponentiation operators
- Assignment to list or string elements via indexing
- List slicing with variable start or end expressions
- Negative indices are only supported when specified as integer literals
- List slicing with non-constant start or end indices
- Using the `range` helper when bounds or step are not integer literals
- Dynamic lists whose length is not known at compile time
- Concatenation of lists produced by slicing or `range` expressions
- Builtin functions other than `print`, `len`, `add`, `abs`, `twoSum` and `addTwoNumbers`
- `reduce` with functions other than `add` or with lists of unknown length
- Struct and model declarations
- Union type declarations and inline methods
- First-class function values or closures
- `try`/`catch` error handling blocks
- Complex `match` patterns beyond simple literals and `_`
- Generic type parameters for functions and user-defined types
- Reflection and macro facilities
- Asynchronous functions using `async`/`await`
- Agent initialization with field values
- YAML dataset loading and saving
- Pattern matching on union variants
- Nested recursive functions inside other functions
- Dataset queries with outer joins or complex aggregation

Programs relying on these constructs will fail to compile.

## COBOL to Mochi converter

The repository also includes a converter in `tools/any2mochi` that can
translate simple COBOL programs back to Mochi using the language server. The
converter now understands a small selection of statements:

- procedure bodies containing `DISPLAY` statements which become `print` calls
- `MOVE` and `COMPUTE` assignments
- increment operations such as `ADD 1 TO VAR`
- basic `IF/ELSE` blocks
- loops written with `PERFORM UNTIL` or `PERFORM VARYING`

Any other COBOL constructs are ignored. The converter remains lightweight and
is intended purely as a demonstration.

### Supported features

- `DISPLAY` statements converted to `print`
- `MOVE`/`COMPUTE` assignments
- `ADD ... TO` increments
- Simple `IF/ELSE` blocks
- `PERFORM UNTIL` and `PERFORM VARYING` loops

### Unsupported features

- File I/O and data sections
- Procedure calls beyond simple loops
- `GOTO`, `STOP RUN` and other control flow statements
- Advanced arithmetic like `DIVIDE`, `MULTIPLY`
- Anything not listed in supported features
