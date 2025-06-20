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

## Unsupported features

Only a narrow slice of Mochi is recognised by this backend. Missing
capabilities include:

- Structs, unions and other complex types
- Map literals and indexing operations
- Match expressions
- Query expressions such as `from`/`sort by`/`select`
- Agents, streams and extern objects
- Logic programming with facts and rules
- HTTP helpers such as `fetch`, `load` and `save`
- Foreign function interface and package imports
- LLM helpers and other advanced runtime features

Programs using these constructs will fail to compile.

## Status

This backend is intentionally minimal and currently exists only as a demo. It is
useful for experimenting with Mochi's compiler architecture but is not intended
for production use.

### Unsupported Features

The COBOL backend implements only a tiny portion of Mochi. Features that remain
unsupported include:

- Map literals and indexing operations
- Match expressions
- Dataset query expressions like `from ... sort by ...`
- Agents, stream handling, and concurrency primitives
- Logic programming with facts and rules
- HTTP helpers such as `fetch`, `load` and `save`
- Foreign function interface and package imports
- LLM helpers and other advanced runtime features

Programs relying on these constructs will fail to compile.
