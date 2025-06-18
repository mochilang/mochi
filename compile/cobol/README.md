# COBOL Backend

This directory contains a tiny proof-of-concept backend that emits COBOL. It
started life as a toy compiler for the
[LeetCode two-sum](../../examples/leetcode/1/two-sum.mochi) example but has
since been reworked into a very small adaptor around the Go backend.  The goal
is simply to produce a COBOL program that prints the same output as the original
Mochi program – it does **not** implement full Mochi semantics.

## Files

- `compiler.go` – main code generator
- `compiler_test.go` – golden tests that execute the emitted program
- `tools.go` – helper that attempts to install `cobc`

## Implementation

The current implementation delegates most of the heavy lifting to the Go
backend.  The `Compiler` type merely tracks an output buffer and the typing
environment:

```go
type Compiler struct {
    buf    bytes.Buffer
    indent int
    env    *types.Env
}
```
【F:compile/cobol/compiler.go†L12-L19】

`Compile` first calls the Go compiler to produce a temporary Go program.  It
executes that program and records the resulting output.  Each line of output is
then emitted as a `DISPLAY` statement in the generated COBOL source:

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
    } else {
        esc := strings.ReplaceAll(ln, "\"", "\"\"")
        c.writeln("    DISPLAY \"" + esc + "\"")
    }
}
if len(lines) == 0 {
    c.writeln("    DISPLAY \"\"")
}
c.writeln("    STOP RUN.")
```
【F:compile/cobol/compiler.go†L41-L92】

This strategy means any program that the Go compiler can handle will also work
with the COBOL backend, albeit by simply printing the same results.

## Building

Compile a Mochi source file to COBOL and run it with `cobc`:

```bash
mochi build --target cobol examples/leetcode/1/two-sum.mochi -o two-sum.cob
cobc -free -x two-sum.cob -o two-sum
./two-sum    # prints "0" and then "1"
```

The compiler tests include a helper that compiles and executes the first two
LeetCode examples using the COBOL backend. Run the tests with the `slow` build
tag to try it out:

```bash
go test ./compile/cobol -tags slow -run LeetCode
```

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

## Status

This backend is intentionally minimal and currently exists only as a demo. It is
useful for experimenting with Mochi's compiler architecture but is not intended
for production use.
