# COBOL Backend

This directory contains a tiny proof-of-concept backend that emits COBOL. It was
written to compile the [LeetCode two-sum](../../examples/leetcode/1/two-sum.mochi)
example and does not attempt to support general Mochi code.

## Files

- `compiler.go` – main code generator
- `compiler_test.go` – golden tests that execute the emitted program
- `tools.go` – helper that attempts to install `cobc`

## Implementation

The `Compiler` type maintains an output buffer and some helper methods:

```go
// Compiler is a very small COBOL code generator able to compile
// the LeetCode two-sum example. It handles only a tiny subset of
// Mochi expressions.
type Compiler struct {
    buf    bytes.Buffer
    indent int
    env    *types.Env
}
```
【F:compile/cobol/compiler.go†L12-L19】

`Compile` walks the parsed program looking for a call to `twoSum` with a literal
list of integers and a target value. It then writes out a COBOL program that
implements a nested loop search:

```go
c.writeln("IDENTIFICATION DIVISION.")
c.writeln("PROGRAM-ID. MAIN.")
c.writeln("DATA DIVISION.")
c.writeln("WORKING-STORAGE SECTION.")
c.writeln(fmt.Sprintf("01 N        PIC 9(4) VALUE %d.", len(nums)))
```
【F:compile/cobol/compiler.go†L63-L67】

```go
c.writeln("    PERFORM VARYING I FROM 1 BY 1 UNTIL I > N")
c.writeln("        PERFORM VARYING J FROM I + 1 BY 1 UNTIL J > N")
c.writeln("            IF NUMS(I) + NUMS(J) = TARGET")
c.writeln("                DISPLAY I")
c.writeln("                DISPLAY J")
```
【F:compile/cobol/compiler.go†L73-L78】

Only literal lists and integers are handled. Helper functions like `listInts` and
`intLit` verify these patterns.

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

They compile `examples/leetcode/1/two-sum.mochi` to COBOL, build it using `cobc`
and verify the program prints `0` and `1`.

## Status

This backend is intentionally minimal and currently exists only as a demo. It is
useful for experimenting with Mochi's compiler architecture but is not intended
for production use.
