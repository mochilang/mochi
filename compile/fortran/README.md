# Fortran Backend

The Fortran backend generates simple Fortran 90 source code from a very small
subset of Mochi.  It was initially written to demonstrate code generation by
compiling the classic leetcode “two sum” example.  Only a handful of language
constructs are supported and the output is intentionally minimal.

## Files

- `compiler.go` – core code generator
- `compiler_test.go` – golden tests that build and execute the generated code
- `tools.go` – utility for locating or installing `gfortran`

## Compiler overview

`Compiler` maintains a buffer and indentation level and emits formatted
statements via `writeln`.  The top of the file notes the limited scope of this
backend:

```go
// Compiler emits very small Fortran 90 code for a limited subset of Mochi.
// It only supports constructs needed for the leetcode two-sum example.
```
【F:compile/fortran/compiler.go†L11-L12】

`sanitizeName` converts Mochi identifiers into valid Fortran names by replacing
invalid characters and prefixing names that do not start with a letter:
```go
func sanitizeName(name string) string {
    ...
    if s == "" || !((s[0] >= 'A' && s[0] <= 'Z') || (s[0] >= 'a' && s[0] <= 'z')) {
        s = "v_" + s
    }
    return s
}
```
【F:compile/fortran/compiler.go†L28-L41】

The `Compile` method writes a `program main` wrapper, declares variables for
`let` statements and emits function definitions when present:
```go
c.writeln("program main")
c.indent++
c.writeln("implicit none")
// crude variable declarations for lets in main
...
if len(funs) > 0 {
    c.writeln("contains")
    ...
}
c.writeln("end program main")
```
【F:compile/fortran/compiler.go†L64-L117】

Loop variable names discovered in the main body are declared automatically so
the generated program remains valid with `implicit none`.

Functions are written using `compileFun`, which declares parameters and local
variables then walks the body:
```go
c.writeln(fmt.Sprintf("function %s(nums, target) result(%s)", sanitizeName(fn.Name), resVar))
...
c.writeln("integer :: res(2)")
for _, st := range fn.Body {
    if err := c.compileStmt(st, resVar); err != nil { ... }
}
```
【F:compile/fortran/compiler.go†L120-L170】

`compileStmt` handles the small set of supported statements (`let`, `return`,
`if`, `for` and expression statements).  Loop bodies become `do` blocks and a
simple `print` helper converts `print()` calls into `print *` statements:
```go
case s.For != nil:
    ...
    c.writeln(fmt.Sprintf("do %s = %s, %s - 1", name, start, end))
    ...
    c.writeln("end do")
case s.Expr != nil:
    if call, ok := printCall(s.Expr.Expr); ok {
        ...
        c.writeln("print *, " + strings.Join(args, ", "))
    } else {
        expr, err := c.compileExpr(s.Expr.Expr)
        ...
        c.writeln(expr)
    }
```
【F:compile/fortran/compiler.go†L173-L233】

Binary expressions only recognise a few operators. Unsupported ones result in an
error:
```go
switch op.Op {
case "+", "-", "*", "/", "==" :
    expr = fmt.Sprintf("(%s %s %s)", expr, op.Op, right)
default:
    return "", fmt.Errorf("unsupported op %s", op.Op)
}
```
【F:compile/fortran/compiler.go†L317-L323】

`compileCallExpr` maps the built‑in `len()` function to Fortran’s `size()` and
otherwise sanitises function names:
```go
case "len":
    if len(args) != 1 {
        return "", fmt.Errorf("len expects 1 arg")
    }
    return fmt.Sprintf("size(%s)", args[0]), nil
```
【F:compile/fortran/compiler.go†L401-L411】

## gfortran helper

`tools.go` provides `EnsureFortran` which looks for `gfortran` and, when
possible, attempts to install it using `apt-get` on Linux or Homebrew on macOS:
```go
switch runtime.GOOS {
case "linux":
    if _, err := exec.LookPath("apt-get"); err == nil {
        cmd := exec.Command("apt-get", "update")
        ...
        cmd = exec.Command("apt-get", "install", "-y", "gfortran")
        ...
    }
case "darwin":
    if _, err := exec.LookPath("brew"); err == nil {
        cmd := exec.Command("brew", "install", "gcc")
        ...
    }
}
```
【F:compile/fortran/tools.go†L16-L41】
If `gfortran` is still not found, an error is returned.

## Building

The Fortran backend can be used via `mochi build` by selecting the `fortran`
target (or by giving the output file a `.f90` extension).  This will emit a
Fortran 90 source file that can be compiled with `gfortran`:
```bash
mochi build --target fortran source.mochi -o program.f90
gfortran program.f90 -o program
./program
```
As a quick test you can build and run the LeetCode *two sum* example included
in the repository:
```bash
mochi build --target fortran examples/leetcode/1/two-sum.mochi -o two-sum.f90
gfortran two-sum.f90 -o two-sum
./two-sum
# 0
# 1
```
You can also compile and run the second LeetCode example, *add two numbers*:
```bash
mochi build --target fortran examples/leetcode/2/add-two-numbers.mochi -o add-two-numbers.f90
gfortran -ffree-line-length-none add-two-numbers.f90 -o add-two-numbers
./add-two-numbers
```
The tests show the full workflow, including a check for `gfortran` availability
and execution of the resulting binary:
```go
gfortran, err := ftncode.EnsureFortran()
if err != nil {
    t.Skipf("gfortran not installed: %v", err)
}
...
if out, err := exec.Command(gfortran, ffile, "-o", exe).CombinedOutput(); err != nil {
    t.Fatalf("gfortran error: %v\n%s", err, out)
}
```
【F:compile/fortran/compiler_test.go†L19-L44】

## Tests

The package includes golden tests under `tests/compiler/fortran`.  They are
marked with the `slow` build tag because they invoke `gfortran`:
```bash
go test ./compile/fortran -tags slow
```

While limited, this backend demonstrates how Mochi’s AST can be translated into
another language and may serve as a starting point for more complete Fortran
support.
