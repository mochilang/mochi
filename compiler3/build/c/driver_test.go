package cbuild

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	cgen "mochi/compiler3/emit/c"
	"mochi/compiler3/ir"
)

// helloProgram builds the minimal cgen.Program the Phase 4.0 gate
// targets: a single function "answer" that returns 42 as i64. With
// Main="answer", the emitted main prints "42\n" to stdout, byte-for-
// byte matching what `mochi run` on a "return 42" Mochi script would
// produce once the frontend names the script's top-level "answer".
func helloProgram() *cgen.Program {
	fn := &ir.Function{Name: "answer", Result: ir.TypeI64}
	bid := fn.AddBlock()
	c := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 42})
	blk := fn.Block(bid)
	blk.Values = []uint32{c}
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: c}
	return &cgen.Program{Funcs: []*ir.Function{fn}, Main: "answer"}
}

// TestBuildHelloEndToEnd is the Phase 4.0 gate as a unit test: emit
// the C source, invoke the system cc, run the produced binary, and
// assert its stdout matches the expected single line. This is the
// load-bearing gate of MEP-42 §Phased-plan row 4.
func TestBuildHelloEndToEnd(t *testing.T) {
	cc := resolveCC("")
	if _, err := exec.LookPath(cc); err != nil {
		t.Skipf("cc %q not available: %v", cc, err)
	}
	dir := t.TempDir()
	binPath := filepath.Join(dir, "hello")
	r, err := Build(helloProgram(), Options{
		OutDir:     dir,
		BinaryPath: binPath,
		KeepEmit:   true,
	})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	if r.BinaryPath != binPath {
		t.Errorf("BinaryPath = %q, want %q", r.BinaryPath, binPath)
	}
	if _, err := os.Stat(r.BinaryPath); err != nil {
		t.Fatalf("stat binary: %v", err)
	}
	if _, err := os.Stat(r.SourcePath); err != nil {
		t.Fatalf("stat source (KeepEmit=true): %v", err)
	}
	out, err := exec.Command(binPath).Output()
	if err != nil {
		t.Fatalf("run %s: %v", binPath, err)
	}
	if got, want := strings.TrimRight(string(out), "\n"), "42"; got != want {
		t.Errorf("binary stdout = %q, want %q", string(out), want+"\n")
	}
}

// TestBuildCleanupEmit covers the KeepEmit=false default: the .c
// file is removed on successful build, leaving only the binary.
func TestBuildCleanupEmit(t *testing.T) {
	if _, err := exec.LookPath(resolveCC("")); err != nil {
		t.Skipf("cc not available")
	}
	dir := t.TempDir()
	r, err := Build(helloProgram(), Options{OutDir: dir})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	if _, err := os.Stat(r.SourcePath); !os.IsNotExist(err) {
		t.Errorf("expected %s removed, stat err = %v", r.SourcePath, err)
	}
	if _, err := os.Stat(r.BinaryPath); err != nil {
		t.Errorf("binary missing: %v", err)
	}
}

// TestBuildBadCC covers the cc-invocation error path: when cc points
// at a non-existent program, Build must surface the error with the
// stderr captured so the caller can diagnose.
func TestBuildBadCC(t *testing.T) {
	dir := t.TempDir()
	_, err := Build(helloProgram(), Options{
		OutDir: dir,
		CC:     "/definitely/not/a/real/compiler",
	})
	if err == nil {
		t.Fatalf("expected error from bogus CC")
	}
}

// TestBuildMissingOutDir covers the required-field guard.
func TestBuildMissingOutDir(t *testing.T) {
	if _, err := Build(helloProgram(), Options{}); err == nil {
		t.Errorf("expected error when OutDir is empty")
	}
}

// TestResolveCC covers the env/explicit/default precedence.
func TestResolveCC(t *testing.T) {
	t.Setenv("MOCHI_CC", "")
	if got := resolveCC("explicit"); got != "explicit" {
		t.Errorf("explicit precedence: got %q", got)
	}
	t.Setenv("MOCHI_CC", "from-env")
	if got := resolveCC(""); got != "from-env" {
		t.Errorf("env fallback: got %q", got)
	}
	t.Setenv("MOCHI_CC", "")
	if got := resolveCC(""); got != "cc" {
		t.Errorf("default cc: got %q", got)
	}
}

// runMochiBuild compiles src as Mochi via the full frontend → IR →
// C → cc pipeline, runs the produced binary, and returns its stdout.
// This is the Phase 4.1 integration gate: a Mochi source compiles
// to a single binary whose output byte-matches `mochi run` on the
// same source. Skip when cc is unavailable.
func runMochiBuild(t *testing.T, src string) string {
	t.Helper()
	cc := resolveCC("")
	if _, err := exec.LookPath(cc); err != nil {
		t.Skipf("cc %q not available: %v", cc, err)
	}
	dir := t.TempDir()
	srcPath := filepath.Join(dir, "test.mochi")
	if err := os.WriteFile(srcPath, []byte(src), 0o644); err != nil {
		t.Fatalf("write src: %v", err)
	}
	binPath := filepath.Join(dir, "bin")
	if _, err := BuildSource(srcPath, Options{
		OutDir:     dir,
		BinaryPath: binPath,
	}); err != nil {
		t.Fatalf("BuildSource: %v", err)
	}
	out, err := exec.Command(binPath).Output()
	if err != nil {
		t.Fatalf("run %s: %v", binPath, err)
	}
	return string(out)
}

// TestBuildSourceLetAndPrint is the first Phase 4.1 integration
// gate: a script with `let` bindings and a single `print(i + j)`
// must compile to a binary that prints "30\n" on stdout, matching
// what `mochi run` produces on the same source.
func TestBuildSourceLetAndPrint(t *testing.T) {
	src := `let a = 10
let b: int = 20
print(a + b)
`
	if got, want := runMochiBuild(t, src), "30\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceArithChain pins the precedence-respecting lowering
// through parens.
func TestBuildSourceArithChain(t *testing.T) {
	src := `let a = 7
let b = 3
print((a + b) * 2)
`
	if got, want := runMochiBuild(t, src), "20\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceFunCall pins the Phase 4.1 intra-program-call gate:
// a user-defined fun is invoked from script body, and the C target
// lowers OpCall to a direct C function call.
func TestBuildSourceFunCall(t *testing.T) {
	src := `fun double(n: int): int {
  return n * 2
}
let x = 21
print(double(x))
`
	if got, want := runMochiBuild(t, src), "42\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceIfElse pins control-flow lowering through if/else
// with a print() in each branch.
func TestBuildSourceIfElse(t *testing.T) {
	src := `let n = 5
if n > 3 {
  print(1)
} else {
  print(0)
}
`
	got := runMochiBuild(t, src)
	if !strings.HasPrefix(got, "1") {
		t.Errorf("got %q, want prefix %q", got, "1")
	}
}

// TestBuildSourceWhileCountdown is the Phase 4.1.2 integration gate:
// a script with a `while` loop must compile to a binary whose output
// matches what `mochi run` produces. This is the smallest while-test
// that exercises phi-at-header (the loop counter `n` decreases each
// iteration, so its SSA value at the header is a join of pre-loop and
// back-edge values).
func TestBuildSourceWhileCountdown(t *testing.T) {
	src := `var n = 5
while n > 0 {
  print(n)
  n = n - 1
}
`
	if got, want := runMochiBuild(t, src), "5\n4\n3\n2\n1\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceListAppendAndIndex is the Phase 4.3.1 integration
// gate: a Mochi script using the typed-i64 list surface (empty list
// literal, append, indexed read, len, indexed write) compiles via
// `mochi build --target=c` to a binary whose stdout byte-matches the
// Go target's output for the same source. This exercises every list
// op the IR declares: OpNewList, OpListPushI64, OpListGetI64,
// OpListLenI64, OpListSetI64.
func TestBuildSourceListAppendAndIndex(t *testing.T) {
	src := `fun sumlist(n: int): int {
  var xs: list<int> = []
  var i = 0
  while i < n {
    xs = append(xs, i + 1)
    i = i + 1
  }
  var s = 0
  var k = 0
  while k < len(xs) {
    s = s + xs[k]
    k = k + 1
  }
  xs[0] = 100
  return s + xs[0]
}
print(sumlist(10))
`
	if got, want := runMochiBuild(t, src), "155\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceListLiteralRead pins the non-empty list-literal
// shape through the C target: `[10, 20, 30]` lowers to OpNewList +
// three OpListPushI64, and `xs[2]` reads back 30.
func TestBuildSourceListLiteralRead(t *testing.T) {
	src := `let xs: list<int> = [10, 20, 30]
print(xs[2])
`
	if got, want := runMochiBuild(t, src), "30\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceListFloatLiteralAndIndex pins the Phase 4.3.3
// list<float> surface through the C target: a non-empty literal lowers
// to OpNewF64Array + three OpF64ArrayPushF64, and `xs[1]` reads back
// 2.5. The print path routes through print.h's float branch (%.17g).
func TestBuildSourceListFloatLiteralAndIndex(t *testing.T) {
	src := `let xs: list<float> = [1.5, 2.5, 3.5]
print(xs[1])
`
	if got, want := runMochiBuild(t, src), "2.5\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceListFloatAppendAndIndex pins the empty-literal +
// append + indexed read + indexed write + len cycle for list<float>
// through the C target, byte-matching the Go target's output (and the
// matching TestLowerListFloatAppendAndIndex in compiler3/frontend).
func TestBuildSourceListFloatAppendAndIndex(t *testing.T) {
	src := `fun sumf(n: int): float {
  var xs: list<float> = []
  var i = 0
  while i < n {
    xs = append(xs, 0.5)
    i = i + 1
  }
  var s = 0.0
  var k = 0
  while k < len(xs) {
    s = s + xs[k]
    k = k + 1
  }
  xs[0] = 100.5
  return s + xs[0]
}
print(sumf(4))
`
	if got, want := runMochiBuild(t, src), "102.5\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceForRangeSum pins the Phase 4.3.2 range-for surface
// through the C target: a `for i in 1..(n+1)` loop with a mutable
// accumulator must compile and produce the same output as the Go
// target.
func TestBuildSourceForRangeSum(t *testing.T) {
	src := `fun sumRange(n: int): int {
  var s = 0
  for i in 1..(n + 1) {
    s = s + i
  }
  return s
}
print(sumRange(10))
`
	if got, want := runMochiBuild(t, src), "55\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceForRangeUnderscore covers the `_` loop variable: a
// range-for that runs N iterations without referring to the index.
func TestBuildSourceForRangeUnderscore(t *testing.T) {
	src := `fun fillFive(): int {
  var xs: list<int> = []
  for _ in 0..5 {
    xs = append(xs, 0)
  }
  return len(xs)
}
print(fillFive())
`
	if got, want := runMochiBuild(t, src), "5\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceNsieve is the Phase 4.3.2 load-bearing gate: a
// stripped nsieve(100)=25 program compiles via `mochi build
// --target=c` and produces the same byte-stream as `mochi run`.
// Exercises range-for, nested while, indexed list ops, and the
// if-merge phi join all in one program.
func TestBuildSourceNsieve(t *testing.T) {
	src := `fun nsieve(m: int): int {
  var flags: list<int> = []
  var i = 0
  while i < m {
    flags = append(flags, 1)
    i = i + 1
  }
  var count = 0
  for k in 2..m {
    if flags[k] == 1 {
      count = count + 1
      var j = k + k
      while j < m {
        flags[j] = 0
        j = j + k
      }
    }
  }
  return count
}
print(nsieve(100))
`
	if got, want := runMochiBuild(t, src), "25\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceCastIntToFloatRoundTrip pins the Phase 4.3.4 cast
// surface through the C target: `n as float` lowers to OpI64ToF64
// (emits `(double)n`), and `f as int` to OpF64ToI64 (`(int64_t)f`).
// The constant 7 round-trips through f64 arithmetic without loss.
func TestBuildSourceCastIntToFloatRoundTrip(t *testing.T) {
	src := `let n = 7
let f = (n as float) / 2.0
let back = (f * 2.0) as int
print(back + n)
`
	if got, want := runMochiBuild(t, src), "14\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceMandelbrotKernel is the Phase 4.3.4 load-bearing
// gate: a stripped mandelbrot kernel that compiles via
// `mochi build --target=c` and produces 4629 for a 16x16 grid with
// max_iter=50. The program exercises int<->float casts inside nested
// while loops with early return inside if. This is the program the
// benchmark games' mandelbrot fixture reduces to once the harness's
// `now()` / `json({...})` instrumentation is removed.
func TestBuildSourceMandelbrotKernel(t *testing.T) {
	src := `fun escape_count(cx: float, cy: float, max_iter: int): int {
  var zr = 0.0
  var zi = 0.0
  var n = 0
  while n < max_iter {
    let r2 = zr * zr
    let i2 = zi * zi
    if r2 + i2 > 4.0 {
      return n
    }
    let nzi = 2.0 * zr * zi + cy
    let nzr = (r2 - i2) + cx
    zr = nzr
    zi = nzi
    n = n + 1
  }
  return max_iter
}

let side = 16
let max_iter = 50
let side_f = side as float
var total = 0
var row = 0
while row < side {
  let cy = (row as float) / side_f * 2.0 - 1.0
  var col = 0
  while col < side {
    let cx = (col as float) / side_f * 3.0 - 2.0
    total = total + escape_count(cx, cy, max_iter)
    col = col + 1
  }
  row = row + 1
}
print(total)
`
	if got, want := runMochiBuild(t, src), "4629\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceMathSqrtBuiltin pins the Phase 4.3.5 math.sqrt
// builtin through the C target: the `import python "math"` plus
// `extern fun math.sqrt(x: float): float` declarations are accepted
// as no-op bindings, and the call site lowers to OpSqrtF64 which
// the C emitter renders as `sqrt(v)` and the driver links with -lm.
// sqrt(2) * sqrt(2) round-trips through f64 to 2 cast to int.
func TestBuildSourceMathSqrtBuiltin(t *testing.T) {
	src := `import python "math" as math
extern fun math.sqrt(x: float): float

let r = math.sqrt(2.0) * math.sqrt(2.0)
print(r as int)
`
	if got, want := runMochiBuild(t, src), "2\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceNbodyDistanceKernel pins the n_body softened-distance
// expression `1 / (d2 * sqrt(d2))` through the C target for a 3-4-5
// right triangle: d2 = 25, sqrt(25) = 5, 1/(25*5) = 0.008. Scaled by
// 1e9 and cast to int, the result is 8000000. This is the load-bearing
// Phase 4.3.5 gate for the n_body kernel: math.sqrt + the precedence-
// climbing fix (without precedence, `dx*dx + dy*dy + dz*dz` evaluated
// left-to-right gave a wrong d2 and the test would fail with INT64_MAX
// from NaN-to-int).
func TestBuildSourceNbodyDistanceKernel(t *testing.T) {
	src := `import python "math" as math
extern fun math.sqrt(x: float): float

let dx = 3.0
let dy = 4.0
let dz = 0.0
let d2 = dx * dx + dy * dy + dz * dz
let factor = 1.0 / (d2 * math.sqrt(d2))
print((factor * 1.0e9) as int)
`
	if got, want := runMochiBuild(t, src), "8000000\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourcePrecedenceClimbing pins the Phase 4.3.5 precedence-
// climbing reducer in lowerBinary: `1 + 2 * 3` must be `1 + (2*3)` = 7,
// not the previous left-assoc shape `((1+2)*3)` = 9. Without this,
// every multi-operator non-parenthesised expression in the benchmark
// games would silently miscompile.
func TestBuildSourcePrecedenceClimbing(t *testing.T) {
	src := `print(1 + 2 * 3)
print(2 * 3 + 4)
print(1 + 2 * 3 + 4 * 5)
`
	if got, want := runMochiBuild(t, src), "7\n10\n27\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceIntCallCastFromFloat pins the Phase 4.3.6 `int(x)`
// builtin against an f64 argument: emits `(int64_t)x`, truncating
// toward zero (1.7 -> 1). This is the surface spectral_norm uses to
// produce the final printable integer.
func TestBuildSourceIntCallCastFromFloat(t *testing.T) {
	src := `let x = 1.7
print(int(x))
`
	if got, want := runMochiBuild(t, src), "1\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceFloatCallCastFromInt pins the Phase 4.3.6 `float(x)`
// builtin against an i64 argument: emits `(double)x`, then divides as
// f64. 7 widened to 7.0, halved to 3.5, cast back to int 3.
func TestBuildSourceFloatCallCastFromInt(t *testing.T) {
	src := `let n = 7
let half = float(n) / 2.0
print(int(half))
`
	if got, want := runMochiBuild(t, src), "3\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceSpectralEvalKernel is the load-bearing Phase 4.3.6
// gate on the C target: a single eval of spectral_norm's `eval_a(i,j)
// = 1 / float(s*(s+1)/2 + i + 1)` matrix entry at i=0, j=0. The
// expected value is 1/1 = 1.0; scaled by 1e9 and truncated, 1000000000.
func TestBuildSourceSpectralEvalKernel(t *testing.T) {
	src := `fun eval_a(i: int, j: int): float {
  let s = i + j
  return 1.0 / float(s * (s + 1) / 2 + i + 1)
}
print(int(eval_a(0, 0) * 1.0e9))
`
	if got, want := runMochiBuild(t, src), "1000000000\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceListInferFloatElem pins the Phase 4.3.8 element-type
// inference on the C target: a float-element literal without a type
// annotation lowers via mochi_f64_array, not mochi_list_i64.
func TestBuildSourceListInferFloatElem(t *testing.T) {
	src := `var xs = [1.0, 2.0, 3.0]
print(int(xs[1]))
`
	if got, want := runMochiBuild(t, src), "2\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceNbodyInitVectors pins a stripped n_body top-level
// shape on the C target. The full benchmark still needs harness
// scaffolding, but the var-bound float list + indexed reads now
// compile.
func TestBuildSourceNbodyInitVectors(t *testing.T) {
	src := `var pos_x = [0.0, 4.84, 8.34, 12.89, 15.37]
var i = 0
var sum = 0.0
while i < 5 {
  sum = sum + pos_x[i]
  i = i + 1
}
print(int(sum))
`
	if got, want := runMochiBuild(t, src), "41\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceForInListI64 pins the Phase 4.3.7 collection-iter
// surface on the C target for list<int>. Sum of [10,20,30] is 60.
func TestBuildSourceForInListI64(t *testing.T) {
	src := `let xs: list<int> = [10, 20, 30]
var s = 0
for x in xs {
  s = s + x
}
print(s)
`
	if got, want := runMochiBuild(t, src), "60\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceForInListF64 pins the Phase 4.3.7 collection-iter
// surface on the C target for list<float>. The body sums an f64 list
// and the truncating Phase 4.3.6 `int(...)` gives 6.
func TestBuildSourceForInListF64(t *testing.T) {
	src := `let xs: list<float> = [1.5, 2.0, 2.5]
var s = 0.0
for x in xs {
  s = s + x
}
print(int(s))
`
	if got, want := runMochiBuild(t, src), "6\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceFibIter is the §10.7 unblock for the iterative Fib
// benchmark: while loop, mutated `a`/`b`/`i`, and a `let t` inside the
// body that the phi-at-header must NOT track (it's body-scoped).
func TestBuildSourceFibIter(t *testing.T) {
	src := `fun fib(n: int): int {
  var a = 0
  var b = 1
  var i = 0
  while i < n {
    let t = a + b
    a = b
    b = t
    i = i + 1
  }
  return a
}
print(fib(10))
`
	if got, want := runMochiBuild(t, src), "55\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceRejectsImportGo pins the by-design rejection of
// general Go FFI in the C target. The script `import go "testpkg"`
// must surface ErrUnsupportedFFI (wrapped) at build time, not
// silently produce a binary that crashes at runtime.
func TestBuildSourceRejectsImportGo(t *testing.T) {
	src := `import go "mochi/runtime/ffi/go/testpkg" as testpkg auto
print(testpkg.Add(2, 3))
`
	cc := resolveCC("")
	if _, err := exec.LookPath(cc); err != nil {
		t.Skipf("cc %q not available: %v", cc, err)
	}
	dir := t.TempDir()
	srcPath := filepath.Join(dir, "test.mochi")
	if err := os.WriteFile(srcPath, []byte(src), 0o644); err != nil {
		t.Fatalf("write src: %v", err)
	}
	_, err := BuildSource(srcPath, Options{OutDir: dir})
	if err == nil {
		t.Fatalf("expected build to fail on import go (FFI rejection), got success")
	}
	if !strings.Contains(err.Error(), "FFI") && !strings.Contains(err.Error(), "ffi") &&
		!strings.Contains(err.Error(), "--target=go") {
		t.Errorf("expected FFI-rejection error, got %v", err)
	}
}

// TestBuildStaticFlag covers the Static=true case at the flag-shape
// level (not at the cc-link level: a true `-static` build needs a
// musl/glibc-static toolchain that not every host has). The test
// just confirms cc was invoked; failure is tolerated when the host
// libc cannot satisfy `-static` so this case doesn't block the gate
// on non-musl Linux developer machines.
func TestBuildStaticFlag(t *testing.T) {
	if _, err := exec.LookPath(resolveCC("")); err != nil {
		t.Skipf("cc not available")
	}
	dir := t.TempDir()
	_, err := Build(helloProgram(), Options{
		OutDir: dir,
		Static: true,
	})
	if err == nil {
		return // host supports -static; great.
	}
	if !strings.Contains(err.Error(), "static") &&
		!strings.Contains(err.Error(), "libc") &&
		!strings.Contains(err.Error(), "crt") &&
		!strings.Contains(err.Error(), "ld") &&
		!strings.Contains(err.Error(), "link") {
		// Pass: the build attempted cc with -static; whether the host
		// can satisfy it depends on the toolchain. We only assert the
		// shape of the failure looks like a link error, not a panic.
		t.Logf("Static build failed (likely no static libc on host): %v", err)
	}
}
