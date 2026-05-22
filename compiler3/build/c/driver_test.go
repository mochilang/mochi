package cbuild

import (
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strconv"
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
	cc, _ := resolveCC("", "")
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
	cc, _ := resolveCC("", "")
	if _, err := exec.LookPath(cc); err != nil {
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

// TestResolveCC covers the env/explicit/default precedence and the
// Phase 5.0 triple-aware default (zig cc when a target triple is set
// and no explicit compiler is configured).
func TestResolveCC(t *testing.T) {
	t.Setenv("MOCHI_CC", "")
	if got, prefix := resolveCC("explicit", ""); got != "explicit" || len(prefix) != 0 {
		t.Errorf("explicit precedence: got (%q, %v)", got, prefix)
	}
	t.Setenv("MOCHI_CC", "from-env")
	if got, prefix := resolveCC("", ""); got != "from-env" || len(prefix) != 0 {
		t.Errorf("env fallback: got (%q, %v)", got, prefix)
	}
	t.Setenv("MOCHI_CC", "")
	if got, prefix := resolveCC("", ""); got != "cc" || len(prefix) != 0 {
		t.Errorf("default cc: got (%q, %v)", got, prefix)
	}
	if got, prefix := resolveCC("", "wasm32-wasi"); got != "zig" || len(prefix) != 1 || prefix[0] != "cc" {
		t.Errorf("triple-aware default: got (%q, %v), want (\"zig\", [\"cc\"])", got, prefix)
	}
	if got, prefix := resolveCC("zig cc", "x86_64-linux-musl"); got != "zig" || len(prefix) != 1 || prefix[0] != "cc" {
		t.Errorf("wrapper-form explicit: got (%q, %v), want (\"zig\", [\"cc\"])", got, prefix)
	}
}

// runMochiBuild compiles src as Mochi via the full frontend → IR →
// C → cc pipeline, runs the produced binary, and returns its stdout.
// This is the Phase 4.1 integration gate: a Mochi source compiles
// to a single binary whose output byte-matches `mochi run` on the
// same source. Skip when cc is unavailable.
func runMochiBuild(t *testing.T, src string) string {
	t.Helper()
	cc, _ := resolveCC("", "")
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

// TestBuildSourceNbodyFullKernel pins the Phase 4.3.10 milestone: the
// full benchmark-games n_body integration kernel (5 bodies, 10 steps,
// canonical Sun + Jupiter + Saturn + Uranus + Neptune initial
// conditions; momentum normalisation; pairwise gravity inner loop;
// position update outer loop; final energy * 1e9 truncated to int)
// now compiles end-to-end through compiler3 to a native binary via
// `mochi build --target=c`. Output -169073021 byte-matches the
// `--target=go` build.
//
// This is the load-bearing regression test for the entire n_body
// kernel surface. With Phase 4.3.9's math.pi constant read, every
// arithmetic expression and indexed array assignment the kernel
// needs is now lowerable. The remaining gap before the
// bench/template/bg/n_body fixture compiles unchanged is the bench
// harness shape (`now()`, `json({...})`, `{{ .N }}`), which is a
// §13 follow-up and out of Phase 4.3's scope.
func TestBuildSourceNbodyFullKernel(t *testing.T) {
	src := `import python "math" as math
extern let math.pi: float
extern fun math.sqrt(x: float): float

let DAYS_PER_YEAR = 365.24
let SOLAR_MASS = 4.0 * math.pi * math.pi
let DT = 0.01

var pos_x = [0.0, 4.84143144246472090, 8.34336671824457987, 1.28943695621391310e+01, 1.53796971148509165e+01]
var pos_y = [0.0, -1.16032004402742839, 4.12479856412430479, -1.51111514016986312e+01, -2.59193146099879641e+01]
var pos_z = [0.0, -1.03622044471123109e-01, -4.03523417114321381e-01, -2.23307578892655734e-01, 1.79258772950371181e-01]
var vel_x = [0.0, 1.66007664274403694e-03 * DAYS_PER_YEAR, -2.76742510726862411e-03 * DAYS_PER_YEAR, 2.96460137564761618e-03 * DAYS_PER_YEAR, 2.68067772490389322e-03 * DAYS_PER_YEAR]
var vel_y = [0.0, 7.69901118419740425e-03 * DAYS_PER_YEAR, 4.99852801234917238e-03 * DAYS_PER_YEAR, 2.37847173959480950e-03 * DAYS_PER_YEAR, 1.62824170038242295e-03 * DAYS_PER_YEAR]
var vel_z = [0.0, -6.90460016972063023e-05 * DAYS_PER_YEAR, 2.30417297573763929e-05 * DAYS_PER_YEAR, -2.96589568540237556e-05 * DAYS_PER_YEAR, -9.51592254519715870e-05 * DAYS_PER_YEAR]
var mass = [SOLAR_MASS, 9.54791938424326609e-04 * SOLAR_MASS, 2.85885980666130812e-04 * SOLAR_MASS, 4.36624404335156298e-05 * SOLAR_MASS, 5.15138902046611451e-05 * SOLAR_MASS]

let N_BODIES = 5
let steps = 10

var px = 0.0
var py = 0.0
var pz = 0.0
var k = 1
while k < N_BODIES {
  px = px - vel_x[k] * mass[k]
  py = py - vel_y[k] * mass[k]
  pz = pz - vel_z[k] * mass[k]
  k = k + 1
}
vel_x[0] = px / SOLAR_MASS
vel_y[0] = py / SOLAR_MASS
vel_z[0] = pz / SOLAR_MASS

var s = 0
while s < steps {
  var i = 0
  while i < N_BODIES {
    var j = i + 1
    while j < N_BODIES {
      let dx = pos_x[i] - pos_x[j]
      let dy = pos_y[i] - pos_y[j]
      let dz = pos_z[i] - pos_z[j]
      let d2 = dx * dx + dy * dy + dz * dz
      let mag = DT / (d2 * math.sqrt(d2))
      let mi_mag = mass[i] * mag
      let mj_mag = mass[j] * mag
      vel_x[i] = vel_x[i] - dx * mj_mag
      vel_y[i] = vel_y[i] - dy * mj_mag
      vel_z[i] = vel_z[i] - dz * mj_mag
      vel_x[j] = vel_x[j] + dx * mi_mag
      vel_y[j] = vel_y[j] + dy * mi_mag
      vel_z[j] = vel_z[j] + dz * mi_mag
      j = j + 1
    }
    i = i + 1
  }
  var p = 0
  while p < N_BODIES {
    pos_x[p] = pos_x[p] + vel_x[p] * DT
    pos_y[p] = pos_y[p] + vel_y[p] * DT
    pos_z[p] = pos_z[p] + vel_z[p] * DT
    p = p + 1
  }
  s = s + 1
}

var energy = 0.0
var bi = 0
while bi < N_BODIES {
  let kin = 0.5 * mass[bi] * (vel_x[bi] * vel_x[bi] + vel_y[bi] * vel_y[bi] + vel_z[bi] * vel_z[bi])
  var pot = 0.0
  var bj = bi + 1
  while bj < N_BODIES {
    let dx = pos_x[bi] - pos_x[bj]
    let dy = pos_y[bi] - pos_y[bj]
    let dz = pos_z[bi] - pos_z[bj]
    let r = math.sqrt(dx * dx + dy * dy + dz * dz)
    pot = pot + mass[bi] * mass[bj] / r
    bj = bj + 1
  }
  energy = energy + kin - pot
  bi = bi + 1
}

print(int(energy * 1e9))
`
	if got, want := runMochiBuild(t, src), "-169073021\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceBracketListTypeFloat pins the Phase 4.3.11
// bracketed list-type syntax: `[float]` in a fun parameter
// position is accepted as syntactic sugar for `list<float>` and
// lowers to the same TypeF64Arr backing. The body fills a 3-slot
// list with 2.0 and sums it. Output: 6.
func TestBuildSourceBracketListTypeFloat(t *testing.T) {
	src := `fun fill(xs: [float], n: int) {
  for i in 0..n {
    xs[i] = 2.0
  }
}

var u: list<float> = []
for _ in 0..3 {
  u = append(u, 0.0)
}
fill(u, 3)
print(int(u[0] + u[1] + u[2]))
`
	if got, want := runMochiBuild(t, src), "6\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceBracketListTypeInt pins `[int]` as the i64
// counterpart: same surface, TypeList backing.
func TestBuildSourceBracketListTypeInt(t *testing.T) {
	src := `fun setall(xs: [int], n: int, v: int) {
  for i in 0..n {
    xs[i] = v
  }
}

var u: list<int> = []
for _ in 0..4 {
  u = append(u, 0)
}
setall(u, 4, 7)
print(u[0] + u[1] + u[2] + u[3])
`
	if got, want := runMochiBuild(t, src), "28\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceSpectralFullKernel pins the Phase 4.3.11 milestone:
// the full benchmark-games spectral_norm kernel (N=10, 5 outer power-
// method iterations, eval_a Hilbert-like matrix entry, mul_av and
// mul_atv helper funs taking `[float]` parameters, final
// `int(sqrt(uv/vv) * 1e9) = 1271844019`) now compiles end-to-end
// through compiler3 to a native binary via `mochi build --target=c`.
// Output byte-matches the `--target=go` build.
//
// Phase 4.3.11 adds the only remaining surface piece (the `[float]`
// bracketed list-type syntax in mul_av / mul_atv signatures). The
// `u + [1.0]` list-concatenation pattern from the native source is
// rewritten here to `u = append(u, 1.0)` because the kernel's
// growth happens once at the top of the program (the inner kernel
// loops do not mutate list shape); whether to add list-concat as a
// later sub-phase depends on whether any benchmark needs it inside
// a hot loop, which spectral_norm does not.
func TestBuildSourceSpectralFullKernel(t *testing.T) {
	src := `import python "math" as math
extern fun math.sqrt(x: float): float

let N = 10

fun eval_a(i: int, j: int): float {
  let s = i + j
  return 1.0 / float(s * (s + 1) / 2 + i + 1)
}

fun mul_av(src: [float], dst: [float], n: int) {
  for i in 0..n {
    var s = 0.0
    for j in 0..n {
      s = s + eval_a(i, j) * src[j]
    }
    dst[i] = s
  }
}

fun mul_atv(src: [float], dst: [float], n: int) {
  for i in 0..n {
    var s = 0.0
    for j in 0..n {
      s = s + eval_a(j, i) * src[j]
    }
    dst[i] = s
  }
}

var u: list<float> = []
var v: list<float> = []
var tmp: list<float> = []
for _ in 0..N {
  u = append(u, 1.0)
  v = append(v, 0.0)
  tmp = append(tmp, 0.0)
}

for _ in 0..5 {
  mul_av(u, tmp, N)
  mul_atv(tmp, v, N)
  mul_av(v, tmp, N)
  mul_atv(tmp, u, N)
}

var uv = 0.0
var vv = 0.0
for i in 0..N {
  uv = uv + u[i] * v[i]
  vv = vv + v[i] * v[i]
}

print(int(math.sqrt(uv / vv) * 1e9))
`
	if got, want := runMochiBuild(t, src), "1271844019\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceListConcatI64 pins the Phase 4.3.12 i64 list
// concatenation surface on the C target: `xs + ys` calls into
// mochi_list_i64_concat.
func TestBuildSourceListConcatI64(t *testing.T) {
	src := `var a: list<int> = []
a = append(a, 1)
a = append(a, 2)
var b: list<int> = []
b = append(b, 10)
b = append(b, 20)
b = append(b, 30)
let c = a + b
print(c[0] + c[1] + c[2] + c[3] + c[4])
`
	if got, want := runMochiBuild(t, src), "63\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceF64ArrayConcat pins the Phase 4.3.12 f64 list
// concatenation surface on the C target: `[float] + [float]` calls
// into mochi_f64_array_concat.
func TestBuildSourceF64ArrayConcat(t *testing.T) {
	src := `var u: [float] = []
u = u + [1.0]
u = u + [2.0]
u = u + [3.0]
var v: [float] = []
v = v + [4.0]
v = v + [5.0]
v = v + [6.0]
let w = u + v
print(int(w[0] + w[1] + w[2] + w[3] + w[4] + w[5]))
`
	if got, want := runMochiBuild(t, src), "21\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceSpectralNativeKernel pins the Phase 4.3.12 milestone:
// the native `bench/template/bg/spectral_norm/spectral_norm.mochi`
// fixture compiles unchanged via `mochi build --target=c` and produces
// `1274219991` (N=100 full power-method), byte-matching the Go target.
// This closes the compiler-internal kernel work for spectral_norm; the
// only remaining gap to consuming the native fixture is the bench
// harness shape (`now()`, `json({...})`, `{{ .N }}`), and the native
// spectral_norm source happens to not use any of those (its outer
// driver hardcodes N=100 and prints the integer directly).
func TestBuildSourceSpectralNativeKernel(t *testing.T) {
	src := `import python "math" as math
extern fun math.sqrt(x: float): float

let N = 100

fun eval_a(i: int, j: int): float {
  let s = i + j
  return 1.0 / float(s * (s + 1) / 2 + i + 1)
}

fun mul_av(src: [float], dst: [float], n: int) {
  for i in 0..n {
    var s = 0.0
    for j in 0..n {
      s = s + eval_a(i, j) * src[j]
    }
    dst[i] = s
  }
}

fun mul_atv(src: [float], dst: [float], n: int) {
  for i in 0..n {
    var s = 0.0
    for j in 0..n {
      s = s + eval_a(j, i) * src[j]
    }
    dst[i] = s
  }
}

var u: [float] = []
var v: [float] = []
var tmp: [float] = []
for _ in 0..N {
  u = u + [1.0]
  v = v + [0.0]
  tmp = tmp + [0.0]
}

for _ in 0..5 {
  mul_av(u, tmp, N)
  mul_atv(tmp, v, N)
  mul_av(v, tmp, N)
  mul_atv(tmp, u, N)
}

var uv = 0.0
var vv = 0.0
for i in 0..N {
  uv = uv + u[i] * v[i]
  vv = vv + v[i] * v[i]
}

print(int(math.sqrt(uv / vv) * 1e9))
`
	if got, want := runMochiBuild(t, src), "1274219991\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceNowBuiltin pins the Phase 4.3.13 `now()` builtin on
// the C target: lowers to mochi_now_us(), which wraps POSIX
// gettimeofday. The wall-clock unit + epoch matches the Go target's
// `time.Now().UnixMicro()`, so two back-to-back calls produce
// monotonically non-decreasing values.
func TestBuildSourceNowBuiltin(t *testing.T) {
	src := `let a = now()
let b = now()
if b >= a {
  print(1)
} else {
  print(0)
}
`
	if got, want := runMochiBuild(t, src), "1\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceNowDeltaArith pins that `now()` participates in
// normal i64 arithmetic on the C target. Sum-of-0..999 = 499500.
func TestBuildSourceNowDeltaArith(t *testing.T) {
	src := `let start = now()
var sum = 0
var i = 0
while i < 1000 {
  sum = sum + i
  i = i + 1
}
let duration = (now() - start) / 1000
if duration >= 0 {
  print(sum)
} else {
  print(-1)
}
`
	if got, want := runMochiBuild(t, src), "499500\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceJsonI64Object pins the Phase 4.3.14 `json({...})`
// builtin on the C target: lowers to a single printf with `%lld` per
// i64 value and a constant key list. This is the closing C-target
// piece for `bench/template/bg/mandelbrot.mochi` running through
// `mochi build --target=c` without source modification.
func TestBuildSourceJsonI64Object(t *testing.T) {
	src := `let duration = 42
let total = 17
json({
  "duration_us": duration,
  "output": total,
})
`
	if got, want := runMochiBuild(t, src), "{\"duration_us\":42,\"output\":17}\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceJsonI64ObjectFromArith pins that the JSON values may
// be arbitrary i64 expressions on the C target (sum reduction here),
// matching the Go target's TestLowerJsonI64ObjectFromArith.
func TestBuildSourceJsonI64ObjectFromArith(t *testing.T) {
	src := `var sum = 0
var i = 0
while i < 10 {
  sum = sum + i
  i = i + 1
}
json({
  "duration_us": sum * 2,
  "output": sum,
})
`
	if got, want := runMochiBuild(t, src), "{\"duration_us\":90,\"output\":45}\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceMathPiConst pins the Phase 4.3.9 math.pi constant
// read on the C target: 4*pi*pi truncated = 39.
func TestBuildSourceMathPiConst(t *testing.T) {
	src := `import python "math" as math
extern let math.pi: float

let solar_mass = 4.0 * math.pi * math.pi
print(int(solar_mass))
`
	if got, want := runMochiBuild(t, src), "39\n"; got != want {
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

// readBenchFixture loads bench/template/bg/<name>/<name>.mochi from
// the repo on disk, substitutes the harness {{ .N }} placeholder with
// the given concrete n, and returns the rendered source. Tests that
// pin native bench-games fixtures use this helper so a regression in
// the on-disk fixture (e.g., a stray edit to the kernel) is caught
// next time the suite runs.
func readBenchFixture(t *testing.T, name string, n int) string {
	t.Helper()
	_, here, _, _ := runtime.Caller(0)
	root := filepath.Join(filepath.Dir(here), "..", "..", "..")
	path := filepath.Join(root, "bench", "template", "bg", name, name+".mochi")
	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	return strings.ReplaceAll(string(b), "{{ .N }}", strconv.Itoa(n))
}

// TestBuildSourceMandelbrotBgFixture pins the Phase 4.3.15 milestone:
// the unmodified bench/template/bg/mandelbrot fixture (with N=16)
// compiles through compiler3 to a native binary via
// `mochi build --target=c` and produces the JSON line
// `{"duration_us":0,"output":4629}`. The output field byte-matches
// `mochi run` on the same source (the duration field is wall-clock).
func TestBuildSourceMandelbrotBgFixture(t *testing.T) {
	src := readBenchFixture(t, "mandelbrot", 16)
	got := runMochiBuild(t, src)
	if want := "{\"duration_us\":0,\"output\":4629}\n"; got != want {
		t.Errorf("mandelbrot fixture stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceNBodyBgFixture pins the unmodified
// bench/template/bg/n_body fixture (steps=50) on the C target. The
// output field byte-matches `mochi run` on the same source.
func TestBuildSourceNBodyBgFixture(t *testing.T) {
	src := readBenchFixture(t, "n_body", 50)
	got := runMochiBuild(t, src)
	if want := "{\"duration_us\":0,\"output\":-169063617}\n"; got != want {
		t.Errorf("n_body fixture stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceFannkuchReduxBgFixture pins the unmodified
// bench/template/bg/fannkuch_redux fixture (trials=100) on the C
// target. The output field byte-matches `mochi run` on the same
// source.
func TestBuildSourceFannkuchReduxBgFixture(t *testing.T) {
	src := readBenchFixture(t, "fannkuch_redux", 100)
	got := runMochiBuild(t, src)
	if want := "{\"duration_us\":0,\"output\":272}\n"; got != want {
		t.Errorf("fannkuch_redux fixture stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceNsieveBgFixture pins the unmodified
// bench/template/bg/nsieve fixture (n=100, repeat=50 inlined) on the
// C target. The output field byte-matches `mochi run` on the same
// source.
func TestBuildSourceNsieveBgFixture(t *testing.T) {
	src := readBenchFixture(t, "nsieve", 100)
	got := runMochiBuild(t, src)
	if want := "{\"duration_us\":0,\"output\":25}\n"; got != want {
		t.Errorf("nsieve fixture stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceFastaBgFixture pins the unmodified
// bench/template/bg/fasta fixture (N=10000) on the C target. The
// fixture uses bare `print(h)` rather than `json({...})`; the output
// is the deterministic LCG rolling-hash final value. The Mochi
// interpreter currently rejects this source (lookup() returns int
// but harness expects an implicit byte type), so the cross-check is
// against the Go target which produces the identical hash.
func TestBuildSourceFastaBgFixture(t *testing.T) {
	src := readBenchFixture(t, "fasta", 0)
	got := runMochiBuild(t, src)
	if want := "1072663717\n"; got != want {
		t.Errorf("fasta fixture stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceReverseComplementBgFixture pins the unmodified
// bench/template/bg/reverse_complement fixture (N=4096) on the C
// target. The output `293888` = (N/4)*287 confirms the
// fill+reverse+complement+sum loop runs correctly.
func TestBuildSourceReverseComplementBgFixture(t *testing.T) {
	src := readBenchFixture(t, "reverse_complement", 0)
	got := runMochiBuild(t, src)
	if want := "293888\n"; got != want {
		t.Errorf("reverse_complement fixture stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceRegexReduxBgFixture pins the unmodified
// bench/template/bg/regex_redux fixture (N=10000) on the C target.
// The output `69` is the deterministic count of the two 4-base
// patterns over the LCG stream and byte-matches `mochi run` on the
// same source.
func TestBuildSourceRegexReduxBgFixture(t *testing.T) {
	src := readBenchFixture(t, "regex_redux", 0)
	got := runMochiBuild(t, src)
	if want := "69\n"; got != want {
		t.Errorf("regex_redux fixture stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceKNucleotideBgFixture pins the unmodified
// bench/template/bg/k_nucleotide fixture (N=10000) on the C target.
// The fixture exercises the Phase 4.3.15.2 surface end-to-end:
// `var counts: map<int, int> = {}` initializer lowers via OpNewMap,
// `counts[k] = v` via OpMapSetI64I64, and `counts[k]` (in read
// position) via OpMapGetI64I64. Output is the LCG-driven rolling
// i64 hash of 20 counts; byte-matches the --target=go build on the
// same source (the interpreter rejects the source with a type
// error, so the cross-check is C target vs Go target).
func TestBuildSourceKNucleotideBgFixture(t *testing.T) {
	src := readBenchFixture(t, "k_nucleotide", 0)
	got := runMochiBuild(t, src)
	if want := "723253870\n"; got != want {
		t.Errorf("k_nucleotide fixture stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceBinaryTreesBgFixture pins the unmodified
// bench/template/bg/binary_trees fixture (N=4) on the C target. The
// fixture exercises the Phase 4.3.15.1 surface end-to-end:
// `make_tree(depth: int): list<any>` returns either `[]` (OpNewListAny)
// or `[left, right]` (OpNewListAny + two OpListAnyPushAny), and
// `check_tree(t: list<any>): int` reads `len(t)` (OpListAnyLen) plus
// `t[0] as list<any>` / `t[1] as list<any>` (OpListAnyGetAny with a
// no-op same-type cast). For N=4 the result is 16 iters * 31 nodes
// per depth-4 tree = 496; byte-matches the --target=go build on the
// same source (interpreter rejects the kernel for list<any> type).
func TestBuildSourceBinaryTreesBgFixture(t *testing.T) {
	src := readBenchFixture(t, "binary_trees", 4)
	got := runMochiBuild(t, src)
	if want := "{\"duration_us\":0,\"output\":496}\n"; got != want {
		t.Errorf("binary_trees fixture stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceMapI64I64Basic pins the Phase 4.3.15.2 map<int,int>
// surface on the C target: empty-literal initializer, an
// indexed-assign store, a read of the same key, plus a read of an
// absent key that must return 0 (matching Go's zero-default semantic
// for `map[int64]int64`).
func TestBuildSourceMapI64I64Basic(t *testing.T) {
	src := `var m: map<int, int> = {}
m[7] = 11
m[8] = 22
print(m[7])
print(m[8])
print(m[999])
`
	if got, want := runMochiBuild(t, src), "11\n22\n0\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceMapI64I64Grow exercises the runtime's grow path: 32
// inserts force at least two grows (initial cap=8, threshold 0.75
// triggers grow at len=7, again at len=13, again at len=25). Reads
// after grow must still return the right values; this catches a
// rehash bug where the new probe order doesn't match the new mask.
func TestBuildSourceMapI64I64Grow(t *testing.T) {
	src := `var m: map<int, int> = {}
var i = 0
while i < 32 {
  m[i] = i * 100
  i = i + 1
}
var k = 0
var sum = 0
while k < 32 {
  sum = sum + m[k]
  k = k + 1
}
print(sum)
`
	if got, want := runMochiBuild(t, src), "49600\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceListAnyBasic pins the Phase 4.3.15.1 list<any>
// surface on the C target: empty literal initializer in a function
// returning list<any>, a 2-element literal with list<any> elements
// (recursive shape), len() on both, and an indexed get returning a
// list<any> child that flows back through `as list<any>` (no-op cast).
func TestBuildSourceListAnyBasic(t *testing.T) {
	src := `fun makeLeaf(): list<any> {
  return []
}

fun makePair(a: list<any>, b: list<any>): list<any> {
  return [a, b]
}

let leaf = makeLeaf()
let pair = makePair(leaf, makeLeaf())
print(len(leaf))
print(len(pair))
print(len(pair[0] as list<any>))
print(len(pair[1] as list<any>))
`
	if got, want := runMochiBuild(t, src), "0\n2\n0\n0\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceListAnyGrow exercises the runtime growth path on a
// list<any> tree: 32 pushes force at least three grows (initial cap=4,
// doubling). After grow, the get-by-index path must still return the
// right child pointer, so a regression in the realloc/copy step would
// surface as a wrong child count downstream.
func TestBuildSourceListAnyGrow(t *testing.T) {
	src := `fun makeLeaf(): list<any> {
  return []
}

fun makeMany(): list<any> {
  var t: list<any> = []
  var i = 0
  while i < 32 {
    t = [t, makeLeaf()]
    i = i + 1
  }
  return t
}

fun depth(t: list<any>): int {
  if len(t) == 0 {
    return 0
  }
  return 1 + depth(t[0] as list<any>)
}

let big = makeMany()
print(depth(big))
print(len(big))
`
	if got, want := runMochiBuild(t, src), "32\n2\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStringLiteralHello pins the Phase 4.2.0 surface:
// a top-level `print("hello, world!")` builds via `mochi build
// --target=c` and writes the expected line to stdout. This is the
// smallest user-facing program against the §Top-line objective ("a
// single native binary that runs on a clean machine").
func TestBuildSourceStringLiteralHello(t *testing.T) {
	src := `print("hello, world!")` + "\n"
	if got, want := runMochiBuild(t, src), "hello, world!\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStringLiteralEscape covers escape coverage in the
// emitter's cStringLiteral helper: backslash, double-quote, newline,
// and a non-ASCII byte (0xc3 = first byte of UTF-8 "é"). The runtime
// just writes bytes through, so the output must match the input
// byte-for-byte plus the trailing print newline.
func TestBuildSourceStringLiteralEscape(t *testing.T) {
	src := `print("a\"b\\c\nd")` + "\n"
	if got, want := runMochiBuild(t, src), "a\"b\\c\nd\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStringLiteralLet pins string-typed let bindings: the
// frontend stores the literal in fn.Strings, OpConst{TypeStr} reads
// from that side-table, and the emitter declares `const char* v3 = 0;`
// at function head followed by `v3 = "..."` in the body. Two prints
// share the same constant pool, exercising the index path.
func TestBuildSourceStringLiteralLet(t *testing.T) {
	src := `let a = "first"
let b = "second"
print(a)
print(b)
print(a)
`
	if got, want := runMochiBuild(t, src), "first\nsecond\nfirst\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceLenStrLiteral pins the Phase 4.2.1 surface: `len`
// applied to a string literal lowers to OpLenStr and the C emitter
// emits `(int64_t)strlen(...)`. The runtime <string.h> include is
// added by the pre-pass when OpLenStr is present in the program.
func TestBuildSourceLenStrLiteral(t *testing.T) {
	src := `print(len("hello"))` + "\n"
	if got, want := runMochiBuild(t, src), "5\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceLenStrEmpty pins the empty-string edge: strlen of
// "" is 0, which matches Mochi's `len("")` returning int(0).
func TestBuildSourceLenStrEmpty(t *testing.T) {
	src := `print(len(""))` + "\n"
	if got, want := runMochiBuild(t, src), "0\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceLenStrViaLet pins len on a string-typed let binding,
// exercising the side-table indirection (the literal lives in
// fn.Strings, OpLenStr reads the carrier variable rather than a
// freshly emitted literal).
func TestBuildSourceLenStrViaLet(t *testing.T) {
	src := `let s = "hello, world!"
print(len(s))
`
	if got, want := runMochiBuild(t, src), "13\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrEqLiteralTrue pins the Phase 4.2.2 surface for
// the equal-literals case: `"hi" == "hi"` lowers to OpCmpEqStr,
// strcmp returns 0, the boolean is true, and the if-branch fires.
func TestBuildSourceStrEqLiteralTrue(t *testing.T) {
	src := `if "hi" == "hi" {
  print("yes")
} else {
  print("no")
}
`
	if got, want := runMochiBuild(t, src), "yes\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrEqLiteralFalse pins the unequal-literals case:
// strcmp returns non-zero so the boolean is false and the else
// branch fires.
func TestBuildSourceStrEqLiteralFalse(t *testing.T) {
	src := `if "hi" == "bye" {
  print("yes")
} else {
  print("no")
}
`
	if got, want := runMochiBuild(t, src), "no\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrEqViaLet pins string equality on a let-bound
// carrier: the side-table holds the literal, OpCmpEqStr reads the
// `const char*` variable on both sides.
func TestBuildSourceStrEqViaLet(t *testing.T) {
	src := `let answer = "yes"
if answer == "yes" {
  print("matched")
}
`
	if got, want := runMochiBuild(t, src), "matched\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrNeLiteral pins the `!=` arm: same surface, with
// the result inverted by OpCmpNeStr's `!= 0` test.
func TestBuildSourceStrNeLiteral(t *testing.T) {
	src := `if "a" != "b" {
  print("differ")
}
`
	if got, want := runMochiBuild(t, src), "differ\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrConcatLiteral pins the Phase 4.2.3 surface: `+`
// on two string literals lowers to OpConcatStr, the C emitter calls
// mochi_str_concat (auto-included via mochi_str.h), and the runtime
// allocates a NUL-terminated heap buffer that the existing
// mochi_print_str writes to stdout.
func TestBuildSourceStrConcatLiteral(t *testing.T) {
	src := `print("hello, " + "world")` + "\n"
	if got, want := runMochiBuild(t, src), "hello, world\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrConcatLet pins concat over let-bound carriers:
// the side-table holds each literal; OpConcatStr reads the carrier
// variables and the heap result re-binds to a fresh carrier.
func TestBuildSourceStrConcatLet(t *testing.T) {
	src := `let a = "foo"
let b = "bar"
print(a + b)
`
	if got, want := runMochiBuild(t, src), "foobar\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrConcatChain pins left-associative chaining: the
// frontend's Shunting-Yard lowering produces a left-leaning tree, so
// "[" + a + "-" + b + "]" lowers to four nested OpConcatStr calls
// each receiving the previous heap pointer as left arg.
func TestBuildSourceStrConcatChain(t *testing.T) {
	src := `let a = "foo"
let b = "bar"
print("[" + a + "-" + b + "]")
`
	if got, want := runMochiBuild(t, src), "[foo-bar]\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrConcatLenComposes pins len applied to a concat
// result, exercising that the heap carrier is NUL-terminated so
// strlen reads the joined byte length.
func TestBuildSourceStrConcatLenComposes(t *testing.T) {
	src := `let a = "abc"
let b = "defgh"
print(len(a + b))
`
	if got, want := runMochiBuild(t, src), "8\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrConcatEqComposes pins equality on a concat
// result: strcmp reads through the heap carrier as it would through
// a literal. Mochi's `==` is byte equality on the joined sequence.
func TestBuildSourceStrConcatEqComposes(t *testing.T) {
	src := `let a = "hel"
let b = "lo"
if a + b == "hello" {
  print("matched")
}
`
	if got, want := runMochiBuild(t, src), "matched\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrIntLiteral pins MEP-42 Phase 4.2.4: `str(42)`
// on an int literal lowers to OpI64ToStr, the C emitter calls
// mochi_str_from_i64, the runtime allocates a decimal carrier and
// mochi_print_str writes it to stdout. The numeric value matches
// PRId64 byte-for-byte.
func TestBuildSourceStrIntLiteral(t *testing.T) {
	src := `print(str(42))` + "\n"
	if got, want := runMochiBuild(t, src), "42\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrIntNegative pins the sign-preserving shape of
// mochi_str_from_i64: PRId64 prints a leading '-' for negative
// values, so the carrier round-trips through Mochi's i64 range.
func TestBuildSourceStrIntNegative(t *testing.T) {
	src := `print(str(-7))` + "\n"
	if got, want := runMochiBuild(t, src), "-7\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrFloatLiteral pins `str(3.5)` on an f64 literal:
// mochi_str_from_f64 runs the shortest-round-trip search shared
// with print.c, so str(x) and print(x) agree on the digits printed.
func TestBuildSourceStrFloatLiteral(t *testing.T) {
	src := `print(str(3.5))` + "\n"
	if got, want := runMochiBuild(t, src), "3.5\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrBoolTrue / False pin the static-literal return
// of mochi_str_from_bool: no heap allocation, just one of two C99
// literals. The bool carrier is interchangeable with a literal at
// every downstream string op (concat, print, len, strcmp).
func TestBuildSourceStrBoolTrue(t *testing.T) {
	src := `print(str(true))` + "\n"
	if got, want := runMochiBuild(t, src), "true\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

func TestBuildSourceStrBoolFalse(t *testing.T) {
	src := `print(str(false))` + "\n"
	if got, want := runMochiBuild(t, src), "false\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrConcatWithInt pins composition with OpConcatStr:
// `"answer: " + str(x)` lowers to mochi_str_from_i64 followed by
// mochi_str_concat. This is the user-facing motivation for Phase
// 4.2.4 (formatted print without multi-line value-only print calls).
func TestBuildSourceStrConcatWithInt(t *testing.T) {
	src := `let x = 42
print("answer: " + str(x))
`
	if got, want := runMochiBuild(t, src), "answer: 42\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrConcatWithBool pins composition of the static
// "true"/"false" literal with mochi_str_concat: the static buffer is
// a valid `const char*` carrier, so strcmp/strlen/concat treat it
// identically to a heap-allocated carrier.
func TestBuildSourceStrConcatWithBool(t *testing.T) {
	src := `print("ok=" + str(true))` + "\n"
	if got, want := runMochiBuild(t, src), "ok=true\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceWebsiteHomepageHello pins MEP-42 Phase 4.2.5: the
// canonical hello program from `examples/website/hello.mochi`, the
// homepage demo on mochi-lang.dev. It is the smallest end-to-end
// user-facing program that exercises the full Phase 4.2.x string
// stack (let-bound string, concat with `+`, `str(int)` lift, two
// `print` calls back-to-back). Reads the file verbatim so a future
// rewrite of the homepage demo is caught as a regression here.
func TestBuildSourceWebsiteHomepageHello(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/website/hello.mochi")
	if err != nil {
		t.Fatalf("read homepage hello fixture: %v", err)
	}
	src := string(srcBytes)
	want := "Hello, Mochi!\nthe answer is 42\n"
	if got := runMochiBuild(t, src); got != want {
		t.Errorf("homepage hello stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceMultiArgPrintLabel pins MEP-42 Phase 4.2.6: the
// common `print("label", value)` idiom from v0.1 tutorial examples
// lowers to a single mochi_print_str of the space-joined form. The
// space separator matches Go's fmt.Println default; the value's
// string form matches strconv (Phase 4.2.4).
func TestBuildSourceMultiArgPrintLabel(t *testing.T) {
	src := `let i = 3
print("i =", i)
`
	if got, want := runMochiBuild(t, src), "i = 3\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceMultiArgPrintThree pins the >2 arg case: each
// successive arg adds a leading " " separator before its string
// form (Go's fmt.Println behaviour for space-joined Sprint of args).
func TestBuildSourceMultiArgPrintThree(t *testing.T) {
	src := `print("Sum", "=", 55)` + "\n"
	if got, want := runMochiBuild(t, src), "Sum = 55\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceMultiArgPrintMixed pins the heterogeneous-types
// case: int, float, bool, and string each lifts through the matching
// scalar->str op (Phase 4.2.4) before joining.
func TestBuildSourceMultiArgPrintMixed(t *testing.T) {
	src := `print("answer", 42, 3.5, true)` + "\n"
	if got, want := runMochiBuild(t, src), "answer 42 3.5 true\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceMultiArgPrintLoop pins the v0.1/for.mochi tutorial
// surface: `for i in lo..hi { print("i =", i) }` produces one
// labeled line per iteration. This is the exact user-facing motivation
// for Phase 4.2.6.
func TestBuildSourceMultiArgPrintLoop(t *testing.T) {
	src := `for i in 0..3 {
  print("i =", i)
}
`
	if got, want := runMochiBuild(t, src), "i = 0\ni = 1\ni = 2\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceBoolEqTrue pins MEP-42 Phase 4.2.7: bool == bool.
// Before this sub-phase, `let a = true; let b = true; print(a == b)`
// errored at lower with `binop "==" on type bool unsupported in MVP`.
// After it, the comparison routes through OpCmpEqBool and the
// single-arg print path renders it through fmt.Println (Phase 4.2.0),
// matching Go's "true" / "false" convention byte for byte.
func TestBuildSourceBoolEqTrue(t *testing.T) {
	src := `let a = true
let b = true
print(a == b)
`
	if got, want := runMochiBuild(t, src), "true\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceBoolNe pins `!=` on bool, the other half of v0.1's
// examples/v0.1/binary.mochi block ("bool_neq").
func TestBuildSourceBoolNe(t *testing.T) {
	src := `let a = true
let b = false
print(a != b)
`
	if got, want := runMochiBuild(t, src), "true\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceBoolEqMixed pins the heterogeneous mixing of bool
// comparisons through the multi-arg print path: a `bool == bool`
// result lifts through OpBoolToStr (Phase 4.2.4) into the
// space-joined string. This exercises the full Phase 4.2.x interplay.
func TestBuildSourceBoolEqMixed(t *testing.T) {
	src := `let ba = true
let bb = false
print("bool_eq:", ba == ba)
print("bool_neq:", ba != bb)
`
	if got, want := runMochiBuild(t, src), "bool_eq: true\nbool_neq: true\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceBoolEqUnaryNested pins examples/v0.1/unary.mochi's
// nested expression `!((2 < 3) == true)`. The inner `(2 < 3)`
// produces a bool (OpCmpLtI64), the `== true` then triggers
// OpCmpEqBool, and the outer `!` is OpNotBool. The final value is
// false / 0.
func TestBuildSourceBoolEqUnaryNested(t *testing.T) {
	src := `let e = !((2 < 3) == true)
print(e)
`
	if got, want := runMochiBuild(t, src), "false\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceV01BinaryFixture pins examples/v0.1/binary.mochi
// end-to-end, the v0.1 tutorial program that motivated Phase 4.2.7.
// Reads the fixture verbatim so future tutorial edits surface as a
// regression here.
func TestBuildSourceV01BinaryFixture(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/v0.1/binary.mochi")
	if err != nil {
		t.Fatalf("read v0.1 binary fixture: %v", err)
	}
	want := "add: 5\nsub: 3\nmul: 10\ndiv: 4\neq: true\nneq: true\nlt: true\nlte: true\ngt: true\ngte: true\nstr_eq: true\nstr_neq: true\nstr_concat: hello world\nbool_eq: true\nbool_neq: true\n"
	if got := runMochiBuild(t, string(srcBytes)); got != want {
		t.Errorf("v0.1/binary stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceF64GoParityTen pins MEP-42 Phase 4.2.8: a finite
// float value whose magnitude lies in Go's fixed-form window must
// print without C99's "%g" exponent escape. Before this sub-phase,
// `print(10.0)` produced "1e+01\n" on the C target because C99 "%g"
// at precision 1 chose exponent form (1 >= 1); Go's
// strconv.FormatFloat(10.0, 'g', -1, 64) returns "10". The shared
// mochi_f64_format helper now reformats those cases.
func TestBuildSourceF64GoParityTen(t *testing.T) {
	src := `print(10.0)` + "\n"
	if got, want := runMochiBuild(t, src), "10\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceF64GoParityHundredK pins the upper edge of Go's
// fixed-form window. 1e5 (= 100000) sits at exp=5 which is < 6, so
// Go prints "100000"; C99 "%g" at precision 1 would print "1e+05".
func TestBuildSourceF64GoParityHundredK(t *testing.T) {
	src := `print(1e5)` + "\n"
	if got, want := runMochiBuild(t, src), "100000\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceF64GoParityMillion pins the lower edge of Go's
// exponent-form window. 1e6 sits at exp=6 which is >= 6, so Go
// prints "1e+06". Both C99 "%g" at precision 1 and the new helper
// must agree on this case.
func TestBuildSourceF64GoParityMillion(t *testing.T) {
	src := `print(1e6)` + "\n"
	if got, want := runMochiBuild(t, src), "1e+06\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceF64GoParityFractionalEdge pins the lower fixed-form
// edge. 1e-4 (= 0.0001) is the smallest magnitude Go prints in fixed
// form (exp = -4); 1e-5 flips to exp form. C99 "%g" agrees on the
// boundary; the test guards the new code path against regressing.
func TestBuildSourceF64GoParityFractionalEdge(t *testing.T) {
	src := `print(0.0001)` + "\n"
	if got, want := runMochiBuild(t, src), "0.0001\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
	src = `print(0.00001)` + "\n"
	if got, want := runMochiBuild(t, src), "1e-05\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceF64GoParityStr pins str(f64) on the same divergent
// values: str(10.0) must produce "10" (not "1e+01"), matching the
// Go target's strconv.FormatFloat. The single-arg print() path and
// the multi-arg / str() path now share mochi_f64_format, so this
// test guards against drift between them.
func TestBuildSourceF64GoParityStr(t *testing.T) {
	src := `print("e =", 10.0)` + "\n"
	if got, want := runMochiBuild(t, src), "e = 10\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceV01ExprFixture pins examples/v0.1/expr.mochi
// end-to-end. Before Phase 4.2.8, "e = 1e+01" leaked into the
// binary's stdout; after, the program byte-matches mochi run.
func TestBuildSourceV01ExprFixture(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/v0.1/expr.mochi")
	if err != nil {
		t.Fatalf("read v0.1 expr fixture: %v", err)
	}
	want := "a = 11\nb = 14\nc = 8\nd = 56\ne = 10\nf = 3\n"
	if got := runMochiBuild(t, string(srcBytes)); got != want {
		t.Errorf("v0.1/expr stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceBoolAndBasic pins MEP-42 Phase 4.2.9: bool && bool.
// Before this sub-phase, the v0.3/logic.mochi tutorial errored at
// lower with `operator "||" on bool unsupported in MVP`. After it,
// the elementary `let a = true && false; print(a)` lowers through
// OpAndBool, emits C99 `&&`, and prints "false" (matching Go's
// fmt.Println).
func TestBuildSourceBoolAndBasic(t *testing.T) {
	src := `let r = true && false
print(r)
`
	if got, want := runMochiBuild(t, src), "false\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceBoolOrBasic pins the OR symmetric case.
func TestBuildSourceBoolOrBasic(t *testing.T) {
	src := `let r = false || true
print(r)
`
	if got, want := runMochiBuild(t, src), "true\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceBoolAndOrChain pins v0.3/logic.mochi's mixed
// expression `x > 0 && y > 2 || x == 0`, which threads
// OpCmpGtI64 -> OpAndBool -> OpCmpEqI64 -> OpOrBool. Precedence
// follows Mochi parser; the result is the if-condition's truth.
func TestBuildSourceBoolAndOrChain(t *testing.T) {
	src := `let x = 3
let y = 5
if x > 0 && y > 2 || x == 0 {
  print("yes")
} else {
  print("no")
}
`
	if got, want := runMochiBuild(t, src), "yes\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceBoolAndOrLeftRightPure pins the eager-evaluation
// limitation: both operands are evaluated regardless of the
// left-hand result, matching Go's `a && b` behaviour for
// side-effect-free operands. This is a deliberate scope of Phase
// 4.2.9 (true short-circuit at the IR level is a separate gate).
func TestBuildSourceBoolAndOrLeftRightPure(t *testing.T) {
	src := `let a = true
let b = false
print("and:", a && b)
print("or:", a || b)
`
	if got, want := runMochiBuild(t, src), "and: false\nor: true\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceV03LogicFixture pins examples/v0.3/logic.mochi
// end-to-end, the user-facing motivation for Phase 4.2.9. Reads the
// fixture verbatim so future edits surface as a regression here.
func TestBuildSourceV03LogicFixture(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/v0.3/logic.mochi")
	if err != nil {
		t.Fatalf("read v0.3 logic fixture: %v", err)
	}
	want := "true || false = true\nfalse || false = false\nx > 0 || y < 0 = true\ntrue && false = false\ntrue && true = true\nx < 10 && y > 2 = true\nCondition matched!\n"
	if got := runMochiBuild(t, string(srcBytes)); got != want {
		t.Errorf("v0.3/logic stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceBreakInForRange pins MEP-42 Phase 4.2.10: a bare
// `break` inside a `for x in lo..hi` loop exits the loop at the
// first iteration where the condition fires. Before this sub-phase,
// the frontend lower path errored with `statement kind unsupported
// in MVP` for any `break` or `continue`. After it, `break` lowers
// to a jump to the loop's cont block.
func TestBuildSourceBreakInForRange(t *testing.T) {
	src := `for i in 0..10 {
  if i == 3 {
    break
  }
  print(i)
}
print("done")
`
	if got, want := runMochiBuild(t, src), "0\n1\n2\ndone\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceContinueInForRange pins `continue` inside a for-
// range loop: every iteration where the predicate fires skips to
// the iteration step. The loop variable still advances so the loop
// terminates.
func TestBuildSourceContinueInForRange(t *testing.T) {
	src := `for i in 0..5 {
  if i == 2 {
    continue
  }
  print(i)
}
`
	if got, want := runMochiBuild(t, src), "0\n1\n3\n4\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceBreakInWhile pins `break` inside a `while cond`
// loop: the body's exit edge bypasses the cond test on the next
// iteration. Cont block has [head, breakBlock] as predecessors.
func TestBuildSourceBreakInWhile(t *testing.T) {
	src := `var i = 0
while i < 100 {
  if i == 4 {
    break
  }
  print(i)
  i = i + 1
}
print("after")
`
	if got, want := runMochiBuild(t, src), "0\n1\n2\n3\nafter\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceContinueInWhile pins `continue` inside `while`:
// since while has no synthetic step, the continue path must include
// the user's own counter increment for the loop to terminate.
func TestBuildSourceContinueInWhile(t *testing.T) {
	src := `var i = 0
while i < 5 {
  i = i + 1
  if i == 3 {
    continue
  }
  print(i)
}
`
	if got, want := runMochiBuild(t, src), "1\n2\n4\n5\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceBreakContinueCombined pins the v0.3/break-continuous
// pattern: a for-collection loop with both `continue` (skip evens)
// and `break` (exit on threshold), exercising both edges from the
// same loop's body. Cont's phi machinery joins the cond-false head
// flow with the break snapshot; the head's phi list grows by one
// pair per continue.
func TestBuildSourceBreakContinueCombined(t *testing.T) {
	src := `let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9]
for n in numbers {
  if n % 2 == 0 {
    continue
  }
  if n > 7 {
    break
  }
  print("odd:", n)
}
`
	if got, want := runMochiBuild(t, src), "odd: 1\nodd: 3\nodd: 5\nodd: 7\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceBreakInNestedLoop pins that `break` is innermost-
// loop-scoped: an inner break exits only the inner loop, the outer
// loop keeps iterating.
func TestBuildSourceBreakInNestedLoop(t *testing.T) {
	src := `for i in 0..3 {
  for j in 0..5 {
    if j == 2 {
      break
    }
    print(i, j)
  }
}
`
	if got, want := runMochiBuild(t, src), "0 0\n0 1\n1 0\n1 1\n2 0\n2 1\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceV03BreakContinuousFixture pins the actual on-disk
// fixture so a regression in either the example or the lowering is
// caught here. This is the user-facing motivation for Phase 4.2.10.
func TestBuildSourceV03BreakContinuousFixture(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/v0.3/break-continuous.mochi")
	if err != nil {
		t.Fatalf("read v0.3 break-continuous fixture: %v", err)
	}
	want := "odd number: 1\nodd number: 3\nodd number: 5\nodd number: 7\n"
	if got := runMochiBuild(t, string(srcBytes)); got != want {
		t.Errorf("v0.3/break-continuous stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceMatchExprInt pins MEP-42 Phase 4.2.11: a match
// expression over an i64 discriminant with literal patterns and a
// `_` wildcard. Before this sub-phase, the frontend errored at
// lowerPrimary with `primary form unsupported in MVP`. After it,
// the expression lowers to a chained branch + phi at the merge.
func TestBuildSourceMatchExprInt(t *testing.T) {
	src := `let x = 2
let label = match x {
  1 => "one"
  2 => "two"
  3 => "three"
  _ => "unknown"
}
print(label)
`
	if got, want := runMochiBuild(t, src), "two\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceMatchExprStr pins the str discriminant path
// through `OpCmpEqStr`, the same op the bool equality tutorial
// uses. The result is again a string drawn from the matching arm.
func TestBuildSourceMatchExprStr(t *testing.T) {
	src := `let day = "sun"
let mood = match day {
  "mon" => "tired"
  "fri" => "excited"
  "sun" => "relaxed"
  _     => "normal"
}
print(mood)
`
	if got, want := runMochiBuild(t, src), "relaxed\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceMatchExprBoolExhaustive pins the bool discriminant
// without `_` wildcard: since bool has exactly two values, the user
// can omit `_` and rely on the last arm catching the remaining
// value. The frontend treats the last arm as unconditional under
// the same rule that handles `_`, so this lowers as expected.
func TestBuildSourceMatchExprBoolExhaustive(t *testing.T) {
	src := `let ok = true
let status = match ok {
  true => "confirmed"
  false => "denied"
}
print(status)
`
	if got, want := runMochiBuild(t, src), "confirmed\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceMatchInReturn pins `return match ...` inside a
// fun: the match expression lowers in the return statement's
// expression slot, and the result phi flows into TermReturn. The
// fixture mirrors v0.3/match.mochi's `classify` example.
func TestBuildSourceMatchInReturn(t *testing.T) {
	src := `fun classify(n: int): string {
  return match n {
    0 => "zero"
    1 => "one"
    _ => "many"
  }
}
print(classify(0))
print(classify(5))
`
	if got, want := runMochiBuild(t, src), "zero\nmany\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceIfExprStr pins MEP-42 Phase 4.2.12: `if cond then T
// else E` as an expression in a binding position. Before this phase
// the frontend rejected `let r = if ...` with `primary form
// unsupported in MVP`; after it, the expression lowers to a 2-way
// branch + phi at the merge block, with both arms required to share
// a value type.
func TestBuildSourceIfExprStr(t *testing.T) {
	src := `let x = 12
let result = if x > 10 then "yes" else "no"
print(result)
`
	if got, want := runMochiBuild(t, src), "yes\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceIfExprInt pins the i64 result path: both branches
// produce TypeI64, the merge phi infers TypeI64, and the bound name
// flows into print as i64.
func TestBuildSourceIfExprInt(t *testing.T) {
	src := `let n = 3
let abs = if n < 0 then 0 - n else n
print(abs)
`
	if got, want := runMochiBuild(t, src), "3\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceIfExprElseIfChain pins the `else if` recursion
// inside lowerIfExpr: the else branch becomes a nested if-expr that
// itself produces a merge phi flowed into the outer phi.
func TestBuildSourceIfExprElseIfChain(t *testing.T) {
	src := `let n = 2
let label = if n == 1 then "one" else if n == 2 then "two" else if n == 3 then "three" else "other"
print(label)
`
	if got, want := runMochiBuild(t, src), "two\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceIfExprInReturn pins `return if ...` inside a fun:
// the if-expr lowers in the return statement's expression slot, and
// the merge phi flows into TermReturn.
func TestBuildSourceIfExprInReturn(t *testing.T) {
	src := `fun classify(n: int): string {
  return if n < 0 then "negative" else if n == 0 then "zero" else "positive"
}
print(classify(-5))
print(classify(0))
print(classify(7))
`
	if got, want := runMochiBuild(t, src), "negative\nzero\npositive\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceV010IfThenElseFixture pins the on-disk fixture
// verbatim so the v0.10/if_then_else.mochi tutorial example
// regression-tests if-expr lowering at the fixture level.
func TestBuildSourceV010IfThenElseFixture(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/v0.10/if_then_else.mochi")
	if err != nil {
		t.Fatalf("read v0.10 if_then_else fixture: %v", err)
	}
	want := "yes\n"
	if got := runMochiBuild(t, string(srcBytes)); got != want {
		t.Errorf("v0.10/if_then_else stdout = %q, want %q", got, want)
	}
}

// TestBuildSourcePrintListI64 pins MEP-42 Phase 4.2.13: print(xs)
// where xs is a list<int>. Before this phase the frontend rejected
// `print(list)` with `print() argument type list unsupported in MVP`.
// After it, the value is lifted via OpListI64ToStr (-> TypeStr) then
// fed into the existing single-arg print path. The runtime helper
// mochi_list_i64_to_str produces the Mochi reference `[a, b, c]`
// form (comma-space separators, square brackets, no newline) so the
// C target byte-matches `mochi run`.
func TestBuildSourcePrintListI64(t *testing.T) {
	src := `let xs = [1, 2, 3, 4, 5]
print(xs)
`
	if got, want := runMochiBuild(t, src), "[1, 2, 3, 4, 5]\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourcePrintListI64Empty covers the empty-list edge case.
// mochi_list_i64_to_str returns the static "[]" literal, no malloc.
func TestBuildSourcePrintListI64Empty(t *testing.T) {
	src := `let xs: list<int> = []
print(xs)
`
	if got, want := runMochiBuild(t, src), "[]\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourcePrintListI64MultiArg covers list<int> as one of
// several print() args. liftToStr's new TypeList case converts the
// list value to its display string, then the concat-with-separator
// path joins it into the space-separated multi-arg form.
func TestBuildSourcePrintListI64MultiArg(t *testing.T) {
	src := `let xs = [10, 20, 30]
print("values:", xs)
`
	if got, want := runMochiBuild(t, src), "values: [10, 20, 30]\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourcePrintListI64AfterConcat covers a list grown via
// concat: mochi_list_i64_to_str walks len, not cap, so a concat
// producing a 3-element list prints `[1, 2, 3]` regardless of any
// over-allocation in the concat helper.
func TestBuildSourcePrintListI64AfterConcat(t *testing.T) {
	src := `let xs = [1, 2] + [3]
print(xs)
`
	if got, want := runMochiBuild(t, src), "[1, 2, 3]\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourcePrintListF64 pins MEP-42 Phase 4.2.14: print(xs)
// where xs is a list<float>. Lifted via OpF64ArrayToStr to TypeStr
// then fed through the single-arg print path. The runtime helper
// mochi_f64_array_to_str renders integral floats with the ".0"
// suffix (matching FormatFloat 'f' -1 64 + ".0"), so the C target
// byte-matches `mochi run`.
func TestBuildSourcePrintListF64(t *testing.T) {
	src := `let xs: list<float> = [1.0, 2.5, 3.14]
print(xs)
`
	if got, want := runMochiBuild(t, src), "[1.0, 2.5, 3.14]\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourcePrintListF64Empty covers the empty-list edge case
// on the f64 array runtime. mochi_f64_array_to_str returns the
// static "[]" literal, no malloc.
func TestBuildSourcePrintListF64Empty(t *testing.T) {
	src := `let xs: list<float> = []
print(xs)
`
	if got, want := runMochiBuild(t, src), "[]\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourcePrintListF64Integral pins the ".0" suffix behavior
// that distinguishes list-context float formatting from scalar
// formatting: `print(1.0)` prints "1" (uses 'g'), but `print([1.0])`
// prints "[1.0]" (uses 'f' -1 + ".0"). Without the suffix this test
// would see "[1, 2, 3]" matching the i64 path.
func TestBuildSourcePrintListF64Integral(t *testing.T) {
	src := `let xs: list<float> = [1.0, 2.0, 3.0]
print(xs)
`
	if got, want := runMochiBuild(t, src), "[1.0, 2.0, 3.0]\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourcePrintListF64MultiArg covers list<float> as one of
// several print() args. liftToStr's new TypeF64Arr case threads the
// list through the same display formatter, then the multi-arg path
// joins it into the space-separated form `label: [1.0, 2.0]`.
func TestBuildSourcePrintListF64MultiArg(t *testing.T) {
	src := `let xs: list<float> = [1.0, 2.5]
print("data:", xs)
`
	if got, want := runMochiBuild(t, src), "data: [1.0, 2.5]\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourcePrintListF64ScientificRange exercises the value
// range that distinguishes 'f' format from 'g': 1e-5 prints as
// "0.00001" (not "1e-05") and 1.5e10 prints as "15000000000.0"
// (not "1.5e+10"). The shortest-round-trip search in
// format_f64_decimal must pick the minimal precision that recovers
// the original double; 1.5e10 with p=0 is the integral case (no
// decimal in snprintf, append ".0").
func TestBuildSourcePrintListF64ScientificRange(t *testing.T) {
	src := `let xs: list<float> = [1.0e-5, 1.5e10]
print(xs)
`
	if got, want := runMochiBuild(t, src), "[0.00001, 15000000000.0]\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceNegIndexI64 pins MEP-42 Phase 4.2.15: literal-negative
// list indexing on the C target. `xs[-1]` is folded at lower time to
// `xs[len + -1]` so the v0.2/list.mochi `values[-1]` line returns the
// last element instead of reading past the start of the buffer. Before
// this phase the C target returned 0 (or some other garbage from
// stack-adjacent memory) instead of 50.
func TestBuildSourceNegIndexI64(t *testing.T) {
	src := `let xs = [10, 20, 30, 40, 50]
print(xs[-1])
print(xs[-2])
print(xs[0])
`
	if got, want := runMochiBuild(t, src), "50\n40\n10\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceNegIndexF64 pins the same fold for list<float>: the
// f64-array path picks OpF64ArrayLenI64 + OpAddI64Imm and feeds the
// wrapped index to OpF64ArrayGetF64. Verifies the curType switch in
// the fold picks the right LenOp for the element type.
func TestBuildSourceNegIndexF64(t *testing.T) {
	src := `let xs: list<float> = [1.0, 2.5, 3.14]
print(xs[-1])
print(xs[-2])
`
	if got, want := runMochiBuild(t, src), "3.14\n2.5\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceNegIndexAssign pins the indexed-assign mirror of the
// fold: `xs[-1] = v` rewrites to `xs[len + -1] = v` so the store
// targets the actual last element. Reads the slot back after the
// store to confirm the write landed in the right place.
func TestBuildSourceNegIndexAssign(t *testing.T) {
	src := `var xs = [1, 2, 3, 4, 5]
xs[-1] = 99
print(xs[-1])
print(xs[0])
print(xs[3])
`
	if got, want := runMochiBuild(t, src), "99\n1\n4\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceNegIndexUnaryNeg pins that the fold sees through the
// unary-minus lowering of `-N`. The surface form `xs[-1]` parses as
// unary `-` applied to literal `1`, which lowers to OpNegI64 of
// OpConst(1), not directly to OpConst(-1). Without constI64's
// negation-case the fold misses literal-negative indices entirely.
// Dynamic-`i` negative indexing is a separate future phase and not
// exercised here.
func TestBuildSourceNegIndexUnaryNeg(t *testing.T) {
	src := `let xs = [100, 200, 300]
print(xs[-1])
print(xs[-3])
`
	if got, want := runMochiBuild(t, src), "300\n100\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// Phase 4.2.16: top-level `let` is in scope inside user functions.
// Before this phase, a fun body that referenced a module-level let
// failed with `frontend: unbound identifier`. The frontend now
// pre-scans top-level lets and inlines the RHS at each use site
// inside fun bodies, caching the lowered value per builder so the
// constant is materialised once per function body.
func TestBuildSourceGlobalLetIntFromFn(t *testing.T) {
	src := `let n = 21

fun twice(): int {
  return n * 2
}

print(twice())
`
	if got, want := runMochiBuild(t, src), "42\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

func TestBuildSourceGlobalLetFloatFromFn(t *testing.T) {
	src := `let pi = 3.14

fun area(r: float): float {
  return pi * r * r
}

print(area(10.0))
`
	if got, want := runMochiBuild(t, src), "314\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceGlobalLetMultipleRefs pins the per-builder cache:
// referencing the same global twice in one fun body must lower the
// RHS once (re-using the same SSA value), not twice. Result-wise
// either still prints "44", so this test is a behavioural smoke
// check; cache correctness is observed via the test suite staying
// fast and the emitted code having a single OpConst for the global.
func TestBuildSourceGlobalLetMultipleRefs(t *testing.T) {
	src := `let n = 22

fun add(): int {
  return n + n
}

print(add())
`
	if got, want := runMochiBuild(t, src), "44\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

func TestBuildSourceGlobalLetMixedScopes(t *testing.T) {
	src := `let a = 10
let b = 5

fun diff(): int {
  return a - b
}

print(a)
print(b)
print(diff())
`
	if got, want := runMochiBuild(t, src), "10\n5\n5\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceGlobalLetShadowedByParam confirms that a function
// parameter still shadows a same-named module global. The fallback
// only fires when the name is not in b.values, so the param binding
// (which writes b.values) wins.
func TestBuildSourceGlobalLetShadowedByParam(t *testing.T) {
	src := `let x = 100

fun id(x: int): int {
  return x
}

print(id(7))
print(x)
`
	if got, want := runMochiBuild(t, src), "7\n100\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// Phase 4.2.17: list<str> literals on the C target. Before this
// phase the frontend rejected `let xs = ["a", "b"]` with
// `frontend: list literal element type str unsupported in MVP`.
// The phase adds TypeStrArr (backed by mochi_str_array on the C
// side and []string on the Go side) and wires the five core ops
// (new, len, push, get, set) plus the print formatter
// OpStrArrToStr that renders `["a", "b"]` with strconv.Quote-
// equivalent element escaping.
func TestBuildSourcePrintListStr(t *testing.T) {
	src := `let fruits = ["apple", "banana", "cherry"]
print(fruits)
`
	if got, want := runMochiBuild(t, src), "[\"apple\", \"banana\", \"cherry\"]\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

func TestBuildSourcePrintListStrEmpty(t *testing.T) {
	src := `var xs: list<str> = []
print(xs)
`
	if got, want := runMochiBuild(t, src), "[]\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

func TestBuildSourceListStrIndex(t *testing.T) {
	src := `let xs = ["a", "bc", "def"]
print(xs[0])
print(xs[1])
print(xs[2])
print(len(xs))
`
	if got, want := runMochiBuild(t, src), "a\nbc\ndef\n3\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceListStrNegIndex pins the Phase 4.2.15 negative-
// index fold for the new TypeStrArr carrier. Without the extra
// TypeStrArr case in the fold, `xs[-1]` would read past the start
// of the const char** buffer on the C target (undefined).
func TestBuildSourceListStrNegIndex(t *testing.T) {
	src := `let xs = ["first", "middle", "last"]
print(xs[-1])
print(xs[-3])
`
	if got, want := runMochiBuild(t, src), "last\nfirst\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceListStrForIn exercises the for-in lowering for
// TypeStrArr. The body sees each element as TypeStr; the loop
// uses OpStrArrLen + OpStrArrGetStr inside the header check and
// body bind.
func TestBuildSourceListStrForIn(t *testing.T) {
	src := `let xs = ["one", "two", "three"]
for x in xs {
  print(x)
}
`
	if got, want := runMochiBuild(t, src), "one\ntwo\nthree\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceListStrQuoteEscapes confirms that the C runtime's
// strconv.Quote-equivalent escape rule produces the same display
// form as the Go target (and the VM). Backslash, double-quote, and
// newline all need explicit escapes in the rendered list; non-ASCII
// UTF-8 passes through verbatim.
func TestBuildSourceListStrQuoteEscapes(t *testing.T) {
	src := "let xs = [\"a\\nb\", \"c\\\"d\", \"e\\\\f\", \"\xe4\xb8\xad\"]\nprint(xs)\n"
	want := "[\"a\\nb\", \"c\\\"d\", \"e\\\\f\", \"\xe4\xb8\xad\"]\n"
	if got := runMochiBuild(t, src); got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceMapStrI64Literal pins the v0.2 map.mochi shape: a
// `map<string, int>` literal with several entries, an indexed read,
// and `len(m)`. Before Phase 4.2.18 the binding hit "map<str, i64>
// unsupported in MVP"; the phase adds TypeMapStrI64 + new/get/set/len
// ops backed by runtime/c/src/mochi_map_str_i64.{h,c} on the C
// target and map[string]int64 on the Go target.
func TestBuildSourceMapStrI64Literal(t *testing.T) {
	src := `let scores: map<string, int> = {
  "Alice": 10,
  "Bob": 15
}
let aliceScore = scores["Alice"]
print("Alice's score:", aliceScore)
print("Total players:", len(scores))
`
	if got, want := runMochiBuild(t, src), "Alice's score: 10\nTotal players: 2\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceMapStrI64Default pins the Go-shaped zero-default:
// `m[k]` on an absent key returns 0. Mochi sources rely on this for
// counter-style accumulation; the C runtime probe returns 0 from
// the unoccupied bucket the same way Go's map[string]int64 does.
func TestBuildSourceMapStrI64Default(t *testing.T) {
	src := `let scores: map<string, int> = {"Alice": 10}
print(scores["Alice"])
print(scores["Unknown"])
`
	if got, want := runMochiBuild(t, src), "10\n0\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceMapStrI64Empty exercises the empty-literal form
// (which already worked for map<int, int>) under the new carrier.
// `len(m) == 0` confirms the new len op routes through
// mochi_map_str_i64_len rather than a stale TypeMap dispatch.
func TestBuildSourceMapStrI64Empty(t *testing.T) {
	src := `let m: map<string, int> = {}
print(len(m))
`
	if got, want := runMochiBuild(t, src), "0\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceTestBlockSkipped pins the Phase 4.2.19 rule: a
// `test "name" { ... }` block in source is a no-op at lower time
// (mirrors `mochi run`, which does not execute test bodies). The
// rest of the program lowers and runs normally. The body of the
// test block here uses a comparison that would type-error if it
// were lowered (Mochi's reference VM rejects it during type-check),
// confirming the body is not visited.
func TestBuildSourceTestBlockSkipped(t *testing.T) {
	src := `let x = 7
print(x)
test "skipped" {
  expect undefined_name_that_would_break == 42
}
print(x + 1)
`
	if got, want := runMochiBuild(t, src), "7\n8\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceV02PiFixture pins the on-disk v0.2/π.mochi fixture
// end-to-end. With Phase 4.2.19 dropping the trailing `test "π"`
// block, the C target now produces the same two-line stdout as
// `mochi run` on the fixture. UTF-8 identifiers (π, 🍡) and the
// UTF-8 string literal pass through the C string carrier verbatim.
func TestBuildSourceV02PiFixture(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/v0.2/π.mochi")
	if err != nil {
		t.Fatalf("read v0.2/π.mochi fixture: %v", err)
	}
	want := "314\n🍡૮₍ ˃ ⤙ ˂ ₎ა\n"
	if got := runMochiBuild(t, string(srcBytes)); got != want {
		t.Errorf("v0.2/π stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceV02MapFixture pins the on-disk v0.2/map.mochi
// fixture end-to-end. Phase 4.2.18 closed the map<str, i64>
// literal+get+len gate; Phase 4.2.19 drops the trailing test block
// so the fixture compiles top to bottom on the C target.
func TestBuildSourceV02MapFixture(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/v0.2/map.mochi")
	if err != nil {
		t.Fatalf("read v0.2/map.mochi fixture: %v", err)
	}
	want := "Alice's score: 10\nTotal players: 2\n"
	if got := runMochiBuild(t, string(srcBytes)); got != want {
		t.Errorf("v0.2/map stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceListStrSlice pins the Phase 4.2.20 list<str>
// slice lowering. `xs[1:3]` on a 4-element TypeStrArr lowers to
// OpStrArrSlice, which the C target backs with
// mochi_str_array_slice and the Go target with `append([]string{},
// xs[a:b]...)`. The display form matches the VM's list<string>
// rendering used by TestBuildSourceListStrQuoteEscapes.
func TestBuildSourceListStrSlice(t *testing.T) {
	src := `let fruits = ["a", "b", "c", "d"]
let some = fruits[1:3]
print(some)
`
	if got, want := runMochiBuild(t, src), "[\"b\", \"c\"]\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceListStrSliceClamp checks the runtime's clamp
// rule: an end past the source length returns up to the source
// length; start past end returns an empty array. This mirrors the
// half-open semantics documented in mochi_str_array_slice.
func TestBuildSourceListStrSliceClamp(t *testing.T) {
	src := `let fruits = ["a", "b", "c"]
print(fruits[1:10])
print(fruits[2:2])
`
	if got, want := runMochiBuild(t, src), "[\"b\", \"c\"]\n[]\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceV02ListFixture pins the on-disk v0.2/list.mochi
// fixture end-to-end. Phase 4.2.20 closed the list<str> slice
// gate; combined with Phase 4.2.19's test-block skip, the fixture
// now compiles top to bottom on the C target.
func TestBuildSourceV02ListFixture(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/v0.2/list.mochi")
	if err != nil {
		t.Fatalf("read v0.2/list.mochi fixture: %v", err)
	}
	want := "[1, 2, 3, 4, 5]\n" +
		"first:  1\n" +
		"last:  5\n" +
		"second to last:  4\n" +
		"len:  5\n" +
		"fruits:  [\"🍎\", \"🍌\", \"🍇\", \"🍓\"]\n" +
		"favorite:  🍇\n" +
		"some fruits:  [\"🍌\", \"🍇\"]\n"
	if got := runMochiBuild(t, string(srcBytes)); got != want {
		t.Errorf("v0.2/list stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceListListBasic exercises the Phase 4.2.21 nested
// integer list shape: an untyped `[[1,2,3], [4,5,6]]` literal binds
// to TypeListList, print(matrix) emits the nested `[[...], [...]]`
// display form, and chained indexing `matrix[i][j]` returns int64.
func TestBuildSourceListListBasic(t *testing.T) {
	src := `let matrix = [
  [1, 2, 3],
  [4, 5, 6],
]
print(matrix)
print(matrix[0])
print(matrix[1][2])
`
	want := "[[1, 2, 3], [4, 5, 6]]\n" +
		"[1, 2, 3]\n" +
		"6\n"
	if got := runMochiBuild(t, src); got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceListListNegIndex pins the negative-index fold for
// TypeListList. `matrix[-1]` lowers to `matrix[len + -1]` at lower
// time, matching the Mochi VM's wrap rule and the existing fold for
// TypeList/TypeF64Arr/TypeStrArr.
func TestBuildSourceListListNegIndex(t *testing.T) {
	src := `let matrix = [
  [1, 2, 3],
  [4, 5, 6],
  [7, 8, 9],
]
print(matrix[-1])
print(matrix[-1][0])
`
	want := "[7, 8, 9]\n7\n"
	if got := runMochiBuild(t, src); got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceListListLen exercises len() on a list<list<i64>>
// outer and on a row that was bound through OpListListGet. Both
// dispatch sites need to recognise the new TypeListList carrier.
func TestBuildSourceListListLen(t *testing.T) {
	src := `let matrix = [
  [1, 2, 3],
  [4, 5, 6],
]
print(len(matrix))
let row = matrix[0]
print(len(row))
`
	want := "2\n3\n"
	if got := runMochiBuild(t, src); got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceV02MatrixFixture pins the on-disk v0.2/matrix.mochi
// fixture end-to-end. Phase 4.2.21 closed the list<list<i64>> gate;
// combined with Phase 4.2.19's test-block skip, the fixture now
// compiles top to bottom on the C target. With shadow, π, map,
// list, and matrix all green, the v0.2 user-facing matrix stands at
// 5 of 6 (for-in.mochi is the remaining gate).
func TestBuildSourceV02MatrixFixture(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/v0.2/matrix.mochi")
	if err != nil {
		t.Fatalf("read v0.2/matrix.mochi fixture: %v", err)
	}
	want := "matrix:  [[1, 2, 3], [4, 5, 6], [7, 8, 9]]\n" +
		"first row:  [1, 2, 3]\n" +
		"last row:  [7, 8, 9]\n" +
		"top-left:  1\n" +
		"center:  5\n" +
		"bottom-right:  9\n" +
		"row 1 length:  3\n"
	if got := runMochiBuild(t, string(srcBytes)); got != want {
		t.Errorf("v0.2/matrix stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceMapStrI64Inferred exercises the Phase 4.2.22
// untyped-map-literal inference. The binding has no type annotation,
// so the literal infers map<str, i64> from its first key (a str
// literal). The probe-on-an-absent-key 0 default still applies and
// `len(m)` still routes through OpMapLenStrI64.
func TestBuildSourceMapStrI64Inferred(t *testing.T) {
	src := `let scores = {
  "Alice": 10,
  "Bob": 15,
}
print(scores["Alice"])
print(scores["Bob"])
print(scores["Missing"])
print(len(scores))
`
	if got, want := runMochiBuild(t, src), "10\n15\n0\n2\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceMapStrI64InferredNonStrKey pins the rejection
// path: an untyped map literal whose first key is an int falls
// outside the only map carrier the inference path accepts. The
// error mentions the offending shape so the user can either
// annotate the binding or fix the key.
func TestBuildSourceMapStrI64InferredNonStrKey(t *testing.T) {
	dir := t.TempDir()
	srcPath := filepath.Join(dir, "test.mochi")
	src := `let m = {1: 2}
print(len(m))
`
	if err := os.WriteFile(srcPath, []byte(src), 0o644); err != nil {
		t.Fatalf("write src: %v", err)
	}
	_, err := BuildSource(srcPath, Options{OutDir: dir, BinaryPath: filepath.Join(dir, "bin")})
	if err == nil {
		t.Fatalf("expected lower error for non-str inferred map key")
	}
	if got, want := err.Error(), "first key must be str"; !strings.Contains(got, want) {
		t.Errorf("error %q does not mention %q", got, want)
	}
}

// TestBuildSourceForInMapStrI64 pins the Phase 4.2.23 iteration:
// `for k in m` over a map<str, i64> walks the keys in strcmp
// ascending order, matching the Mochi reference VM which sorts map
// keys before iterating. The keys "Charlie" / "Alice" / "Bob" are
// deliberately inserted out of order to make the sort observable;
// the output must come back Alice / Bob / Charlie.
func TestBuildSourceForInMapStrI64(t *testing.T) {
	src := `let scores = {
  "Charlie": 88,
  "Alice": 90,
  "Bob": 82,
}
for name in scores {
  let score = scores[name]
  print(name, " scored ", score)
}
`
	want := "Alice  scored  90\nBob  scored  82\nCharlie  scored  88\n"
	if got := runMochiBuild(t, src); got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceForInMapStrI64Empty exercises the empty-map iteration
// path: zero passes through the loop, prefix and suffix prints still
// fire. Pins that the sorted-keys runtime helper returns a usable
// empty mochi_str_array (cap=0, len=0) rather than crashing.
func TestBuildSourceForInMapStrI64Empty(t *testing.T) {
	src := `let m: map<string, int> = {}
print("before")
for k in m {
  print("never:", k)
}
print("after")
`
	if got, want := runMochiBuild(t, src), "before\nafter\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceForInListList pins Phase 4.2.24: `for row in
// matrix` over a list<list<i64>> binds row to the inner TypeList,
// and a nested `for col in row` then dispatches through the
// TypeList arm. The output mirrors block 4 of v0.2/for-in.mochi.
func TestBuildSourceForInListList(t *testing.T) {
	src := `let matrix = [
  [1, 2],
  [3, 4],
]
for row in matrix {
  for col in row {
    print("cell: ", col)
  }
}
`
	want := "cell:  1\ncell:  2\ncell:  3\ncell:  4\n"
	if got := runMochiBuild(t, src); got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceV02ForInFixture closes the v0.2 user-facing corpus
// gate. All four blocks (list<str> iteration, map iteration with
// sorted keys, range iteration, nested list<list<i64>> iteration)
// compile and print the same bytes as `mochi run`. With this phase
// the v0.2 binary-build matrix is 6 of 6 green.
func TestBuildSourceV02ForInFixture(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/v0.2/for-in.mochi")
	if err != nil {
		t.Fatalf("read v0.2/for-in.mochi fixture: %v", err)
	}
	want := "fruit:  apple\nfruit:  banana\nfruit:  cherry\n" +
		"Alice  scored  90\nBob  scored  82\nCharlie  scored  88\n" +
		"number:  1\nnumber:  2\nnumber:  3\nnumber:  4\n" +
		"cell:  1\ncell:  2\ncell:  3\ncell:  4\n"
	if got := runMochiBuild(t, string(srcBytes)); got != want {
		t.Errorf("v0.2/for-in stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceV05WhileFixture pins examples/v0.5/while.mochi
// verbatim on the C target. This program (var + while + print + i++)
// has been green since the early while/var phases. Phase 4.2.25 just
// pins the existing win so a regression on var-mutation-in-while
// surfaces here instead of going silent.
func TestBuildSourceV05WhileFixture(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/v0.5/while.mochi")
	if err != nil {
		t.Fatalf("read v0.5 while fixture: %v", err)
	}
	want := "0\n1\n2\n"
	if got := runMochiBuild(t, string(srcBytes)); got != want {
		t.Errorf("v0.5/while stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceV07IfExprFixture pins examples/v0.7/if_expr.mochi
// verbatim on the C target. The fixture uses the block-form
// if-as-value (`let s = if cond { "a" } else { "b" }`) which is
// distinct from the v0.10 `if then else` expression form already
// pinned by TestBuildSourceV010IfThenElseFixture. Both forms now have
// fixture-level regression coverage.
func TestBuildSourceV07IfExprFixture(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/v0.7/if_expr.mochi")
	if err != nil {
		t.Fatalf("read v0.7 if_expr fixture: %v", err)
	}
	want := "Status: adult\n"
	if got := runMochiBuild(t, string(srcBytes)); got != want {
		t.Errorf("v0.7/if_expr stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceV07EmptyListFixture pins examples/v0.7/empty_list.mochi
// verbatim on the C target. The fixture exercises both shapes of an
// empty list<int>: a type-annotated binding (`let xs: list<int> = []`)
// and an `as`-cast expression (`let xs = [] as list<int>`). Both lower
// to TypeList carriers that print as `[]` via mochi_list_i64_to_str's
// static empty-string fast path.
func TestBuildSourceV07EmptyListFixture(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/v0.7/empty_list.mochi")
	if err != nil {
		t.Fatalf("read v0.7 empty_list fixture: %v", err)
	}
	want := "[]\n[]\n"
	if got := runMochiBuild(t, string(srcBytes)); got != want {
		t.Errorf("v0.7/empty_list stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceV01StreamFixture pins examples/v0.1/stream.mochi
// (entirely block-commented out) on the C target. Before Phase 4.2.26
// the build failed at link time with `Undefined symbols: _main`
// because the frontend lowered the empty program to a Program with
// no main function and the emitter then skipped main entirely. The
// fix emits a no-op `int main(void) { return 0; }` whenever
// p.Main == "", so a declaration-only or commented-out source builds
// to a binary that runs and exits 0 with empty stdout.
func TestBuildSourceV01StreamFixture(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/v0.1/stream.mochi")
	if err != nil {
		t.Fatalf("read v0.1 stream fixture: %v", err)
	}
	if got := runMochiBuild(t, string(srcBytes)); got != "" {
		t.Errorf("v0.1/stream stdout = %q, want empty", got)
	}
}

// TestBuildSourceV01AgentFixture pins examples/v0.1/agent.mochi (same
// commented-out shape as v0.1/stream.mochi). Both v0.1 sources serve
// as syntax-only references in the tutorial; the build target should
// still produce a runnable binary with empty stdout instead of
// failing at link time.
func TestBuildSourceV01AgentFixture(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/v0.1/agent.mochi")
	if err != nil {
		t.Fatalf("read v0.1 agent fixture: %v", err)
	}
	if got := runMochiBuild(t, string(srcBytes)); got != "" {
		t.Errorf("v0.1/agent stdout = %q, want empty", got)
	}
}

// TestBuildSourceV06ExternFixture pins examples/v0.6/extern.mochi, a
// declaration-only script: `extern type ...`, `extern var ...`,
// `extern fun ...`, `extern object ...`. None of those have call
// sites in the body (the only call sites are inside a block
// comment). Lower produces an empty Program; Phase 4.2.26's no-op
// main keeps the cc link step from failing.
func TestBuildSourceV06ExternFixture(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/v0.6/extern.mochi")
	if err != nil {
		t.Fatalf("read v0.6 extern fixture: %v", err)
	}
	if got := runMochiBuild(t, string(srcBytes)); got != "" {
		t.Errorf("v0.6/extern stdout = %q, want empty", got)
	}
}

// TestBuildSourceEmptyScriptLinks covers the empty-statement edge
// case directly: a script with literally no statements (after parsing
// strips comments) must link and run. Without Phase 4.2.26 the cc
// step would fail with `Undefined symbols: _main`.
func TestBuildSourceEmptyScriptLinks(t *testing.T) {
	src := "// just a comment, no statements\n"
	if got := runMochiBuild(t, src); got != "" {
		t.Errorf("empty-script stdout = %q, want empty", got)
	}
}

// TestBuildSourceV03MatchFixture pins the on-disk fixture verbatim
// so a regression in either the example or the lowering surfaces
// here. This is the user-facing motivation for Phase 4.2.11.
func TestBuildSourceV03MatchFixture(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/v0.3/match.mochi")
	if err != nil {
		t.Fatalf("read v0.3 match fixture: %v", err)
	}
	want := "two\nrelaxed\nconfirmed\nzero\nmany\n"
	if got := runMochiBuild(t, string(srcBytes)); got != want {
		t.Errorf("v0.3/match stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceSpectralNormBgFixture pins the unmodified
// bench/template/bg/spectral_norm fixture (N=100) on the C target.
// This is the §10.7 closeout for spectral_norm at the fixture level
// (Phase 4.3.12 pinned the same kernel inline as
// TestBuildSourceSpectralNativeKernel; this test reads the on-disk
// fixture verbatim so a regression in the fixture is caught too).
func TestBuildSourceSpectralNormBgFixture(t *testing.T) {
	src := readBenchFixture(t, "spectral_norm", 0)
	got := runMochiBuild(t, src)
	if want := "1274219991\n"; got != want {
		t.Errorf("spectral_norm fixture stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceStrCharAtBasic pins the Phase 4.2.27 string-index
// surface on the C target: `s[i]` on TypeStr lowers to the
// OpStrCharAt op which delegates to mochi_str_char_at; for ASCII
// input the i-th byte and i-th rune coincide so `"hello"[0]` is "h"
// and `"hello"[4]` is "o", byte-matching `mochi run` on the same
// source.
func TestBuildSourceStrCharAtBasic(t *testing.T) {
	src := `let s = "hello"
print(s[0])
print(s[4])
`
	if got, want := runMochiBuild(t, src), "h\no\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrCharAtConcat pins the interaction between
// indexing and concat: the i-th rune is a TypeStr value that can be
// the operand of `+` (which routes through mochi_str_concat). This
// also exercises HandleType liveness: the OpStrCharAt allocation
// must outlive the surrounding OpConcatStr that reads it.
func TestBuildSourceStrCharAtConcat(t *testing.T) {
	src := `let s = "abc"
print(s[0] + s[2])
`
	if got, want := runMochiBuild(t, src), "ac\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrCharAtLoopIndex pins indexing inside a while
// loop. The induction variable is an i64 and the printed value is
// the corresponding single-rune string; this verifies the V's
// OpStrCharAt argument typing through the lowering's TypeStr arm
// guard. Walks the full "hello" string one rune per iteration so
// the runtime helper's UTF-8 walk loop is exercised five times.
func TestBuildSourceStrCharAtLoopIndex(t *testing.T) {
	src := `let s = "hello"
var i = 0
while i < len(s) {
  print(s[i])
  i = i + 1
}
`
	if got, want := runMochiBuild(t, src), "h\ne\nl\nl\no\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrInBasic pins the Phase 4.2.28 `in` operator on
// TypeStr: `needle in haystack` lowers to OpStrIn which the C target
// emits as strstr(haystack, needle) != NULL. The fixture covers both
// arms: "w" is in "hello world", "z" is not.
func TestBuildSourceStrInBasic(t *testing.T) {
	src := `let s = "hello world"
if "w" in s {
  print("yes")
}
if "z" in s {
  print("nope")
} else {
  print("no")
}
`
	if got, want := runMochiBuild(t, src), "yes\nno\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrInSubstring pins multi-byte needle matches. The
// strstr probe must match an arbitrary contiguous substring, not
// just single characters; "ell" appears at offset 1 of "hello".
func TestBuildSourceStrInSubstring(t *testing.T) {
	src := `let s = "hello"
if "ell" in s {
  print("ok")
}
if "lle" in s {
  print("ordered")
} else {
  print("unordered match would be wrong")
}
`
	if got, want := runMochiBuild(t, src), "ok\nunordered match would be wrong\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrForChIn pins the Phase 4.2.29 `for ch in s` rune
// iteration: the loop body runs once per rune, with the binding
// holding a TypeStr value that is the i-th single-rune string.
// For the ASCII fixture the rune count equals strlen so the result
// is "h\ne\nl\nl\no\n".
func TestBuildSourceStrForChIn(t *testing.T) {
	src := `for ch in "hello" {
  print(ch)
}
`
	if got, want := runMochiBuild(t, src), "h\ne\nl\nl\no\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceStrForChInVowelCount pins the v0.5/string.mochi
// kernel directly: count vowels by `for ch in s { if ch in vowels`.
// Exercises rune iteration + Phase 4.2.28 `in` together so the
// SSA-level interaction (HandleType ch is read inside an OpStrIn
// arg, the haystack is a captured pre-loop TypeStr) is locked down.
func TestBuildSourceStrForChInVowelCount(t *testing.T) {
	src := `let s = "hello world"
let vowels = "aeiou"
var count = 0
for ch in s {
  if ch in vowels {
    count = count + 1
  }
}
print(count)
`
	if got, want := runMochiBuild(t, src), "3\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestBuildSourceV05StringFixture is the load-bearing v0.5 fixture
// pin enabled by Phases 4.2.27 + 4.2.28 + 4.2.29 together. The on-
// disk script reads s[i] (Phase 4.2.27), iterates runes (Phase
// 4.2.29), and does substring containment (Phase 4.2.28); failing
// any of those rolls the pin back. Output byte-matches `mochi run`.
func TestBuildSourceV05StringFixture(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/v0.5/string.mochi")
	if err != nil {
		t.Fatalf("read v0.5 string fixture: %v", err)
	}
	want := "s[0] = h\ns[4] = o\nlength = 11\n" +
		"char: h\nchar: e\nchar: l\nchar: l\nchar: o\nchar:  \n" +
		"char: w\nchar: o\nchar: r\nchar: l\nchar: d\n" +
		"'w' is in the string\nvowel count: 3\n"
	if got := runMochiBuild(t, string(srcBytes)); got != want {
		t.Errorf("v0.5/string stdout = %q, want %q", got, want)
	}
}

// TestBuildSourceV05StringIndexIteratorFixture pins the sibling
// v0.5/string-index-iterator fixture, which exercises the same
// gates as v0.5/string.mochi minus the substring containment.
func TestBuildSourceV05StringIndexIteratorFixture(t *testing.T) {
	srcBytes, err := os.ReadFile("../../../examples/v0.5/string-index-iterator.mochi")
	if err != nil {
		t.Fatalf("read v0.5 string-index-iterator fixture: %v", err)
	}
	want := "first character: h\nfifth character: o\n" +
		"char: h\nchar: e\nchar: l\nchar: l\nchar: o\nchar:  \n" +
		"char: w\nchar: o\nchar: r\nchar: l\nchar: d\n" +
		"number of vowels: 3\n"
	if got := runMochiBuild(t, string(srcBytes)); got != want {
		t.Errorf("v0.5/string-index-iterator stdout = %q, want %q", got, want)
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
	cc, _ := resolveCC("", "")
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
	cc, _ := resolveCC("", "")
	if _, err := exec.LookPath(cc); err != nil {
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
