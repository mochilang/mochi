package frontend

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	gogen "mochi/compiler3/emit/go"
	"mochi/parser"
)

// runEnd2End lowers src to Go via the frontend + emitter, writes it
// to a temp dir, and runs `go run`. Returns the program's stdout.
func runEnd2End(t *testing.T, src string) string {
	t.Helper()
	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	p, err := Lower(prog)
	if err != nil {
		t.Fatalf("lower: %v", err)
	}
	out, err := gogen.Emit(p)
	if err != nil {
		t.Fatalf("emit: %v\n%s", err, out)
	}
	dir := t.TempDir()
	path := filepath.Join(dir, "main.go")
	if err := os.WriteFile(path, out, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("go", "run", path)
	cmdOut, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("go run failed: %v\nsource:\n%s\noutput:\n%s", err, out, cmdOut)
	}
	return string(cmdOut)
}

func TestLowerLetAndPrint(t *testing.T) {
	src := `let a = 10
let b: int = 20
print(a + b)
`
	got := runEnd2End(t, src)
	if got != "30\n" {
		t.Errorf("got %q, want %q", got, "30\n")
	}
}

func TestLowerArithChain(t *testing.T) {
	src := `let a = 7
let b = 3
print((a + b) * 2)
`
	got := runEnd2End(t, src)
	if got != "20\n" {
		t.Errorf("got %q, want %q", got, "20\n")
	}
}

func TestLowerFunCall(t *testing.T) {
	src := `fun double(n: int): int {
  return n * 2
}
let x = 21
print(double(x))
`
	got := runEnd2End(t, src)
	if got != "42\n" {
		t.Errorf("got %q, want %q", got, "42\n")
	}
}

func TestLowerIfElse(t *testing.T) {
	src := `let n = 5
if n > 3 {
  print(1)
} else {
  print(0)
}
`
	got := runEnd2End(t, src)
	if !strings.HasPrefix(got, "1") {
		t.Errorf("got %q, want prefix %q", got, "1")
	}
}

func TestLowerWhileCountdown(t *testing.T) {
	src := `var n = 5
while n > 0 {
  print(n)
  n = n - 1
}
`
	got := runEnd2End(t, src)
	if got != "5\n4\n3\n2\n1\n" {
		t.Errorf("got %q, want %q", got, "5\n4\n3\n2\n1\n")
	}
}

func TestLowerWhileFibIter(t *testing.T) {
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
	got := runEnd2End(t, src)
	if got != "55\n" {
		t.Errorf("got %q, want %q", got, "55\n")
	}
}

func TestLowerWhileSkippedWhenFalse(t *testing.T) {
	src := `var n = 0
while n > 0 {
  print(999)
  n = n - 1
}
print(42)
`
	got := runEnd2End(t, src)
	if got != "42\n" {
		t.Errorf("got %q, want %q", got, "42\n")
	}
}

// TestLowerUnsupportedSurfacesError asserts that an unsupported form
// produces a frontend error rather than a silent miscompile. The A/B
// harness relies on this to mark the fixture skipped.
func TestLowerUnsupportedSurfacesError(t *testing.T) {
	src := `let s = "hi"
`
	prog, err := parser.Parser.ParseString("t.mochi", src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if _, err := Lower(prog); err == nil {
		t.Fatal("expected error for unsupported string literal in MVP, got nil")
	}
}

// TestLowerListAppendAndIndex pins the Phase 4.3.1 typed-i64-array
// surface: empty list literal, append, indexed read, len, and indexed
// write must all lower and produce the same output under both
// emitters. This test verifies the Go path; the C path is pinned by
// the matching TestBuildSourceListAppendAndIndex in build/c.
func TestLowerListAppendAndIndex(t *testing.T) {
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
	got := runEnd2End(t, src)
	if got != "155\n" {
		t.Errorf("got %q, want %q", got, "155\n")
	}
}

// TestLowerListLiteralWithElems exercises the non-empty list literal
// shape: `[1, 2, 3]` lowers to OpNewList followed by three pushes. The
// program reads back the third element to confirm push-then-get is a
// round trip.
func TestLowerListLiteralWithElems(t *testing.T) {
	src := `let xs: list<int> = [10, 20, 30]
print(xs[2])
`
	got := runEnd2End(t, src)
	if got != "30\n" {
		t.Errorf("got %q, want %q", got, "30\n")
	}
}

// TestLowerForRangeSum pins the Phase 4.3.2 range-for surface: a
// `for i in 1..(n+1)` loop with a mutable accumulator must lower to
// the same phi-at-header CFG shape as while, with the loop variable
// participating as one of the snapshotted bindings. Sum 1..10 = 55.
func TestLowerForRangeSum(t *testing.T) {
	src := `fun sumRange(n: int): int {
  var s = 0
  for i in 1..(n + 1) {
    s = s + i
  }
  return s
}
print(sumRange(10))
`
	got := runEnd2End(t, src)
	if got != "55\n" {
		t.Errorf("got %q, want %q", got, "55\n")
	}
}

// TestLowerForRangeUnderscore exercises the `_` loop variable: the
// body does not reference the index, but the loop still iterates the
// right number of times. After 5 iterations, len(xs) is 5.
func TestLowerForRangeUnderscore(t *testing.T) {
	src := `fun fillFive(): int {
  var xs: list<int> = []
  for _ in 0..5 {
    xs = append(xs, 0)
  }
  return len(xs)
}
print(fillFive())
`
	got := runEnd2End(t, src)
	if got != "5\n" {
		t.Errorf("got %q, want %q", got, "5\n")
	}
}

// TestLowerListFloatLiteralAndIndex pins the Phase 4.3.3 list<float>
// surface through the Go target: a non-empty literal lowers to
// OpNewF64Array plus three OpF64ArrayPushF64 ops, and `xs[i]` lowers
// to OpF64ArrayGetF64. The final print exercises the f64-to-string
// path that print.h already supports.
func TestLowerListFloatLiteralAndIndex(t *testing.T) {
	src := `let xs: list<float> = [1.5, 2.5, 3.5]
print(xs[1])
`
	got := runEnd2End(t, src)
	if got != "2.5\n" {
		t.Errorf("got %q, want %q", got, "2.5\n")
	}
}

// TestLowerListFloatAppendAndIndex pins the empty-literal + append +
// indexed read + indexed write + len cycle for list<float>, paralleling
// the i64 list test from Phase 4.3.1. The body initialises three slots
// to 0.0, sums them via len-bounded iteration, then writes back into
// xs[0] and reads it again to confirm the set round-trips.
func TestLowerListFloatAppendAndIndex(t *testing.T) {
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
	got := runEnd2End(t, src)
	if got != "102.5\n" {
		t.Errorf("got %q, want %q", got, "102.5\n")
	}
}

// TestLowerCastIntToFloatRoundTrip pins the Phase 4.3.4 `as` cast
// surface: an i64 widened to f64 and the result floored back through
// `as int`. The constant 7 round-trips through f64 arithmetic without
// loss, so the program prints 7 plus the original sum.
func TestLowerCastIntToFloatRoundTrip(t *testing.T) {
	src := `let n = 7
let f = (n as float) / 2.0
let back = (f * 2.0) as int
print(back + n)
`
	got := runEnd2End(t, src)
	if got != "14\n" {
		t.Errorf("got %q, want %q", got, "14\n")
	}
}

// TestLowerMandelbrotKernel is the load-bearing Phase 4.3.4 gate: a
// stripped mandelbrot kernel that exercises int<->float casts inside
// nested while loops with early return inside if. The Go and C targets
// both produce 4629 for a 16x16 grid with max_iter=50.
func TestLowerMandelbrotKernel(t *testing.T) {
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
	got := runEnd2End(t, src)
	if got != "4629\n" {
		t.Errorf("got %q, want %q", got, "4629\n")
	}
}

// TestLowerMathSqrtBuiltin pins the Phase 4.3.5 `math.sqrt(x)` builtin:
// an `import python "math" as math` + `extern fun math.sqrt(x: float):
// float` pair must be accepted as no-op declarations, and the call site
// must lower to OpSqrtF64. The expression `math.sqrt(2.0) * math.sqrt(2.0)`
// returns 2.0 (within FP rounding) cast to int 2.
func TestLowerMathSqrtBuiltin(t *testing.T) {
	src := `import python "math" as math
extern fun math.sqrt(x: float): float

let r = math.sqrt(2.0) * math.sqrt(2.0)
print(r as int)
`
	got := runEnd2End(t, src)
	if got != "2\n" {
		t.Errorf("got %q, want %q", got, "2\n")
	}
}

// TestLowerNbodyDistanceKernel pins a focused fragment of the n_body
// inner loop: compute the gravitational softening factor `1 / (d2 *
// sqrt(d2))` for a known 3-4-5 right triangle (d2 = 25, sqrt(25) = 5,
// 1/(25*5) = 1/125 = 0.008). Scaled by 1e9 and cast to int, the
// result is 8000000. This exercises the OpSqrtF64 op inside the same
// expression shape n_body uses to compute its softened distance.
func TestLowerNbodyDistanceKernel(t *testing.T) {
	src := `import python "math" as math
extern fun math.sqrt(x: float): float

let dx = 3.0
let dy = 4.0
let dz = 0.0
let d2 = dx * dx + dy * dy + dz * dz
let factor = 1.0 / (d2 * math.sqrt(d2))
print((factor * 1.0e9) as int)
`
	got := runEnd2End(t, src)
	if got != "8000000\n" {
		t.Errorf("got %q, want %q", got, "8000000\n")
	}
}

// TestLowerIntCallCastFromFloat pins the Phase 4.3.6 `int(x)` builtin
// against an f64 argument: 1.7 truncates to 1, byte-matching the C
// target's `(int64_t)1.7`. This is the surface spectral_norm uses
// (`int(math.sqrt(uv / vv) * 1e9)`).
func TestLowerIntCallCastFromFloat(t *testing.T) {
	src := `let x = 1.7
print(int(x))
`
	got := runEnd2End(t, src)
	if got != "1\n" {
		t.Errorf("got %q, want %q", got, "1\n")
	}
}

// TestLowerFloatCallCastFromInt pins the Phase 4.3.6 `float(x)` builtin
// against an i64 argument: 7 widens to 7.0, the f64 divide produces
// 3.5, and the result casts back to int 3.
func TestLowerFloatCallCastFromInt(t *testing.T) {
	src := `let n = 7
let half = float(n) / 2.0
print(int(half))
`
	got := runEnd2End(t, src)
	if got != "3\n" {
		t.Errorf("got %q, want %q", got, "3\n")
	}
}

// TestLowerSpectralEvalKernel is the load-bearing Phase 4.3.6 gate: a
// single eval of spectral_norm's `eval_a(i, j) = 1 / float(s*(s+1)/2 +
// i + 1)` matrix entry for i=0, j=0. The expected value is 1/1=1.0,
// scaled by 1e9 and cast back to int is 1000000000.
func TestLowerSpectralEvalKernel(t *testing.T) {
	src := `fun eval_a(i: int, j: int): float {
  let s = i + j
  return 1.0 / float(s * (s + 1) / 2 + i + 1)
}
print(int(eval_a(0, 0) * 1.0e9))
`
	got := runEnd2End(t, src)
	if got != "1000000000\n" {
		t.Errorf("got %q, want %q", got, "1000000000\n")
	}
}

// TestLowerNbodyFullKernel pins the Phase 4.3.10 milestone: the full
// benchmark-games n_body integration kernel (5 bodies, 10 steps,
// canonical Sun + Jupiter + Saturn + Uranus + Neptune initial
// conditions; momentum normalisation; pairwise gravity inner loop;
// position update outer loop; final energy * 1e9 truncated to int)
// now lowers through compiler3 and emits valid Go. The C-target
// mirror is `TestBuildSourceNbodyFullKernel` in
// compiler3/build/c/driver_test.go; both produce -169073021.
func TestLowerNbodyFullKernel(t *testing.T) {
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
	got := runEnd2End(t, src)
	if got != "-169073021\n" {
		t.Errorf("got %q, want %q", got, "-169073021\n")
	}
}

// TestLowerBracketListTypeFloat pins the Phase 4.3.11 bracketed
// list-type syntax through the Go target: `[float]` in a fun
// parameter is accepted as syntactic sugar for `list<float>` and
// lowers to the same TypeF64Arr backing.
func TestLowerBracketListTypeFloat(t *testing.T) {
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
	got := runEnd2End(t, src)
	if got != "6\n" {
		t.Errorf("got %q, want %q", got, "6\n")
	}
}

// TestLowerBracketListTypeInt mirrors the bracketed surface for the
// i64 backing: `[int]` lowers to TypeList.
func TestLowerBracketListTypeInt(t *testing.T) {
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
	got := runEnd2End(t, src)
	if got != "28\n" {
		t.Errorf("got %q, want %q", got, "28\n")
	}
}

// TestLowerSpectralFullKernel pins the Phase 4.3.11 milestone: the
// full benchmark-games spectral_norm kernel (N=10, 5 outer power-
// method iterations, eval_a Hilbert-like matrix entry, mul_av and
// mul_atv helper funs taking `[float]` parameters, final
// `int(sqrt(uv/vv) * 1e9) = 1271844019`) now lowers through
// compiler3 and emits valid Go. The C-target mirror is
// `TestBuildSourceSpectralFullKernel`.
func TestLowerSpectralFullKernel(t *testing.T) {
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
	got := runEnd2End(t, src)
	if got != "1271844019\n" {
		t.Errorf("got %q, want %q", got, "1271844019\n")
	}
}

// TestLowerListConcatI64 pins the Phase 4.3.12 i64 list concatenation
// surface: `xs + ys` on two `list<int>` operands returns a fresh
// list of i64 with the operands' elements in order.
func TestLowerListConcatI64(t *testing.T) {
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
	got := runEnd2End(t, src)
	if got != "63\n" {
		t.Errorf("got %q, want %q", got, "63\n")
	}
}

// TestLowerF64ArrayConcat pins the Phase 4.3.12 f64 list concatenation
// surface: `xs + ys` on two `[float]` operands. Output is the truncated
// sum 1+2+3+4+5+6 = 21.
func TestLowerF64ArrayConcat(t *testing.T) {
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
	got := runEnd2End(t, src)
	if got != "21\n" {
		t.Errorf("got %q, want %q", got, "21\n")
	}
}

// TestLowerSpectralNativeKernel pins the Phase 4.3.12 milestone: the
// native `bench/template/bg/spectral_norm/spectral_norm.mochi` shape
// (N=100, `[float]` parameters, `u + [1.0]` list-concat initialisation,
// final `int(sqrt(uv/vv) * 1e9) = 1274219991`) now lowers through
// compiler3 and emits valid Go. The C-target mirror is
// `TestBuildSourceSpectralNativeKernel`.
func TestLowerSpectralNativeKernel(t *testing.T) {
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
	got := runEnd2End(t, src)
	if got != "1274219991\n" {
		t.Errorf("got %q, want %q", got, "1274219991\n")
	}
}

// TestLowerNowBuiltin pins the Phase 4.3.13 `now()` builtin: lowers
// to OpNow with TypeI64 result; two back-to-back calls return a
// monotonically non-decreasing value (the second is >= the first).
// The test checks the ordering invariant rather than a fixed value
// because `now()` is wall-clock by design.
func TestLowerNowBuiltin(t *testing.T) {
	src := `let a = now()
let b = now()
if b >= a {
  print(1)
} else {
  print(0)
}
`
	got := runEnd2End(t, src)
	if got != "1\n" {
		t.Errorf("got %q, want %q", got, "1\n")
	}
}

// TestLowerNowDeltaArith pins that `now()` participates in normal i64
// arithmetic: a duration computed as `(now() - start) / 1000` is an
// i64 expression with no surprises in the lowerer.
func TestLowerNowDeltaArith(t *testing.T) {
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
	got := runEnd2End(t, src)
	if got != "499500\n" {
		t.Errorf("got %q, want %q", got, "499500\n")
	}
}

// TestLowerJsonI64Object pins the Phase 4.3.14 `json({...})` builtin:
// a string-keyed map literal with i64 values lowers to OpJsonI64Object
// and prints a single-line JSON object. This is the closing piece for
// `bench/template/bg/mandelbrot.mochi` running through the compiler3
// frontend without source modification.
func TestLowerJsonI64Object(t *testing.T) {
	src := `let duration = 42
let total = 17
json({
  "duration_us": duration,
  "output": total,
})
`
	got := runEnd2End(t, src)
	want := "{\"duration_us\":42,\"output\":17}\n"
	if got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestLowerJsonI64ObjectFromArith pins that the JSON values may be
// arbitrary i64 expressions (not just identifiers): the map values are
// SSA placeholders fed through OpJsonI64Object.Args, so `(now()-now())`
// + arithmetic + cast composition all reach the printer.
func TestLowerJsonI64ObjectFromArith(t *testing.T) {
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
	got := runEnd2End(t, src)
	want := "{\"duration_us\":90,\"output\":45}\n"
	if got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestLowerMathPiConst pins the Phase 4.3.9 `math.pi` constant read:
// `extern let math.pi: float` is accepted as a no-op binding; the
// selector read lowers to OpConst of TypeF64 with the math.Pi value.
// 4*pi*pi truncated to int gives 39 (= 4 * 3.14159^2 = 39.478).
func TestLowerMathPiConst(t *testing.T) {
	src := `import python "math" as math
extern let math.pi: float

let solar_mass = 4.0 * math.pi * math.pi
print(int(solar_mass))
`
	got := runEnd2End(t, src)
	if got != "39\n" {
		t.Errorf("got %q, want %q", got, "39\n")
	}
}

// TestLowerMathEConst pins the secondary `math.e` constant: e^2
// truncated to int gives 7 (= 2.71828^2 = 7.389).
func TestLowerMathEConst(t *testing.T) {
	src := `import python "math" as math
extern let math.e: float

print(int(math.e * math.e))
`
	got := runEnd2End(t, src)
	if got != "7\n" {
		t.Errorf("got %q, want %q", got, "7\n")
	}
}

// TestLowerListInferFloatElem pins the Phase 4.3.8 element-type
// inference: `var xs = [1.0, 2.0, 3.0]` (no type annotation) lowers
// to OpNewF64Array, not the default OpNewList, because the first
// element is an f64 literal. Indexed read returns 2.0; truncated to
// int gives 2.
func TestLowerListInferFloatElem(t *testing.T) {
	src := `var xs = [1.0, 2.0, 3.0]
print(int(xs[1]))
`
	got := runEnd2End(t, src)
	if got != "2\n" {
		t.Errorf("got %q, want %q", got, "2\n")
	}
}

// TestLowerListInferIntElem pins backward compat: an untyped int
// literal list still infers TypeI64 from the first element, matching
// the pre-Phase-4.3.8 default.
func TestLowerListInferIntElem(t *testing.T) {
	src := `var xs = [10, 20, 30]
print(xs[2])
`
	got := runEnd2End(t, src)
	if got != "30\n" {
		t.Errorf("got %q, want %q", got, "30\n")
	}
}

// TestLowerNbodyInitVectors pins a stripped n_body top-level shape: a
// var-bound float list at module scope, indexed reads inside a while
// loop. This is the gated form that Phase 4.3.8 inference unlocks; the
// full n_body fixture still needs the harness shape.
func TestLowerNbodyInitVectors(t *testing.T) {
	src := `var pos_x = [0.0, 4.84, 8.34, 12.89, 15.37]
var i = 0
var sum = 0.0
while i < 5 {
  sum = sum + pos_x[i]
  i = i + 1
}
print(int(sum))
`
	got := runEnd2End(t, src)
	if got != "41\n" {
		t.Errorf("got %q, want %q", got, "41\n")
	}
}

// TestLowerForInListI64 pins the Phase 4.3.7 collection-iter surface
// for list<int>: iterating `for x in xs` over a 3-element list and
// summing the values produces 60.
func TestLowerForInListI64(t *testing.T) {
	src := `let xs: list<int> = [10, 20, 30]
var s = 0
for x in xs {
  s = s + x
}
print(s)
`
	got := runEnd2End(t, src)
	if got != "60\n" {
		t.Errorf("got %q, want %q", got, "60\n")
	}
}

// TestLowerForInListF64 pins the Phase 4.3.7 collection-iter surface
// for list<float>: iterating a 3-element f64 list, summing, then
// truncating to int via the Phase 4.3.6 `int(...)` builtin gives 6
// (= 1.5 + 2.0 + 2.5 truncated).
func TestLowerForInListF64(t *testing.T) {
	src := `let xs: list<float> = [1.5, 2.0, 2.5]
var s = 0.0
for x in xs {
  s = s + x
}
print(int(s))
`
	got := runEnd2End(t, src)
	if got != "6\n" {
		t.Errorf("got %q, want %q", got, "6\n")
	}
}

// TestLowerForInListEmpty pins the zero-iteration shape: a `for x in xs`
// over an empty list runs the body 0 times, so the accumulator keeps
// its pre-loop value.
func TestLowerForInListEmpty(t *testing.T) {
	src := `var xs: list<int> = []
var s = 42
for x in xs {
  s = s + x
}
print(s)
`
	got := runEnd2End(t, src)
	if got != "42\n" {
		t.Errorf("got %q, want %q", got, "42\n")
	}
}

// TestLowerNsieve is the load-bearing Phase 4.3.2 gate: a stripped
// nsieve(100) returning the prime count (25). It exercises range-for
// with nested while, indexed reads/writes, len(), and the synthetic
// loop-variable increment all in one program. This is the program the
// benchmark games' nsieve fixture reduces to once the list element-
// type widening is removed.
// TestLowerMapI64I64Basic pins the Phase 4.3.15.2 map<int,int>
// surface end-to-end through the frontend. Confirms empty-literal
// `{}` lowers to OpNewMap under the type-annotated `var m: map<int,
// int> = {}`, that `m[k] = v` and `m[k]` lower to OpMapSetI64I64 /
// OpMapGetI64I64, and that a read of an absent key returns 0
// (matching Go's zero-default semantic).
func TestLowerMapI64I64Basic(t *testing.T) {
	src := `var m: map<int, int> = {}
m[7] = 11
m[8] = 22
print(m[7])
print(m[8])
print(m[999])
`
	got := runEnd2End(t, src)
	if want := "11\n22\n0\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

// TestLowerListAnyBasic pins the Phase 4.3.15.1 list<any> surface
// through the frontend. Confirms that `list<any>` lowers to a
// distinct IR type (TypeListAny), the `[]` and `[a, b]` literals
// route through OpNewListAny / OpListAnyPushAny, `len(t)` dispatches
// to OpListAnyLen, indexed read `t[i]` to OpListAnyGetAny, and the
// `as list<any>` cast collapses to a same-type no-op.
func TestLowerListAnyBasic(t *testing.T) {
	src := `fun leaf(): list<any> {
  return []
}

fun pair(a: list<any>, b: list<any>): list<any> {
  return [a, b]
}

let lf = leaf()
let pr = pair(lf, leaf())
print(len(lf))
print(len(pr))
print(len(pr[0] as list<any>))
`
	got := runEnd2End(t, src)
	if want := "0\n2\n0\n"; got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

func TestLowerNsieve(t *testing.T) {
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
	got := runEnd2End(t, src)
	if got != "25\n" {
		t.Errorf("got %q, want %q", got, "25\n")
	}
}
