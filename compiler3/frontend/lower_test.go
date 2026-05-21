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
	prog, err := parser.Parser.ParseString("test.mochi", src)
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

// TestLowerNsieve is the load-bearing Phase 4.3.2 gate: a stripped
// nsieve(100) returning the prime count (25). It exercises range-for
// with nested while, indexed reads/writes, len(), and the synthetic
// loop-variable increment all in one program. This is the program the
// benchmark games' nsieve fixture reduces to once the list element-
// type widening is removed.
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
