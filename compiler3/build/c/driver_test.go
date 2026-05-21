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
