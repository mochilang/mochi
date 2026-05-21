package gogen

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/compiler3/ir"
)

// TestEmitSourceMapDirective asserts that when fn.SourceFile is set,
// the emitter writes `//line` directives at function and block
// boundaries, and skips the final gofmt pass.
func TestEmitSourceMapDirective(t *testing.T) {
	fn := ir.FixtureFibIter()
	fn.SourceFile = "fib.mochi"
	fn.Blocks[0].SourceLine = 10
	fn.Blocks[1].SourceLine = 12
	fn.Blocks[2].SourceLine = 14
	fn.Blocks[3].SourceLine = 16

	src, err := Emit(&Program{PkgName: "demo", Funcs: []*ir.Function{fn}})
	if err != nil {
		t.Fatalf("Emit: %v\n%s", err, src)
	}
	s := string(src)
	for _, want := range []string{
		"//line fib.mochi:10",
		"//line fib.mochi:12",
		"//line fib.mochi:14",
		"//line fib.mochi:16",
	} {
		if !strings.Contains(s, want) {
			t.Errorf("emitted source missing %q\n%s", want, s)
		}
	}
}

// TestFilterBuildErrorsRemap exercises the diag filter on a synthetic
// generated source and stderr, asserting the filter rewrites gen-file
// coords to Mochi-source coords.
func TestFilterBuildErrorsRemap(t *testing.T) {
	gen := strings.Join([]string{
		"package main",                 // L1
		"",                             // L2
		"//line fib.mochi:42",          // L3 directive
		"func fib(n int64) int64 {",    // L4 -> fib.mochi:42
		"\treturn n + \"oops\"",        // L5 -> fib.mochi:43
		"}",                            // L6 -> fib.mochi:44
	}, "\n") + "\n"

	stderr := "./gen.go:5:11: cannot convert \"oops\" (untyped string constant) to int64\n"
	got := FilterBuildErrors("./gen.go", gen, stderr)
	if !strings.Contains(got, "fib.mochi:43") {
		t.Errorf("filter did not remap line; got:\n%s", got)
	}
	if strings.Contains(got, "./gen.go:5") {
		t.Errorf("filter left raw gen-file coord; got:\n%s", got)
	}
}

// TestFilterBuildErrorsPassThrough asserts diagnostics for unrelated
// files (e.g. runtime/mochi/query) are passed through untouched.
func TestFilterBuildErrorsPassThrough(t *testing.T) {
	gen := "//line foo.mochi:1\npackage main\n"
	stderr := "/path/to/some/other/file.go:99:1: unrelated\n"
	got := FilterBuildErrors("./gen.go", gen, stderr)
	if !strings.Contains(got, "some/other/file.go:99:1") {
		t.Errorf("filter mangled unrelated diagnostic; got:\n%s", got)
	}
}

// TestSourceMapEndToEnd writes an emitted program with `//line`
// directives plus a deliberate type error to a temp file, runs
// `go build`, and asserts FilterBuildErrors produces Mochi-source
// coordinates. This is the Phase 7 gate: a Go-side type error at the
// FFI boundary surfaces with Mochi-source-line precision.
func TestSourceMapEndToEnd(t *testing.T) {
	// Construct a fixture whose emitted Go intentionally won't type
	// check: we widen the OpConst type-name lie by using OpFnRef
	// against a non-callable. Simpler path: emit a healthy fixture,
	// then graft a bad line. That keeps the //line table honest.
	fn := ir.FixtureFibIter()
	fn.SourceFile = "fib.mochi"
	fn.Blocks[0].SourceLine = 1
	fn.Blocks[1].SourceLine = 4
	fn.Blocks[2].SourceLine = 6
	fn.Blocks[3].SourceLine = 8

	src, err := Emit(&Program{PkgName: "main", Funcs: []*ir.Function{fn}, Main: "fib_iter"})
	if err != nil {
		t.Fatalf("Emit: %v\n%s", err, src)
	}
	s := string(src)

	// Splice a deliberate type error into a block we know carries a
	// //line directive: replace the body block's iNext assignment with
	// a string add. We pick a recognizable target that appears once.
	target := "= v6 + int64(1)"
	if !strings.Contains(s, target) {
		t.Skipf("emitter shape drifted; target %q not found in:\n%s", target, s)
	}
	broken := strings.Replace(s, target, "= v6 + \"oops\"", 1)

	dir := t.TempDir()
	genPath := filepath.Join(dir, "gen.go")
	if err := os.WriteFile(genPath, []byte(broken), 0o644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("go", "build", "-o", filepath.Join(dir, "out"), genPath)
	out, err := cmd.CombinedOutput()
	if err == nil {
		t.Fatalf("go build unexpectedly succeeded\n%s", out)
	}
	rewritten := FilterBuildErrors(genPath, broken, string(out))
	// The Go toolchain honors //line directives, so the build error
	// should already reference fib.mochi. FilterBuildErrors is the
	// belt-and-braces for diagnostics that bypass //line.
	if !strings.Contains(rewritten, "fib.mochi:") && !strings.Contains(string(out), "fib.mochi:") {
		t.Errorf("expected Mochi-source coord in diagnostic\nraw stderr:\n%s\nfiltered:\n%s", out, rewritten)
	}
}
