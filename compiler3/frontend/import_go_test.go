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

// runMochiInRepo lowers src to Go, writes the emitted source as a
// throwaway main.go inside the repo (so module-relative imports such
// as `mochi/runtime/ffi/...` resolve against the parent module),
// then runs `go run`. Returns stdout. The throwaway file is cleaned
// up via t.Cleanup so failed runs leave the tree clean.
func runMochiInRepo(t *testing.T, src string) string {
	t.Helper()
	prog, err := parser.Parser.ParseString("t.mochi", src)
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
	root := repoRoot(t)
	f, err := os.CreateTemp(filepath.Join(root, "compiler3", "frontend"), "importgo_*.go")
	if err != nil {
		t.Fatalf("create temp main: %v", err)
	}
	path := f.Name()
	if _, err := f.Write(out); err != nil {
		f.Close()
		t.Fatalf("write temp main: %v", err)
	}
	f.Close()
	t.Cleanup(func() { os.Remove(path) })
	cmd := exec.Command("go", "run", path)
	cmd.Dir = root
	cmdOut, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("go run failed: %v\nsource:\n%s\noutput:\n%s", err, out, cmdOut)
	}
	return string(cmdOut)
}

func repoRoot(t *testing.T) string {
	t.Helper()
	wd, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	// compiler3/frontend -> compiler3 -> repo root
	return filepath.Clean(filepath.Join(wd, "..", ".."))
}

// TestLowerImportGoEndToEnd exercises the full `import go "path" as
// alias` pipeline: parser -> resolver -> Lower -> emit -> `go run`.
// The fixture mirrors tests/vm/valid/go_auto.mochi; the expected
// stdout matches tests/vm/valid/go_auto.out. MEP-43 Phase 6
// full-gate closeout.
func TestLowerImportGoEndToEnd(t *testing.T) {
	src := `import go "mochi/runtime/ffi/go/testpkg" as testpkg auto

print(testpkg.Add(2,3))
print(testpkg.Pi)
print(testpkg.Answer)
`
	got := runMochiInRepo(t, src)
	want := "5\n3.14\n42\n"
	if got != want {
		t.Errorf("go_auto fixture stdout mismatch:\nwant: %q\ngot:  %q", want, got)
	}
}

// TestLowerImportGoSealHandles asserts `! meta` on an import-go
// statement propagates SealHandles=true to every binding emitted
// from that package. MEP-43 Phase 10 full-gate closeout.
func TestLowerImportGoSealHandles(t *testing.T) {
	src := `import go "mochi/runtime/ffi/go/testpkg" as testpkg auto ! meta

print(testpkg.Add(2,3))
`
	prog, err := parser.Parser.ParseString("t.mochi", src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	p, err := Lower(prog)
	if err != nil {
		t.Fatalf("lower: %v", err)
	}
	var found bool
	for _, fn := range p.Funcs {
		for _, b := range fn.GoBindings {
			if b.Pkg == "mochi/runtime/ffi/go/testpkg" && b.Name == "Add" {
				if !b.SealHandles {
					t.Errorf("testpkg.Add binding: SealHandles=false, want true (meta effect)")
				}
				found = true
			}
		}
	}
	if !found {
		t.Fatal("did not find testpkg.Add GoBinding in emitted IR")
	}
}

// TestLowerImportGoUnknownSymbol asserts the frontend reports a real
// error when an `import go` alias refers to a symbol the package does
// not export. The A/B harness should record a hard failure here, not
// a skip, since the import path itself resolved.
func TestLowerImportGoUnknownSymbol(t *testing.T) {
	src := `import go "mochi/runtime/ffi/go/testpkg" as testpkg auto

print(testpkg.Nope())
`
	prog, err := parser.Parser.ParseString("t.mochi", src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	_, err = Lower(prog)
	if err == nil || !strings.Contains(err.Error(), "Nope") {
		t.Errorf("expected lower error mentioning Nope; got %v", err)
	}
}
