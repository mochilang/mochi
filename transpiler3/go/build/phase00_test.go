package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/transpiler3/c/aotir"
	gemit "mochi/transpiler3/go/emit"
	"mochi/transpiler3/go/gotree"
	glower "mochi/transpiler3/go/lower"
)

// repoRoot walks up from cwd until it finds go.mod so tests
// resolve repo-relative paths regardless of `go test`'s cwd.
func repoRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			t.Fatalf("no go.mod found above %s", dir)
		}
		dir = parent
	}
}

// TestPhase0Skeleton is the MEP-54 Phase 0 gate. It verifies
// that the four shadow-AST / lower / emit / build packages
// compile and vet cleanly, that the Driver.Build stub returns
// the documented error, and that the runtime subpackages all
// import cleanly.
func TestPhase0Skeleton(t *testing.T) {
	root := repoRoot(t)

	for _, target := range []string{
		"./transpiler3/go/...",
	} {
		t.Run("build_"+strings.ReplaceAll(target, "/", "_"), func(t *testing.T) {
			cmd := exec.Command("go", "build", target)
			cmd.Dir = root
			var stderr bytes.Buffer
			cmd.Stderr = &stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("go build %s: %v\n%s", target, err, stderr.String())
			}
		})
		t.Run("vet_"+strings.ReplaceAll(target, "/", "_"), func(t *testing.T) {
			cmd := exec.Command("go", "vet", target)
			cmd.Dir = root
			var stderr bytes.Buffer
			cmd.Stderr = &stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("go vet %s: %v\n%s", target, err, stderr.String())
			}
		})
	}
}

// TestPhase0DriverRejectsEmptyPaths verifies Driver.Build
// rejects empty src / out arguments with a descriptive error
// rather than panicking on the upstream parser.
func TestPhase0DriverRejectsEmptyPaths(t *testing.T) {
	d := &Driver{}
	if err := d.Build("", "out", "", ""); err == nil {
		t.Fatalf("expected error for empty src, got nil")
	}
	if err := d.Build("src.mochi", "", "", ""); err == nil {
		t.Fatalf("expected error for empty out, got nil")
	}
}

// TestPhase0LowerEmpty verifies lower.Lower on a minimal empty
// aotir.Program returns a renderable gotree.File whose output
// is a valid `package main` file.
func TestPhase0LowerEmpty(t *testing.T) {
	prog := &aotir.Program{Functions: []*aotir.Function{
		{Name: "main", Body: &aotir.Block{}},
	}}
	f, err := glower.Lower(prog)
	if err != nil {
		t.Fatalf("Lower: %v", err)
	}
	src, err := f.Render()
	if err != nil {
		t.Fatalf("Render: %v", err)
	}
	if !strings.Contains(string(src), "package main") {
		t.Fatalf("missing package clause:\n%s", src)
	}
}

// TestPhase0EmitWritesFile verifies emit.Emit writes a file in
// outDir/fileName with gofmt-clean contents.
func TestPhase0EmitWritesFile(t *testing.T) {
	dir := t.TempDir()
	f := &gotree.File{PackageName: "main"}
	if err := gemit.Emit(f, dir, "main.go"); err != nil {
		t.Fatalf("Emit: %v", err)
	}
	got, err := os.ReadFile(filepath.Join(dir, "main.go"))
	if err != nil {
		t.Fatalf("read emitted file: %v", err)
	}
	if !strings.Contains(string(got), "package main") {
		t.Fatalf("emitted file missing package clause:\n%s", got)
	}
}
