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
