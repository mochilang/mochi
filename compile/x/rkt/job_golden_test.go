package rktcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	rktcode "mochi/compile/x/rkt"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

// TestRacketCompiler_JOBQ1 compiles the JOB benchmark query 1 to Racket and
// verifies both the generated code and runtime output match the golden files.
func TestRacketCompiler_JOBQ1(t *testing.T) {
	if err := rktcode.EnsureRacket(); err != nil {
		t.Skipf("racket not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "job", "q1.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := rktcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	wantCode, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "job", "compiler", "rkt", "q1.rkt.out"))
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q1.rkt.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(wantCode))
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.rkt")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("racket", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("racket run error: %v\n%s", err, out)
	}
	gotOut := bytes.TrimSpace(out)
	wantOut, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "job", "compiler", "rkt", "q1.out"))
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for q1.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", gotOut, bytes.TrimSpace(wantOut))
	}
}
