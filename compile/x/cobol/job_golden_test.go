//go:build slow

package cobolcode_test

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"

	cobolcode "mochi/compile/x/cobol"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCobolCompiler_JOBQ1_Golden(t *testing.T) {
	if err := cobolcode.EnsureCOBOL(); err != nil {
		t.Skipf("cobol not installed: %v", err)
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
	code, err := cobolcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	wantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "cobol", "q1.cob.out")
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(want)) {
		t.Errorf("generated code mismatch for q1.cob.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(want))
	}
}

func TestCobolCompiler_JOBQ2_Golden(t *testing.T) {
	if err := cobolcode.EnsureCOBOL(); err != nil {
		t.Skipf("cobol not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "job", "q2.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := cobolcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	wantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "cobol", "q2.cob.out")
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(want)) {
		t.Errorf("generated code mismatch for q2.cob.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(want))
	}
}
