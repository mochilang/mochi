//go:build slow

package stcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	stcode "mochi/compile/x/st"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestSTCompiler_JOBQ1(t *testing.T) {
	if err := stcode.EnsureSmalltalk(); err != nil {
		t.Skipf("smalltalk not installed: %v", err)
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
	code, err := stcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	codeWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "st", "q1.st.out")
	wantCode, err := os.ReadFile(codeWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q1.st.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(wantCode))
	}

	dir := t.TempDir()
	file := filepath.Join(dir, "main.st")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("gst", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("gst error: %v\n%s", err, out)
	}
	gotOut := bytes.TrimSpace(out)
	outWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "st", "q1.out")
	wantOut, err := os.ReadFile(outWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for q1.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", gotOut, bytes.TrimSpace(wantOut))
	}
}
