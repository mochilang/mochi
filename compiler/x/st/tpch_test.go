//go:build slow

package st_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	st "mochi/compiler/x/st"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func runTPCHQuery(t *testing.T, base string, gstPath string) {
	root := testutil.FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-h", base+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := st.New().Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	codeWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "st", base+".st.out")
	wantCode, err := os.ReadFile(codeWant)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for %s.st.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, got, bytes.TrimSpace(wantCode))
	}
	if gstPath == "" {
		t.Skip("gst not installed")
	}
	dir := t.TempDir()
	file := filepath.Join(dir, base+".st")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command(gstPath, file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("gst run error: %v\n%s", err, out)
	}
	gotOut := bytes.TrimSpace(out)
	outWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "st", base+".out")
	wantOut, err := os.ReadFile(outWant)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotOut, bytes.TrimSpace(wantOut))
	}
}

func TestSTCompiler_TPCHQueries(t *testing.T) {
	gstPath := ensureGST()
	for _, q := range []string{"q1", "q2"} {
		t.Run(q, func(t *testing.T) { runTPCHQuery(t, q, gstPath) })
	}
}
