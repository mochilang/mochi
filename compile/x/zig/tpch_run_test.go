//go:build slow

package zigcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/compile/x/testutil"
	zigcode "mochi/compile/x/zig"
	"mochi/parser"
	"mochi/types"
)

// TestZigCompiler_TPCH_Run compiles and executes TPCH query q1
// and compares the output with the VM golden file.
func TestZigCompiler_TPCH_Run(t *testing.T) {
	zigc, err := zigcode.EnsureZig()
	if err != nil {
		t.Skipf("zig not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	q := "q1"
	src := filepath.Join(root, "tests", "dataset", "tpc-h", q+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := zigcode.New(env).Compile(prog)
	if err != nil {
		t.Skipf("TPCH %s unsupported: %v", q, err)
	}
	tmp := t.TempDir()
	file := filepath.Join(tmp, "main.zig")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	exe := filepath.Join(tmp, "main")
	if out, err := exec.Command(zigc, "build-exe", file, "-O", "ReleaseSafe", "-femit-bin="+exe).CombinedOutput(); err != nil {
		t.Fatalf("zig build error: %v\n%s", err, out)
	}
	cmd := exec.Command(exe)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	got := bytes.TrimSpace(out)
	wantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "out", q+".out")
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(got, bytes.TrimSpace(want)) {
		t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(want))
	}
}
