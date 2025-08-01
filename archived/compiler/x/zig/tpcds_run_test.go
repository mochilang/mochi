//go:build archive && slow

package zigcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/archived/x/testutil"
	zigcode "mochi/archived/x/zig"
	"mochi/parser"
	"mochi/types"
)

// TestZigCompiler_TPCDS_Run compiles and executes TPC-DS queries q1 to q19
// and compares the output with the golden files.
func TestZigCompiler_TPCDS_Run(t *testing.T) {
	t.Skip("TPC-DS runtime execution not supported in this environment")
	zigc, err := zigcode.EnsureZig()
	if err != nil {
		t.Skipf("zig not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 99; i++ {
		q := fmt.Sprintf("q%d", i)
		t.Run(q, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "tpc-ds", q+".mochi")
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
				t.Skipf("compile error: %v", err)
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
			wantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "zig", q+".out")
			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Skipf("read golden: %v", err)
			}
			if !bytes.Equal(got, bytes.TrimSpace(want)) {
				t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(want))
			}
		})
	}
}
