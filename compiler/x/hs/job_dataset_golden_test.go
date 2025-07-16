//go:build slow

package hscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	hscode "mochi/compiler/x/hs"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

// TestHSCompiler_JOB_Dataset_Golden compiles the JOB q1-q10 examples and
// verifies the generated Haskell code and program output.
func TestHSCompiler_JOB_Dataset_Golden(t *testing.T) {
	if err := hscode.EnsureHaskell(); err != nil {
		t.Skipf("haskell not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 10; i++ {
		base := fmt.Sprintf("q%d", i)
		t.Run(base, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "job", base+".mochi")
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := hscode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
                       dir := t.TempDir()
			file := filepath.Join(dir, "main.hs")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("runhaskell", file)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			wantOutPath := filepath.Join(root, "tests", "dataset", "job", "out", base+".out")
			wantOut, err := os.ReadFile(wantOutPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}
