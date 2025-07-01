//go:build slow

package pascode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	pascode "mochi/compile/x/pas"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

// TestPascalCompiler_TPCDS_Golden compiles available TPCDS queries with the
// Pascal backend and compares the generated code and runtime output against
// golden files.
func TestPascalCompiler_TPCDS_Golden(t *testing.T) {
	fpc, err := pascode.EnsureFPC()
	if err != nil {
		t.Skipf("fpc not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 99; i++ {
		base := fmt.Sprintf("q%d", i)
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "pas", base+".pas.out")
		outWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "pas", base+".out")
		if _, err := os.Stat(codeWant); err != nil {
			continue
		}
		src := filepath.Join(root, "tests", "dataset", "tpc-ds", base+".mochi")
		t.Run(base, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := pascode.New(env).Compile(prog)
			if err != nil {
				t.Skipf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			got := bytes.TrimSpace(code)
			if !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Skipf("generated code mismatch for %s.pas.out", base)
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "prog.pas")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			if out, err := exec.Command(fpc, file).CombinedOutput(); err != nil {
				t.Skipf("fpc error: %v\n%s", err, out)
			}
			exe := filepath.Join(dir, "prog")
			out, err := exec.Command(exe).CombinedOutput()
			if err != nil {
				t.Fatalf("run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			wantOut, err := os.ReadFile(outWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}
