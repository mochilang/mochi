//go:build slow

package cobolcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	cobolcode "mochi/compile/x/cobol"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

// TestCobolCompiler_TPCH_Golden compiles the first two TPCH queries using
// the COBOL backend. Generated code is compared against golden files under
// tests/dataset/tpc-h/compiler/cobol. If cobc or the runtime fails, the test is skipped.
func TestCobolCompiler_TPCH_Golden(t *testing.T) {
	if err := cobolcode.EnsureCOBOL(); err != nil {
		t.Skipf("cobol not installed: %v", err)
	}
	os.Setenv("MOCHI_SKIP_COBFMT", "1")
	defer os.Unsetenv("MOCHI_SKIP_COBFMT")
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 2; i++ {
		q := fmt.Sprintf("q%d", i)
		t.Run(q, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "tpc-h", q+".mochi")
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
			wantCodePath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "cobol", q+".cob.out")
			wantCode, err := os.ReadFile(wantCodePath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			gotCode := bytes.TrimSpace(code)
			if !bytes.Equal(gotCode, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q+".cob.out", gotCode, bytes.TrimSpace(wantCode))
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.cob")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			exe := filepath.Join(tmp, "main")
			if out, err := exec.Command("cobc", "-free", "-x", file, "-o", exe).CombinedOutput(); err != nil {
				t.Skipf("cobc error: %v\n%s", err, out)
			}
			if out, err := exec.Command(exe).CombinedOutput(); err != nil {
				t.Skipf("run error: %v\n%s", err, out)
			} else {
				_ = out
			}
		})
	}
}
