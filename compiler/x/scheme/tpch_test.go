package schemecode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	schemecode "mochi/compiler/x/scheme"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestSchemeCompiler_TPCH(t *testing.T) {
	schemePath, err := schemecode.EnsureScheme()
	if err != nil {
		t.Skipf("scheme not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 3; i++ {
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
			code, err := schemecode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCodePath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "scheme", q+".scm")
			wantCode, err := os.ReadFile(wantCodePath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			got := stripHeader(bytes.TrimSpace(code))
			wantCodeNorm := stripHeader(bytes.TrimSpace(wantCode))
			if !bytes.Equal(got, wantCodeNorm) {
				t.Errorf("generated code mismatch for %s.scm\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, wantCodeNorm)
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.scm")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command(schemePath, "-m", "chibi", file)
			cmd.Dir = root
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Logf("scheme exited with error: %v", err)
			}
			gotOut := normalizeOut(bytes.TrimSpace(out))
			wantOutPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "scheme", q+".out")
			wantOut, err := os.ReadFile(wantOutPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			wantNormalized := normalizeOut(bytes.TrimSpace(wantOut))
			if !bytes.Equal(gotOut, wantNormalized) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, wantNormalized)
			}
		})
	}
}
