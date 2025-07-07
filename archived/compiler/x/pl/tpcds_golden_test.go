//go:build archived && slow

package plcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	plcode "mochi/archived/x/pl"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestPrologCompiler_TPCDS_Golden(t *testing.T) {
	if err := plcode.EnsureSWIPL(); err != nil {
		t.Skipf("swipl not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 3; i++ {
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
			code, err := plcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCodePath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "pl", q+".pl.out")
			wantCode, err := os.ReadFile(wantCodePath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.pl.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.pl")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			out, err := exec.Command("swipl", "-q", file).CombinedOutput()
			if err != nil {
				t.Fatalf("swipl error: %v\n%s", err, out)
			}
			wantOutPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "pl", q+".out")
			wantOut, err := os.ReadFile(wantOutPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			gotOut := bytes.TrimSpace(out)
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}
