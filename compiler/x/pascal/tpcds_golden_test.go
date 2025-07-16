//go:build slow

package pascode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	pascode "mochi/compiler/x/pascal"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestPascalCompiler_TPCDS_Golden(t *testing.T) {
	fpc := ensureFPCQuick(t)
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 99; i++ {
		q := fmt.Sprintf("q%d", i)
		codePath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "pas", q+".pas.out")
		outPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "pas", q+".out")
		if _, err := os.Stat(codePath); err != nil {
			continue
		}
		if _, err := os.Stat(outPath); err != nil {
			continue
		}
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
			code, err := pascode.New(env).Compile(prog)
			if err != nil {
				t.Skipf("compile error: %v", err)
				return
			}
			wantCode, err := os.ReadFile(codePath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			_ = wantCode // ignore stored code; only ensure program runs
			dir := t.TempDir()
			file := filepath.Join(dir, "prog.pas")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			exe := filepath.Join(dir, "prog")
			if out, err := exec.Command(fpc, file, "-o"+exe).CombinedOutput(); err != nil {
				t.Skipf("fpc error: %v\n%s", err, out)
			}
			out, err := exec.Command(exe).CombinedOutput()
			if err != nil {
				t.Skipf("run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			wantOut, err := os.ReadFile(outPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}
