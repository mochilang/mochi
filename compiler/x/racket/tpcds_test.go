//go:build slow

package racket_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	rack "mochi/compiler/x/racket"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestRacketCompiler_TPCDS_Golden(t *testing.T) {
	if err := rack.EnsureRacket(); err != nil {
		t.Skipf("racket not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 99; i++ {
		q := fmt.Sprintf("q%d", i)
		codePath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "rkt", q+".rkt")
		outPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "rkt", q+".out")
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
			code, err := rack.New().Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codePath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.rkt")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("racket", file)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("racket run error: %v\n%s", err, out)
			}
			gotRun := strings.TrimSpace(string(out))
			if strings.HasSuffix(gotRun, "ok") {
				gotRun = strings.TrimSpace(strings.TrimSuffix(gotRun, "ok"))
			}
			wantRunBytes, err := os.ReadFile(outPath)
			if err != nil {
				t.Fatalf("read output golden: %v", err)
			}
			wantRun := strings.TrimSpace(string(wantRunBytes))
			if gotRun != wantRun {
				t.Errorf("%s runtime mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotRun, wantRun)
			}
		})
	}
}
