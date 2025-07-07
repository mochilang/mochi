//go:build archived && slow

package plcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	plcode "mochi/archived/x/pl"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

// TestPrologCompiler_JOB compiles JOB queries q1 through q10 using the Prolog
// backend. Generated code is compared with golden files and, when a runtime
// output is available, the compiled program is executed with SWI-Prolog and the
// result compared against the expected output.
func TestPrologCompiler_JOB(t *testing.T) {
	if err := plcode.EnsureSWIPL(); err != nil {
		t.Skipf("swipl not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 10; i++ {
		q := fmt.Sprintf("q%d", i)
		t.Run(q, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "job", q+".mochi")
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
			wantCodePath := filepath.Join(root, "tests", "dataset", "job", "compiler", "pl", q+".pl.out")
			wantCode, err := os.ReadFile(wantCodePath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			gotCode := bytes.TrimSpace(code)
			if !bytes.Equal(gotCode, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q+".pl.out", gotCode, bytes.TrimSpace(wantCode))
			}

			// Execute compiled code and compare output if a golden result exists.
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.pl")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("swipl", "-q", file)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("swipl error: %v\n%s", err, out)
			}
			wantOutPath := filepath.Join(root, "tests", "dataset", "job", "out", q+".out")
			if data, err := os.ReadFile(wantOutPath); err == nil {
				want := strings.TrimSpace(string(data))
				got := strings.TrimSpace(string(out))
				if got != want {
					t.Errorf("%s output mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, want)
				}
			}
		})
	}
}
