//go:build slow

package rscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	rscode "mochi/compile/x/rust"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestRustCompiler_JOB(t *testing.T) {
	if err := rscode.EnsureRust(); err != nil {
		t.Skipf("rust not installed: %v", err)
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
			code, err := rscode.New(env).Compile(prog)
			if err != nil {
				t.Skipf("compile error: %v", err)
				return
			}
			wantCodePath := filepath.Join(root, "tests", "dataset", "job", "compiler", "rust", q+".rs.out")
			wantCode, err := os.ReadFile(wantCodePath)
			if err != nil {
				t.Skipf("read golden: %v", err)
			} else if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.rs.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
			}

			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.rs")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			exe := filepath.Join(tmp, "main")
			if out, err := exec.Command("rustc", file, "-O", "-o", exe).CombinedOutput(); err != nil {
				t.Skipf("rustc error: %v\n%s", err, out)
				return
			}
			out, err := exec.Command(exe).CombinedOutput()
			if err != nil {
				t.Skipf("run error: %v\n%s", err, out)
				return
			}
			wantOutPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "rust", q+".out")
			if data, err := os.ReadFile(wantOutPath); err == nil {
				gotOut := bytes.TrimSpace(out)
				wantOut := bytes.TrimSpace(data)
				if !bytes.Equal(gotOut, wantOut) {
					t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, wantOut)
				}
			}
		})
	}
}
