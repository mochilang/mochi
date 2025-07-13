//go:build slow

package rbcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	rbcode "mochi/compiler/x/rb"
	"mochi/parser"
	"mochi/types"
)

func TestRBCompiler_JOBQueries(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	root := findRepoRoot(t)
	for i := 1; i <= 20; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "job", base+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "job", "compiler", "rb", base+".rb")
		outWant := filepath.Join(root, "tests", "dataset", "job", "compiler", "rb", base+".out")
		if _, err := os.Stat(codeWant); err != nil {
			continue
		}
		t.Run(base, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := rbcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			if shouldUpdate() {
				_ = os.WriteFile(codeWant, code, 0644)
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.rb")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("ruby", file)
			cmd.Dir = findRepoRoot(t)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("ruby run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			if shouldUpdate() {
				_ = os.WriteFile(outWant, append(gotOut, '\n'), 0644)
			} else {
				wantOut, err := os.ReadFile(outWant)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
					t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotOut, bytes.TrimSpace(wantOut))
				}
			}
		})
	}
}
