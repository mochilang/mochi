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

func repoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found")
	return ""
}

func TestRBCompiler_TPCHQueries(t *testing.T) {
	if _, err := exec.LookPath("ruby"); err != nil {
		t.Skip("ruby not installed")
	}
	root := repoRoot(t)
	for i := 1; i <= 22; i++ {
		q := fmt.Sprintf("q%d", i)
		if q != "q1" && q != "q2" {
			t.Run(q, func(t *testing.T) {
				t.Skip("unsupported")
			})
			continue
		}
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
			code, err := rbcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "rb", q+".rb.out")
			wantCode, err := os.ReadFile(codeWantPath)
			if err == nil {
				if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
					t.Errorf("generated code mismatch for %s.rb.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
				}
			} else if !os.IsNotExist(err) {
				t.Fatalf("read golden: %v", err)
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.rb")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("ruby", file)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("ruby run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			outWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "rb", q+".out")
			wantOut, err := os.ReadFile(outWantPath)
			if err == nil {
				if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
					t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
				}
			} else if !os.IsNotExist(err) {
				t.Fatalf("read golden: %v", err)
			}
		})
	}
}
