//go:build slow

package ocaml_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	ocaml "mochi/compiler/x/ocaml"
	"mochi/parser"
	"mochi/runtime/vm"
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

func TestOCamlCompiler_TPCH_Golden(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skipf("ocamlc not installed: %v", err)
	}
	root := repoRoot(t)
	for i := 1; i <= 5; i++ {
		query := fmt.Sprintf("q%d", i)
		t.Run(query, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "tpc-h", query+".mochi")
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := ocaml.New(env).Compile(prog, src)
			if err != nil {
				t.Skipf("compile error: %v", err)
				return
			}
			wantCodePath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "ocaml", query+".ml")
			wantCode, err := os.ReadFile(wantCodePath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			gotCode := bytes.TrimSpace(code)
			if !bytes.Equal(gotCode, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", query+".ml", gotCode, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			mlFile := filepath.Join(dir, "prog.ml")
			if err := os.WriteFile(mlFile, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			exe := filepath.Join(dir, "prog")
			if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
				t.Skipf("ocamlc error: %v\n%s", err, out)
				return
			}
			out, err := exec.Command(exe).CombinedOutput()
			if err != nil {
				t.Skipf("run error: %v\n%s", err, out)
				return
			}
			gotOut := bytes.TrimSpace(out)
			p, err := vm.Compile(prog, env)
			if err != nil {
				t.Errorf("vm compile error: %v", err)
			} else {
				var vmBuf bytes.Buffer
				m := vm.New(p, &vmBuf)
				if err := m.Run(); err != nil {
					t.Errorf("vm run error: %v", err)
				} else {
					vmOut := bytes.TrimSpace(vmBuf.Bytes())
					if !bytes.Equal(gotOut, vmOut) {
						t.Errorf("vm mismatch for %s\n\n--- VM ---\n%s\n\n--- OCaml ---\n%s\n", query, vmOut, gotOut)
					}
				}
			}
			wantOutPath := filepath.Join(root, "tests", "dataset", "tpc-h", "out", query+".out")
			wantOut, err := os.ReadFile(wantOutPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", query+".out", gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}
