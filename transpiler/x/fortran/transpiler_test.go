//go:build slow

package fortran_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
	ftn "mochi/transpiler/x/fortran"
	"mochi/types"
)

var update = flag.Bool("update", false, "update golden files")

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

func updateEnabled() bool {
	return *update
}

func transpileFile(src string) ([]byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return nil, err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, errs[0]
	}
	ast, err := ftn.Transpile(prog, env)
	if err != nil {
		return nil, err
	}
	return ast.Emit(), nil
}

func transpileAndRun(src string) ([]byte, error) {
	code, err := transpileFile(src)
	if err != nil {
		return nil, err
	}
	tmp, err := os.MkdirTemp("", "ftn")
	if err != nil {
		return nil, err
	}
	base := filepath.Base(src)
	f90Path := filepath.Join(tmp, base+".f90")
	exe := filepath.Join(tmp, base)
	if err := os.WriteFile(f90Path, code, 0o644); err != nil {
		return nil, err
	}
	if out, err := exec.Command("gfortran", f90Path, "-o", exe).CombinedOutput(); err != nil {
		return nil, fmt.Errorf("compile failed: %v: %s", err, string(out))
	}
	return exec.Command(exe).CombinedOutput()
}

func normalize(root string, b []byte) []byte {
	out := string(b)
	out = strings.ReplaceAll(out, filepath.ToSlash(root)+"/", "")
	out = strings.TrimSpace(out)
	if !strings.HasSuffix(out, "\n") {
		out += "\n"
	}
	return []byte(out)
}

func TestTranspilePrintHello(t *testing.T) {
	if _, err := exec.LookPath("gfortran"); err != nil {
		t.Skip("gfortran not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "fortran")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "print_hello.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ftn.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	f90Path := filepath.Join(outDir, "print_hello.f90")
	if err := os.WriteFile(f90Path, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	exe := filepath.Join(outDir, "print_hello")
	if out, err := exec.Command("gfortran", f90Path, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "print_hello.error"), out, 0o644)
		t.Fatalf("compile error: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		os.WriteFile(filepath.Join(outDir, "print_hello.error"), out, 0o644)
		t.Fatalf("run error: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "print_hello.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch: got %s want %s", got, want)
	}
}

func TestTranspileTypedLet(t *testing.T) {
	if _, err := exec.LookPath("gfortran"); err != nil {
		t.Skip("gfortran not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "fortran")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", "typed_let.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := ftn.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	f90Path := filepath.Join(outDir, "typed_let.f90")
	if err := os.WriteFile(f90Path, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	exe := filepath.Join(outDir, "typed_let")
	if out, err := exec.Command("gfortran", f90Path, "-o", exe).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, "typed_let.error"), out, 0o644)
		t.Fatalf("compile error: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		os.WriteFile(filepath.Join(outDir, "typed_let.error"), out, 0o644)
		t.Fatalf("run error: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, "typed_let.out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch: got %s want %s", got, want)
	}
}

func TestTranspilerGolden(t *testing.T) {
	if _, err := exec.LookPath("gfortran"); err != nil {
		t.Skip("gfortran not installed")
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "fortran")
	os.MkdirAll(outDir, 0o755)
       files := []string{
               filepath.Join(srcDir, "print_hello.mochi"),
               filepath.Join(srcDir, "typed_let.mochi"),
               filepath.Join(srcDir, "typed_var.mochi"),
               filepath.Join(srcDir, "var_assignment.mochi"),
               filepath.Join(srcDir, "basic_compare.mochi"),
               filepath.Join(srcDir, "math_ops.mochi"),
               filepath.Join(srcDir, "unary_neg.mochi"),
       }
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		wantOut := filepath.Join(outDir, name+".out")
		t.Run(name, func(t *testing.T) {
			code, err := transpileFile(src)
			if err != nil {
				t.Fatalf("transpile: %v", err)
			}
			if updateEnabled() {
				norm := normalize(root, code)
				if err := os.WriteFile(filepath.Join(outDir, name+".f90"), norm, 0o644); err != nil {
					t.Fatalf("write code: %v", err)
				}
			}
			out, runErr := transpileAndRun(src)
			if runErr != nil {
				if updateEnabled() {
					errPath := filepath.Join(outDir, name+".error")
					os.WriteFile(errPath, []byte(runErr.Error()+"\n"), 0o644)
				}
				t.Fatalf("run: %v", runErr)
			}
			if updateEnabled() {
				_ = os.Remove(filepath.Join(outDir, name+".error"))
				trimmed := bytes.TrimSpace(out)
				os.WriteFile(wantOut, trimmed, 0o644)
			}
			trimmed := bytes.TrimSpace(out)
			want, err := os.ReadFile(wantOut)
			if err != nil {
				t.Fatalf("read want: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(trimmed, want) {
				t.Errorf("output mismatch: got %s want %s", trimmed, want)
			}
		})
	}
}
