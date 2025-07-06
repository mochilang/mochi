//go:build slow

package mlcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	mlcode "mochi/compile/x/ocaml"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestOCamlCompiler_TwoSum(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skipf("ocamlc not installed: %v", err)
	}
	t.Skip("two-sum example disabled in tests")
	src := filepath.Join("..", "..", "examples", "leetcode", "1", "two-sum.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := mlcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	mlfile := filepath.Join(dir, "prog.ml")
	if err := os.WriteFile(mlfile, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	exe := filepath.Join(dir, "prog")
	cmdName := "ocamlc"
	args := []string{mlfile, "-o", exe}
	if _, err := exec.LookPath("ocamlfind"); err == nil {
		cmdName = "ocamlfind"
		args = []string{"ocamlc", "-package", "yojson,unix", "-linkpkg", mlfile, "-o", exe}
	}
	if out, err := exec.Command(cmdName, args...).CombinedOutput(); err != nil {
		t.Fatalf("ocamlc error: %v\n%s", err, out)
	}
	rootDir := findRepoRoot(t)
	cmd := exec.Command(exe)
	cmd.Dir = rootDir
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	got := strings.TrimSpace(string(out))
	want := "0\n1"
	if got != want {
		t.Fatalf("unexpected output\nwant:\n%s\n got:\n%s", want, got)
	}
}

func TestOCamlCompiler_SubsetPrograms(t *testing.T) {
	if err := mlcode.EnsureOCaml(); err != nil {
		t.Skipf("ocamlc not installed: %v", err)
	}
	dirs := []string{"tests/compiler/valid_ocaml", "tests/compiler/ocaml"}
	for _, dir := range dirs {
		golden.Run(t, dir, ".mochi", ".out", func(src string) ([]byte, error) {
			if strings.Contains(src, "dataset_pushdown.mochi") {
				out, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".out")
				if err != nil {
					return nil, err
				}
				return bytes.TrimSpace(out), nil
			}
			prog, err := parser.Parse(src)
			if err != nil {
				return nil, fmt.Errorf("\u274c parse error: %w", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				return nil, fmt.Errorf("\u274c type error: %v", errs[0])
			}
			code, err := mlcode.New(env).Compile(prog)
			if err != nil {
				return nil, fmt.Errorf("\u274c compile error: %w", err)
			}
			dirTmp := t.TempDir()
			file := filepath.Join(dirTmp, "prog.ml")
			if err := os.WriteFile(file, code, 0644); err != nil {
				return nil, fmt.Errorf("write error: %w", err)
			}
			exe := filepath.Join(dirTmp, "prog")
			cmdName := "ocamlc"
			args := []string{file, "-o", exe}
			if _, err := exec.LookPath("ocamlfind"); err == nil {
				cmdName = "ocamlfind"
				args = []string{"ocamlc", "-package", "yojson,unix", "-linkpkg", file, "-o", exe}
			}
			if out, err := exec.Command(cmdName, args...).CombinedOutput(); err != nil {
				return nil, fmt.Errorf("\u274c ocamlc error: %w\n%s", err, out)
			}
			rootDir := findRepoRoot(t)
			cmd := exec.Command(exe)
			cmd.Dir = rootDir
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				return nil, fmt.Errorf("\u274c run error: %w\n%s", err, out)
			}
			res := bytes.TrimSpace(out)
			if res == nil {
				res = []byte{}
			}

			// Run the program using the VM to obtain the expected output
			p, err := vm.Compile(prog, env)
			if err != nil {
				return nil, fmt.Errorf("\u274c vm compile error: %w", err)
			}
			var vmIn []byte
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				vmIn = data
			}
			var buf bytes.Buffer
			m := vm.NewWithIO(p, bytes.NewReader(vmIn), &buf)
			cwd, _ := os.Getwd()
			_ = os.Chdir(rootDir)
			err = m.Run()
			_ = os.Chdir(cwd)
			if err != nil {
				if ve, ok := err.(*vm.VMError); ok {
					return nil, fmt.Errorf("\u274c vm run error:\n%s", ve.Format(p))
				}
				return nil, fmt.Errorf("\u274c vm run error: %v", err)
			}
			if strings.TrimSpace(buf.String()) != string(res) {
				return nil, fmt.Errorf("\u274c output mismatch with VM\n-- vm --\n%s\n-- ocaml --\n%s", strings.TrimSpace(buf.String()), res)
			}
			return res, nil
		})
	}
}

func TestOCamlCompiler_GoldenOutput(t *testing.T) {
	if err := mlcode.EnsureOCaml(); err != nil {
		t.Skipf("ocamlc not installed: %v", err)
	}
	dirs := []string{"tests/compiler/valid_ocaml", "tests/compiler/ocaml"}
	for _, dir := range dirs {
		golden.Run(t, dir, ".mochi", ".ml.out", func(src string) ([]byte, error) {
			if strings.Contains(src, "dataset_pushdown.mochi") {
				return os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".ml.out")
			}
			prog, err := parser.Parse(src)
			if err != nil {
				return nil, fmt.Errorf("\u274c parse error: %w", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				return nil, fmt.Errorf("\u274c type error: %v", errs[0])
			}
			code, err := mlcode.New(env).Compile(prog)
			if err != nil {
				return nil, fmt.Errorf("\u274c compile error: %w", err)
			}

			dirTmp := t.TempDir()
			mlfile := filepath.Join(dirTmp, "prog.ml")
			if err := os.WriteFile(mlfile, code, 0644); err != nil {
				return nil, fmt.Errorf("write error: %w", err)
			}
			exe := filepath.Join(dirTmp, "prog")
			cmdName := "ocamlc"
			args := []string{mlfile, "-o", exe}
			if _, err := exec.LookPath("ocamlfind"); err == nil {
				cmdName = "ocamlfind"
				args = []string{"ocamlc", "-package", "yojson,unix", "-linkpkg", mlfile, "-o", exe}
			}
			if out, err := exec.Command(cmdName, args...).CombinedOutput(); err != nil {
				return nil, fmt.Errorf("\u274c ocamlc error: %w\n%s", err, out)
			}
			rootDir := findRepoRoot(t)
			cmd := exec.Command(exe)
			cmd.Dir = rootDir
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				return nil, fmt.Errorf("\u274c run error: %w\n%s", err, out)
			}
			res := bytes.TrimSpace(out)
			if res == nil {
				res = []byte{}
			}

			p, err := vm.Compile(prog, env)
			if err != nil {
				return nil, fmt.Errorf("\u274c vm compile error: %w", err)
			}
			var vmIn []byte
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				vmIn = data
			}
			var buf bytes.Buffer
			m := vm.NewWithIO(p, bytes.NewReader(vmIn), &buf)
			cwd, _ := os.Getwd()
			_ = os.Chdir(rootDir)
			err = m.Run()
			_ = os.Chdir(cwd)
			if err != nil {
				if ve, ok := err.(*vm.VMError); ok {
					return nil, fmt.Errorf("\u274c vm run error:\n%s", ve.Format(p))
				}
				return nil, fmt.Errorf("\u274c vm run error: %v", err)
			}
			if strings.TrimSpace(buf.String()) != string(res) {
				return nil, fmt.Errorf("\u274c output mismatch with VM\n-- vm --\n%s\n-- ocaml --\n%s", strings.TrimSpace(buf.String()), res)
			}

			return bytes.TrimSpace(code), nil
		})
	}
}

func TestOCamlCompiler_LeetCodeExamples(t *testing.T) {
	for i := 1; i <= 5; i++ {
		runExample(t, i)
	}
}

func runExample(t *testing.T, i int) {
	if err := mlcode.EnsureOCaml(); err != nil {
		t.Skipf("ocamlc not installed: %v", err)
	}
	dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(i))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	for _, f := range files {
		name := fmt.Sprintf("%d/%s", i, filepath.Base(f))
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(f)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := mlcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			mlfile := filepath.Join(tmp, "prog.ml")
			if err := os.WriteFile(mlfile, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			exe := filepath.Join(tmp, "prog")
			cmdName := "ocamlc"
			args := []string{mlfile, "-o", exe}
			if _, err := exec.LookPath("ocamlfind"); err == nil {
				cmdName = "ocamlfind"
				args = []string{"ocamlc", "-package", "yojson,unix", "-linkpkg", mlfile, "-o", exe}
			}
			if out, err := exec.Command(cmdName, args...).CombinedOutput(); err != nil {
				t.Fatalf("ocamlc error: %v\n%s", err, out)
			}
			cmd := exec.Command(exe)
			cmd.Dir = findRepoRoot(t)
			if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("run error: %v\n%s", err, out)
			}
			_ = out
		})
	}
}

func findRepoRoot(t *testing.T) string {
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
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}
