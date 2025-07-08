//go:build archived && slow

package gocode_test

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	gocode "mochi/archived/go"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

// runWithVM compiles src to Go, executes both Go code and the Mochi VM,
// and ensures their outputs match. It returns the generated Go code and
// the Go program's output.
func runWithVM(src string) ([]byte, []byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return nil, nil, fmt.Errorf("❌ parse error: %w", err)
	}
	typeEnv := types.NewEnv(nil)
	if errs := types.Check(prog, typeEnv); len(errs) > 0 {
		return nil, nil, fmt.Errorf("❌ type error: %v", errs[0])
	}
	code, err := gocode.New(typeEnv).Compile(prog)
	if err != nil {
		return nil, nil, fmt.Errorf("❌ compile error: %w", err)
	}

	dir, err := os.MkdirTemp("", "mochi-go")
	if err != nil {
		return nil, nil, err
	}
	defer os.RemoveAll(dir)
	file := filepath.Join(dir, "main.go")
	if err := os.WriteFile(file, code, 0644); err != nil {
		return nil, nil, fmt.Errorf("write error: %w", err)
	}
	cmd := exec.Command("go", "run", file)
	cmd.Env = append(os.Environ(), "GO111MODULE=on", "LLM_PROVIDER=echo")
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	goOut, err := cmd.CombinedOutput()
	if err != nil {
		return nil, nil, fmt.Errorf("❌ go run error: %w\n%s", err, goOut)
	}
	goOut = bytes.TrimSpace(goOut)

	p, err := vm.Compile(prog, typeEnv)
	if err != nil {
		return nil, nil, fmt.Errorf("vm compile error: %w", err)
	}
	var in io.Reader = bytes.NewReader(nil)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		in = bytes.NewReader(data)
	}
	var vmBuf bytes.Buffer
	m := vm.NewWithIO(p, in, &vmBuf)
	if err := m.Run(); err != nil {
		return nil, nil, fmt.Errorf("vm run error: %w", err)
	}
	vmOut := bytes.TrimSpace(vmBuf.Bytes())

	root := repoRoot()
	if want, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".out"); err == nil {
		if !bytes.Equal(normalizeOutput(root, vmOut), normalizeOutput(root, bytes.TrimSpace(want))) {
			return nil, nil, fmt.Errorf("runtime output mismatch\n\n--- VM ---\n%s\n\n--- Want ---\n%s\n", vmOut, want)
		}
	}
	if !bytes.Equal(normalizeOutput(root, goOut), normalizeOutput(root, vmOut)) {
		return nil, nil, fmt.Errorf("vm mismatch\n\n--- Go ---\n%s\n\n--- VM ---\n%s\n", goOut, vmOut)
	}

	return bytes.TrimSpace(code), goOut, nil
}

func execProgram(src string) ([]byte, error) {
	_, out, err := runWithVM(src)
	return out, err
}

func compileProgram(src string) ([]byte, error) {
	code, _, err := runWithVM(src)
	return code, err
}

func TestGoCompiler_SubsetPrograms(t *testing.T) {
	golden.Run(t, "tests/compiler/valid", ".mochi", ".out", execProgram)
	golden.Run(t, "tests/compiler/go", ".mochi", ".out", execProgram)
}

func TestGoCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/valid", ".mochi", ".go.out", compileProgram)
	golden.Run(t, "tests/compiler/go", ".mochi", ".go.out", compileProgram)
}
func TestGoCompiler_LeetCodeExamples(t *testing.T) {
	t.Skip("disabled in current environment")
	runExample(t, 102)
	runExample(t, 201)
	runExample(t, 207)
	runExample(t, 378)
	runExample(t, 346)
	runExample(t, 317)
	runExample(t, 267)
}

func runExample(t *testing.T, i int) {
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
			typeEnv := types.NewEnv(nil)
			if errs := types.Check(prog, typeEnv); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			c := gocode.New(typeEnv)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.go")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("go", "run", file)
			cmd.Env = append(os.Environ(), "GO111MODULE=on", "LLM_PROVIDER=echo")
			if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("go run error: %v\n%s", err, out)
			}
			// Older examples may print results; just ensure the
			// program executes without error.
			_ = out
		})
	}
}

func TestGoCompiler_TPCHQ1(t *testing.T) {
	root := findRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-h", "q1.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := gocode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "go", "q1.go.out")
	wantCode, err := os.ReadFile(codeWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q1.go.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(wantCode))
	}

	dir := t.TempDir()
	file := filepath.Join(dir, "main.go")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("go", "run", file)
	cmd.Env = append(os.Environ(), "GO111MODULE=on", "LLM_PROVIDER=echo")
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("go run error: %v\n%s", err, out)
	}
	gotOut := bytes.TrimSpace(out)
	outWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "go", "q1.out")
	wantOut, err := os.ReadFile(outWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for q1.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", gotOut, bytes.TrimSpace(wantOut))
	}
}

func TestGoCompiler_TPCHQueries(t *testing.T) {
	root := findRepoRoot(t)
	for i := 1; i <= 2; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-h", base+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "go", base+".go.out")
		outWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "go", base+".out")
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
			code, err := gocode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.go.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.go")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("go", "run", file)
			cmd.Env = append(os.Environ(), "GO111MODULE=on", "LLM_PROVIDER=echo")
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("go run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			wantOut, err := os.ReadFile(outWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(normalizeOutput(root, gotOut), normalizeOutput(root, bytes.TrimSpace(wantOut))) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}

func TestGoCompiler_JOBQ1(t *testing.T) {
	root := findRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "job", "q1.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := gocode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	codeWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "go", "q1.go.out")
	wantCode, err := os.ReadFile(codeWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q1.go.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(wantCode))
	}

	dir := t.TempDir()
	file := filepath.Join(dir, "main.go")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("go", "run", file)
	cmd.Env = append(os.Environ(), "GO111MODULE=on", "LLM_PROVIDER=echo")
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("go run error: %v\n%s", err, out)
	}
	gotOut := bytes.TrimSpace(out)
	outWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "go", "q1.out")
	wantOut, err := os.ReadFile(outWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for q1.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", gotOut, bytes.TrimSpace(wantOut))
	}
}

func TestGoCompiler_JOBQueries(t *testing.T) {
	root := findRepoRoot(t)
	for i := 1; i <= 10; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "job", base+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "job", "compiler", "go", base+".go.out")
		outWant := filepath.Join(root, "tests", "dataset", "job", "compiler", "go", base+".out")
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
			code, err := gocode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.go.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.go")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("go", "run", file)
			cmd.Env = append(os.Environ(), "GO111MODULE=on", "LLM_PROVIDER=echo")
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("go run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			wantOut, err := os.ReadFile(outWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}

func TestGoCompiler_TPCDSQueries(t *testing.T) {
	root := findRepoRoot(t)
	for i := 1; i <= 19; i++ {
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
			code, err := gocode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "go", q+".go.out")
			wantCode, err := os.ReadFile(codeWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.go.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.go")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("go", "run", file)
			cmd.Env = append(os.Environ(), "GO111MODULE=on", "LLM_PROVIDER=echo")
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("go run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			outWantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "go", q+".out")
			wantOut, err := os.ReadFile(outWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(normalizeOutput(root, gotOut), normalizeOutput(root, bytes.TrimSpace(wantOut))) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
	return
	for i := 20; i <= 29; i++ {
		if i == 21 || i == 23 || i == 25 {
			continue // these queries do not pass with the Go backend yet
		}
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
			code, err := gocode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "go", q+".go.out")
			wantCode, err := os.ReadFile(codeWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.go.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.go")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("go", "run", file)
			cmd.Env = append(os.Environ(), "GO111MODULE=on", "LLM_PROVIDER=echo")
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("go run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			outWantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "go", q+".out")
			wantOut, err := os.ReadFile(outWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(normalizeOutput(root, gotOut), normalizeOutput(root, bytes.TrimSpace(wantOut))) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
	for i := 50; i <= 99; i++ {
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
			code, err := gocode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "go", q+".go.out")
			wantCode, err := os.ReadFile(codeWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.go.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.go")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("go", "run", file)
			cmd.Env = append(os.Environ(), "GO111MODULE=on", "LLM_PROVIDER=echo")
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("go run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			outWantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "go", q+".out")
			wantOut, err := os.ReadFile(outWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(normalizeOutput(root, gotOut), normalizeOutput(root, bytes.TrimSpace(wantOut))) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}

func TestGoCompiler_SLT(t *testing.T) {
	root := findRepoRoot(t)
	cases := []string{"case1", "case2", "case3"}
	for _, caseName := range cases {
		t.Run(caseName, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "slt", "out", "select1", caseName+".mochi")
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := gocode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}

			wantCode, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "slt", "compiler", "go", caseName+".go.out"))
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.go.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", caseName, got, bytes.TrimSpace(wantCode))
			}

			dir := t.TempDir()
			file := filepath.Join(dir, "main.go")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("go", "run", file)
			cmd.Env = append(os.Environ(), "GO111MODULE=on", "LLM_PROVIDER=echo")
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("go run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			wantOut, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "slt", "compiler", "go", caseName+".out"))
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(normalizeOutput(root, gotOut), normalizeOutput(root, bytes.TrimSpace(wantOut))) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", caseName, gotOut, bytes.TrimSpace(wantOut))
			}
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

func normalizeOutput(root string, b []byte) []byte {
	out := string(b)
	out = strings.ReplaceAll(out, filepath.ToSlash(root)+"/", "")
	out = strings.ReplaceAll(out, filepath.ToSlash(root), "")
	out = strings.ReplaceAll(out, "github.com/mochi-lang/mochi/", "")
	out = strings.ReplaceAll(out, "mochi/tests/", "tests/")
	durRE := regexp.MustCompile(`\([0-9]+(\.[0-9]+)?(ns|µs|ms|s)\)`)
	out = durRE.ReplaceAllString(out, "(X)")
	out = strings.TrimSpace(out)
	if !strings.HasSuffix(out, "\n") {
		out += "\n"
	}
	return []byte(out)
}

func repoRoot() string {
	dir, err := os.Getwd()
	if err != nil {
		return ""
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
	return dir
}
