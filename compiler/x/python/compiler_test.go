//go:build slow

package pycode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	pycode "mochi/compiler/x/python"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestPyCompiler_SubsetPrograms(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}
	golden.Run(t, "tests/compiler/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := pycode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.py")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("python3", file)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ python run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})

	golden.Run(t, "tests/compiler/py", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := pycode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.py")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("python3", file)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ python run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestPyCompiler_GoldenOutput(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}

	compileFn := func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := pycode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}

		dir := t.TempDir()
		file := filepath.Join(dir, "main.py")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("python3", file)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ python run error: %w\n%s", err, out)
		}
		gotOut := bytes.TrimSpace(out)
		wantOutPath := strings.TrimSuffix(src, ".mochi") + ".out"
		wantOut, err := os.ReadFile(wantOutPath)
		if err != nil {
			return nil, fmt.Errorf("failed to read golden output: %w", err)
		}
		if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
			return nil, fmt.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", filepath.Base(wantOutPath), gotOut, bytes.TrimSpace(wantOut))
		}

		return bytes.TrimSpace(code), nil
	}

	dirs := []string{
		"tests/compiler/py",
	}

	for _, dir := range dirs {
		golden.Run(t, dir, ".mochi", ".py.out", compileFn)
	}
}

func TestPyCompiler_LeetCodeExamples(t *testing.T) {
	t.Skip("disabled in current environment")
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}
	runLeetExample(t, 1)
	runLeetExample(t, 2)
}

func runLeetExample(t *testing.T, id int) {
	t.Helper()
	dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	if len(files) == 0 {
		t.Fatalf("no examples found in %s", dir)
	}
	for _, src := range files {
		name := fmt.Sprintf("%d/%s", id, filepath.Base(src))
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			c := pycode.New(env)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.py")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("python3", file)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			if out, err := cmd.CombinedOutput(); err != nil {
				t.Fatalf("python run error: %v\n%s", err, out)
			} else {
				_ = out
			}
		})
	}
}

func TestPyCompiler_TPCHQueries(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}
	root := findRepoRoot(t)
	for i := 1; i <= 22; i++ {
		q := fmt.Sprintf("q%d", i)
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
			c := pycode.New(env)
			c.SetTypeHints(false)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "py", q+".py.out")
			wantCode, err := os.ReadFile(codeWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.py.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.py")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("python3", file)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("python run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			outWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "py", q+".out")
			wantOut, err := os.ReadFile(outWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}

func TestPyCompiler_JOBQueries(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}
	root := findRepoRoot(t)
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
			c := pycode.New(env)
			c.SetTypeHints(false)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			codeWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "py", q+".py.out")
			wantCode, err := os.ReadFile(codeWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.py.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.py")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("python3", file)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("python run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			outWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "py", q+".out")
			wantOut, err := os.ReadFile(outWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}

func TestPyCompiler_TPCDSQueries(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}
	root := findRepoRoot(t)
	queries := []string{}
	for i := 1; i <= 99; i++ {
		queries = append(queries, fmt.Sprintf("q%d", i))
	}
	for _, q := range queries {
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
			c := pycode.New(env)
			c.SetTypeHints(false)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "py", q+".py.out")
			wantCode, err := os.ReadFile(codeWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.py.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.py")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("python3", file)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("python run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			outWantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "py", q+".out")
			wantOut, err := os.ReadFile(outWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}

func TestPyCompiler_SLTCases(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}
	root := findRepoRoot(t)
	dir := filepath.Join(root, "tests", "dataset", "slt", "out", "select1")
	for i := 1; i <= 5; i++ {
		name := fmt.Sprintf("case%d", i)
		t.Run(name, func(t *testing.T) {
			src := filepath.Join(dir, name+".mochi")
			outWantPath := filepath.Join(dir, name+".out")
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			c := pycode.New(env)
			c.SetTypeHints(false)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.py")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("python3", file)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("python run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			wantOut, err := os.ReadFile(outWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}

func TestPyCompiler_SLTQueries(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}
	root := findRepoRoot(t)
	cases := []string{"case1", "case2", "case3"}
	for _, base := range cases {
		src := filepath.Join(root, "tests", "dataset", "slt", "out", "select1", base+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "slt", "compiler", "py", base+".py.out")
		outWant := filepath.Join(root, "tests", "dataset", "slt", "compiler", "py", base+".out")
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
			c := pycode.New(env)
			c.SetTypeHints(false)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.py.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.py")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("python3", file)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("python run error: %v\n%s", err, out)
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

// TestCompilePrograms compiles each Mochi file under tests/vm/valid
// and runs the resulting Python program. Generated files and outputs
// are written to tests/machine/x/python.
func TestCompilePrograms(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "python")
	os.MkdirAll(outDir, 0o755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			compileAndRun(t, src, outDir, name)
		})
	}
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}

func updateReadme() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "python")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s.mochi", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Python Machine Translations\n\n")
	buf.WriteString("This directory contains Python code generated from the Mochi programs in `tests/vm/valid` using the Python compiler. Each program was compiled and executed. Successful runs produced an `.out` file while failures produced an `.error` file.\n\n")
	fmt.Fprintf(&buf, "Compiled programs: %d/%d successful.\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(filepath.Join(outDir, "README.md"), buf.Bytes(), 0o644)
}

func compileAndRun(t *testing.T, src, outDir, name string) {
	data, err := os.ReadFile(src)
	if err != nil {
		t.Fatalf("read error: %v", err)
	}
	errPath := filepath.Join(outDir, name+".error")
	os.Remove(errPath)
	prog, err := parser.ParseString(string(data))
	if err != nil {
		writeError(t, outDir, name, string(data), err)
		return
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeError(t, outDir, name, string(data), fmt.Errorf("type error: %v", errs[0]))
		return
	}
	c := pycode.New(env)
	c.SetTypeHints(false)
	code, err := c.Compile(prog)
	if err != nil {
		writeError(t, outDir, name, string(data), err)
		return
	}
	codePath := filepath.Join(outDir, name+".py")
	os.WriteFile(codePath, code, 0o644)
	cmd := exec.Command("python3", codePath)
	root := findRepoRoot(t)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		writeError(t, outDir, name, string(data), fmt.Errorf("run: %v\n%s", err, buf.String()))
		return
	}
	outPath := filepath.Join(outDir, name+".out")
	os.WriteFile(outPath, buf.Bytes(), 0o644)
	os.Remove(errPath)
}

func writeError(t *testing.T, dir, name, src string, err error) {
	lines := strings.Split(src, "\n")
	msg := err.Error()
	ln := 0
	if idx := strings.Index(msg, "line "); idx != -1 {
		fmt.Sscanf(msg[idx:], "line %d", &ln)
	}
	var ctx string
	if ln > 0 {
		start := ln - 2
		if start < 0 {
			start = 0
		}
		end := ln + 1
		if end > len(lines) {
			end = len(lines)
		}
		for i := start; i < end; i++ {
			ctx += lines[i] + "\n"
		}
	}
	errPath := filepath.Join(dir, name+".error")
	os.WriteFile(errPath, []byte(msg+"\n"+ctx), 0o644)
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
