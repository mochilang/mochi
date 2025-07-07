//go:build archived && slow

package excode_test

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"testing"

	excode "mochi/archived/x/ex"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestExCompiler_SubsetPrograms(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/ex", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := excode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.exs")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("elixir", file)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		var buf bytes.Buffer
		cmd.Stdout = &buf
		cmd.Stderr = &buf
		if err := cmd.Run(); err != nil {
			return nil, fmt.Errorf("❌ elixir run error: %w\n%s", err, buf.Bytes())
		}
		return bytes.TrimSpace(buf.Bytes()), nil
	})
}

func TestExCompiler_GoldenOutput(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}

	runCompile := func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := excode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}

		dir := t.TempDir()
		file := filepath.Join(dir, "main.exs")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("elixir", file)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ elixir run error: %w\n%s", err, out)
		}
		got := bytes.TrimSpace(out)
		if got == nil {
			got = []byte{}
		}

		vmOut, err := runVM(src)
		if err != nil {
			return nil, fmt.Errorf("vm error: %w", err)
		}
		if !bytes.Equal(got, vmOut) {
			return nil, fmt.Errorf("runtime output mismatch\n\n--- VM ---\n%s\n\n--- EX ---\n%s\n", vmOut, got)
		}

		outPath := strings.TrimSuffix(src, ".mochi") + ".out"
		if want, err := os.ReadFile(outPath); err == nil {
			root := repoRoot()
			if !bytes.Equal(normalizeOutput(root, got), normalizeOutput(root, bytes.TrimSpace(want))) {
				return nil, fmt.Errorf("runtime output mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, want)
			}
		}

		return bytes.TrimSpace(code), nil
	}

	golden.Run(t, "tests/compiler/valid", ".mochi", ".ex.out", runCompile)
	golden.Run(t, "tests/compiler/ex", ".mochi", ".ex.out", runCompile)
}

func runExample(t *testing.T, id int) {
	dir := filepath.Join("..", "..", "examples", "leetcode", strconv.Itoa(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	for _, f := range files {
		name := fmt.Sprintf("%d/%s", id, filepath.Base(f))
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(f)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := excode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.exs")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("elixir", file)
			if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("elixir run error: %v\n%s", err, out)
			}
			_ = out
		})
	}
}

func TestExCompiler_LeetCode1(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	runExample(t, 1)
}

func TestExCompiler_LeetCode2(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	runExample(t, 2)
}

func TestExCompiler_TPCHQ1(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
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
	code, err := excode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "ex", "q1.ex.out")
	wantCode, err := os.ReadFile(codeWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q1.ex.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(wantCode))
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.exs")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("elixir", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("elixir run error: %v\n%s", err, out)
	}
	gotOut := bytes.TrimSpace(out)
	outWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "ex", "q1.out")
	wantOut, err := os.ReadFile(outWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for q1.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", gotOut, bytes.TrimSpace(wantOut))
	}
}

func TestExCompiler_TPCHQ2(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	root := findRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-h", "q2.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := excode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "ex", "q2.ex.out")
	wantCode, err := os.ReadFile(codeWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q2.ex.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(wantCode))
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.exs")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("elixir", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("elixir run error: %v\n%s", err, out)
	}
	gotOut := bytes.TrimSpace(out)
	outWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "ex", "q2.out")
	wantOut, err := os.ReadFile(outWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for q2.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", gotOut, bytes.TrimSpace(wantOut))
	}
}

// runVM executes the given Mochi source file using the runtime VM and returns
// the trimmed stdout.
func runVM(src string) ([]byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		return nil, fmt.Errorf("vm compile error: %w", err)
	}
	var in io.Reader = os.Stdin
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		in = bytes.NewReader(data)
	}
	var out bytes.Buffer
	m := vm.NewWithIO(p, in, &out)
	if err := m.Run(); err != nil {
		return nil, fmt.Errorf("vm run error: %w", err)
	}
	return bytes.TrimSpace(out.Bytes()), nil
}

// runElixir compiles the Mochi program to Elixir, runs it and returns stdout.
func runElixir(t *testing.T, src string) ([]byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return nil, fmt.Errorf("❌ parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("❌ type error: %v", errs[0])
	}
	code, err := excode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("❌ compile error: %w", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.exs")
	if err := os.WriteFile(file, code, 0644); err != nil {
		return nil, fmt.Errorf("write error: %w", err)
	}
	cmd := exec.Command("elixir", file)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("❌ elixir run error: %w\n%s", err, buf.Bytes())
	}
	return bytes.TrimSpace(buf.Bytes()), nil
}

// TestExCompiler_ValidVM regenerates the valid compiler tests and ensures the
// generated Elixir program behaves the same as the runtime VM.
func TestExCompiler_ValidVM(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	root := findRepoRoot(t)
	files, err := filepath.Glob(filepath.Join(root, "tests", "compiler", "valid", "*.mochi"))
	if err != nil {
		t.Fatal(err)
	}
	var errs []string
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			got, err := runElixir(t, src)
			if err != nil {
				errs = append(errs, fmt.Sprintf("%s: %v", name, err))
				t.Error(err)
				return
			}
			vmOut, err := runVM(src)
			if err != nil {
				errs = append(errs, fmt.Sprintf("%s: %v", name, err))
				t.Error(err)
				return
			}
			if !bytes.Equal(got, vmOut) {
				errs = append(errs, fmt.Sprintf("%s: output mismatch", name))
				t.Errorf("output mismatch\n--- VM ---\n%s\n--- EX ---\n%s\n", vmOut, got)
			}
		})
	}
	writeErrors(filepath.Join(root, "compile", "x", "ex"), errs)
}

func writeErrors(dir string, errs []string) {
	_ = os.MkdirAll(dir, 0755)
	path := filepath.Join(dir, "ERRORS.md")
	var b bytes.Buffer
	b.WriteString("# Errors\n\n")
	if len(errs) == 0 {
		b.WriteString("None\n")
	} else {
		for _, e := range errs {
			b.WriteString("- " + e + "\n")
		}
	}
	_ = os.WriteFile(path, b.Bytes(), 0644)
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
