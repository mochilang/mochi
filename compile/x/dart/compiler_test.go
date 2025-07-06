//go:build slow

package dartcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	dartcode "mochi/compile/x/dart"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestDartCompiler_LeetCodeExamples(t *testing.T) {
	if err := dartcode.EnsureDart(); err != nil {
		t.Skipf("dart not installed: %v", err)
	}
	runLeetExample(t, 1, "0\n1")
	runLeetExample(t, 2, "")
	runLeetExample(t, 3, "")
}

func TestDartCompiler_SubsetPrograms(t *testing.T) {
	if err := dartcode.EnsureDart(); err != nil {
		t.Skipf("dart not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/dart", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := dartcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.dart")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("dart", file)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ dart run error: %w\n%s", err, out)
		}
		res := bytes.TrimSpace(out)
		if res == nil {
			res = []byte{}
		}
		return res, nil
	})
}

func TestDartCompiler_GoldenOutput(t *testing.T) {
	if err := dartcode.EnsureDart(); err != nil {
		t.Skipf("dart not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/dart", ".mochi", ".dart.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := dartcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}

func TestDartCompiler_ValidGoldenOutput(t *testing.T) {
	if err := dartcode.EnsureDart(); err != nil {
		t.Skipf("dart not installed: %v", err)
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
		code, err := dartcode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}

		dir := t.TempDir()
		file := filepath.Join(dir, "main.dart")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("dart", file)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		dartOut, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ dart run error: %w\n%s", err, dartOut)
		}

		p, err := vm.Compile(prog, env)
		if err != nil {
			return nil, fmt.Errorf("vm compile error: %w", err)
		}
		var buf bytes.Buffer
		m := vm.New(p, &buf)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			m = vm.NewWithIO(p, bytes.NewReader(data), &buf)
		}
		if err := m.Run(); err != nil {
			if ve, ok := err.(*vm.VMError); ok {
				return nil, fmt.Errorf("vm run error:\n%s", ve.Format(p))
			}
			return nil, fmt.Errorf("vm run error: %v", err)
		}
		root := repoRoot()
		if !bytes.Equal(normalizeOutput(root, bytes.TrimSpace(dartOut)), normalizeOutput(root, bytes.TrimSpace(buf.Bytes()))) {
			return nil, fmt.Errorf("runtime output mismatch\n\n--- VM ---\n%s\n\n--- Dart ---\n%s\n", buf.Bytes(), dartOut)
		}
		return bytes.TrimSpace(code), nil
	}

	golden.Run(t, "tests/compiler/valid", ".mochi", ".dart.out", runCompile)
}

// runLeetExample compiles the Mochi LeetCode solution for the given ID and runs
// the generated Dart code. If want is non-empty, the program output must match.
func runLeetExample(t *testing.T, id int, want string) {
	t.Helper()
	dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil || len(files) == 0 {
		t.Fatalf("no source for problem %d", id)
	}
	src := files[0]
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := dartcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	tmp := t.TempDir()
	file := filepath.Join(tmp, "main.dart")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("dart", file)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("dart run error: %v\n%s", err, out)
	}
	if want == "" {
		return
	}
	got := strings.ReplaceAll(string(out), "\r\n", "\n")
	if strings.TrimSpace(got) != want {
		t.Fatalf("unexpected output\nwant:\n%s\n got:\n%s", want, got)
	}
}

func findRoot(t *testing.T) string {
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
