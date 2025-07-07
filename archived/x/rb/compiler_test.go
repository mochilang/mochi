//go:build archived && slow

package rbcode_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"testing"

	rbcode "mochi/archived/x/rb"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

// compileAndRun compiles src to Ruby and executes the generated program.
// The program's trimmed combined output is returned.
func compileAndRun(t *testing.T, src string) (string, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return "", fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return "", fmt.Errorf("type error: %v", errs[0])
	}
	code, err := rbcode.New(env).Compile(prog)
	if err != nil {
		return "", fmt.Errorf("compile error: %w", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.rb")
	if err := os.WriteFile(file, code, 0644); err != nil {
		return "", err
	}
	cmd := exec.Command("ruby", file)
	cmd.Dir = findRepoRoot(t)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	return strings.TrimSpace(string(out)), err
}

// runLeetExample compiles and executes all Mochi files under examples/leetcode/<id>.
func runLeetExample(t *testing.T, id int) {
	root := findRepoRoot(t)
	dir := filepath.Join(root, "examples", "leetcode", strconv.Itoa(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	for _, src := range files {
		name := fmt.Sprintf("%d/%s", id, filepath.Base(src))
		t.Run(name, func(t *testing.T) {
			out, err := compileAndRun(t, src)
			if err != nil {
				t.Fatalf("ruby run error: %v\n%s", err, out)
			}
		})
	}
}

func TestRBCompiler_TwoSum(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	root := findRepoRoot(t)
	src := filepath.Join(root, "examples", "leetcode", "1", "two-sum.mochi")
	got, err := compileAndRun(t, src)
	if err != nil {
		t.Fatalf("ruby run error: %v\n%s", err, got)
	}
	if got != "0\n1" {
		t.Fatalf("unexpected output: %q", got)
	}
}

func TestRBCompiler_LeetCodeExamples(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	for i := 1; i <= 30; i++ {
		runLeetExample(t, i)
	}
}

func TestRBCompiler_SubsetPrograms(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/rb", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, errs[0]
		}
		code, err := rbcode.New(env).Compile(prog)
		if err != nil {
			return nil, err
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.rb")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, err
		}
		cmd := exec.Command("ruby", file)
		cmd.Dir = findRepoRoot(t)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("ruby run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestRBCompiler_ValidPrograms(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "compiler", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	skip := map[string]bool{
		"generate_echo":      true,
		"generate_embedding": true,
		"test_block":         true,
		"stream_on_emit":     true,
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		if skip[name] {
			continue
		}
		t.Run(name, func(t *testing.T) {
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
			dir := t.TempDir()
			file := filepath.Join(dir, "main.rb")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("ruby", file)
			cmd.Dir = findRepoRoot(t)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("ruby run error: %v\n%s", err, out)
			}
			got := bytes.TrimSpace(out)

			// Execute the program with the Mochi VM for comparison.
			p, err := vm.Compile(prog, env)
			if err != nil {
				t.Fatalf("vm compile error: %v", err)
			}
			var buf bytes.Buffer
			m := vm.New(p, &buf)
			if err := m.Run(); err != nil {
				if ve, ok := err.(*vm.VMError); ok {
					t.Fatalf("vm run error:\n%s", ve.Format(p))
				}
				t.Fatalf("vm run error: %v", err)
			}
			want := bytes.TrimSpace(buf.Bytes())

			if !bytes.Equal(got, want) {
				t.Errorf("vm mismatch for %s\n\n--- Ruby ---\n%s\n\n--- VM ---\n%s", name, got, want)
			}
		})
	}
}

func TestRBCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/rb", ".mochi", ".rb.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, errs[0]
		}
		code, err := rbcode.New(env).Compile(prog)
		if err != nil {
			return nil, err
		}
		return bytes.TrimSpace(code), nil
	})
}

func TestRBCompiler_ValidGoldenOutput(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	runCompile := func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		code, err := rbcode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}

		dir := t.TempDir()
		file := filepath.Join(dir, "main.rb")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("ruby", file)
		cmd.Dir = findRepoRoot(t)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		rubyOut, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("ruby run error: %w\n%s", err, rubyOut)
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

		root := findRepoRoot(t)
		if !bytes.Equal(normalizeOutput(root, bytes.TrimSpace(rubyOut)), normalizeOutput(root, bytes.TrimSpace(buf.Bytes()))) {
			return nil, fmt.Errorf("runtime output mismatch\n\n--- VM ---\n%s\n\n--- Ruby ---\n%s\n", buf.Bytes(), rubyOut)
		}
		return bytes.TrimSpace(code), nil
	}

	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "compiler", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	skip := map[string]bool{
		"generate_echo":      true,
		"generate_embedding": true,
		"test_block":         true,
		"stream_on_emit":     true,
		"local_recursion":    true,
		"reduce":             true,
		"union_inorder":      true,
		"union_match":        true,
	}
	update := flag.Lookup("update") != nil && flag.Lookup("update").Value.String() == "true"
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		if skip[name] {
			continue
		}
		wantPath := filepath.Join(root, "tests", "compiler", "valid", name+".rb.out")
		t.Run(name, func(t *testing.T) {
			out, err := runCompile(src)
			if err != nil {
				t.Fatalf("%v", err)
			}
			out = normalizeOutput(root, out)
			if update {
				if err := os.WriteFile(wantPath, out, 0644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
				t.Logf("updated: %s", wantPath)
				return
			}
			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(out, normalizeOutput(root, want)) {
				t.Errorf("golden mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, out, want)
			}
		})
	}
}

func TestRBCompiler_TPCHQ1(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
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
	code, err := rbcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "rb", "q1.rb.out")
	wantCode, err := os.ReadFile(codeWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q1.rb.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", got, bytes.TrimSpace(wantCode))
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
	outWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "rb", "q1.out")
	wantOut, err := os.ReadFile(outWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for q1.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", gotOut, bytes.TrimSpace(wantOut))
	}
}

func TestRBCompiler_TPCHQ2(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
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
	code, err := rbcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "rb", "q2.rb.out")
	wantCode, err := os.ReadFile(codeWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q2.rb.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", got, bytes.TrimSpace(wantCode))
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
	outWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "rb", "q2.out")
	wantOut, err := os.ReadFile(outWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for q2.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", gotOut, bytes.TrimSpace(wantOut))
	}
}

func TestRBCompiler_JOBQ1(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
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
	code, err := rbcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	codeWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "rb", "q1.rb.out")
	wantCode, err := os.ReadFile(codeWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q1.rb.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", got, bytes.TrimSpace(wantCode))
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
	outWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "rb", "q1.out")
	wantOut, err := os.ReadFile(outWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for q1.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", gotOut, bytes.TrimSpace(wantOut))
	}
}

func TestRBCompiler_JOBQueries(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	root := findRepoRoot(t)
	for i := 1; i <= 10; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "job", base+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "job", "compiler", "rb", base+".rb.out")
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
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.rb.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, got, bytes.TrimSpace(wantCode))
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
			wantOut, err := os.ReadFile(outWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}

func TestRBCompiler_TPCDSQueries(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	root := findRepoRoot(t)
	for i := 1; i <= 99; i++ {
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
			code, err := rbcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "rb", q+".rb.out")
			wantCode, err := os.ReadFile(codeWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.rb.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", q, got, bytes.TrimSpace(wantCode))
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
			outWantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "rb", q+".out")
			wantOut, err := os.ReadFile(outWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", q, gotOut, bytes.TrimSpace(wantOut))
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
