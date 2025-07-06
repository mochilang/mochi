//go:build slow

package fscode_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	fscode "mochi/compile/x/fs"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestFSCompiler_SubsetPrograms(t *testing.T) {
	if err := fscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	if err := exec.Command("dotnet", "fsi", "--help").Run(); err != nil {
		t.Skipf("dotnet fsi not runnable: %v", err)
	}
	golden.Run(t, "tests/compiler/fs", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := fscode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.fsx")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("dotnet", "fsi", "--quiet", file)
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ fsi error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestFSCompiler_GoldenOutput(t *testing.T) {
	compileRun := func(src string) ([]byte, error) {
		if _, err := exec.LookPath("dotnet"); err != nil {
			return nil, fmt.Errorf("dotnet not installed")
		}
		if err := exec.Command("dotnet", "fsi", "--help").Run(); err != nil {
			return nil, fmt.Errorf("dotnet fsi not runnable: %v", err)
		}
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := fscode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}

		// Execute the generated F# code.
		dir := t.TempDir()
		file := filepath.Join(dir, "main.fsx")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("dotnet", "fsi", "--quiet", file)
		var input []byte
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
			input = data
		}
		outFS, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ fsi error: %w\n%s", err, outFS)
		}
		outFS = bytes.TrimSpace(outFS)

		// Run the original program via the Mochi VM for reference.
		progBytes, rerr := os.ReadFile(src)
		if rerr != nil {
			return nil, rerr
		}
		p, err := vm.CompileWithSource(prog, env, string(progBytes))
		if err != nil {
			return nil, fmt.Errorf("❌ vm compile error: %w", err)
		}
		var vmOut bytes.Buffer
		vmIn := bytes.NewReader(input)
		m := vm.NewWithIO(p, vmIn, &vmOut)
		if err := m.Run(); err != nil {
			if ve, ok := err.(*vm.VMError); ok {
				return nil, fmt.Errorf("❌ vm run error:\n%s", ve.Format(p))
			}
			return nil, fmt.Errorf("❌ vm run error: %v", err)
		}
		outVM := bytes.TrimSpace(vmOut.Bytes())

		if !bytes.Equal(outFS, outVM) {
			return nil, fmt.Errorf("runtime mismatch\n\n--- F# ---\n%s\n\n--- VM ---\n%s\n", outFS, outVM)
		}

		return bytes.TrimSpace(code), nil
	}

	runGoldenSkip(t, "tests/compiler/valid", ".mochi", ".fs.out", compileRun)
	golden.Run(t, "tests/compiler/fs", ".mochi", ".fs.out", compileRun)
}

func runGoldenSkip(t *testing.T, dir, srcExt, goldenExt string, fn func(string) ([]byte, error)) {
	rootDir := findRepoRoot(t)
	pattern := filepath.Join(rootDir, dir, "*"+srcExt)

	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("failed to list %s files in %s: %v", srcExt, dir, err)
	}
	if len(files) == 0 {
		t.Fatalf("no test files found: %s", pattern)
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), srcExt)
		wantPath := filepath.Join(rootDir, dir, name+goldenExt)

		t.Run(name, func(t *testing.T) {

			got, err := fn(src)
			if err != nil {
				t.Skipf("skipping unsupported file: %v", err)
				return
			}
			if got == nil {
				t.Fatal("got nil output")
			}
			got = normalizeOutput(rootDir, got)

			if flag.Lookup("update") != nil && flag.Lookup("update").Value.String() == "true" {
				if err := os.WriteFile(wantPath, got, 0644); err != nil {
					t.Fatalf("failed to write golden: %v", err)
				}
				t.Logf("updated: %s", wantPath)
				return
			}

			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Fatalf("failed to read golden: %v", err)
			}
			want = bytes.TrimSpace(want)

			if !bytes.Equal(got, want) {
				t.Errorf("golden mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name+goldenExt, got, want)
				return
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
	return []byte(strings.TrimSpace(out))
}

// runLeetExample compiles the Mochi solution for the given LeetCode ID to F#
// and executes the generated program. The test fails if compilation or
// execution returns an error. For problem 1 (two-sum) the output is verified.
func runLeetExample(t *testing.T, id int) {
	t.Helper()
	dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(id))
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
			code, err := fscode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.fsx")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("dotnet", "fsi", "--quiet", file)
			if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("fsi error: %v\n%s", err, out)
			}
			if id == 1 {
				got := strings.TrimSpace(string(out))
				if got != "0\n1" {
					t.Fatalf("unexpected output: %q", got)
				}
			}
		})
	}
}

func TestFSCompiler_LeetCode1(t *testing.T) {
	if err := fscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	if err := exec.Command("dotnet", "fsi", "--help").Run(); err != nil {
		t.Skipf("dotnet fsi not runnable: %v", err)
	}
	runLeetExample(t, 1)
}

func TestFSCompiler_LeetCodeExamples(t *testing.T) {
	if err := fscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	if err := exec.Command("dotnet", "fsi", "--help").Run(); err != nil {
		t.Skipf("dotnet fsi not runnable: %v", err)
	}
	for i := 1; i <= 30; i++ {
		runLeetExample(t, i)
	}
}
