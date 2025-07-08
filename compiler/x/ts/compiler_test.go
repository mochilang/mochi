//go:build slow

package tscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	tscode "mochi/compiler/x/ts"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/mod"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestTSCompiler_SubsetPrograms(t *testing.T) {
	if err := tscode.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
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
		modRoot, _ := mod.FindRoot(filepath.Dir(src))
		if modRoot == "" {
			modRoot = filepath.Dir(src)
		}
		c := tscode.New(typeEnv, modRoot)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.ts")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", file)
		cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ deno run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
	golden.Run(t, "tests/compiler/ts", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		modRoot, _ := mod.FindRoot(filepath.Dir(src))
		if modRoot == "" {
			modRoot = filepath.Dir(src)
		}
		c := tscode.New(typeEnv, modRoot)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.ts")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", file)
		cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ deno run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestTSCompiler_GoldenOutput(t *testing.T) {
	if err := tscode.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}

	compileRun := func(t *testing.T, src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		modRoot, _ := mod.FindRoot(filepath.Dir(src))
		if modRoot == "" {
			modRoot = filepath.Dir(src)
		}
		c := tscode.New(typeEnv, modRoot)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}

		// Verify the emitted code can run correctly and match the
		// behaviour of the Mochi VM.
		dir := t.TempDir()
		file := filepath.Join(dir, "main.ts")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", file)
		cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
		var inData []byte
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			inData = data
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ deno run error: %w\n%s", err, out)
		}
		gotOut := bytes.TrimSpace(out)

		// Execute with the Mochi VM for reference output.
		vmProg, err := vm.CompileWithSource(prog, typeEnv, string(mustRead(src)))
		if err != nil {
			return nil, fmt.Errorf("vm compile error: %w", err)
		}
		var buf bytes.Buffer
		vmExec := vm.NewWithIO(vmProg, bytes.NewReader(inData), &buf)
		if err := vmExec.Run(); err != nil {
			if ve, ok := err.(*vm.VMError); ok {
				return nil, fmt.Errorf("vm run error:\n%s", ve.Format(vmProg))
			}
			return nil, fmt.Errorf("vm run error: %v", err)
		}
		want := bytes.TrimSpace(buf.Bytes())

		if !bytes.Equal(gotOut, want) {
			t.Errorf("output mismatch vs VM for %s\n\n--- Deno ---\n%s\n\n--- VM ---\n%s\n", filepath.Base(src), gotOut, want)
		}

		return bytes.TrimSpace(code), nil
	}

	golden.Run(t, "tests/compiler/valid", ".mochi", ".ts.out", func(src string) ([]byte, error) {
		return compileRun(t, src)
	})
	golden.Run(t, "tests/compiler/ts", ".mochi", ".ts.out", func(src string) ([]byte, error) {
		return compileRun(t, src)
	})
}

func TestTSCompiler_LeetCodeExamples(t *testing.T) {
	t.Skip("disabled in current environment")
	if err := tscode.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	for i := 1; i <= 133; i++ {
		runExample(t, i)
	}
	runExample(t, 272)
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
			c := tscode.New(typeEnv, "")
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.ts")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", file)
			cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
			if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("deno run error: %v\n%s", err, out)
			}
			_ = out
		})
	}
}

func TestTSCompiler_TPCHQueries(t *testing.T) {
	if err := tscode.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := findRepoRoot(t)
	for i := 1; i <= 2; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-h", base+".mochi")
		codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "ts", base+".ts.out")
		outWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "ts", base+".out")
		if _, err := os.Stat(codeWantPath); err != nil {
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
			modRoot, _ := mod.FindRoot(filepath.Dir(src))
			if modRoot == "" {
				modRoot = filepath.Dir(src)
			}
			code, err := tscode.New(env, modRoot).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codeWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.ts.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.ts")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", file)
			cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("deno run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			wantOut, err := os.ReadFile(outWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}

func TestTSCompiler_JOBQueries(t *testing.T) {
	if err := tscode.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := findRepoRoot(t)
	for i := 1; i <= 10; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "job", base+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "job", "compiler", "ts", base+".ts.out")
		outWant := filepath.Join(root, "tests", "dataset", "job", "compiler", "ts", base+".out")
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
			modRoot, _ := mod.FindRoot(filepath.Dir(src))
			if modRoot == "" {
				modRoot = filepath.Dir(src)
			}
			code, err := tscode.New(env, modRoot).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.ts.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.ts")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", "--ext=ts", file)
			cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("deno run error: %v\n%s", err, out)
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

func TestTSCompiler_TPCDSQueries(t *testing.T) {
	if err := tscode.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := findRepoRoot(t)
	for i := 1; i <= 99; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-ds", base+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "ts", base+".ts.out")
		outWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "ts", base+".out")
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
			modRoot, _ := mod.FindRoot(filepath.Dir(src))
			if modRoot == "" {
				modRoot = filepath.Dir(src)
			}
			code, err := tscode.New(env, modRoot).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.ts.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.ts")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", file)
			cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("deno run error: %v\n%s", err, out)
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

func TestTSCompiler_SLTQueries(t *testing.T) {
	if err := tscode.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := findRepoRoot(t)
	cases := []string{"case1", "case2", "case3", "case4", "case5"}
	for _, base := range cases {
		src := filepath.Join(root, "tests", "dataset", "slt", "out", "select1", base+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "slt", "compiler", "ts", base+".ts.out")
		outWant := filepath.Join(root, "tests", "dataset", "slt", "compiler", "ts", base+".out")
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
			modRoot, _ := mod.FindRoot(filepath.Dir(src))
			if modRoot == "" {
				modRoot = filepath.Dir(src)
			}
			code, err := tscode.New(env, modRoot).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.ts.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.ts")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", file)
			cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("deno run error: %v\n%s", err, out)
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

func mustRead(path string) []byte {
	data, err := os.ReadFile(path)
	if err != nil {
		panic(err)
	}
	return data
}
