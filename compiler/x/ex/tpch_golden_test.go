//go:build slow

package excode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	excode "mochi/compiler/x/ex"
	"mochi/parser"
	"mochi/types"
)

func repoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
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

func TestExCompiler_TPCHQueries(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	root := repoRoot(t)
	for i := 1; i <= 2; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-h", base+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "ex", base+".ex.out")
		outWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "ex", base+".out")
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
			code, err := excode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.ex.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.exs")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("elixir", file)
			cmd.Dir = root
			var stdout, stderr bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = &stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("elixir run error: %v\n%s", err, stderr.Bytes())
			}
			gotOut := bytes.TrimSpace(stdout.Bytes())
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
