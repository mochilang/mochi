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

func TestExCompiler_TPCDSQueries(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	root := repoRoot(t)
	for i := 1; i <= 99; i++ {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-ds", q+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "ex", q+".ex.out")
		outWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "ex", q+".out")
		errFile := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "ex", q+".error")
		if _, err := os.Stat(codeWant); err != nil {
			continue
		}
		if _, err := os.Stat(outWant); err != nil {
			continue
		}
		t.Run(q, func(t *testing.T) {
			scriptCmd := exec.Command("go", "run", "-tags=slow,archive", "./scripts/compile_tpcds_ex.go")
			scriptCmd.Env = append(os.Environ(), "QUERIES="+fmt.Sprint(i))
			scriptCmd.Dir = root
			if out, err := scriptCmd.CombinedOutput(); err != nil {
				t.Fatalf("compile script error: %v\n%s", err, out)
			}
			if b, err := os.ReadFile(errFile); err == nil {
				t.Fatalf("elixir run failed:\n%s", b)
			}
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
			strip := func(b []byte) []byte {
				if i := bytes.IndexByte(b, '\n'); i >= 0 {
					return bytes.TrimSpace(b[i+1:])
				}
				return bytes.TrimSpace(b)
			}
			got := strip(code)
			want := strip(wantCode)
			if !bytes.Equal(got, want) {
				t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", q+".ex.out", got, want)
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
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", q, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}
