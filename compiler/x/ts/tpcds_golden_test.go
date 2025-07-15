//go:build slow

package tscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/compiler/x/testutil"
	tscode "mochi/compiler/x/ts"
	"mochi/parser"
	"mochi/types"
)

func TestTSCompiler_TPCDSQueries(t *testing.T) {
	if err := tscode.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 99; i++ {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-ds", q+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "ts", q+".ts")
		outWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "out", q+".out")
		errFile := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "ts", q+".error")
		if _, err := os.Stat(src); err != nil {
			continue
		}
		t.Run(q, func(t *testing.T) {
			scriptCmd := exec.Command("go", "run", "-tags=slow,archive", "./scripts/compile_tpcds_ts.go")
			scriptCmd.Env = append(os.Environ(), "QUERIES="+fmt.Sprint(i))
			scriptCmd.Dir = root
			if out, err := scriptCmd.CombinedOutput(); err != nil {
				t.Fatalf("compile script error: %v\n%s", err, out)
			}
			if b, err := os.ReadFile(errFile); err == nil {
				t.Fatalf("deno run failed:\n%s", b)
			}
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := tscode.New(env, "").Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			got := stripHeader(bytes.TrimSpace(code))
			want := stripHeader(bytes.TrimSpace(wantCode))
			if !bytes.Equal(got, want) {
				t.Errorf("generated code mismatch for %s.ts\n\n--- Got ---\n%s\n\n--- Want ---\n%s", q, got, want)
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
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", q, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}
