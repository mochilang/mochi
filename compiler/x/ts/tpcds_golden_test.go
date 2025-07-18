//go:build slow

package tscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	tscode "mochi/compiler/x/ts"
	"mochi/parser"
	"mochi/types"
)

func shouldUpdateTPCDS() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

func runTPCDSQueryGolden(t *testing.T, q string) {
	root := repoRoot(t)
	script := exec.Command("go", "run", "-tags=archive,slow", "./scripts/compile_tpcds_ts.go")
	script.Env = append(os.Environ(), "GOTOOLCHAIN=local", "QUERIES="+q[1:])
	script.Dir = root
	if out, err := script.CombinedOutput(); err != nil {
		t.Fatalf("compile script error: %v\n%s", err, out)
	}

	errFile := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "ts", q+".error")
	if b, err := os.ReadFile(errFile); err == nil {
		t.Skipf("deno run failed:\n%s", b)
	}

	src := filepath.Join(root, "tests", "dataset", "tpc-ds", q+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := tscode.New(env, filepath.Base(src)).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	codePath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "ts", q+".ts")
	if shouldUpdateTPCDS() {
		_ = os.WriteFile(codePath, code, 0644)
	} else {
		_ = os.WriteFile(codePath, code, 0644)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.ts")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
       cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", "--allow-env", file)
	cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("deno run error: %v\n%s", err, out)
	}
	gotOut := bytes.TrimSpace(out)
	outWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "ts", q+".out")
	if shouldUpdateTPCDS() {
		_ = os.WriteFile(outWant, append(gotOut, '\n'), 0644)
	} else if wantOut, err := os.ReadFile(outWant); err == nil {
		if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
			t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", q, gotOut, bytes.TrimSpace(wantOut))
		}
	}
}

func TestTSCompiler_TPCDS_Golden(t *testing.T) {
	if err := tscode.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	for i := 1; i <= 99; i++ {
		q := fmt.Sprintf("q%d", i)
		if _, err := os.Stat(filepath.Join(repoRoot(t), "tests", "dataset", "tpc-ds", "compiler", "ts", q+".ts")); err != nil {
			continue
		}
		t.Run(q, func(t *testing.T) { runTPCDSQueryGolden(t, q) })
	}
}
