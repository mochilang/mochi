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

// runTPCH compiles the given query and compares generated code and
// runtime output against the golden files under tests/dataset/tpc-h.
func runTPCH(t *testing.T, base string) {
	t.Helper()
	if _, err := exec.LookPath("deno"); err != nil {
		t.Skip("deno not installed")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-h", base+".mochi")
	codeWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "ts", base+".ts")
	outWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "ts", base+".out")

	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := tscode.New(env, root).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	if want, err := os.ReadFile(codeWant); err == nil {
		got := stripHeader(bytes.TrimSpace(code))
		want = stripHeader(bytes.TrimSpace(want))
		if !bytes.Equal(got, want) {
			t.Errorf("generated code mismatch for %s.ts\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, got, want)
		}
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.ts")
	if err := os.WriteFile(file, code, 0o644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", file)
	cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("deno run error: %v\n%s", err, out)
	}
	gotOut := bytes.TrimSpace(out)
	if wantOut, err := os.ReadFile(outWant); err == nil {
		if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
			t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotOut, bytes.TrimSpace(wantOut))
		}
	}
}

func TestTSCompiler_TPCH(t *testing.T) {
	for i := 1; i <= 22; i++ {
		base := fmt.Sprintf("q%d", i)
		if _, err := os.Stat(filepath.Join(repoRoot(t), "tests", "dataset", "tpc-h", base+".mochi")); err != nil {
			continue
		}
		t.Run(base, func(t *testing.T) { runTPCH(t, base) })
	}
}
