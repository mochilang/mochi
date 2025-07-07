//go:build archived && slow

package excode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	excode "mochi/archived/x/ex"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestExCompiler_JOB_Golden(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 10; i++ {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "job", q+".mochi")
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
		wantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "ex", q+".ex.out")
		want, err := os.ReadFile(wantPath)
		if err != nil {
			t.Logf("missing golden %s", wantPath)
			continue
		}
		if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(want)) {
			t.Errorf("%s.ex.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(want))
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
		outWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "ex", q+".out")
		wantOut, err := os.ReadFile(outWantPath)
		if err != nil {
			t.Logf("missing golden %s", outWantPath)
			continue
		}
		if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
			t.Errorf("%s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
		}
	}
}
