//go:build slow

package fscode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	fscode "mochi/compile/x/fs"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestFSCompiler_TPCDS(t *testing.T) {
	if err := fscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	if err := fscode.EnsureFantomas(); err != nil {
		t.Skipf("fantomas not installed: %v", err)
	}
	q := "q1"
	t.Run(q, func(t *testing.T) {
		root := testutil.FindRepoRoot(t)
		src := filepath.Join(root, "tests", "dataset", "tpc-ds", q+".mochi")
		prog, err := parser.Parse(src)
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
		codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "fs", q+".fs.out")
		wantCode, err := os.ReadFile(codeWantPath)
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
			t.Errorf("generated code mismatch for %s.fs.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.fsx")
		if err := os.WriteFile(file, code, 0644); err != nil {
			t.Fatalf("write error: %v", err)
		}
		cmd := exec.Command("dotnet", "fsi", "--quiet", file)
		out, err := cmd.CombinedOutput()
		if err != nil {
			t.Fatalf("fsi error: %v\n%s", err, out)
		}
		gotOut := bytes.TrimSpace(out)
		outWantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "fs", q+".out")
		wantOut, err := os.ReadFile(outWantPath)
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
			t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
		}
	})
}
