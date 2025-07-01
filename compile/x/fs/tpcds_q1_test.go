//go:build slow

package fscode_test

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"

	fscode "mochi/compile/x/fs"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestFSCompiler_TPCDSQ1(t *testing.T) {
	if err := fscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	if home, err := os.UserHomeDir(); err == nil {
		os.Setenv("PATH", os.Getenv("PATH")+":"+filepath.Join(home, ".dotnet", "tools"))
	}
	if err := fscode.EnsureFantomas(); err != nil {
		t.Skipf("fantomas not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-ds", "q1.mochi")
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
	wantCodePath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "fs", "q1.fs.out")
	if want, err := os.ReadFile(wantCodePath); err == nil {
		if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(want)) {
			t.Errorf("generated code mismatch for q1.fs.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(want))
		}
	}
}
