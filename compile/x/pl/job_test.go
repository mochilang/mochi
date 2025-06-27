//go:build slow

package plcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	plcode "mochi/compile/x/pl"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestPrologCompiler_JOBQ1(t *testing.T) {
	if err := plcode.EnsureSWIPL(); err != nil {
		t.Skipf("swipl not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "job", "q1.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := plcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	wantCode, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "job", "compiler", "pl", "q1.pl.out"))
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q1.pl.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(wantCode))
	}
	_ = exec.Command("true").Run() // placeholder to ensure runtime tools available
}
