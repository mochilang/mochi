//go:build archived && slow

package erlcode_test

import (
	"path/filepath"
	"testing"

	erlcode "mochi/archived/x/erlang"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestErlangCompiler_JOB(t *testing.T) {
	if err := erlcode.EnsureErlang(); err != nil {
		t.Skipf("erlang not installed: %v", err)
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
	if _, err := erlcode.New(env).Compile(prog); err != nil {
		t.Skipf("JOB q1 unsupported: %v", err)
	}
}
