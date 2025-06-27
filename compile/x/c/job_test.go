//go:build slow

package ccode_test

import (
	"testing"

	ccode "mochi/compile/x/c"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCCompiler_JOB(t *testing.T) {
	if _, err := ccode.EnsureCC(); err != nil {
		t.Skipf("C compiler not installed: %v", err)
	}
	testutil.CompileJob(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return ccode.New(env).Compile(prog)
	})
}
