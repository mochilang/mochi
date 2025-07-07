//go:build archived && slow

package asm_test

import (
	"testing"

	asm "mochi/archived/x/asm"
	ccode "mochi/archived/x/c"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestAsmCompiler_TPCH(t *testing.T) {
	if _, err := ccode.EnsureCC(); err != nil {
		t.Skipf("C compiler not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return asm.New(env).Compile(prog)
	})
}
