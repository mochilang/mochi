//go:build archived && slow

package javacode_test

import (
	"testing"

	javacode "mochi/archived/x/java"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestJavaCompiler_TPCH(t *testing.T) {
	if err := javacode.EnsureJavac(); err != nil {
		t.Skipf("javac not installed: %v", err)
	}
	testutil.CompileTPCH(t, "q1", func(env *types.Env, prog *parser.Program) ([]byte, error) {
		return javacode.New(env).Compile(prog)
	})
}
