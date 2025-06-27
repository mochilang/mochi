//go:build slow

package javacode_test

import (
	"testing"

	javacode "mochi/compile/x/java"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestJavaCompiler_JOB(t *testing.T) {
	if err := javacode.EnsureJavac(); err != nil {
		t.Skipf("javac not installed: %v", err)
	}
	for _, q := range []string{"q1", "q2"} {
		testutil.CompileJOB(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return javacode.New(env).Compile(prog)
		})
	}
}
