//go:build archived && slow

package javacode_test

import (
	"testing"

	javacode "mochi/archived/x/java"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestJavaCompiler_JOB(t *testing.T) {
	if err := javacode.EnsureJavac(); err != nil {
		t.Skipf("javac not installed: %v", err)
	}
	for _, q := range []string{"q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10"} {
		testutil.CompileJOB(t, q, func(env *types.Env, prog *parser.Program) ([]byte, error) {
			return javacode.New(env).Compile(prog)
		})
	}
}
