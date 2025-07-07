//go:build slow

package java

import (
	"path/filepath"
	"testing"

	javacode "mochi/archived/x/java"
	"mochi/parser"
	any2mochi "mochi/archived/tools/any2mochi"
	"mochi/types"
)

// compileMochiToJava compiles a Mochi source file into Java code.
func compileMochiToJava(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, errs[0]
	}
	return javacode.New(env).Compile(prog)
}

func TestJava_VM_RoundTrip(t *testing.T) {
	if err := javacode.EnsureJavac(); err != nil {
		t.Skipf("javac not installed: %v", err)
	}
	root := any2mochi.FindRepoRoot(t)
	status := any2mochi.RunCompileConvertRunStatus(
		t,
		filepath.Join(root, "tests/vm/valid"),
		"*.mochi",
		compileMochiToJava,
		ConvertFile,
		"java",
	)
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests/any2mochi/java_vm"), status)
}
