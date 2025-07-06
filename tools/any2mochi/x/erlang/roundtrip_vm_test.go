//go:build slow

package erlang

import (
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	erlcode "mochi/compile/x/erlang"
	"mochi/parser"
	any2mochi "mochi/tools/any2mochi"
	"mochi/types"
)

func TestErlangRoundtripVM(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	if _, err := exec.LookPath("escript"); err != nil {
		files, _ := filepath.Glob(filepath.Join(root, "tests/vm/valid", "*.mochi"))
		status := make(map[string]string)
		for _, f := range files {
			name := strings.TrimSuffix(filepath.Base(f), filepath.Ext(f))
			status[name] = "escript not found"
		}
		any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests/any2mochi/erlang_vm"), status)
		t.Skipf("escript not found: %v", err)
	}
	compileFn := func(prog *parser.Program, env *types.Env) ([]byte, error) {
		c := erlcode.New(env)
		return c.Compile(prog)
	}
	convertFn := func(path string) ([]byte, error) {
		return ConvertFile(path)
	}
	status := any2mochi.RunCompileConvertRunStatus(t, filepath.Join(root, "tests/vm/valid"), "*.mochi", compileFn, convertFn, "erl")
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests/any2mochi/erlang_vm"), status)
}
