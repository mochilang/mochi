//go:build slow

package erlang

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	erlcode "mochi/archived/x/erlang"
	"mochi/parser"
	any2mochi "mochi/archived/tools/any2mochi"
	"mochi/types"
)

func compileMochiToErl(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	code, err := erlcode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	return code, nil
}

func TestErlangRoundtripVMValid(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skipf("erlang not installed: %v", err)
	}
	tmp := t.TempDir()
	fake := filepath.Join(tmp, "erlfmt")
	if err := os.WriteFile(fake, []byte("#!/bin/sh\ncat"), 0755); err == nil {
		oldPath := os.Getenv("PATH")
		os.Setenv("PATH", tmp+":"+oldPath)
		t.Cleanup(func() { os.Setenv("PATH", oldPath) })
	}

	root := any2mochi.FindRepoRoot(t)
	status := any2mochi.RunCompileConvertRunStatus(
		t,
		filepath.Join(root, "tests/vm/valid"),
		"*.mochi",
		compileMochiToErl,
		ConvertFile,
		"erl",
	)
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests/any2mochi/erl_vm"), status)
}
