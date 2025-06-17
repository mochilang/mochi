package rscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	rscode "mochi/compile/rust"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestRustCompiler_SubsetPrograms(t *testing.T) {
	if err := rscode.EnsureRust(); err != nil {
		t.Skipf("rust not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/rust", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c := rscode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.rs")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		exe := filepath.Join(dir, "main")
		if out, err := exec.Command("rustc", file, "-O", "-o", exe).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("\u274c rustc error: %w\n%s", err, out)
		}
		cmd := exec.Command(exe)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("\u274c run error: %w\n%s", err, out)
		}
		res := bytes.TrimSpace(out)
		if res == nil {
			res = []byte{}
		}
		return res, nil
	})
}

func TestRustCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/rust", ".mochi", ".rs.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c := rscode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}
