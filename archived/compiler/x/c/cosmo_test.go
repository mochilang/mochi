//go:build archived && cosmo && libcosmo && slow

package ccode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	ccode "mochi/archived/x/c"
	"mochi/golden"
	"mochi/parser"
	"mochi/tools/cosmo"
	"mochi/types"
)

func compileRun(t *testing.T, code []byte, stdin []byte) ([]byte, error) {
	dir := t.TempDir()
	src := filepath.Join(dir, "prog.c")
	if err := os.WriteFile(src, code, 0644); err != nil {
		return nil, err
	}
	bin := filepath.Join(dir, "prog")
	if err := cosmo.CompileToFile(string(code), bin); err != nil {
		return nil, fmt.Errorf("cosmo: %w", err)
	}
	cmd := exec.Command(bin)
	if stdin != nil {
		cmd.Stdin = bytes.NewReader(stdin)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("run error: %w\n%s", err, out)
	}
	return bytes.TrimSpace(out), nil
}

func TestCosmoSubsetPrograms(t *testing.T) {
	if err := cosmo.EnsureCosmo(); err != nil {
		t.Skipf("cosmo not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c := ccode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		var stdin []byte
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			stdin = data
		}
		return compileRun(t, code, stdin)
	})
	golden.Run(t, "tests/compiler/c", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c := ccode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		var stdin []byte
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			stdin = data
		}
		return compileRun(t, code, stdin)
	})
}
