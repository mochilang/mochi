//go:build archived && slow

package asm_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	asm "mochi/archived/x/asm"
	ccode "mochi/archived/x/c"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

// TestAsmCompiler_SubsetPrograms compiles programs to assembly and runs them.
func TestAsmCompiler_SubsetPrograms(t *testing.T) {
	cc, err := ccode.EnsureCC()
	if err != nil {
		t.Skipf("C compiler not installed: %v", err)
	}
	run := func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := asm.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		sfile := filepath.Join(dir, "prog.s")
		if err := os.WriteFile(sfile, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		bin := filepath.Join(dir, "prog")
		if out, err := exec.Command(cc, sfile, "-o", bin).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("❌ cc error: %w\n%s", err, out)
		}
		cmd := exec.Command(bin)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	}
	golden.Run(t, "tests/compiler/valid", ".mochi", ".out", run)
	golden.Run(t, "tests/compiler/asm", ".mochi", ".out", run)
}

// TestAsmCompiler_GoldenOutput verifies that the generated assembly matches
// checked-in golden files.
func TestAsmCompiler_GoldenOutput(t *testing.T) {
	if _, err := ccode.EnsureCC(); err != nil {
		t.Skipf("C compiler not installed: %v", err)
	}
	compileAsm := func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c := asm.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	}

	golden.Run(t, "tests/compiler/asm", ".mochi", ".s.out", compileAsm)
}
