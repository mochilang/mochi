//go:build slow

package racket_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"testing"

	rack "mochi/compiler/x/racket"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

// TestRacketCompiler_VM_Code_Golden compiles each program under tests/vm/valid
// to Racket source and compares with the .rkt.out golden files.
func TestRacketCompiler_VM_Code_Golden(t *testing.T) {
	if err := rack.EnsureRacket(); err != nil {
		t.Skipf("racket not installed: %v", err)
	}
	golden.Run(t, "tests/vm/valid", ".mochi", ".rkt.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		code, err := rack.New().Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		return code, nil
	})
}

// TestRacketCompiler_VM_Run_Golden compiles and executes each program under
// tests/vm/valid and compares the runtime output with the .out files.
func TestRacketCompiler_VM_Run_Golden(t *testing.T) {
	if err := rack.EnsureRacket(); err != nil {
		t.Skipf("racket not installed: %v", err)
	}
	golden.Run(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		code, err := rack.New().Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		tmp, err := os.CreateTemp("", "mochi_*.rkt")
		if err != nil {
			return nil, err
		}
		defer os.Remove(tmp.Name())
		if _, err := tmp.Write(code); err != nil {
			return nil, err
		}
		tmp.Close()
		cmd := exec.Command("racket", tmp.Name())
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return out, fmt.Errorf("run error: %w", err)
		}
		return bytes.TrimSpace(out), nil
	})
}
