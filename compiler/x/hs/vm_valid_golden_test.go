//go:build slow

package hscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	hscode "mochi/compiler/x/hs"
	"mochi/compiler/x/testutil"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

// TestHSCompiler_VMValid_GoldenRun compiles each program and executes the
// generated Haskell code, comparing the output with the reference .out files.
func TestHSCompiler_VMValid_GoldenRun(t *testing.T) {
	if err := hscode.EnsureHaskell(); err != nil {
		t.Skipf("haskell not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "hs")
	if err := os.MkdirAll(outDir, 0755); err != nil {
		t.Fatalf("mkout: %v", err)
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
		code, err := hscode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		hsPath := filepath.Join(outDir, name+".hs")
		if err := os.WriteFile(hsPath, code, 0644); err != nil {
			return nil, err
		}
		cmd := exec.Command("runhaskell", hsPath)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		var out bytes.Buffer
		cmd.Stdout = &out
		cmd.Stderr = &out
		if err := cmd.Run(); err != nil {
			errPath := filepath.Join(outDir, name+".error")
			_ = os.WriteFile(errPath, out.Bytes(), 0644)
			return nil, fmt.Errorf("runhaskell error: %w", err)
		}
		if err := os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(out.Bytes()), 0644); err != nil {
			return nil, err
		}
		_ = os.Remove(filepath.Join(outDir, name+".error"))
		return bytes.TrimSpace(out.Bytes()), nil
	})
}
