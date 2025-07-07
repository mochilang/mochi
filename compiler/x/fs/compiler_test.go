//go:build slow

package fscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	fscode "mochi/compiler/x/fs"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

// repoRoot locates the repository root directory containing go.mod.
func repoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found")
	return ""
}

func TestFSCompiler(t *testing.T) {
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "fs")
	if err := os.MkdirAll(outDir, 0755); err != nil {
		t.Fatal(err)
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

		code, err := fscode.CompileFile(src)
		if err != nil {
			// write error file and return error
			name := strings.TrimSuffix(filepath.Base(src), filepath.Ext(src))
			errPath := filepath.Join(outDir, name+".error")
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			return nil, err
		}

		name := strings.TrimSuffix(filepath.Base(src), filepath.Ext(src))
		fsPath := filepath.Join(outDir, name+".fs")
		if err := os.WriteFile(fsPath, code, 0644); err != nil {
			return nil, err
		}

		exePath := filepath.Join(outDir, name+".exe")
		cmd := exec.Command("fsharpc", "--target:exe", fmt.Sprintf("--out:%s", exePath), fsPath)
		out, err := cmd.CombinedOutput()
		if err != nil {
			errPath := filepath.Join(outDir, name+".error")
			os.WriteFile(errPath, out, 0644)
			return nil, fmt.Errorf("fsharpc error: %w", err)
		}

		run := exec.Command("mono", exePath)
		var stdout bytes.Buffer
		run.Stdout = &stdout
		run.Stderr = &stdout
		if err := run.Run(); err != nil {
			errPath := filepath.Join(outDir, name+".error")
			os.WriteFile(errPath, stdout.Bytes(), 0644)
			return nil, fmt.Errorf("mono error: %w", err)
		}
		outPath := filepath.Join(outDir, name+".out")
		os.WriteFile(outPath, bytes.TrimSpace(stdout.Bytes()), 0644)
		return bytes.TrimSpace(stdout.Bytes()), nil
	})
}
