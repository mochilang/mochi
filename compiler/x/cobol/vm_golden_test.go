//go:build slow

package cobol_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	cobol "mochi/compiler/x/cobol"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestCobolCompiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("cobc"); err != nil {
		t.Skip("cobc not installed")
	}
	root := findRepoRootVM(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "cobol")
	os.MkdirAll(outDir, 0o755)

	golden.Run(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".cob")
		outPath := filepath.Join(outDir, base+".out")
		errPath := filepath.Join(outDir, base+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		code, err := cobol.New(env).Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte("compile: "+err.Error()), 0o644)
			return nil, fmt.Errorf("compile error: %w", err)
		}
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "prog.cob")
		if err := os.WriteFile(file, code, 0o644); err != nil {
			return nil, err
		}
		bin := filepath.Join(dir, "prog")
		if out, err := exec.Command("cobc", "-free", "-std=cobol2002", "-x", "-o", bin, file).CombinedOutput(); err != nil {
			os.WriteFile(errPath, out, 0o644)
			return nil, fmt.Errorf("cobc: %w", err)
		}
		outBytes, err := exec.Command(bin).CombinedOutput()
		if err != nil {
			os.WriteFile(errPath, outBytes, 0o644)
			return nil, fmt.Errorf("run: %w", err)
		}
		os.Remove(errPath)
		outBytes = bytes.TrimSpace(outBytes)
		os.WriteFile(outPath, outBytes, 0o644)
		return outBytes, nil
	})
}

func findRepoRootVM(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
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
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}
