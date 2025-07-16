//go:build slow

package pl_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	pl "mochi/compiler/x/pl"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestPrologCompiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := findRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "pl")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatal(err)
	}

	golden.Run(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte(err.Error()), 0o644)
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte(errs[0].Error()), 0o644)
			return nil, errs[0]
		}
		code, err := pl.New().Compile(prog)
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte(err.Error()), 0o644)
			return nil, err
		}
		codePath := filepath.Join(outDir, base+".pl")
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		cmd := exec.Command("swipl", "-q", codePath)
		if inData, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(inData)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), append([]byte(err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		out = bytes.TrimSpace(out)
		if err := os.WriteFile(filepath.Join(outDir, base+".out"), out, 0o644); err != nil {
			return nil, err
		}
		os.Remove(filepath.Join(outDir, base+".error"))
		return out, nil
	})
}

func findRepoRoot(t *testing.T) string {
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
