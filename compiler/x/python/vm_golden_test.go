//go:build slow

package pycode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	pycode "mochi/compiler/x/python"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestPythonCompiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}
	root := findRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "python")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("parse: "+err.Error()), 0o644)
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("type: "+errs[0].Error()), 0o644)
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		c := pycode.New(env)
		c.SetTypeHints(false)
		code, err := c.Compile(prog)
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("compile: "+err.Error()), 0o644)
			return nil, fmt.Errorf("compile error: %w", err)
		}
		pyFile := filepath.Join(outDir, base+".py")
		if err := os.WriteFile(pyFile, code, 0o644); err != nil {
			return nil, err
		}
		cmd := exec.Command("python3", pyFile)
		cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), out, 0o644)
			return nil, fmt.Errorf("run error: %w", err)
		}
		out = bytes.TrimSpace(out)
		_ = os.WriteFile(filepath.Join(outDir, base+".out"), out, 0o644)
		_ = os.Remove(filepath.Join(outDir, base+".error"))
		return out, nil
	})
}
