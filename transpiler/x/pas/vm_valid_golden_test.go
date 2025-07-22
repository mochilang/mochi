//go:build slow

package pas_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	"mochi/golden"
	"mochi/parser"
	pas "mochi/transpiler/x/pas"
	"mochi/types"
)

func TestPascalTranspiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("fpc"); err != nil {
		t.Skip("fpc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pas")
	os.MkdirAll(outDir, 0o755)

	// clean previous error markers
	if files, _ := filepath.Glob(filepath.Join(outDir, "*.error")); len(files) > 0 {
		for _, f := range files {
			_ = os.Remove(f)
		}
	}

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".pas")
		outPath := filepath.Join(outDir, base+".out")
		errPath := filepath.Join(outDir, base+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
			return nil, errs[0]
		}
		ast, err := pas.Transpile(env, prog)
		if err != nil {
			_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
			return nil, err
		}
		code := ast.Emit()
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		exe := filepath.Join(outDir, base)
		if out, err := exec.Command("fpc", codePath, "-o"+exe).CombinedOutput(); err != nil {
			_ = os.WriteFile(errPath, append([]byte("compile: "+err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		cmd := exec.Command(exe)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		got := bytes.TrimSpace(out)
		if err != nil {
			_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		_ = os.Remove(errPath)
		_ = os.WriteFile(outPath, got, 0o644)
		return got, nil
	})

	if errs, _ := filepath.Glob(filepath.Join(outDir, "*.error")); len(errs) > 0 {
		sort.Strings(errs)
		t.Fatalf("first failing program: %s", strings.TrimSuffix(filepath.Base(errs[0]), ".error"))
	}
}
