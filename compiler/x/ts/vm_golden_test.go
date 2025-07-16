//go:build slow

package tscode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	tscode "mochi/compiler/x/ts"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

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
	t.Fatal("go.mod not found")
	return ""
}

// TestTSCompiler_VMValid_Golden compiles programs under tests/vm/valid to
// TypeScript, executes them with Deno and compares runtime output with the
// golden .out files. Generated code and error logs are written under
// tests/machine/x/ts.
func TestTSCompiler_VMValid_Golden(t *testing.T) {
	if err := tscode.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := findRepoRootVM(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "ts")
	os.MkdirAll(outDir, 0o755)

	golden.Run(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
		os.Setenv("SOURCE_DATE_EPOCH", "0")
		defer os.Unsetenv("MOCHI_HEADER_TIME")
		defer os.Unsetenv("SOURCE_DATE_EPOCH")
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("parse: "+err.Error()), 0o644)
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("type: "+errs[0].Error()), 0o644)
			return nil, errs[0]
		}
		code, err := tscode.New(env, filepath.Dir(src)).Compile(prog)
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("compile: "+err.Error()), 0o644)
			return nil, err
		}
		tsFile := filepath.Join(outDir, base+".ts")
		if err := os.WriteFile(tsFile, code, 0o644); err != nil {
			return nil, err
		}
		cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", tsFile)
		cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
		cmd.Dir = filepath.Join(filepath.Dir(src), "..")
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), out, 0o644)
			return nil, err
		}
		out = bytes.TrimSpace(out)
		_ = os.WriteFile(filepath.Join(outDir, base+".out"), append(out, '\n'), 0o644)
		_ = os.Remove(filepath.Join(outDir, base+".error"))
		return out, nil
	})
}
