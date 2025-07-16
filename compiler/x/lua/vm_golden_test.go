//go:build slow

package luacode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	luacode "mochi/compiler/x/lua"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestLuaCompiler_VMValid_Golden(t *testing.T) {
	if err := luacode.EnsureLua(); err != nil {
		t.Skipf("lua not installed: %v", err)
	}
	root := findRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "lua")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatal(err)
	}

	golden.Run(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		data, err := os.ReadFile(src)
		if err != nil {
			return nil, fmt.Errorf("read src: %w", err)
		}
		prog, err := parser.Parse(src)
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		if err != nil {
			errPath := filepath.Join(outDir, name+".error")
			writeError(errPath, data, err)
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			errPath := filepath.Join(outDir, name+".error")
			writeError(errPath, data, errs[0])
			return nil, errs[0]
		}
		code, err := luacode.New(env).Compile(prog)
		if err != nil {
			errPath := filepath.Join(outDir, name+".error")
			writeError(errPath, data, err)
			return nil, err
		}
		codePath := filepath.Join(outDir, name+".lua")
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		cmd := exec.Command("lua", codePath)
		if inData, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(inData)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			errPath := filepath.Join(outDir, name+".error")
			writeError(errPath, data, fmt.Errorf("run error: %w\n%s", err, out))
			return nil, err
		}
		out = bytes.TrimSpace(out)
		outFile := filepath.Join(outDir, name+".out")
		if err := os.WriteFile(outFile, out, 0o644); err != nil {
			return nil, err
		}
		os.Remove(filepath.Join(outDir, name+".error"))
		return out, nil
	})
}
