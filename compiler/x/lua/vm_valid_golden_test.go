//go:build slow

package luacode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	luacode "mochi/compiler/x/lua"
	testutil "mochi/compiler/x/testutil"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestLuaCompiler_VMValid_Golden(t *testing.T) {
	if err := luacode.EnsureLua(); err != nil {
		t.Skipf("lua not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "lua")
	os.MkdirAll(outDir, 0755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".lua")
		outPath := filepath.Join(outDir, base+".out")
		errPath := filepath.Join(outDir, base+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(errPath, []byte(errs[0].Error()), 0644)
			return nil, errs[0]
		}
		os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
		os.Setenv("SOURCE_DATE_EPOCH", "1136214245")
		code, err := luacode.New(env).Compile(prog)
		os.Unsetenv("MOCHI_HEADER_TIME")
		os.Unsetenv("SOURCE_DATE_EPOCH")
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			return nil, err
		}
		if err := os.WriteFile(codePath, code, 0644); err != nil {
			return nil, err
		}
		cmd := exec.Command("lua", codePath)
		if in, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(in)
		}
		var buf bytes.Buffer
		cmd.Stdout = &buf
		cmd.Stderr = &buf
		if err := cmd.Run(); err != nil {
			os.WriteFile(errPath, append([]byte(err.Error()+"\n"), buf.Bytes()...), 0644)
			return nil, err
		}
		outBytes := bytes.TrimSpace(buf.Bytes())
		os.WriteFile(outPath, append(outBytes, '\n'), 0644)
		os.Remove(errPath)
		return outBytes, nil
	})
}
