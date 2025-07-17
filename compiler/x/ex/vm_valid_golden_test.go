//go:build slow

package excode_test

import (
	"bytes"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	excode "mochi/compiler/x/ex"
	testutil "mochi/compiler/x/testutil"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func goldenShouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func TestExCompiler_VMValid_Golden(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "ex")
	os.MkdirAll(outDir, 0755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".exs")
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
		code, err := excode.New(env).Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			return nil, err
		}
		if err := os.WriteFile(codePath, code, 0644); err != nil {
			return nil, err
		}
		cmd := exec.Command("elixir", codePath)
		cmd.Dir = root
		var buf bytes.Buffer
		cmd.Stdout = &buf
		cmd.Stderr = &buf
		if err := cmd.Run(); err != nil {
			os.WriteFile(errPath, append([]byte(err.Error()+"\n"), buf.Bytes()...), 0644)
			return nil, err
		}
		outBytes := bytes.TrimSpace(buf.Bytes())
		os.WriteFile(outPath, outBytes, 0644)
		os.Remove(errPath)
		// only compare runtime output, not generated code
		return outBytes, nil
	})

	if goldenShouldUpdate() {
		t.Log("golden files updated")
	}
}
