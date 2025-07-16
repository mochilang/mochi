//go:build slow

package cpp_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	cpp "mochi/compiler/x/cpp"
	"mochi/compiler/x/testutil"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestCPPCompiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("g++"); err != nil {
		t.Skip("g++ not installed")
	}
	os.Setenv("SOURCE_DATE_EPOCH", "1136214245")
	defer os.Unsetenv("SOURCE_DATE_EPOCH")
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "cpp")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, name+".cpp")
		outPath := filepath.Join(outDir, name+".out")
		errPath := filepath.Join(outDir, name+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0644)
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0644)
			return nil, errs[0]
		}
		code, err := cpp.New().Compile(prog)
		if err != nil {
			_ = os.WriteFile(errPath, []byte("compile: "+err.Error()), 0644)
			return nil, err
		}
		if err := os.WriteFile(codePath, code, 0644); err != nil {
			return nil, err
		}
		bin := filepath.Join(outDir, name)
		if out, err := exec.Command("g++", codePath, "-std=c++17", "-o", bin).CombinedOutput(); err != nil {
			_ = os.WriteFile(errPath, out, 0644)
			return nil, fmt.Errorf("g++ error: %w", err)
		}
		defer os.Remove(bin)
		cmd := exec.Command(bin)
		if inData, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(inData)
		}
		outBytes, err := cmd.CombinedOutput()
		if err != nil {
			_ = os.WriteFile(errPath, outBytes, 0644)
			return nil, fmt.Errorf("run error: %w", err)
		}
		outBytes = bytes.TrimSpace(outBytes)
		os.WriteFile(outPath, outBytes, 0644)
		os.Remove(errPath)
		return outBytes, nil
	})
}
