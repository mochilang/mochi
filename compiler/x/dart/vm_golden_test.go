//go:build slow

package dart_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	dart "mochi/compiler/x/dart"
	"mochi/compiler/x/testutil"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func runVMGolden(t *testing.T, src, outDir string) ([]byte, error) {
	base := strings.TrimSuffix(filepath.Base(src), ".mochi")
	errFile := filepath.Join(outDir, base+".error")
	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(errFile, []byte(err.Error()), 0644)
		return nil, err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errFile, []byte(errs[0].Error()), 0644)
		return nil, errs[0]
	}
	code, err := dart.New(env).Compile(prog)
	if err != nil {
		_ = os.WriteFile(errFile, []byte(err.Error()), 0644)
		return nil, err
	}
	codePath := filepath.Join(outDir, base+".dart")
	if err := os.WriteFile(codePath, code, 0644); err != nil {
		return nil, err
	}
	cmd := exec.Command("dart", codePath)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(errFile, append([]byte(err.Error()+"\n"), out...), 0644)
		return nil, err
	}
	out = bytes.TrimSpace(out)
	_ = os.WriteFile(filepath.Join(outDir, base+".out"), out, 0644)
	_ = os.Remove(errFile)
	return out, nil
}

func TestDartCompiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("dart"); err != nil {
		t.Skip("dart not installed")
	}
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "dart")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		return runVMGolden(t, src, outDir)
	})
}
