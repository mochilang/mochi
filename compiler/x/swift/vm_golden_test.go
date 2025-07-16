package swift_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	swift "mochi/compiler/x/swift"
	"mochi/compiler/x/testutil"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func compileValid(t *testing.T, swiftExe, src, outDir string) ([]byte, error) {
	data, err := os.ReadFile(src)
	if err != nil {
		return nil, err
	}
	name := strings.TrimSuffix(filepath.Base(src), filepath.Ext(src))
	errPath := filepath.Join(outDir, name+".error")
	os.Remove(errPath)

	prog, err := parser.Parse(src)
	if err != nil {
		writeError(outDir, name, data, err)
		return nil, err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeError(outDir, name, data, errs[0])
		return nil, errs[0]
	}
	code, err := swift.New(env).Compile(prog)
	if err != nil {
		writeError(outDir, name, data, err)
		return nil, err
	}
	swiftFile := filepath.Join(outDir, name+".swift")
	if err := os.WriteFile(swiftFile, code, 0644); err != nil {
		return nil, err
	}
	cmd := exec.Command(swiftExe, swiftFile)
	if in, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(in)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		writeRunError(outDir, name, swiftFile, out, err)
		return nil, err
	}
	out = bytes.TrimSpace(out)
	os.WriteFile(filepath.Join(outDir, name+".out"), out, 0644)
	return out, nil
}

func TestSwiftCompiler_VMGolden(t *testing.T) {
	swiftExe := ensureSwift(t)
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "swift")
	os.MkdirAll(outDir, 0755)
	golden.Run(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		return compileValid(t, swiftExe, src, outDir)
	})
}
