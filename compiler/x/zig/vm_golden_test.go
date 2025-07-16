//go:build slow

package zigcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/compiler/x/testutil"
	zigcode "mochi/compiler/x/zig"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func writeError(path string, src []byte, err error) {
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "%v\n", err)
	buf.Write(src)
	_ = os.WriteFile(path, buf.Bytes(), 0o644)
}

func TestZigCompiler_VMValid_Golden(t *testing.T) {
	zigc, err := zigcode.EnsureZig()
	if err != nil {
		t.Skipf("zig not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "zig")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatal(err)
	}
	golden.Run(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		data, err := os.ReadFile(src)
		if err != nil {
			return nil, fmt.Errorf("read src: %w", err)
		}
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			writeError(filepath.Join(outDir, name+".error"), data, err)
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			writeError(filepath.Join(outDir, name+".error"), data, errs[0])
			return nil, errs[0]
		}
		code, err := zigcode.New(env).Compile(prog)
		if err != nil {
			writeError(filepath.Join(outDir, name+".error"), data, err)
			return nil, err
		}
		codePath := filepath.Join(outDir, name+".zig")
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		exe := filepath.Join(outDir, name)
		if out, err := exec.Command(zigc, "build-exe", codePath, "-O", "ReleaseSafe", "-femit-bin="+exe).CombinedOutput(); err != nil {
			writeError(filepath.Join(outDir, name+".error"), code, fmt.Errorf("zig build error: %w\n%s", err, out))
			return nil, err
		}
		cmd := exec.Command(exe)
		if inData, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(inData)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			writeError(filepath.Join(outDir, name+".error"), code, fmt.Errorf("run error: %w\n%s", err, out))
			return nil, err
		}
		out = bytes.TrimSpace(out)
		if err := os.WriteFile(filepath.Join(outDir, name+".out"), out, 0o644); err != nil {
			return nil, err
		}
		os.Remove(filepath.Join(outDir, name+".error"))
		return out, nil
	})
}
