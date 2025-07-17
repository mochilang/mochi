//go:build slow

package ccode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	ccode "mochi/compiler/x/c"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

// TestCCompiler_VMValid_Golden compiles programs under tests/vm/valid
// to C, executes them with the system C compiler and compares the
// generated code and output against golden files under tests/machine/x/c.
func TestCCompiler_VMValid_Golden(t *testing.T) {
	cc, err := ccode.EnsureCC()
	if err != nil {
		t.Skipf("C compiler not installed: %v", err)
	}
	root := findRepoRootVM(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "c")
	os.MkdirAll(outDir, 0o755)

    golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".c")
		outPath := filepath.Join(outDir, base+".out")
		errPath := filepath.Join(outDir, base+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
			return nil, errs[0]
		}
		os.Setenv("SOURCE_DATE_EPOCH", "1136214245")
		code, err := ccode.New(env).Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		tmp := t.TempDir()
		exe := filepath.Join(tmp, base)
		if out, err := exec.Command(cc, codePath, "-o", exe).CombinedOutput(); err != nil {
			os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		cmd := exec.Command(exe)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		var buf bytes.Buffer
		cmd.Stdout = &buf
		cmd.Stderr = &buf
		if err := cmd.Run(); err != nil {
			os.WriteFile(errPath, append([]byte(err.Error()+"\n"), buf.Bytes()...), 0o644)
			return nil, err
		}
		outBytes := bytes.TrimSpace(buf.Bytes())
		os.WriteFile(outPath, outBytes, 0o644)
		os.Remove(errPath)
		return outBytes, nil
	})
}

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
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}
