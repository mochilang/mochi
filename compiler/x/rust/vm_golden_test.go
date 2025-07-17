//go:build slow

package rustcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	rustcode "mochi/compiler/x/rust"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

// TestRustCompiler_VMValid_Golden compiles programs under tests/vm/valid
// to Rust, executes them with rustc and compares output against golden files
// under tests/machine/x/rust. Generated source code is written to the same
// directory but not compared.
func TestRustCompiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("rustc"); err != nil {
		t.Skip("rustc not installed")
	}
	root := findRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "rust")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".rs")
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
		t.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
		t.Setenv("SOURCE_DATE_EPOCH", "0")
		code, err := rustcode.New(env).Compile(prog)
		t.Setenv("MOCHI_HEADER_TIME", "")
		t.Setenv("SOURCE_DATE_EPOCH", "")
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		tmp := t.TempDir()
		bin := filepath.Join(tmp, base)
		if out, err := exec.Command("rustc", codePath, "-O", "-o", bin).CombinedOutput(); err != nil {
			os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		outBytes, err := exec.Command(bin).CombinedOutput()
		if err != nil {
			os.WriteFile(errPath, append([]byte(err.Error()+"\n"), outBytes...), 0o644)
			return nil, err
		}
		outBytes = bytes.TrimSpace(outBytes)
		os.WriteFile(filepath.Join(outDir, base+".out"), outBytes, 0o644)
		os.Remove(errPath)
		return outBytes, nil
	})
}

func findRepoRoot(t *testing.T) string {
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
