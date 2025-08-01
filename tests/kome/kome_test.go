//go:build slow

package kome_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/golden"
)

func repoRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
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

func compileKome(t *testing.T, src string) ([]byte, []byte, error) {
	t.Helper()
	root := repoRoot(t)
	tmpDir := t.TempDir()
	outName := filepath.Join(tmpDir, "hello")
	cmd := exec.Command("go", "run", "./tools/kome", "-c", "-o", outName, src)
	cmd.Dir = root
	cmd.Env = append(os.Environ(), "SOURCE_DATE_EPOCH=0")
	if out, err := cmd.CombinedOutput(); err != nil {
		return nil, nil, fmt.Errorf("kome: %w\n%s", err, out)
	}
	code, err := os.ReadFile(outName + ".go")
	if err != nil {
		return nil, nil, err
	}
	tagged := append([]byte("//go:build slow && ignore\n\n"), code...)
	if err := os.WriteFile(outName+".go", tagged, 0644); err != nil {
		return nil, nil, err
	}
	runOut, err := exec.Command("go", "run", "-tags", "slow", outName+".go").CombinedOutput()
	if err != nil {
		return tagged, nil, fmt.Errorf("run: %w\n%s", err, runOut)
	}
	return tagged, bytes.TrimSpace(runOut), nil
}

func TestKomeCompile(t *testing.T) {
	golden.Run(t, "tests/kome", ".kome", ".go", func(src string) ([]byte, error) {
		code, _, err := compileKome(t, src)
		return code, err
	})
}

func TestKomeRun(t *testing.T) {
	golden.Run(t, "tests/kome", ".kome", ".out", func(src string) ([]byte, error) {
		_, out, err := compileKome(t, src)
		return out, err
	})
}
