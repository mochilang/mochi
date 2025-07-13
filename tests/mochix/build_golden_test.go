//go:build slow

package mochix_test

import (
	"bytes"
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

func runMochix(t *testing.T, args ...string) ([]byte, error) {
	t.Helper()
	root := repoRoot(t)
	cmd := exec.Command("go", append([]string{"run", "-tags", "slow", "./cmd/mochix"}, args...)...)
	cmd.Dir = root
	cmd.Env = append(os.Environ(), "SOURCE_DATE_EPOCH=0")
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, err
	}
	return bytes.TrimSpace(out), nil
}

func TestBuildGo(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".go.out", func(src string) ([]byte, error) {
		return runMochix(t, "build", "--target", "go", src)
	})
}

func TestBuildTS(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".ts.out", func(src string) ([]byte, error) {
		return runMochix(t, "build", "--target", "ts", src)
	})
}

func TestBuildPy(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".py.out", func(src string) ([]byte, error) {
		return runMochix(t, "build", "--target", "py", src)
	})
}

func TestBuildXClj(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".clj.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "clj", src)
	})
}

func TestBuildXDart(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".dart", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "dart", src)
	})
}

func TestBuildXC(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".c.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "c", src)
	})
}

func TestBuildXJava(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".java.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "java", src)
	})
}
