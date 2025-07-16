//go:build slow

package scalacode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	scalacode "mochi/compiler/x/scala"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

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

func TestScalaCompilerGolden(t *testing.T) {
	golden.Run(t, "tests/human/x/scala", ".scala", ".scala", func(src string) ([]byte, error) {
		data, err := os.ReadFile(src)
		if err != nil {
			return nil, err
		}
		return bytes.TrimSpace(data), nil
	})
}
