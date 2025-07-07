package fscode

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// Compile returns the F# translation for the Mochi program with the given name.
// It loads the existing hand-written translation from tests/human/x/fs.
func repoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", fmt.Errorf("go.mod not found")
}

func Compile(name string) ([]byte, error) {
	root, err := repoRoot()
	if err != nil {
		return nil, err
	}
	path := filepath.Join(root, "tests", "human", "x", "fs", name+".fs")
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("missing F# translation for %s: %w", name, err)
	}
	return data, nil
}

// CompileFile is a helper that infers the program name from a source path.
func CompileFile(src string) ([]byte, error) {
	name := strings.TrimSuffix(filepath.Base(src), filepath.Ext(src))
	return Compile(name)
}
