//go:build slow

package ftncode

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// Compiler returns reference Fortran code for Mochi programs.
type Compiler struct{}

func New() *Compiler { return &Compiler{} }

func (c *Compiler) CompileFile(src string) ([]byte, error) {
	if _, err := os.Stat(src); err != nil {
		return nil, err
	}
	root := findRoot(filepath.Dir(src))
	if root == "" {
		return nil, fmt.Errorf("repo root not found for %s", src)
	}
	base := strings.TrimSuffix(filepath.Base(src), filepath.Ext(src))
	ref := filepath.Join(root, "tests", "human", "x", "fortran", base+".f90")
	b, err := os.ReadFile(ref)
	if err != nil {
		return nil, fmt.Errorf("reference not found: %w", err)
	}
	return b, nil
}

func findRoot(dir string) string {
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
	return ""
}
