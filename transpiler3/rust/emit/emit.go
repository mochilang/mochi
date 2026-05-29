// Package emit writes a rtree.File to a .rs file on disk.
package emit

import (
	"fmt"
	"os"
	"path/filepath"

	"mochi/transpiler3/rust/rtree"
)

// Emit writes f to workDir/<name>.rs and returns the file path.
// The caller (build.Driver) typically points workDir at the
// generated crate's src/ directory.
func Emit(f *rtree.File, workDir, name string) (string, error) {
	if name == "" {
		name = f.Name
	}
	if err := os.MkdirAll(workDir, 0o755); err != nil {
		return "", err
	}
	p := filepath.Join(workDir, name+".rs")
	if err := os.WriteFile(p, []byte(f.RustSource()), 0o644); err != nil {
		return "", fmt.Errorf("emit: write %s: %w", p, err)
	}
	return p, nil
}
