// Package emit writes a sxtree.SourceFile to a .swift file on disk.
package emit

import (
	"fmt"
	"os"
	"path/filepath"

	"mochi/transpiler3/swift/sxtree"
)

// Emit writes sf to workDir/<sf.Name>.swift and returns the file path.
func Emit(sf *sxtree.SourceFile, workDir string) (string, error) {
	src := sf.SwiftSource()
	if err := os.MkdirAll(workDir, 0o755); err != nil {
		return "", err
	}
	p := filepath.Join(workDir, sf.Name+".swift")
	if err := os.WriteFile(p, []byte(src), 0o644); err != nil {
		return "", fmt.Errorf("emit: write %s: %w", p, err)
	}
	return p, nil
}
