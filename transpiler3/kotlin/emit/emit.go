// Package emit writes a ktree.SourceFile to a .kt file on disk.
package emit

import (
	"fmt"
	"os"
	"path/filepath"

	"mochi/transpiler3/kotlin/ktree"
)

// Emit writes sf to workDir/<sf.Name>.kt and returns the file path.
func Emit(sf *ktree.SourceFile, workDir string) (string, error) {
	src := sf.KtSource()
	if err := os.MkdirAll(workDir, 0o755); err != nil {
		return "", err
	}
	p := filepath.Join(workDir, sf.Name+".kt")
	if err := os.WriteFile(p, []byte(src), 0o644); err != nil {
		return "", fmt.Errorf("emit: write %s: %w", p, err)
	}
	return p, nil
}
