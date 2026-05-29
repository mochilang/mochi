// Package emit writes a ptree.PhpFile to a .php file on disk.
//
// The emit pass is intentionally trivial: ptree.PhpFile.PhpSource()
// already returns a complete source string. This package only handles
// the filesystem dance (mkdir + write) so the caller can stay free of
// io/path concerns. Phase 13 plugs php-cs-fixer in after this step.
package emit

import (
	"fmt"
	"os"
	"path/filepath"

	"mochi/transpiler3/php/ptree"
)

// Emit writes file to workDir/<name>.php and returns the file path.
// Phase 0 hard-codes the filename to "main.php"; Phase 15 will switch
// to per-module names when PSR-4 layout lands.
func Emit(file *ptree.PhpFile, workDir, name string) (string, error) {
	if file == nil {
		return "", fmt.Errorf("php emit: nil file")
	}
	if name == "" {
		name = "main"
	}
	if err := os.MkdirAll(workDir, 0o755); err != nil {
		return "", err
	}
	p := filepath.Join(workDir, name+".php")
	if err := os.WriteFile(p, []byte(file.PhpSource()), 0o644); err != nil {
		return "", fmt.Errorf("emit: write %s: %w", p, err)
	}
	return p, nil
}
