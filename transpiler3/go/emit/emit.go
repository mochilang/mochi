package emit

import (
	"os"
	"path/filepath"

	"mochi/transpiler3/go/gotree"
)

// Emit writes file's gofmt-canonical source to outDir/fileName.
// It creates any missing parent directories with permissions
// 0o755 and writes the source with 0o644.
func Emit(file *gotree.File, outDir, fileName string) error {
	src, err := file.Render()
	if err != nil {
		return err
	}
	full := filepath.Join(outDir, fileName)
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		return err
	}
	return os.WriteFile(full, src, 0o644)
}
