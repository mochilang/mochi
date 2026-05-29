package emit

import (
	"fmt"
	"os"
	"path/filepath"

	"mochi/transpiler3/python/pysrc"
)

// Emit serialises a pysrc.Module to .py source bytes and writes them
// at outPath, creating parent directories as needed.
func Emit(mod *pysrc.Module, outPath string) error {
	src := mod.PySource()
	if err := os.MkdirAll(filepath.Dir(outPath), 0o755); err != nil {
		return fmt.Errorf("python/emit: mkdir: %w", err)
	}
	if err := os.WriteFile(outPath, []byte(src), 0o644); err != nil {
		return fmt.Errorf("python/emit: write %s: %w", outPath, err)
	}
	return nil
}

// Source returns the rendered .py text without writing it.
func Source(mod *pysrc.Module) string {
	return mod.PySource()
}
