package build

import (
	"io"
	"os"
	"path/filepath"
)

// copySiblingGoFiles copies every *.go file that lives in the same
// directory as src into workDir. This supports the phase-10.2 Go FFI
// pattern where `extern go fun NAME(...)` declarations in a .mochi
// source file resolve against hand-written Go files placed next to it.
// Non-*.go files and subdirectories are silently ignored.
func copySiblingGoFiles(src, workDir string) error {
	srcDir := filepath.Dir(src)
	entries, err := os.ReadDir(srcDir)
	if err != nil {
		return err
	}
	for _, e := range entries {
		if e.IsDir() {
			continue
		}
		name := e.Name()
		if filepath.Ext(name) != ".go" {
			continue
		}
		if err := copyFile(filepath.Join(srcDir, name), filepath.Join(workDir, name)); err != nil {
			return err
		}
	}
	return nil
}

func copyFile(src, dst string) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	out, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer out.Close()
	_, err = io.Copy(out, in)
	return err
}
