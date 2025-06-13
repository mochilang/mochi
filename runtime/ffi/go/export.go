package goffi

import (
	"fmt"
	"os"
	"path/filepath"

	"mochi/parser"
)

// Export writes extern declarations for the Go package at path into dir.
// The file name mirrors the import path with .mochi extension.
func Export(path, dir string) error {
	info, err := Infer(path)
	if err != nil {
		return err
	}
	alias := parser.AliasFromPath(path)
	content := fmt.Sprintf("import go %q as %s\n%s", path, alias, info.String())
	outPath := filepath.Join(dir, path+".mochi")
	if err := os.MkdirAll(filepath.Dir(outPath), 0o755); err != nil {
		return err
	}
	return os.WriteFile(outPath, []byte(content), 0o644)
}

// ExportAll writes extern declarations for all available Go packages into dir.
// Packages that fail to export are skipped with an error printed to stderr.
func ExportAll(dir string) error {
	pkgs, err := Packages()
	if err != nil {
		return err
	}
	for _, pkg := range pkgs {
		if err := Export(pkg.Path, dir); err != nil {
			fmt.Fprintf(os.Stderr, "export %s: %v\n", pkg.Path, err)
		}
	}
	return nil
}
