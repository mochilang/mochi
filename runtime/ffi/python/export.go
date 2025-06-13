package python

import (
	"fmt"
	"os"
	"path/filepath"

	"mochi/parser"
)

// Export writes extern declarations for the Python module into dir.
// The file name mirrors the module name with a .mochi extension.
func Export(module, dir string) error {
	info, err := Infer(module)
	if err != nil {
		return err
	}
	alias := parser.AliasFromPath(module)
	content := fmt.Sprintf("import python %q as %s\n%s", module, alias, info.String())
	outPath := filepath.Join(dir, module+".mochi")
	if err := os.MkdirAll(filepath.Dir(outPath), 0o755); err != nil {
		return err
	}
	return os.WriteFile(outPath, []byte(content), 0o644)
}

// ExportAll writes extern declarations for all installed Python packages into dir.
// Packages that fail to export are skipped with an error printed to stderr.
func ExportAll(dir string) error {
	pkgs, err := Packages()
	if err != nil {
		return err
	}
	for _, pkg := range pkgs {
		if err := Export(pkg.Name, dir); err != nil {
			fmt.Fprintf(os.Stderr, "export %s: %v\n", pkg.Name, err)
		}
	}
	return nil
}
