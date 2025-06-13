package main

import (
	"fmt"
	"os"
	"path/filepath"

	"mochi/parser"
	goffi "mochi/runtime/ffi/go"
)

func main() {
	pkgs, err := goffi.Packages()
	if err != nil {
		fmt.Fprintf(os.Stderr, "list packages: %v\n", err)
		os.Exit(1)
	}
	for _, pkg := range pkgs {
		info, err := goffi.Infer(pkg.Path)
		if err != nil {
			fmt.Fprintf(os.Stderr, "infer %s: %v\n", pkg.Path, err)
			continue
		}
		alias := parser.AliasFromPath(pkg.Path)
		content := fmt.Sprintf("import go %q as %s\n%s", pkg.Path, alias, info.String())
		outPath := filepath.Join("tools", "library", "go", pkg.Path+".mochi")
		if err := os.MkdirAll(filepath.Dir(outPath), 0o755); err != nil {
			fmt.Fprintf(os.Stderr, "mkdir %s: %v\n", filepath.Dir(outPath), err)
			continue
		}
		if err := os.WriteFile(outPath, []byte(content), 0o644); err != nil {
			fmt.Fprintf(os.Stderr, "write %s: %v\n", outPath, err)
			continue
		}
	}
}
