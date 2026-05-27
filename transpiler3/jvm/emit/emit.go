package emit

import (
	"fmt"
	"os"
	"path/filepath"

	"mochi/transpiler3/jvm/javasrc"
)

// Emit writes one .java file per CompilationUnit to workDir.
// Returns the list of written .java file paths.
func Emit(cu *javasrc.CompilationUnit, workDir string) ([]string, error) {
	src := cu.JavaString()

	// Derive filename from the first type declaration name
	if len(cu.Types) == 0 {
		return nil, fmt.Errorf("emit: compilation unit has no types")
	}

	// Get class name from first type
	var className string
	switch t := cu.Types[0].(type) {
	case *javasrc.ClassDecl:
		className = t.Name
	case *javasrc.RecordDecl:
		className = t.Name
	case *javasrc.SealedInterfaceDecl:
		className = t.Name
	default:
		return nil, fmt.Errorf("emit: unknown type decl %T", cu.Types[0])
	}

	// Create package directory structure
	pkgDir := workDir
	if cu.Package != "" {
		for _, part := range splitPackage(cu.Package) {
			pkgDir = filepath.Join(pkgDir, part)
		}
	}
	if err := os.MkdirAll(pkgDir, 0o755); err != nil {
		return nil, fmt.Errorf("emit: mkdir %s: %w", pkgDir, err)
	}

	javaPath := filepath.Join(pkgDir, className+".java")
	if err := os.WriteFile(javaPath, []byte(src), 0o644); err != nil {
		return nil, fmt.Errorf("emit: write %s: %w", javaPath, err)
	}

	return []string{javaPath}, nil
}

func splitPackage(pkg string) []string {
	var parts []string
	start := 0
	for i := 0; i <= len(pkg); i++ {
		if i == len(pkg) || pkg[i] == '.' {
			if i > start {
				parts = append(parts, pkg[start:i])
			}
			start = i + 1
		}
	}
	return parts
}
