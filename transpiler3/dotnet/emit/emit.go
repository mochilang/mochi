package emit

import (
	"fmt"
	"os"
	"path/filepath"

	"mochi/transpiler3/dotnet/csharpsrc"
)

// Emit writes one .cs file per CompilationUnit to workDir.
// Returns the list of written .cs file paths.
func Emit(cu *csharpsrc.CompilationUnit, workDir string) ([]string, error) {
	src := cu.CSSource()

	if len(cu.Types) == 0 {
		return nil, fmt.Errorf("emit: compilation unit has no types")
	}

	// Derive filename from the first type declaration.
	var typeName string
	switch t := cu.Types[0].(type) {
	case *csharpsrc.ClassDecl:
		typeName = t.Name
	case *csharpsrc.RecordDecl:
		typeName = t.Name
	case *csharpsrc.RecordStructDecl:
		typeName = t.Name
	case *csharpsrc.AbstractRecordDecl:
		typeName = t.Name
	case *csharpsrc.NamespaceDecl:
		typeName = t.Name
	default:
		return nil, fmt.Errorf("emit: unknown type decl %T", cu.Types[0])
	}

	if err := os.MkdirAll(workDir, 0o755); err != nil {
		return nil, fmt.Errorf("emit: mkdir %s: %w", workDir, err)
	}

	csPath := filepath.Join(workDir, typeName+".cs")
	if err := os.WriteFile(csPath, []byte(src), 0o644); err != nil {
		return nil, fmt.Errorf("emit: write %s: %w", csPath, err)
	}

	return []string{csPath}, nil
}
