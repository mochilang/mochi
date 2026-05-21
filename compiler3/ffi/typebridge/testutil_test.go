package typebridge

import (
	"go/types"
	"os"
	"testing"

	"golang.org/x/tools/go/packages"
)

func writeFileImpl(path, content string) error {
	return os.WriteFile(path, []byte(content), 0o644)
}

// loadFromSource writes a single-file synthetic package to a temp
// directory and loads it via packages.Load. Used by tests that need
// to exercise specific Go shapes (generic instantiations etc.).
func loadFromSource(t *testing.T, modName, src string) *types.Package {
	t.Helper()
	dir := t.TempDir()
	if err := os.WriteFile(dir+"/go.mod", []byte("module "+modName+"\n\ngo 1.22\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(dir+"/g.go", []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	cfg := &packages.Config{
		Mode: packages.NeedTypes | packages.NeedTypesInfo | packages.NeedSyntax | packages.NeedDeps | packages.NeedImports | packages.NeedName | packages.NeedFiles,
		Dir:  dir,
	}
	pkgs, err := packages.Load(cfg, ".")
	if err != nil {
		t.Fatalf("packages.Load: %v", err)
	}
	if len(pkgs) == 0 || pkgs[0].Types == nil {
		t.Fatal("no types loaded")
	}
	for _, p := range pkgs {
		if len(p.Errors) > 0 {
			for _, e := range p.Errors {
				t.Logf("err: %v", e)
			}
		}
	}
	return pkgs[0].Types
}
