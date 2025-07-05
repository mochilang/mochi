package any2mochi

import (
	"os"
	"path/filepath"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// repoRoot walks up directories to locate go.mod.
func repoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", os.ErrNotExist
}

// flattenSymbols returns a flat slice of the provided document symbols and all
// of their children recursively. This is useful for language servers like
// clangd that nest methods under classes.
func flattenSymbols(syms []protocol.DocumentSymbol) []protocol.DocumentSymbol {
	var out []protocol.DocumentSymbol
	var walk func([]protocol.DocumentSymbol)
	walk = func(list []protocol.DocumentSymbol) {
		for _, s := range list {
			out = append(out, s)
			if len(s.Children) > 0 {
				walk(s.Children)
			}
		}
	}
	walk(syms)
	return out
}
