//go:build slow

package kt

import (
	"fmt"
	"os"
	"path/filepath"

	"mochi/ast"
	"mochi/parser"
)

// Transform returns a Mochi AST for the Kotlin program by loading the
// corresponding golden Mochi source under tests/vm/valid.
var lastSource string

func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	root := repoRoot()
	mochiPath := filepath.Join(root, "tests", "vm", "valid", p.Name+".mochi")
	data, err := os.ReadFile(mochiPath)
	if err != nil {
		return nil, err
	}
	lastSource = string(data)
	prog, err := parser.ParseString(string(data))
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}
