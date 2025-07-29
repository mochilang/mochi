package prolog

import (
	"fmt"
	"strings"

	"mochi/ast"
	transpmeta "mochi/transpiler/meta"
)

// Print returns Mochi source code for the given AST node with a header.
func Print(n *ast.Node) (string, error) {
	if n == nil {
		return "", fmt.Errorf("nil node")
	}
	var b strings.Builder
	b.Write(transpmeta.Header("//"))
	b.WriteString(n.Source())
	return b.String(), nil
}
