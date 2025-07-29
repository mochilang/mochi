package dart

import (
	"fmt"
	"strings"

	"mochi/ast"
)

// Print returns Mochi source code for the given AST node.
func Print(node *ast.Node) (string, error) {
	if node == nil {
		return "", fmt.Errorf("nil node")
	}
	var b strings.Builder
	if err := ast.Fprint(&b, node); err != nil {
		return "", err
	}
	if b.Len() > 0 && b.String()[b.Len()-1] != '\n' {
		b.WriteByte('\n')
	}
	return b.String(), nil
}
