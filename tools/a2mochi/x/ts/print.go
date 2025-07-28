//go:build slow

package ts

import (
	"strings"

	"mochi/ast"
)

// Print returns the Mochi source code for the given AST node.
func Print(n *ast.Node) (string, error) {
	var b strings.Builder
	if err := ast.Fprint(&b, n); err != nil {
		return "", err
	}
	s := b.String()
	if len(s) > 0 && s[len(s)-1] != '\n' {
		b.WriteByte('\n')
		s = b.String()
	}
	return s, nil
}
