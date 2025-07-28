//go:build slow

package ts

import (
	"strings"

	"mochi/ast"
)

// Print returns the Mochi source code for the given AST node.
func Print(n *ast.Node) string {
	var b strings.Builder
	_ = ast.Fprint(&b, n)
	s := b.String()
	if len(s) > 0 && s[len(s)-1] != '\n' {
		b.WriteByte('\n')
		s = b.String()
	}
	return s
}
