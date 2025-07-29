//go:build slow

package kt

import (
	"fmt"
	"strings"

	"mochi/ast"
	"mochi/transpiler/meta"
)

// Print returns Mochi source code for the given AST node.
func Print(n *ast.Node) (string, error) {
	if n == nil {
		return "", fmt.Errorf("nil node")
	}
	var b strings.Builder
	b.Write(meta.Header("//"))
	b.WriteString(n.Source())
	return b.String(), nil
}
