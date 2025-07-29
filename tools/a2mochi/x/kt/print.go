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
	if err := ast.Fprint(&b, n); err != nil {
		return "", err
	}
	return b.String(), nil
}
