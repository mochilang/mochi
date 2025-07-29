//go:build slow

package clj

import (
	"fmt"
	"strings"

	"mochi/ast"
	transpmeta "mochi/transpiler/meta"
)

// Print returns Mochi source code for the given AST node.
func Print(node *ast.Node) (string, error) {
	if node == nil {
		return "", fmt.Errorf("nil node")
	}
	var b strings.Builder
	b.Write(transpmeta.Header("//"))
	if err := ast.Fprint(&b, node); err != nil {
		return "", err
	}
	return b.String(), nil
}
