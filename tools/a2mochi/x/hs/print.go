//go:build slow

package hs

import (
	"fmt"
	"strings"

	"mochi/ast"
	"mochi/transpiler/meta"
)

// Print returns Mochi source code for the AST node with a header.
func Print(node *ast.Node) (string, error) {
	if node == nil {
		return "", fmt.Errorf("nil node")
	}
	var b strings.Builder
	b.Write(meta.Header("//"))
	b.WriteByte('\n')
	b.WriteString(node.Source())
	return b.String(), nil
}
