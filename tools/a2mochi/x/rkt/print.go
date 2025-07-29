//go:build slow

package rkt

import (
	"strings"

	"mochi/ast"
	transpmeta "mochi/transpiler/meta"
)

// Print renders the Mochi AST node with a standard header.
func Print(n *ast.Node) (string, error) {
	var b strings.Builder
	b.Write(transpmeta.Header("//"))
	if err := ast.Fprint(&b, n); err != nil {
		return "", err
	}
	return b.String(), nil
}
