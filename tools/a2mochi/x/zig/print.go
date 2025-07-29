//go:build slow

package zig

import (
	"strings"

	"mochi/ast"
	"mochi/transpiler/meta"
)

// Print returns the Mochi source for the given AST node with a standard header.
func Print(n *ast.Node) (string, error) {
	var code strings.Builder
	if err := ast.Fprint(&code, n); err != nil {
		return "", err
	}
	var b strings.Builder
	b.Write(meta.Header("//"))
	b.WriteString(code.String())
	if !strings.HasSuffix(code.String(), "\n") {
		b.WriteByte('\n')
	}
	return b.String(), nil
}
