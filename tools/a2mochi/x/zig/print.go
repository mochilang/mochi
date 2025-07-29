//go:build slow

package zig

import (
	"fmt"
	"strings"

	"mochi/ast"
	"mochi/transpiler/meta"
)

// Print returns the Mochi source for the given AST node with a standard header.
func Print(n *ast.Node) (string, error) {
	if n == nil {
		return "", fmt.Errorf("nil node")
	}
	src := n.Source()
	var b strings.Builder
	b.Write(meta.Header("//"))
	for _, line := range strings.Split(src, "\n") {
		b.WriteString(strings.TrimRight(line, " \t"))
		b.WriteByte('\n')
	}
	return b.String(), nil
}
