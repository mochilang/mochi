//go:build slow

package ex

import (
	"fmt"
	"strings"

	"mochi/ast"
	"mochi/transpiler/meta"
)

func nodeSource(n *ast.Node) string {
	code := n.Source()
	code = strings.ReplaceAll(code, "union_all", "union all")
	var out strings.Builder
	out.Write(meta.Header("//"))
	out.WriteByte('\n')
	out.WriteString(code)
	if !strings.HasSuffix(code, "\n") {
		out.WriteByte('\n')
	}
	return out.String()
}

// Print returns Mochi source code with a standard header for the given AST.
func Print(n *ast.Node) (string, error) {
	if n == nil {
		return "", fmt.Errorf("nil node")
	}
	return nodeSource(n), nil
}
