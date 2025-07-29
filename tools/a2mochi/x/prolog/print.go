//go:build slow

package prolog

import (
	"fmt"
	"strings"

	"mochi/ast"
	transpmeta "mochi/transpiler/meta"
)

// Print returns Mochi source code for the given AST node with a header.
func Print(n *ast.Node) (string, error) {
	if n == nil {
		return "", fmt.Errorf("nil node")
	}
	var b strings.Builder
	b.Write(transpmeta.Header("//"))
	code := strings.TrimRight(n.Source(), "\n")
	code = strings.ReplaceAll(code, "union_all", "union all")
	code = strings.ReplaceAll(code, `\n \"`, `\n  \"`)
	b.WriteString(code)
	if !strings.HasSuffix(code, "\n") {
		b.WriteByte('\n')
	}
	return b.String(), nil
}
