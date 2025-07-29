//go:build slow

package lua

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
	b.WriteString(header())
	if err := ast.Fprint(&b, n); err != nil {
		return "", err
	}
	return b.String(), nil
}

func header() string {
	return string(transpmeta.Header("//"))
}
