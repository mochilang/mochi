//go:build slow

package erl

import (
	"fmt"
	"os"
	"strings"

	"mochi/ast"
	"mochi/transpiler/meta"
)

// Print renders the given AST node to Mochi source code with a standard header.
func Print(node *ast.Node) (string, error) {
	if node == nil {
		return "", fmt.Errorf("nil node")
	}
	var code strings.Builder
	if err := ast.Fprint(&code, node); err != nil {
		return "", err
	}
	var out strings.Builder
	prefix := "//"
	if p := os.Getenv("ERL_HEADER_PREFIX"); p != "" {
		prefix = p
	}
	out.Write(meta.Header(prefix))
	trimmed := strings.TrimRight(code.String(), " \t\n")
	out.WriteString(trimmed)
	out.WriteByte('\n')
	return out.String(), nil
}
