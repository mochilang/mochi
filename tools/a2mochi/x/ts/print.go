//go:build slow

package ts

import (
	"strings"

	"mochi/ast"
)

// Print returns the Mochi source code for the given AST node.
func Print(n *ast.Node) (string, error) {
	var b strings.Builder
	if err := ast.Fprint(&b, n); err != nil {
		return "", err
	}
	out := strings.ReplaceAll(b.String(), "\r\n", "\n")
	lines := strings.Split(out, "\n")
	for i, ln := range lines {
		lines[i] = strings.TrimRight(ln, " ")
	}
	s := strings.Join(lines, "\n")
	s = strings.TrimRight(s, " \n")
	if len(s) > 0 && s[len(s)-1] != '\n' {
		s += "\n"
	}
	return s, nil
}
