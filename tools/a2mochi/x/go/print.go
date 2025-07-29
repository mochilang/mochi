//go:build slow

package gox

import (
	"fmt"
	"strings"

	mast "mochi/ast"
)

// Print returns Mochi source code for the given AST node.
func Print(node *mast.Node) (string, error) {
	if node == nil {
		return "", fmt.Errorf("nil node")
	}
	if node.Kind != "program" {
		return "", fmt.Errorf("unexpected root kind: %s", node.Kind)
	}
	var b strings.Builder
	if err := mast.Fprint(&b, node); err != nil {
		return "", err
	}
	out := strings.ReplaceAll(b.String(), "\r\n", "\n")
	out = strings.ReplaceAll(out, "\r", "")
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
