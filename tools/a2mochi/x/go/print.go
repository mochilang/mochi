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
	s := b.String()
	if len(s) > 0 && s[len(s)-1] != '\n' {
		b.WriteByte('\n')
		s = b.String()
	}
	return s, nil
}
