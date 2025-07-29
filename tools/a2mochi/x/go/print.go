package gox

import (
	"fmt"

	mast "mochi/ast"
)

// Print returns Mochi source code for the given AST node.
func Print(node *mast.Node) (string, error) {
	if node == nil {
		return "", fmt.Errorf("nil node")
	}
	return node.Source(), nil
}
