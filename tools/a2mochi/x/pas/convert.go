package pas

import (
	"fmt"

	"mochi/ast"
)

// Node is a placeholder type used until the Pascal
// conversion implementation is written.
type Node struct{}

// ErrNotImplemented is returned for all functions in this
// stub implementation.
var ErrNotImplemented = fmt.Errorf("pascal conversion not implemented")

// Parse returns ErrNotImplemented.
func Parse(src string) (*Node, error) {
	_ = src
	return nil, ErrNotImplemented
}

// Convert returns ErrNotImplemented.
func Convert(n *Node) (*ast.Node, error) {
	_ = n
	return nil, ErrNotImplemented
}
