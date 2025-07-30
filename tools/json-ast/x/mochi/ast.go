package mochi

import (
	"fmt"

	mochiast "mochi/ast"
)

// IncludePositions controls whether positional information is recorded in the
// resulting AST nodes. When false the Start/End fields remain zero and are
// omitted from the marshalled JSON due to the `omitempty` struct tags.
var IncludePositions bool

// Node represents a simplified Mochi AST node. Leaf nodes that carry a textual
// value populate the Text field. Position information follows 1-based line
// numbers and 0-based columns similar to tree-sitter.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Program represents a parsed Mochi file. The root node is the program itself.
type Program struct {
	File *Node `json:"file"`
}

// toNode converts a node from the generic ast package into the JSON AST used
// by this package. Pure syntax nodes with no textual value are omitted so the
// resulting JSON remains small.
func toNode(n *mochiast.Node) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Kind}
	if n.Value != nil {
		node.Text = fmt.Sprint(n.Value)
	}
	if IncludePositions {
		// Position information is currently unavailable when converting
		// from the generic ast.Node type. The fields remain zero so they
		// are omitted from the marshalled JSON.
	}
	for _, c := range n.Children {
		if child := toNode(c); child != nil {
			node.Children = append(node.Children, child)
		}
	}
	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}
