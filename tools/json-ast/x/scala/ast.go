package scala

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a simplified Scala AST node converted from tree-sitter.
//
// Leaf nodes store their raw text in the Text field.  The positional fields
// record line and column numbers using 1-based lines and 0-based columns
// similar to tree-sitter's points.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start"`
	StartCol int     `json:"startCol"`
	End      int     `json:"end"`
	EndCol   int     `json:"endCol"`
	Children []*Node `json:"children,omitempty"`
}

// convert transforms a tree-sitter node into our Node representation.  Non-value
// leaf nodes are skipped entirely so that the resulting JSON focuses on
// meaningful tokens.
func convert(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	start := n.StartPoint()
	end := n.EndPoint()
	node := &Node{
		Kind:     n.Type(),
		Start:    int(start.Row) + 1,
		StartCol: int(start.Column),
		End:      int(end.Row) + 1,
		EndCol:   int(end.Column),
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Type()) {
			node.Text = n.Content(src)
		} else {
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := convert(n.NamedChild(i), src)
		if child != nil {
			node.Children = append(node.Children, child)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

// isValueNode reports whether the given kind represents a leaf node carrying a
// meaningful value that should be preserved in the JSON output.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "type_identifier", "integer_literal", "string", "comment":
		return true
	default:
		return false
	}
}
