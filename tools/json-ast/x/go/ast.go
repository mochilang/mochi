package gox

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a simplified Go AST node converted from tree-sitter.
// IncludePositions controls whether position information is populated when
// converting tree-sitter nodes.  When false (the default) the position fields
// remain zero and are omitted from the marshalled JSON due to the `omitempty`
// struct tags.
var IncludePositions bool

// Node represents a simplified Go AST node converted from tree-sitter.
// Position fields are optional and omitted from the JSON when zero.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "field_identifier", "package_identifier",
		"type_identifier", "int_literal", "interpreted_string_literal",
		"raw_string_literal", "float_literal", "rune_literal",
		"imaginary_literal", "nil", "true", "false", "comment":
		return true
	default:
		return false
	}
}

// convertNode converts a tree-sitter node into our Node representation.
func convertNode(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}

	start := n.StartPoint()
	end := n.EndPoint()
	node := &Node{Kind: n.Type()}
	if IncludePositions {
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Type()) {
			node.Text = n.Content(src)
		} else {
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		if c := convertNode(child, src); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}
