package rs

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a simplified Rust syntax tree node. Only leaf nodes that
// carry a semantic value retain their text content, keeping the resulting JSON
// minimal.
type Node struct {
	Kind     string  `json:"kind"`
	Start    int     `json:"start"`
	StartCol int     `json:"startCol"`
	End      int     `json:"end"`
	EndCol   int     `json:"endCol"`
	Text     string  `json:"text,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Program represents a parsed Rust source file composed of AST nodes.
type Program struct {
	Root *Node `json:"root"`
}

// convert turns a tree-sitter node into our Node representation. Non semantic
// leaves (punctuation) are omitted to keep the tree compact.
func convert(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}

	sp := n.StartPoint()
	ep := n.EndPoint()
	node := &Node{
		Kind:     n.Type(),
		Start:    int(sp.Row) + 1,
		StartCol: int(sp.Column),
		End:      int(ep.Row) + 1,
		EndCol:   int(ep.Column),
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

// isValueNode reports whether the given kind represents a leaf node that
// carries meaningful text and should therefore be kept in the JSON output.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "field_identifier", "type_identifier", "primitive_type",
		"integer_literal", "string_content", "escape_sequence", "mutable_specifier",
		"arguments", "parameters", "self", "token_tree", "line_comment":
		return true
	default:
		return false
	}
}
