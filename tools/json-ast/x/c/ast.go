package c

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a tree-sitter node with byte offsets and optional text.
// Node represents a simplified syntax tree node. Only nodes that carry useful
// textual values are kept in the tree. Position information is stored using
// line/column pairs for easier cross language comparison.
type Node struct {
	Kind     string  `json:"kind"`
	Start    int     `json:"start"`    // 1-based line number
	StartCol int     `json:"startCol"` // 0-based column number
	End      int     `json:"end"`      // 1-based line number
	EndCol   int     `json:"endCol"`
	Text     string  `json:"text,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// toNode converts a tree-sitter node into our Node type.
func toNode(n *sitter.Node, src []byte) *Node {
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
		} else if node.Kind == "comment" {
			node.Text = n.Content(src)
		} else {
			// skip pure syntax leaves
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		if c := toNode(child, src); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "number_literal", "char_literal", "string_literal",
		"system_lib_string", "primitive_type", "type_identifier",
		"string_content", "escape_sequence", "comment":
		return true
	default:
		return false
	}
}
