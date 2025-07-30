package cpp

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a tree-sitter node in a generic form with position
// information. Leaf nodes also carry their text content.
// Node is a minimal representation of a C++ AST node. It records only
// the node kind and textual value for leaves, keeping the structure
// lightweight.
// Node represents a simplified C++ AST node. Only nodes that carry a value
// are kept as leaves. Positional information follows the common Start/End
// scheme using both byte offsets and columns.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start"`
	End      int     `json:"end"`
	StartCol int     `json:"startCol"`
	EndCol   int     `json:"endCol"`
	Children []*Node `json:"children,omitempty"`
}

// convert turns a tree-sitter node into our Node representation.
func convert(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	start := n.StartPoint()
	end := n.EndPoint()
	node := &Node{
		Kind:     n.Type(),
		Start:    int(n.StartByte()),
		End:      int(n.EndByte()),
		StartCol: int(start.Column),
		EndCol:   int(end.Column),
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Type()) || n.Type() == "comment" {
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
		if c := convert(child, src); c != nil {
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
		"string_content", "escape_sequence", "field_identifier",
		"namespace_identifier", "auto", "character", "lambda_capture_specifier":
		return true
	default:
		return false
	}
}
