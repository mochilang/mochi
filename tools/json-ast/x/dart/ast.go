package dart

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node mirrors a tree-sitter node with position information. Only
// leaves that contain a real value (identifiers, literals, etc.) have
// their raw text stored in the Text field so the JSON output stays
// compact.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start"`
	StartCol int     `json:"startCol"`
	End      int     `json:"end"`
	EndCol   int     `json:"endCol"`
	Children []*Node `json:"children,omitempty"`
}

// isValueNode reports whether the given node type should keep its
// textual content. Keywords and pure syntax nodes are omitted.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "type_identifier", "decimal_integer_literal",
		"string_literal", "escape_sequence", "comment":
		return true
	default:
		return false
	}
}

// convertNode recursively converts the given tree-sitter node to a Node
// using the provided source code for leaf values.
func convertNode(n *sitter.Node, src []byte) *Node {
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
			// discard pure syntax leaves
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if c := convertNode(child, src); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}

	return node
}
