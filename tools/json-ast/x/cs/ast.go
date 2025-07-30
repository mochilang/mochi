package cs

import sitter "github.com/smacker/go-tree-sitter"

// Node models a portion of the C# syntax tree as returned by tree-sitter.
// Only leaves carrying a textual value populate the Text field. Position
// information is stored using 1-indexed line numbers and zero-indexed
// columns similar to tree-sitter's Point type.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start"`
	StartCol int     `json:"startCol"`
	End      int     `json:"end"`
	EndCol   int     `json:"endCol"`
	Children []*Node `json:"children,omitempty"`
}

// Program represents a parsed C# file.
type Program struct {
	File *Node `json:"file"`
}

// convert builds a Node tree starting from the given tree-sitter node.
// convert builds a Node tree starting from the given tree-sitter node. Pure
// syntax leaf nodes without textual value are omitted from the resulting tree
// so the JSON output remains compact.
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
		if isValueNode(node.Kind) {
			node.Text = n.Content(src)
		} else {
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		c := n.NamedChild(i)
		if child := convert(c, src); child != nil {
			node.Children = append(node.Children, child)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

// isValueNode reports whether the given kind should keep its textual content.
// Identifiers, literals, comments and modifiers are considered value nodes.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "integer_literal", "real_literal", "string_literal",
		"character_literal", "boolean_literal", "string_content",
		"implicit_type", "predefined_type", "modifier", "comment":
		return true
	default:
		return false
	}
}
