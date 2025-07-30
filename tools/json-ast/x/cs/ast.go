package cs

import sitter "github.com/smacker/go-tree-sitter"

// Node models a portion of the C# syntax tree as returned by tree-sitter.
// Only leaves carrying a textual value populate the Text field. Position
// information is stored using 1-indexed line numbers and zero-indexed
// columns similar to tree-sitter's Point type.
// IncludePositions controls whether the Start/End position fields of Node are
// populated when building the AST.  It is disabled by default so that JSON
// output remains compact.  When false the position fields will be omitted from
// the marshalled JSON via "omitempty" tags.
var IncludePositions bool

// Node models a portion of the C# syntax tree as returned by tree-sitter.
// Only leaves carrying a textual value populate the Text field. Position
// information can optionally be included when IncludePositions is true.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
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
	node := &Node{Kind: n.Type()}
	if IncludePositions {
		sp := n.StartPoint()
		ep := n.EndPoint()
		node.Start = int(sp.Row) + 1
		node.StartCol = int(sp.Column)
		node.End = int(ep.Row) + 1
		node.EndCol = int(ep.Column)
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
