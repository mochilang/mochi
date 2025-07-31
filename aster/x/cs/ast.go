package cs

import sitter "github.com/tree-sitter/go-tree-sitter"

// IncludePositions controls whether positional information is populated when
// converting tree-sitter nodes. When false (the default) all position fields
// remain zero and are omitted from the marshalled JSON due to the omitempty
// struct tags.
var IncludePositions bool

// Node models a portion of the C# syntax tree as returned by tree-sitter.
// Only leaves carrying a textual value populate the Text field. Position
// information can optionally be included when requested by the caller.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	End      int     `json:"end,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Aliases for the node kinds that appear in the JSON output.  These aliases
// allow Program to expose a slightly more structured AST while still using the
// generic Node type under the hood.
type (
	CompilationUnit  Node
	Identifier       Node
	IntegerLiteral   Node
	RealLiteral      Node
	StringLiteral    Node
	CharacterLiteral Node
	BooleanLiteral   Node
	StringContent    Node
	ImplicitType     Node
	PredefinedType   Node
	Modifier         Node
	Comment          Node
)

// Program represents a parsed C# file.
// Program represents a parsed C# file.  The root node is a CompilationUnit.
type Program struct {
	File *CompilationUnit `json:"file"`
}

// convert builds a Node tree starting at the given tree-sitter node. Pure
// syntax leaf nodes without textual value are omitted so the JSON output
// remains compact. When IncludePositions is true the resulting Node records
// line and column information.
func convert(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Kind()}
	if IncludePositions {
		sp := n.StartPosition()
		ep := n.EndPosition()
		node.Start = int(sp.Row) + 1
		node.End = int(ep.Row) + 1
		node.StartCol = int(sp.Column)
		node.EndCol = int(ep.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(node.Kind) {
			node.Text = n.Utf8Text(src)
		} else {
			return nil
		}
	}

	for i := uint(0); i < n.NamedChildCount(); i++ {
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
