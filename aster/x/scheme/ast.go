//go:build slow

package scheme

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
)

// IncludePos controls whether the conversion functions record positional
// information on the produced AST nodes. When false (the default) the fields
// remain zero and will be omitted from the JSON output because of the
// `omitempty` struct tags.
var IncludePos bool

// Node mirrors a tree-sitter node. Only leaves carrying semantic text keep the
// Text field populated so the resulting JSON remains compact. Position fields
// are optional and only populated when IncludePos is true.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// The following typed aliases mirror the subset of tree-sitter node kinds that
// appear in the golden files. They provide a slightly more structured API to
// callers while still serialising exactly like Node.
type (
	Form    Node
	List    Node
	Symbol  Node
	String  Node
	Number  Node
	Comment Node
)

// Program represents a parsed Scheme source file.
type Program struct {
	Forms []*Form `json:"forms"`
}

// isValueNode reports whether the given kind should keep its text content. Only
// semantic leaves are preserved so the resulting JSON stays compact.
func isValueNode(kind string) bool {
	switch kind {
	case "comment", "symbol", "string", "number":
		return true
	default:
		return false
	}
}

// convertNode converts a tree-sitter node to our Node representation.
func convertNode(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}

	node := &Node{Kind: n.Kind()}
	if IncludePos {
		sp := n.StartPosition()
		ep := n.EndPosition()
		node.Start = int(sp.Row) + 1
		node.StartCol = int(sp.Column)
		node.End = int(ep.Row) + 1
		node.EndCol = int(ep.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Kind()) {
			node.Text = n.Utf8Text(src)
		} else {
			return nil
		}
	}

	for i := uint(0); i < n.NamedChildCount(); i++ {
		if c := convertNode(n.NamedChild(i), src); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if node.Text == "" && len(node.Children) == 0 {
		return nil
	}
	return node
}

// convertProgram converts the root tree-sitter node into a Program.
func convertProgram(root *sitter.Node, src []byte) *Program {
	if root == nil {
		return &Program{}
	}
	var forms []*Form
	for i := uint(0); i < root.NamedChildCount(); i++ {
		if n := convertNode(root.NamedChild(i), src); n != nil {
			forms = append(forms, (*Form)(n))
		}
	}
	return &Program{Forms: forms}
}
