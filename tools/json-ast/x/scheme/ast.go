package scheme

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node mirrors a tree-sitter node. Leaf nodes store their source text in Text
// while internal nodes only keep their named children.
// IncludePos controls whether the conversion functions populate position
// information. When false (the default), the position fields are left zero and
// omitted from the JSON output.
var IncludePos bool

// Node mirrors a tree-sitter node. Leaf nodes store their source text in Text
// while internal nodes only keep their named children. Position fields are
// optional and only set when IncludePos is true.
type Node struct {
	Kind     string `json:"kind"`
	Text     string `json:"text,omitempty"`
	Start    int    `json:"start,omitempty"`
	StartCol int    `json:"startCol,omitempty"`
	End      int    `json:"end,omitempty"`
	EndCol   int    `json:"endCol,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// Program represents a parsed Scheme source file.
type Program struct {
	Forms []Node `json:"forms"`
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

	node := &Node{Kind: n.Type()}
	if IncludePos {
		sp := n.StartPoint()
		ep := n.EndPoint()
		node.Start = int(sp.Row) + 1
		node.StartCol = int(sp.Column)
		node.End = int(ep.Row) + 1
		node.EndCol = int(ep.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Type()) {
			node.Text = n.Content(src)
		} else {
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		if c := convertNode(n.NamedChild(i), src); c != nil {
			node.Children = append(node.Children, *c)
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
	var forms []Node
	for i := 0; i < int(root.NamedChildCount()); i++ {
		if n := convertNode(root.NamedChild(i), src); n != nil {
			forms = append(forms, *n)
		}
	}
	return &Program{Forms: forms}
}
