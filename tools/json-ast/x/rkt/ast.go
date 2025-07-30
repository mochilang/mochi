package rkt

import sitter "github.com/smacker/go-tree-sitter"

// Node represents a tree-sitter node in a minimal form. Only nodes that
// contain textual data carry a Text value while the structural information is
// expressed through the Kind and the list of Children.  This keeps the JSON
// representation compact while still exposing all information from the parse
// tree.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// convertProgram converts the root tree-sitter node into a Program.
func convertProgram(root *sitter.Node, src []byte) *Program {
	if root == nil {
		return &Program{}
	}
	var forms []*Node
	for i := 0; i < int(root.NamedChildCount()); i++ {
		forms = append(forms, convertNode(root.NamedChild(i), src))
	}
	return &Program{Forms: forms}
}

// convertNode converts a tree-sitter node into our Node representation.
func convertNode(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Type()}
	if n.NamedChildCount() == 0 {
		node.Text = n.Content(src)
	}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		node.Children = append(node.Children, convertNode(n.NamedChild(i), src))
	}
	return node
}
