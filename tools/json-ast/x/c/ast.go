package c

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a tree-sitter node with byte offsets and optional text.
type Node struct {
	Kind     string  `json:"kind"`
	Start    int     `json:"start"`
	End      int     `json:"end"`
	Text     string  `json:"text,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// toNode converts a tree-sitter node into our Node type.
func toNode(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	node := &Node{
		Kind:  n.Type(),
		Start: int(n.StartByte()),
		End:   int(n.EndByte()),
	}
	if n.ChildCount() == 0 {
		node.Text = n.Content(src)
	}
	for i := 0; i < int(n.ChildCount()); i++ {
		child := n.Child(i)
		if child == nil {
			continue
		}
		node.Children = append(node.Children, toNode(child, src))
	}
	return node
}
