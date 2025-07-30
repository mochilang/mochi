package erl

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a tree-sitter node for Erlang.
type Node struct {
	Type     string `json:"type"`
	Start    int    `json:"start"`
	End      int    `json:"end"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// convert recursively converts a tree-sitter node to our Node type.
func convert(n *sitter.Node, src []byte) Node {
	node := Node{
		Type:  n.Type(),
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
		node.Children = append(node.Children, convert(child, src))
	}
	return node
}
