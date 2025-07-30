package scala

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a simplified Scala AST node.
type Node struct {
	Kind     string `json:"kind"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// convert transforms a tree-sitter node into our Node representation.
func convert(n *sitter.Node, src []byte) Node {
	node := Node{Kind: n.Type()}
	if n.NamedChildCount() == 0 {
		node.Text = n.Content(src)
		return node
	}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		node.Children = append(node.Children, convert(child, src))
	}
	return node
}
