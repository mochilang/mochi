package php

import (
	sitter "github.com/smacker/go-tree-sitter"
	phpts "github.com/smacker/go-tree-sitter/php"
)

// Node represents a tree-sitter node in PHP's syntax tree.
type Node struct {
	Type     string `json:"type"`
	Start    int    `json:"start"`
	End      int    `json:"end"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// convert converts a tree-sitter node to our Node structure.
func convert(n *sitter.Node, src []byte) Node {
	node := Node{Type: n.Type(), Start: int(n.StartByte()), End: int(n.EndByte())}
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

// newParser returns a tree-sitter parser for PHP.
func newParser() *sitter.Parser {
	p := sitter.NewParser()
	p.SetLanguage(phpts.GetLanguage())
	return p
}
