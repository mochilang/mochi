package php

import (
	sitter "github.com/smacker/go-tree-sitter"
	phpts "github.com/smacker/go-tree-sitter/php"
)

// Node represents a tree-sitter node in PHP's syntax tree.
// Node is a lightweight representation of a PHP AST node. Only
// semantically relevant nodes are kept. Leaf nodes contain their
// source text in the Text field.
type Node struct {
	Kind     string `json:"kind"`
	Start    int    `json:"start"`
	End      int    `json:"end"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// convert converts a tree-sitter node to our Node structure.
// convert transforms a tree-sitter node into our Node representation.
// Only named (semantic) children are included to keep the AST compact.
func convert(n *sitter.Node, src []byte) Node {
	node := Node{Kind: n.Type(), Start: int(n.StartByte()), End: int(n.EndByte())}
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

// newParser returns a tree-sitter parser for PHP.
func newParser() *sitter.Parser {
	p := sitter.NewParser()
	p.SetLanguage(phpts.GetLanguage())
	return p
}
