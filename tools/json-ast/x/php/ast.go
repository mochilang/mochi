package php

import (
	sitter "github.com/smacker/go-tree-sitter"
	phpts "github.com/smacker/go-tree-sitter/php"
)

// Node represents a tree-sitter node in PHP's syntax tree.
// Node is a lightweight representation of a PHP AST node. Only
// semantically relevant nodes are kept. Leaf nodes contain their
// source text in the Text field.
// Node represents a compact AST node derived from tree-sitter.
//
// Start and End are 1-indexed line numbers. StartCol and EndCol are
// 0-indexed column offsets. Leaf nodes that carry semantic value store
// their source text in the Text field. Nodes without semantic value and
// no children are omitted during conversion to keep the JSON minimal.
type Node struct {
	Kind     string `json:"kind"`
	Text     string `json:"text,omitempty"`
	Start    int    `json:"start"`
	StartCol int    `json:"startCol"`
	End      int    `json:"end"`
	EndCol   int    `json:"endCol"`
	Children []Node `json:"children,omitempty"`
}

// convert converts a tree-sitter node to our Node structure.
// convert transforms a tree-sitter node into our Node representation.
// Only named (semantic) children are included to keep the AST compact.
func convert(n *sitter.Node, src []byte) Node {
	start := n.StartPoint()
	end := n.EndPoint()

	node := Node{
		Kind:     n.Type(),
		Start:    int(start.Row) + 1,
		StartCol: int(start.Column),
		End:      int(end.Row) + 1,
		EndCol:   int(end.Column),
	}

	if n.NamedChildCount() == 0 {
		node.Text = n.Content(src)
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		c := convert(child, src)
		if c.Text == "" && len(c.Children) == 0 {
			continue
		}
		node.Children = append(node.Children, c)
	}

	if node.Text == "" && len(node.Children) == 1 {
		return node.Children[0]
	}

	return node
}

// newParser returns a tree-sitter parser for PHP.
func newParser() *sitter.Parser {
	p := sitter.NewParser()
	p.SetLanguage(phpts.GetLanguage())
	return p
}
