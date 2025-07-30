package php

import (
	"strings"

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
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Options configures how the AST is generated.
type Options struct {
	// Positions controls whether location information is populated.
	Positions bool
}

// convert converts a tree-sitter node to our Node structure.
// convert transforms a tree-sitter node into our Node representation.
// Only named (semantic) children are included to keep the AST compact.
func convert(n *sitter.Node, src []byte, opts Options) *Node {
	if n == nil {
		return nil
	}

	node := &Node{Kind: n.Type()}
	if opts.Positions {
		start := n.StartPoint()
		end := n.EndPoint()
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Type()) {
			text := n.Content(src)
			if strings.TrimSpace(text) == "" {
				return nil
			}
			node.Text = text
		} else {
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		if c := convert(child, src, opts); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

func isValueNode(kind string) bool {
	switch kind {
	case "name", "variable_name", "integer", "float", "string", "string_content", "comment", "encapsed_string":
		return true
	default:
		return false
	}
}

// newParser returns a tree-sitter parser for PHP.
func newParser() *sitter.Parser {
	p := sitter.NewParser()
	p.SetLanguage(phpts.GetLanguage())
	return p
}
