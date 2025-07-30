package scala

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
)

// Node represents a simplified Scala AST node converted from tree-sitter.
//
// Leaf nodes store their raw text in the Text field.  The positional fields
// record line and column numbers using 1-based lines and 0-based columns
// similar to tree-sitter's points.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// CompilationUnit mirrors the root of a Scala source file.
type CompilationUnit struct{ Node }

// Program wraps a parsed Scala file for JSON serialization.
type Program struct {
	Root *CompilationUnit `json:"root"`
}

// convert transforms a tree-sitter node into our Node representation.  Non-value
// leaf nodes are skipped entirely so that the resulting JSON focuses on
// meaningful tokens.
func convert(n *sitter.Node, src []byte, pos bool) *Node {
	if n == nil {
		return nil
	}
	start := n.StartPosition()
	end := n.EndPosition()
	node := &Node{Kind: n.Kind()}
	if pos {
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Kind()) {
			node.Text = n.Utf8Text(src)
		} else {
			return nil
		}
	}

	for i := uint(0); i < n.NamedChildCount(); i++ {
		child := convert(n.NamedChild(i), src, pos)
		if child != nil {
			node.Children = append(node.Children, child)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

// isValueNode reports whether the given kind represents a leaf node carrying a
// meaningful value that should be preserved in the JSON output.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "type_identifier", "integer_literal", "string", "comment":
		return true
	default:
		return false
	}
}
