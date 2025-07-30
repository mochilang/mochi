package clj

import (
	"strconv"
	"strings"

	sitter "github.com/smacker/go-tree-sitter"
)

// Node is a generic representation of a Clojure form.  Only leaves that carry
// a textual value populate the Text field.  Position information can be
// optionally included via the includePos parameter of convert.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// convert transforms a tree-sitter node into our Node representation.
// Non-value leaves are omitted entirely to keep the result minimal.  When
// includePos is false, position fields are left at their zero values so they
// can be omitted from the JSON output.
func convert(n *sitter.Node, src []byte, includePos bool) *Node {
	if n == nil {
		return nil
	}

	node := &Node{Kind: n.Type()}

	if includePos {
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
			if n.Type() == "str_lit" || n.Type() == "string" {
				text = strings.TrimPrefix(text, "\"")
				text = strings.TrimSuffix(text, "\"")
			}
			if n.Type() == "num_lit" || n.Type() == "number" {
				if f, err := strconv.ParseFloat(text, 64); err == nil {
					text = strconv.FormatFloat(f, 'f', -1, 64)
				}
			}
			node.Text = text
			return node
		}
		return nil
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if c := convert(child, src, includePos); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

// isValueNode reports whether the given tree-sitter node kind carries a
// textual value that should be kept in the JSON representation.
func isValueNode(kind string) bool {
	switch kind {
	case "sym_lit", "symbol", "kwd_lit", "keyword", "str_lit", "string",
		"char_lit", "num_lit", "number", "bool_lit", "boolean",
		"nil_lit", "nil", "comment":
		return true
	default:
		return false
	}
}
