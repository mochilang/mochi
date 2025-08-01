package clj

import (
	"strconv"
	"strings"

	sitter "github.com/tree-sitter/go-tree-sitter"
)

// Node is a generic representation of a Clojure form. Only leaves that carry a
// textual value populate the Text field.
// IncludePositions controls whether positional information is recorded in the
// AST nodes. When false the Start/End fields remain zero so they are omitted
// from the marshalled JSON via the `omitempty` tags.
var IncludePositions bool

// Node models a portion of a Clojure syntax tree. Only leaves that carry a
// textual value populate the Text field. Position fields follow tree-sitter's
// 1-indexed rows and 0-indexed columns.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// A handful of typed aliases provide a more structured view for callers.
type (
	Form    Node
	List    Node
	Vector  Node
	Map     Node
	Set     Node
	Entry   Node
	Symbol  Node
	Keyword Node
	String  Node
	Number  Node
	Boolean Node
	Nil     Node
)

// convert transforms a tree-sitter node into our Node representation. Non-value
// leaves are omitted entirely to keep the result minimal.
func convert(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}

	node := &Node{Kind: n.Kind()}

	if IncludePositions {
		start := n.StartPosition()
		end := n.EndPosition()
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Kind()) {
			text := n.Utf8Text(src)
			if n.Kind() == "str_lit" || n.Kind() == "string" {
				text = strings.TrimPrefix(text, "\"")
				text = strings.TrimSuffix(text, "\"")
			}
			if n.Kind() == "num_lit" || n.Kind() == "number" {
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
		child := n.NamedChild(uint(i))
		if c := convert(child, src); c != nil {
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
