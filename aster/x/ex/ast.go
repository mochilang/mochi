package ex

import (
	"strings"

	sitter "github.com/tree-sitter/go-tree-sitter"
)

// IncludePos controls whether converted nodes include positional information.
// When false (the default) the position fields remain zero and are omitted from
// the marshalled JSON due to the `omitempty` struct tags.
var IncludePos bool

// Node represents a simplified tree-sitter node.  Leaf nodes with semantic
// values keep the raw text in Text. Position fields use 1-based rows and are
// optional in the JSON output.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Structured node types used in the golden tests. They simply embed Node so the
// JSON shape stays the same while providing a more descriptive API.
type (
	Source         struct{ Node }
	Comment        struct{ Node }
	BinaryOperator struct{ Node }
	Identifier     struct{ Node }
	List           struct{ Node }
	Map            struct{ Node }
	Pair           struct{ Node }
	String         struct{ Node }
	Integer        struct{ Node }
	Call           struct{ Node }
	Arguments      struct{ Node }
	Dot            struct{ Node }
	DoBlock        struct{ Node }
	AnonymousFn    struct{ Node }
	StabClause     struct{ Node }
	Body           struct{ Node }
	MapContent     struct{ Node }
	Keywords       struct{ Node }
	Keyword        struct{ Node }
	Interpolation  struct{ Node }
)

// Program represents the root syntax tree for an Elixir file.
type Program struct {
	Root *Source `json:"root"`
}

// convert recursively converts a tree-sitter node into a Node. Non-value leaf
// nodes are discarded to keep the tree small. When IncludePos is false the
// positional fields remain zero.
func convert(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	start := n.StartPosition()
	end := n.EndPosition()
	node := &Node{Kind: n.Kind()}
	if IncludePos {
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}
	if n.NamedChildCount() == 0 {
		if isValueNode(node.Kind) {
			node.Text = n.Utf8Text(src)
		} else {
			return nil
		}
	}

	if n.ChildCount() > n.NamedChildCount() {
		switch node.Kind {
		case "binary_operator":
			if n.ChildCount() >= 3 {
				op := n.Child(1)
				if op != nil && !op.IsNamed() {
					node.Text = strings.TrimSpace(op.Utf8Text(src))
				}
			}
		case "unary_operator":
			op := n.Child(0)
			if op != nil && !op.IsNamed() {
				node.Text = strings.TrimSpace(op.Utf8Text(src))
			}
		}
	}
	for i := uint(0); i < n.NamedChildCount(); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		if c := convert(child, src); c != nil {
			node.Children = append(node.Children, c)
		}
	}
	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

// isValueNode reports whether the given node kind represents a leaf value that
// should be preserved even when it has no named children.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "atom", "integer", "float", "char", "string",
		"string_line", "string_content", "quoted_content", "keyword",
		"comment", "alias":
		return true
	default:
		return false
	}
}
