//go:build slow

package elixir

import (
	"strings"

	sitter "github.com/tree-sitter/go-tree-sitter"
)

// IncludePositions controls whether converted nodes include location
// information. When false (the default) the position fields remain zero
// and will be omitted from the marshalled JSON due to the `omitempty`
// struct tags.
var IncludePositions bool

// Node represents a tree-sitter node in a compact form. Position information is
// stored using line and column numbers to make the output easier to read. Leaf
// nodes that contain a textual value keep that value in the Text field.
// Node represents a simplified tree-sitter node.  Position fields are optional
// in the JSON output and omitted when zero so that callers can choose whether
// to include them.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// A handful of concrete node types used in the cross_join example. They simply
// embed Node so that JSON output keeps the same shape while giving the Go
// compiler a more expressive type hierarchy.
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

// convert recursively converts a tree-sitter node into a Node. It skips leaf
// nodes that do not carry any semantic value so the resulting JSON stays
// compact. When IncludePositions is false, all position fields remain zero and
// are omitted from the marshalled output.
func convert(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}

	start := n.StartPosition()
	end := n.EndPosition()
	node := &Node{Kind: n.Kind()}
	if IncludePositions {
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(node.Kind) {
			node.Text = n.Utf8Text(src)
		} else {
			// Skip pure syntax leaves with no semantic value
			return nil
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

	if node.Kind == "binary_operator" && len(node.Children) == 2 && node.Text == "" {
		left := n.NamedChild(0)
		right := n.NamedChild(1)
		if left != nil && right != nil {
			opBytes := src[left.EndByte():right.StartByte()]
			op := strings.TrimSpace(string(opBytes))
			if op != "" {
				node.Text = op
			}
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

// isValueNode reports whether the given node kind represents a value that
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
