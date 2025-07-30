package elixir

import (
	sitter "github.com/smacker/go-tree-sitter"
)

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

// convert recursively converts a tree-sitter node into our Node representation.
// It skips leaf nodes that do not carry any semantic value to keep the produced
// JSON minimal.
// convert recursively converts a tree-sitter node into a Node. When pos is
// false, location fields are left zero so they are omitted from the JSON output.
func convert(n *sitter.Node, src []byte, pos bool) *Node {
	if n == nil {
		return nil
	}

	start := n.StartPoint()
	end := n.EndPoint()
	node := &Node{Kind: n.Type()}
	if pos {
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(node.Kind) {
			node.Text = n.Content(src)
		} else {
			// Skip pure syntax leaves with no semantic value
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		if c := convert(child, src, pos); c != nil {
			node.Children = append(node.Children, c)
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
		"comment":
		return true
	default:
		return false
	}
}
