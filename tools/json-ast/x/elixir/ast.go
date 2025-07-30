package elixir

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a tree-sitter node in a compact form. Position information is
// stored using line and column numbers to make the output easier to read. Leaf
// nodes that contain a textual value keep that value in the Text field.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start"`
	StartCol int     `json:"startCol"`
	End      int     `json:"end"`
	EndCol   int     `json:"endCol"`
	Children []*Node `json:"children,omitempty"`
}

// Program represents the root syntax tree for an Elixir file.
type Program struct {
	Root *Node `json:"root"`
}

// convert recursively converts a tree-sitter node into our Node representation.
// It skips leaf nodes that do not carry any semantic value to keep the produced
// JSON minimal.
func convert(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}

	start := n.StartPoint()
	end := n.EndPoint()
	node := &Node{
		Kind:     n.Type(),
		Start:    int(start.Row) + 1,
		StartCol: int(start.Column),
		End:      int(end.Row) + 1,
		EndCol:   int(end.Column),
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
		if c := convert(child, src); c != nil {
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
