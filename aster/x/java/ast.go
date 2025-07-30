package java

import sitter "github.com/tree-sitter/go-tree-sitter"

// IncludePos toggles whether positional information should be populated on
// nodes produced by this package. When false, the position fields remain zero
// and are omitted from the resulting JSON due to the `omitempty` tags.
var IncludePos bool

// Node represents a minimal JSON-serialisable AST node.  Only named
// tree-sitter nodes are kept to reduce noise.  Leaf nodes store the raw
// source text in Text.
// Node represents a minimal JSON-serialisable AST node. Only named
// tree-sitter nodes are kept to reduce noise. Leaf nodes store their
// raw source text in Text. Position information is recorded using row
// and column numbers (1-based rows).
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// convert creates a Node from a tree-sitter node.
func convert(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Kind()}
	if IncludePos {
		sp := n.StartPosition()
		ep := n.EndPosition()
		node.Start = int(sp.Row) + 1
		node.StartCol = int(sp.Column)
		node.End = int(ep.Row) + 1
		node.EndCol = int(ep.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(node.Kind) {
			node.Text = n.Utf8Text(src)
		} else {
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

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}

	return node
}

// isValueNode reports whether the given tree-sitter kind represents a leaf that
// carries meaningful text in the final JSON output. Pure syntax tokens are
// ignored to keep the representation compact.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "type_identifier", "decimal_integer_literal",
		"hex_integer_literal", "octal_integer_literal", "binary_integer_literal",
		"decimal_floating_point_literal", "hex_floating_point_literal",
		"string_literal", "string_fragment", "character_literal",
		"true", "false", "null", "this":
		return true
	default:
		return false
	}
}
