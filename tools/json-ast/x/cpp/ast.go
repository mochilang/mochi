package cpp

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a tree-sitter node in a generic form with position
// information. Leaf nodes also carry their text content.
// Node is a minimal representation of a C++ AST node. It records only
// the node kind and textual value for leaves, keeping the structure
// lightweight.
// Node represents a simplified C++ AST node. Only nodes that carry a value
// are kept as leaves. Positional information follows the common Start/End
// scheme using both byte offsets and columns.
// Node represents a simplified C++ AST node. Only leaves that carry user
// facing values store their source text in Text. Position fields are
// optional and omitted from the JSON output when zero.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	End      int     `json:"end,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Options controls how the AST is produced.
type Options struct {
	// WithPositions toggles whether position fields are populated.
	// When false (the default) position fields are left zero and
	// therefore omitted from the JSON output via omitempty tags.
	WithPositions bool
}

// convert turns a tree-sitter node into our Node representation.
// convert turns a tree-sitter node into our Node representation.
// Position information is added only when opts.WithPositions is true.
func convert(n *sitter.Node, src []byte, opts Options) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Type()}
	if opts.WithPositions {
		start := n.StartPoint()
		end := n.EndPoint()
		node.Start = int(n.StartByte())
		node.End = int(n.EndByte())
		node.StartCol = int(start.Column)
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Type()) || n.Type() == "comment" {
			node.Text = n.Content(src)
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
	case "identifier", "number_literal", "char_literal", "string_literal",
		"system_lib_string", "primitive_type", "type_identifier",
		"string_content", "escape_sequence", "field_identifier",
		"namespace_identifier", "auto", "character", "lambda_capture_specifier":
		return true
	default:
		return false
	}
}
