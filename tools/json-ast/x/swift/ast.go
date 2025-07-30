package swift

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// WithPositions controls whether position information is populated on the
// generated AST nodes. When false (the default) all position fields are zero
// and omitted from the JSON output.
var WithPositions bool

// Node represents a node in the Swift syntax tree.
// Each node carries its kind and byte offsets as reported by tree-sitter.
// Children only include named nodes to keep the structure compact.
// Node represents a simplified Swift syntax tree node. Only leaves that carry
// a useful value populate the Text field. Position fields are optional and are
// only included when WithPositions is set to true.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	End      int     `json:"end,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// SourceFile is the root of a Swift AST.
// SourceFile is the root of a Swift AST.
type SourceFile struct {
	*Node
}

// convertNode transforms a tree-sitter node into the Go AST representation.
// The conversion is recursive and ignores anonymous children.
func convertNode(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}

	start := n.StartPoint()
	end := n.EndPoint()

	node := &Node{Kind: n.Type()}
	if WithPositions {
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(node.Kind) {
			node.Text = n.Content(src)
		} else {
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if c := convertNode(child, src); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}

	return node
}

// ConvertFile converts the tree-sitter root node of a Swift file into a
// SourceFile AST value.
func ConvertFile(n *sitter.Node, src []byte) *SourceFile {
	if n == nil {
		return nil
	}
	return &SourceFile{Node: convertNode(n, src)}
}

// isValueNode reports whether the given kind represents a value that should be
// kept as a leaf node in the JSON output.
func isValueNode(kind string) bool {
	switch kind {
	case "simple_identifier", "type_identifier", "integer_literal",
		"line_str_text", "comment":
		return true
	default:
		return false
	}
}
