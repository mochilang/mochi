package swift

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a node in the Swift syntax tree.
// Each node carries its kind and byte offsets as reported by tree-sitter.
// Children only include named nodes to keep the structure compact.
// Node represents a minimal AST node that mirrors the Swift grammar. Only
// nodes carrying a value store their text in the Text field. The Start and End
// fields contain the 1-indexed line numbers while StartCol and EndCol record
// the column offsets.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start"`
	StartCol int     `json:"startCol"`
	End      int     `json:"end"`
	EndCol   int     `json:"endCol"`
	Children []*Node `json:"children,omitempty"`
}

// SourceFile is the root of a Swift AST.
type SourceFile struct{ Node }

// convertNode transforms a tree-sitter node into the Go AST representation.
// The conversion is recursive and ignores anonymous children.
func convertNode(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	start := n.StartPoint()
	end := n.EndPoint()
	out := &Node{
		Kind:     n.Type(),
		Start:    int(start.Row) + 1,
		StartCol: int(start.Column),
		End:      int(end.Row) + 1,
		EndCol:   int(end.Column),
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(out.Kind) {
			out.Text = n.Content(src)
		} else {
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if c := convertNode(child, src); c != nil {
			out.Children = append(out.Children, c)
		}
	}

	if len(out.Children) == 0 && out.Text == "" {
		return nil
	}
	return out
}

// isValueNode reports whether the given kind represents a leaf node that holds
// semantic text which should be preserved in the JSON output.
func isValueNode(kind string) bool {
	switch kind {
	case "simple_identifier", "integer_literal", "line_str_text",
		"type_identifier", "comment", "bang":
		return true
	default:
		return false
	}
}

// ConvertFile converts the tree-sitter root node of a Swift file into a
// SourceFile AST value.
func ConvertFile(n *sitter.Node, src []byte) *SourceFile {
	if n == nil {
		return nil
	}
	root := convertNode(n, src)
	if root == nil {
		return nil
	}
	return &SourceFile{Node: *root}
}
