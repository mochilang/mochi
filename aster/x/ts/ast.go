package ts

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
)

// IncludePositions controls whether Start/End position fields are populated.
// When false (the default) these fields remain zero and are omitted from the
// marshalled JSON due to the `omitempty` tags on the struct fields.
var IncludePositions bool

// Node represents a node in the TypeScript AST. Only leaf nodes that carry
// a value (identifiers, literals, etc.) populate the Text field to keep the
// resulting JSON minimal.
type Node struct {
	Kind     string `json:"kind"`
	Text     string `json:"text,omitempty"`
	Start    int    `json:"start,omitempty"`
	StartCol int    `json:"startCol,omitempty"`
	End      int    `json:"end,omitempty"`
	EndCol   int    `json:"endCol,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// Program represents a parsed TypeScript source file.
type Program struct {
	Statements []Node `json:"statements"`
}

// convertNode converts a tree-sitter node into our Node representation.
// Non-value leaves are omitted to keep the JSON small.
func convertNode(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	start := n.StartPosition()
	end := n.EndPosition()
	node := Node{
		Kind: n.Kind(),
	}
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
			return nil
		}
	}

	for i := uint(0); i < n.NamedChildCount(); i++ {
		child := n.NamedChild(i)
		if c := convertNode(child, src); c != nil {
			node.Children = append(node.Children, *c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}

	return &node
}

// isValueNode reports whether a node type represents a value that should be kept.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "property_identifier", "type_identifier", "number", "string_fragment", "predefined_type", "comment":
		return true
	default:
		return false
	}
}
