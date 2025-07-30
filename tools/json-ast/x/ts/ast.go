package ts

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a node in the TypeScript AST. Only leaf nodes that carry
// a value (identifiers, literals, etc.) populate the Text field to keep the
// resulting JSON minimal.
type Node struct {
	Kind     string `json:"kind"`
	Text     string `json:"text,omitempty"`
	Start    int    `json:"start"`
	StartCol int    `json:"startCol"`
	End      int    `json:"end"`
	EndCol   int    `json:"endCol"`
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
	start := n.StartPoint()
	end := n.EndPoint()
	node := Node{
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
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
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
