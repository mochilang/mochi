package ts

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a node in the TypeScript AST.
type Node struct {
	Kind     string      `json:"kind"`
	Name     string      `json:"name,omitempty"`
	Value    interface{} `json:"value,omitempty"`
	Start    int         `json:"start"`
	StartCol int         `json:"startCol"`
	End      int         `json:"end"`
	EndCol   int         `json:"endCol"`
	Children []Node      `json:"children,omitempty"`
}

// Program represents a parsed TypeScript source file.
type Program struct {
	Root Node `json:"root"`
}

// convertNode converts a tree-sitter node to our Node AST representation.
func convertNode(n *sitter.Node, src []byte) Node {
	start := n.StartPoint()
	end := n.EndPoint()
	node := Node{
		Kind:     n.Type(),
		Start:    int(start.Row) + 1,
		StartCol: int(start.Column),
		End:      int(end.Row) + 1,
		EndCol:   int(end.Column),
	}

	switch n.Type() {
	case "identifier":
		node.Name = n.Content(src)
	case "string", "string_fragment", "string_literal", "template_string", "interpreted_string_literal", "raw_string_literal":
		node.Value = n.Content(src)
	case "number", "decimal_integer_literal", "hex_integer_literal", "octal_integer_literal", "binary_integer_literal", "float":
		node.Value = n.Content(src)
	case "true":
		node.Value = true
	case "false":
		node.Value = false
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		node.Children = append(node.Children, convertNode(child, src))
	}
	return node
}
