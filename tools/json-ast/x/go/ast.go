package gox

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a simplified Go AST node converted from tree-sitter.
type Node struct {
	Kind     string `json:"kind"`
	Name     string `json:"name,omitempty"`
	Value    string `json:"value,omitempty"`
	Start    int    `json:"start"`
	StartCol int    `json:"startCol"`
	End      int    `json:"end"`
	EndCol   int    `json:"endCol"`
	Children []Node `json:"children,omitempty"`
}

// convertNode converts a tree-sitter node into our Node representation.
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

	if n.NamedChildCount() == 0 {
		text := n.Content(src)
		switch n.Type() {
		case "identifier":
			node.Name = text
		case "true":
			node.Value = "true"
		case "false":
			node.Value = "false"
		default:
			node.Value = text
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		node.Children = append(node.Children, convertNode(child, src))
	}
	return node
}
