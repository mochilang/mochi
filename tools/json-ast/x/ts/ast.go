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

	if n.NamedChildCount() == 0 {
		node.Text = n.Content(src)
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		node.Children = append(node.Children, convertNode(child, src))
	}
	return node
}
