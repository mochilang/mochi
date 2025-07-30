package lua

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a tree-sitter node in the Lua syntax tree.
type Node struct {
	Type     string `json:"type"`
	Value    string `json:"value,omitempty"`
	Line     int    `json:"line"`
	Col      int    `json:"col"`
	EndLine  int    `json:"endLine"`
	EndCol   int    `json:"endCol"`
	Children []Node `json:"children,omitempty"`
}

// convertNode converts a tree-sitter node to our Node representation.
func convertNode(n *sitter.Node, src []byte) Node {
	start := n.StartPoint()
	end := n.EndPoint()
	node := Node{
		Type:    n.Type(),
		Line:    int(start.Row) + 1,
		Col:     int(start.Column),
		EndLine: int(end.Row) + 1,
		EndCol:  int(end.Column),
	}
	if n.ChildCount() == 0 {
		node.Value = n.Content(src)
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
