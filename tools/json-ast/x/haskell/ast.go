package haskell

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node describes a tree-sitter node.
type Node struct {
	Type     string `json:"type"`
	Start    int    `json:"start"`
	End      int    `json:"end"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// Program represents a parsed Haskell source file.
type Program struct {
	Root Node `json:"root"`
}

// convertNode converts a tree-sitter node into our Node representation.
func convertNode(n *sitter.Node, src []byte) Node {
	node := Node{
		Type:  n.Type(),
		Start: int(n.StartByte()),
		End:   int(n.EndByte()),
	}
	if n.ChildCount() == 0 {
		node.Text = n.Content(src)
		return node
	}
	for i := 0; i < int(n.ChildCount()); i++ {
		child := n.Child(i)
		if child == nil {
			continue
		}
		node.Children = append(node.Children, convertNode(child, src))
	}
	return node
}
