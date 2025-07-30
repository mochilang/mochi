package fs

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a F# AST node converted from tree-sitter.
type Node struct {
	Kind     string `json:"kind"`
	Start    int    `json:"start"`
	End      int    `json:"end"`
	Children []Node `json:"children,omitempty"`
}

// convertNode recursively converts a tree-sitter Node into our Node structure.
func convertNode(n *sitter.Node, src []byte) Node {
	node := Node{
		Kind:  n.Type(),
		Start: int(n.StartByte()),
		End:   int(n.EndByte()),
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
