package fs

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a F# AST node converted from tree-sitter.
// Node mirrors a tree-sitter node in a JSON-friendly form.
type Node struct {
	Kind     string `json:"kind"`
	Start    int    `json:"start"`
	End      int    `json:"end"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// convertNode recursively converts a tree-sitter Node into our Node structure.
func convertNode(n *sitter.Node, src []byte) Node {
	node := Node{
		Kind:  n.Type(),
		Start: int(n.StartByte()),
		End:   int(n.EndByte()),
	}

	if n.NamedChildCount() == 0 {
		node.Text = n.Content(src)
		return node
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
