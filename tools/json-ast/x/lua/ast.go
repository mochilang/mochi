package lua

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node is a minimal representation of a tree-sitter node. Only leaf nodes store
// their source text in the Text field. Internal nodes simply record their kind
// and children. This keeps the generated JSON small while still containing all
// information required to fully reconstruct the syntax tree.
type Node struct {
	Kind     string `json:"kind"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// convertNode converts a tree-sitter node to our Node representation.
func convertNode(n *sitter.Node, src []byte) Node {
	node := Node{Kind: n.Type()}
	if n.ChildCount() == 0 {
		node.Text = n.Content(src)
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
