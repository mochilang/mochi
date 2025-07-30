package cpp

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a tree-sitter node in a generic form with position
// information. Leaf nodes also carry their text content.
// Node is a minimal representation of a C++ AST node. It records only
// the node kind and textual value for leaves, keeping the structure
// lightweight.
type Node struct {
	Kind     string `json:"kind"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// convert turns a tree-sitter node into our Node representation.
func convert(n *sitter.Node, src []byte) Node {
	if n == nil {
		return Node{}
	}
	node := Node{Kind: n.Type()}
	if n.NamedChildCount() == 0 {
		node.Text = n.Content(src)
		return node
	}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		node.Children = append(node.Children, convert(child, src))
	}
	return node
}
