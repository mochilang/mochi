package dart

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a node in the Dart syntax tree. Only leaf nodes
// carry their raw source text in the Text field to keep the JSON
// output compact.
type Node struct {
	Type     string `json:"type"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// convertNode recursively converts the given tree-sitter node to a Node
// using the provided source code for leaf values.
func convertNode(n *sitter.Node, src []byte) Node {
	node := Node{Type: n.Type()}
	if n.ChildCount() == 0 {
		node.Text = n.Content(src)
	}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		node.Children = append(node.Children, convertNode(child, src))
	}
	return node
}
