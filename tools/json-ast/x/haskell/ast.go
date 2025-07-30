package haskell

import sitter "github.com/smacker/go-tree-sitter"

// Node represents a node in the parsed Haskell AST.
type Node struct {
	Type     string        `json:"node"`
	Children []interface{} `json:"children,omitempty"`
}

// convert recursively converts a tree-sitter node into our Node structure.
// Leaf nodes that have no named children store their source text as a string
// in the Children slice.
func convert(n *sitter.Node, src []byte) Node {
	node := Node{Type: n.Type()}
	if n.NamedChildCount() == 0 {
		// If the node has no named children we keep its raw text.
		node.Children = append(node.Children, n.Content(src))
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
