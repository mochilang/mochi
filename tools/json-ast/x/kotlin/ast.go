package kotlin

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a tree-sitter node in a simplified AST form.
type Node struct {
	Kind     string  `json:"kind"`
	Start    int     `json:"start"`
	End      int     `json:"end"`
	Text     string  `json:"text,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// toNode converts a tree-sitter Node into our AST Node structure.
func toNode(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	node := &Node{
		Kind:  n.Type(),
		Start: int(n.StartByte()),
		End:   int(n.EndByte()),
	}
	if n.ChildCount() == 0 {
		node.Text = n.Content(src)
	}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		node.Children = append(node.Children, toNode(child, src))
	}
	return node
}

type SourceFile struct{ Node }
