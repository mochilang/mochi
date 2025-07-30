package erlang

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents an Erlang AST node converted from tree-sitter.
type Node struct {
	Kind     string  `json:"kind"`
	Start    int     `json:"start"`
	End      int     `json:"end"`
	Text     string  `json:"text,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// convert converts a tree-sitter Node into our Node representation.
func convert(n *sitter.Node, src []byte) *Node {
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
	for i := 0; i < int(n.ChildCount()); i++ {
		child := n.Child(i)
		if child == nil {
			continue
		}
		node.Children = append(node.Children, convert(child, src))
	}
	return node
}
