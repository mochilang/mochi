package zig

import sitter "github.com/smacker/go-tree-sitter"

// Node represents a tree-sitter node in Zig's syntax tree.
type Node struct {
	Type     string `json:"type"`
	Start    int    `json:"start"`
	End      int    `json:"end"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// convertNode converts a tree-sitter Node into our AST Node.
func convertNode(n *sitter.Node, src []byte) Node {
	node := Node{
		Type:  n.Type(),
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
		node.Children = append(node.Children, convertNode(child, src))
	}
	return node
}
