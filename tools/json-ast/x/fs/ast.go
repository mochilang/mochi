package fs

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a F# AST node converted from tree-sitter.
// Node mirrors a tree-sitter node in a JSON-friendly form.
type Node struct {
	Kind     string `json:"kind"`
	Name     string `json:"name,omitempty"`
	Text     string `json:"text,omitempty"`
	Start    int    `json:"start"`
	StartCol int    `json:"startCol"`
	End      int    `json:"end"`
	EndCol   int    `json:"endCol"`
	Children []Node `json:"children,omitempty"`
}

// convertNode recursively converts a tree-sitter Node into our Node structure.
func convertNode(n *sitter.Node, src []byte) Node {
	start := n.StartPoint()
	end := n.EndPoint()
	node := Node{
		Kind:     n.Type(),
		Start:    int(start.Row) + 1,
		StartCol: int(start.Column),
		End:      int(end.Row) + 1,
		EndCol:   int(end.Column),
	}

	if n.NamedChildCount() == 0 {
		text := n.Content(src)
		if n.Type() == "identifier" {
			node.Name = text
		} else {
			node.Text = text
		}
		if node.Name == "" && node.Text == "" {
			return Node{}
		}
		return node
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		c := convertNode(child, src)
		if c.Name == "" && c.Text == "" && len(c.Children) == 0 {
			continue
		}
		node.Children = append(node.Children, c)
	}
	return node
}
