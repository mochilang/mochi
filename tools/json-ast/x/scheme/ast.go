package scheme

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node mirrors a tree-sitter node. Leaf nodes store their source text in Text
// while internal nodes only keep their named children.
type Node struct {
	Kind     string `json:"kind"`
	Text     string `json:"text,omitempty"`
	Start    int    `json:"start"`
	StartCol int    `json:"startCol"`
	End      int    `json:"end"`
	EndCol   int    `json:"endCol"`
	Children []Node `json:"children,omitempty"`
}

// Program represents a parsed Scheme source file.
type Program struct {
	Forms []Node `json:"forms"`
}

// convertNode converts a tree-sitter node to our Node representation.
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

// convertProgram converts the root tree-sitter node into a Program.
func convertProgram(root *sitter.Node, src []byte) *Program {
	if root == nil {
		return &Program{}
	}
	var forms []Node
	for i := 0; i < int(root.NamedChildCount()); i++ {
		forms = append(forms, convertNode(root.NamedChild(i), src))
	}
	return &Program{Forms: forms}
}
