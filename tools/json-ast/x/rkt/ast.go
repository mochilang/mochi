package rkt

import sitter "github.com/smacker/go-tree-sitter"

// Node represents a tree-sitter node in a minimal form. Only nodes that
// contain textual data carry a Text value while the structural information is
// expressed through the Kind and the list of Children.  This keeps the JSON
// representation compact while still exposing all information from the parse
// tree.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start"`
	StartCol int     `json:"startCol"`
	End      int     `json:"end"`
	EndCol   int     `json:"endCol"`
	Children []*Node `json:"children,omitempty"`
}

// convertProgram converts the root tree-sitter node into a Program.
func convertProgram(root *sitter.Node, src []byte) *Program {
	if root == nil {
		return &Program{}
	}
	return &Program{Root: convertNode(root, src)}
}

// convertNode converts a tree-sitter node into our Node representation.
func convertNode(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	start := n.StartPoint()
	end := n.EndPoint()
	node := &Node{
		Kind:     n.Type(),
		Start:    int(start.Row) + 1,
		StartCol: int(start.Column),
		End:      int(end.Row) + 1,
		EndCol:   int(end.Column),
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Type()) {
			node.Text = n.Content(src)
		} else {
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if c := convertNode(child, src); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if node.Text == "" && len(node.Children) == 0 {
		return nil
	}
	return node
}

func isValueNode(kind string) bool {
	switch kind {
	case "comment", "lang_name", "number", "string", "symbol", "list", "quote", "extension":
		return true
	default:
		return false
	}
}
