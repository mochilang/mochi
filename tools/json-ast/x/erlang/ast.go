package erlang

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents an Erlang AST node produced from tree-sitter.  Leaf nodes
// that carry a meaningful value (identifiers, literals, comments) populate the
// Text field; purely syntactic leaves are omitted.  Start and End are 1-based
// line numbers while StartCol and EndCol are zero-based column positions.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start"`
	StartCol int     `json:"startCol"`
	End      int     `json:"end"`
	EndCol   int     `json:"endCol"`
	Children []*Node `json:"children,omitempty"`
}

// convert converts a tree-sitter Node into our Node representation.
func convert(n *sitter.Node, src []byte) *Node {
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
		if child == nil {
			continue
		}
		if c := convert(child, src); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

func isValueNode(kind string) bool {
	switch kind {
	case "atom", "integer", "string", "var", "comment":
		return true
	default:
		return false
	}
}
