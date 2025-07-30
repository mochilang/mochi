package zig

import sitter "github.com/smacker/go-tree-sitter"

// Node represents a simplified AST node for Zig. Only nodes that carry some
// semantic value are kept so that the resulting JSON is compact.
type Node struct {
	Kind     string `json:"kind"`
	Start    int    `json:"start"`
	End      int    `json:"end"`
	StartCol int    `json:"startCol"`
	EndCol   int    `json:"endCol"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// valueKinds lists tree-sitter node types that contain a textual value we want
// to keep in the AST.
var valueKinds = map[string]struct{}{
	"IDENTIFIER":          {},
	"BUILTINIDENTIFIER":   {},
	"BuildinTypeExpr":     {},
	"STRINGLITERALSINGLE": {},
	"INTEGER":             {},
	"EscapeSequence":      {},
	"FormatSequence":      {},
	"line_comment":        {},
}

// convertNode converts a tree-sitter node into a Node. It returns ok=false if
// the node does not contain any semantic information and should be omitted.
func convertNode(n *sitter.Node, src []byte) (node Node, ok bool) {
	start := n.StartPoint()
	end := n.EndPoint()
	node = Node{
		Kind:     n.Type(),
		Start:    int(start.Row) + 1,
		StartCol: int(start.Column),
		End:      int(end.Row) + 1,
		EndCol:   int(end.Column),
	}

	if n.NamedChildCount() == 0 {
		if _, keep := valueKinds[n.Type()]; keep {
			node.Text = n.Content(src)
			return node, true
		}
		// skip pure syntax leaf
		return Node{}, false
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		if c, ok := convertNode(child, src); ok {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return Node{}, false
	}
	return node, true
}
