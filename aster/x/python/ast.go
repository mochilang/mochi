package python

import sitter "github.com/tree-sitter/go-tree-sitter"

// Node represents a simplified Python AST node converted from tree-sitter.
// The positional fields are omitted from the JSON when zero so callers can
// choose whether to include them.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Option controls AST generation. When Positions is true the Start/End fields
// of nodes are populated; otherwise they remain zero and are omitted from the
// JSON output due to the `omitempty` tags.
type Option struct {
	Positions bool
}

// Several typed aliases are provided so Program can expose a more structured
// representation while still using Node internally. Only node kinds that appear
// in the golden test are defined here.
type (
	Module                Node
	Comment               Node
	FutureImportStatement Node
	ImportFromStatement   Node
	DecoratedDefinition   Node
	Decorator             Node
	ClassDefinition       Node
	ExpressionStatement   Node
	Assignment            Node
	Identifier            Node
	Integer               Node
	String                Node
	Call                  Node
	Attribute             Node
	List                  Node
	ListComprehension     Node
	ForStatement          Node
	ForInClause           Node
	ArgumentList          Node
	Type                  Node
)

// convert transforms a tree-sitter node into our Node representation. Pure
// syntax leaves are omitted. When opt.Positions is true the Start/End fields are
// populated; otherwise they remain zero and are omitted from the JSON due to the
// `omitempty` tags.
func convert(n *sitter.Node, src []byte, opt Option) *Node {
	if n == nil {
		return nil
	}

	start := n.StartPosition()
	end := n.EndPosition()
	node := &Node{Kind: n.Kind()}
	if opt.Positions {
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Kind()) {
			node.Text = n.Utf8Text(src)
		} else {
			return nil
		}
	}

	for i := uint(0); i < n.NamedChildCount(); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		if c := convert(child, src, opt); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

// isValueNode reports whether a node kind should appear as a leaf with text in
// the final JSON representation.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "integer", "string", "string_content", "comment":
		return true
	default:
		return false
	}
}
