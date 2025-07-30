package python

import sitter "github.com/smacker/go-tree-sitter"

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

// toNode converts a tree-sitter node to a Node.
// toNode converts a tree-sitter node into our Node structure. It filters out
// pure syntax leaves to keep the JSON representation small.
// toNode converts a tree-sitter node into a Node. When withPos is true the
// positional information is recorded in the Node, otherwise those fields remain
// zero and will be omitted from the resulting JSON due to the omitempty tags.
func toNode(n *sitter.Node, src []byte, withPos bool) *Node {
	if n == nil {
		return nil
	}

	start := n.StartPoint()
	end := n.EndPoint()
	node := &Node{Kind: n.Type()}
	if withPos {
		node.Start = int(n.StartByte())
		node.StartCol = int(start.Column)
		node.End = int(n.EndByte())
		node.EndCol = int(end.Column)
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
		if c := toNode(child, src, withPos); c != nil {
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
	case "identifier", "integer", "string", "comment":
		return true
	default:
		return false
	}
}
