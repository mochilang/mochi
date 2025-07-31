//go:build slow

package py

import sitter "github.com/tree-sitter/go-tree-sitter"

// Node represents a simplified Python AST node converted from tree-sitter.
// Positional fields are omitted from JSON when zero so callers can decide
// whether they are needed.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Option controls how the AST is generated. When Positions is true the
// Start/End fields of nodes are populated, otherwise they remain zero and are
// omitted from the marshalled JSON due to the `omitempty` tags.
type Option struct {
	Positions bool
}

// Typed aliases to expose a slightly more structured API while still using
// Node internally. Only the node kinds that appear in tests are listed.
type (
	Module              Node
	Comment             Node
	DecoratedDefinition Node
	Decorator           Node
	ClassDefinition     Node
	FunctionDefinition  Node
	Parameters          Node
	ExpressionStatement Node
	Assignment          Node
	ReturnStatement     Node
	Identifier          Node
	Integer             Node
	Float               Node
	String              Node
	True                Node
	False               Node
	None                Node
	Call                Node
	Attribute           Node
	List                Node
	ListComprehension   Node
	Dictionary          Node
	Pair                Node
	ForStatement        Node
	ForInClause         Node
	ArgumentList        Node
	Lambda              Node
	LambdaParameters    Node
	Type                Node
)

// convert transforms a tree-sitter node into our Node representation. When
// opt.Positions is true the position fields are populated, otherwise they remain
// zero and are omitted from the JSON output. Non-value leaf nodes are removed to
// keep the result minimal.
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
		if isValueNode(node.Kind) {
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

// isValueNode reports whether the given node kind represents a leaf that
// carries textual content worth keeping in the JSON representation.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "integer", "float", "string", "true", "false", "none", "comment", "string_content":
		return true
	default:
		return false
	}
}
