package zig

import sitter "github.com/tree-sitter/go-tree-sitter"

// Node represents a simplified AST node for Zig. Only nodes that carry some
// semantic value are kept so that the resulting JSON is compact.
// Node represents a simplified AST node for Zig. Only nodes that carry some
// semantic value are kept so that the resulting JSON is compact. Position
// fields are optional and omitted from the JSON when zero.
type Node struct {
	Kind     string `json:"kind"`
	Text     string `json:"text,omitempty"`
	Start    int    `json:"start,omitempty"`
	End      int    `json:"end,omitempty"`
	StartCol int    `json:"startCol,omitempty"`
	EndCol   int    `json:"endCol,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// Typed aliases provide a slightly richer structure when embedding Nodes into
// a Program. Only node kinds that appear in the golden tests are enumerated
// here. Additional kinds still use Node directly.
type (
	SourceFile      Node
	TopLevelDecl    Node
	VarDecl         Node
	FnProto         Node
	Block           Node
	Statement       Node
	ErrorUnionExpr  Node
	SuffixExpr      Node
	FieldOrFnCall   Node
	FnCallArguments Node
	Identifier      Node
	BuiltinIdent    Node
	StringLiteral   Node
	BuildinTypeExpr Node
	InitList        Node
)

// Options control how the AST is produced.
type Options struct {
	// Positions includes line/column information when true.
	Positions bool
}

// valueKinds lists tree-sitter node types that contain a textual value we want
// to keep in the AST.
var valueKinds = map[string]struct{}{
	"identifier":         {},
	"builtin_identifier": {},
	"builtin_type":       {},
	"string_content":     {},
	"integer":            {},
	"float":              {},
	"escape_sequence":    {},
	"format_sequence":    {},
	"comment":            {},
}

// convertNode converts a tree-sitter node into a Node. It returns ok=false if
// the node does not contain any semantic information and should be omitted.
func convertNode(n *sitter.Node, src []byte, withPos bool) (node Node, ok bool) {
	node = Node{Kind: n.Kind()}
	if withPos {
		start := n.StartPosition()
		end := n.EndPosition()
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if _, keep := valueKinds[n.Kind()]; keep {
			node.Text = n.Utf8Text(src)
			return node, true
		}
		switch n.Kind() {
		case "break_expression", "continue_expression":
			return node, true
		default:
			// skip pure syntax leaf
			return Node{}, false
		}
	}

	for i := uint(0); i < n.NamedChildCount(); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		if c, ok := convertNode(child, src, withPos); ok {
			node.Children = append(node.Children, c)
		}
	}

	if node.Kind == "binary_expression" && n.ChildCount() >= 3 {
		op := n.Child(1)
		node.Text = op.Utf8Text(src)
	}

	if len(node.Children) == 0 && node.Text == "" {
		return Node{}, false
	}
	return node, true
}
