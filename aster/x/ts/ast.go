package ts

import (
	"encoding/json"

	sitter "github.com/tree-sitter/go-tree-sitter"
)

// Option controls how AST nodes are generated.
type Option struct {
	// Positions requests inclusion of positional information when true.
	Positions bool
}

// Node represents a simplified TypeScript AST node. Only leaf nodes that carry
// a value keep their text. Position fields are omitted from JSON when zero.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// The following typed aliases give a slightly more structured view mirroring
// the tree-sitter node kinds that appear in the golden tests.
type (
	ProgramNode             Node
	Arguments               Node
	Array                   Node
	ArrayPattern            Node
	ArrayType               Node
	ArrowFunction           Node
	AssignmentExpression    Node
	BinaryExpression        Node
	CallExpression          Node
	Comment                 Node
	ExportStatement         Node
	ExpressionStatement     Node
	ForInStatement          Node
	Identifier              Node
	InterfaceBody           Node
	InterfaceDeclaration    Node
	LexicalDeclaration      Node
	MemberExpression        Node
	Number                  Node
	Object                  Node
	Pair                    Node
	ParenthesizedExpression Node
	PredefinedType          Node
	PropertyIdentifier      Node
	PropertySignature       Node
	ReturnStatement         Node
	SpreadElement           Node
	StatementBlock          Node
	String                  Node
	StringFragment          Node
	SubscriptExpression     Node
	TypeAnnotation          Node
	TypeIdentifier          Node
	VariableDeclarator      Node
)

// convert transforms a tree-sitter node into a *Node using the given option.
func convert(n *sitter.Node, src []byte, opt Option) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Kind()}
	if opt.Positions {
		start := n.StartPosition()
		end := n.EndPosition()
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
		child := convert(n.NamedChild(i), src, opt)
		if child != nil {
			node.Children = append(node.Children, child)
		}
	}
	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

// isValueNode reports whether kind is a leaf node carrying a textual value.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "property_identifier", "type_identifier", "number",
		"string_fragment", "predefined_type", "comment":
		return true
	default:
		return false
	}
}

// Program represents a parsed TypeScript source file.
type Program struct {
	Statements []Statement `json:"statements"`
}

type Statement Node

// MarshalJSON ensures stable output ordering.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
