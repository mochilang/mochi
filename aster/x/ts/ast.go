package ts

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
)

// IncludePositions controls whether Start/End position fields are populated.
// When false (the default) these fields remain zero and are omitted from the
// marshalled JSON due to the `omitempty` tags on the struct fields.
var IncludePositions bool

// Node represents a node in the TypeScript AST.  Leaf nodes that carry a value
// populate the Text field.  Position fields are omitted from the marshalled
// JSON unless IncludePositions is set to true.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// The following aliases give a slightly typed structure mirroring common
// tree-sitter node kinds that appear in the tests.  Only nodes that may carry a
// textual value or are referenced directly are enumerated here.
type (
	ProgramNode             Node
	ExpressionStatement     Node
	CallExpression          Node
	MemberExpression        Node
	Arguments               Node
	Identifier              Node
	PropertyIdentifier      Node
	String                  Node
	StringFragment          Node
	Number                  Node
	ExportStatement         Node
	InterfaceDeclaration    Node
	InterfaceBody           Node
	PropertySignature       Node
	PredefinedType          Node
	TypeIdentifier          Node
	TypeAnnotation          Node
	VariableDeclarator      Node
	LexicalDeclaration      Node
	StatementBlock          Node
	ForInStatement          Node
	ReturnStatement         Node
	BinaryExpression        Node
	AssignmentExpression    Node
	Pair                    Node
	ArrowFunction           Node
	ParenthesizedExpression Node
	SubscriptExpression     Node
	Array                   Node
	ArrayPattern            Node
	ArrayType               Node
	Object                  Node
	SpreadElement           Node
	Comment                 Node
)

// Program is the root of a parsed TypeScript source file.
type Program struct {
	Root *ProgramNode `json:"root"`
}

// convert transforms a tree-sitter node into a *Node. Non-value leaves are
// omitted. When pos is true positional information is recorded.
func convert(n *sitter.Node, src []byte, pos bool) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Kind()}
	if pos {
		start := n.StartPosition()
		end := n.EndPosition()
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
		if c := convert(child, src, pos); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}

	return node
}

// isValueNode reports whether a node type represents a value that should be kept.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "property_identifier", "type_identifier", "number", "string_fragment", "predefined_type", "comment":
		return true
	default:
		return false
	}
}
