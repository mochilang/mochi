package ts

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
)

// Option controls how the AST is generated.
// When Positions is true the Start/End fields are populated, otherwise they
// remain zero and are omitted from the marshalled JSON due to the omitempty
// tags on the struct fields.
type Option struct {
	Positions bool
}

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
	Root   *ProgramNode `json:"root"`
	Source string       `json:"-"`
}

// convert transforms a tree-sitter node into a *Node. Non-value leaves are
// omitted. When opt.Positions is true positional information is recorded.
func convert(n *sitter.Node, src []byte, opt Option) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Kind()}

	// Capture operator text and declaration kind from special fields so the
	// printer can faithfully reconstruct the source code without relying on
	// the original text.
	switch n.Kind() {
	case "binary_expression", "assignment_expression", "unary_expression", "update_expression":
		if op := n.ChildByFieldName("operator"); op != nil {
			node.Text = op.Utf8Text(src)
		}
	case "lexical_declaration":
		if kw := n.ChildByFieldName("kind"); kw != nil {
			node.Text = kw.Utf8Text(src)
		}
	case "variable_declaration":
		node.Text = "var"
	}

	if opt.Positions {
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
		if c := convert(child, src, opt); c != nil {
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
	case "identifier", "property_identifier", "type_identifier", "number", "string_fragment", "predefined_type", "comment", "break_statement", "continue_statement", "null":
		return true
	default:
		return false
	}
}
