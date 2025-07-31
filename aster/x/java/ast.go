package java

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
	"strings"
)

// Node represents a minimal JSON-serialisable AST node.  Only named
// tree-sitter nodes are kept to reduce noise.  Leaf nodes store the raw
// source text in Text.
// Node represents a minimal JSON-serialisable AST node. Only named
// tree-sitter nodes are kept to reduce noise. Leaf nodes store their
// raw source text in Text. Position information is recorded using row
// and column numbers (1-based rows).
// Node represents a minimal AST node used in the JSON output. Only
// named tree-sitter nodes are retained. Leaf nodes that carry a value
// expose it through the Text field. When the positions are omitted the
// related fields remain zero and disappear from the marshalled JSON due
// to the `omitempty` tags.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Options controls how the AST is generated.
// When Positions is false (the default) the position fields are left zero
// and therefore omitted from the JSON output.
type Options struct {
	Positions bool
}

// The following aliases mirror the node kinds that appear in the JSON output.
// Only nodes that may hold textual values are represented to keep the
// structure compact.
type (
	ProgramNode              Node
	ClassDeclaration         Node
	ClassBody                Node
	FieldDeclaration         Node
	VariableDeclarator       Node
	ArrayCreationExpression  Node
	ArrayInitializer         Node
	ArrayAccess              Node
	ArrayType                Node
	AssignmentExpression     Node
	BinaryExpression         Node
	UnaryExpression          Node
	UpdateExpression         Node
	CastExpression           Node
	ObjectCreationExpression Node
	ParenthesizedExpression  Node
	ForStatement             Node
	EnhancedForStatement     Node
	IfStatement              Node
	ReturnStatement          Node
	LocalVariableDeclaration Node
	MethodDeclaration        Node
	ConstructorDeclaration   Node
	ConstructorBody          Node
	FormalParameters         Node
	FormalParameter          Node
	ArgumentList             Node
	MethodInvocation         Node
	FieldAccess              Node
	GenericType              Node
	ScopedTypeIdentifier     Node
	Identifier               Node
	TypeIdentifier           Node
	TypeArguments            Node
	Block                    Node
	ExpressionStatement      Node
	StringLiteral            Node
	StringFragment           Node
	DecimalIntegerLiteral    Node
	True                     Node
	False                    Node
	This                     Node
)

// convert transforms a tree-sitter node into our Node representation. When
// withPos is true positional information is recorded, otherwise the fields
// remain zero and are omitted from the JSON output.
func convert(n *sitter.Node, src []byte, withPos bool) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Kind()}
	if withPos {
		sp := n.StartPosition()
		ep := n.EndPosition()
		node.Start = int(sp.Row) + 1
		node.StartCol = int(sp.Column)
		node.End = int(ep.Row) + 1
		node.EndCol = int(ep.Column)
	}

	if n.Kind() == "binary_expression" && n.ChildCount() >= 3 {
		op := n.Child(1)
		if op != nil && !op.IsNamed() {
			node.Text = strings.TrimSpace(op.Utf8Text(src))
		}
	} else if n.Kind() == "unary_expression" && n.ChildCount() >= 2 {
		op := n.Child(0)
		if op != nil && !op.IsNamed() {
			node.Text = strings.TrimSpace(op.Utf8Text(src))
		}
	} else if n.Kind() == "update_expression" && n.ChildCount() >= 2 {
		op := n.Child(n.ChildCount() - 1)
		if op != nil && !op.IsNamed() {
			node.Text = strings.TrimSpace(op.Utf8Text(src))
		}
	}

	if n.Kind() == "string_literal" {
		node.Text = n.Utf8Text(src)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(node.Kind) {
			node.Text = n.Utf8Text(src)
		} else {
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(uint(i))
		if child == nil {
			continue
		}
		if c := convert(child, src, withPos); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}

	return node
}

// isValueNode reports whether the given tree-sitter kind represents a leaf that
// carries meaningful text in the final JSON output. Pure syntax tokens are
// ignored to keep the representation compact.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "type_identifier", "decimal_integer_literal",
		"hex_integer_literal", "octal_integer_literal", "binary_integer_literal",
		"decimal_floating_point_literal", "hex_floating_point_literal",
		"string_literal", "string_fragment", "character_literal",
		"true", "false", "null", "null_literal", "this",
		"integral_type", "floating_point_type", "void_type", "boolean_type", "primitive_type":
		return true
	default:
		return false
	}
}
