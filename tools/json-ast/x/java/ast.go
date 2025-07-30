package java

import sitter "github.com/smacker/go-tree-sitter"

// IncludePos toggles whether positional information should be populated on
// nodes produced by this package. When false, the position fields remain zero
// and are omitted from the resulting JSON due to the `omitempty` tags.
var IncludePos bool

// Node represents a minimal JSON-serialisable AST node.  Only named
// tree-sitter nodes are kept to reduce noise.  Leaf nodes store the raw
// source text in Text.
// Node represents a minimal JSON-serialisable AST node. Only named
// tree-sitter nodes are kept to reduce noise. Leaf nodes store their
// raw source text in Text. Position information is recorded using row
// and column numbers (1-based rows).
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Typed aliases expose a more structured API while still using Node for the
// underlying representation. Only node kinds that appear in golden tests are
// enumerated here. Additional kinds will still be represented by Node at
// runtime.
type (
	ProgramNode              Node
	Assign                   Node
	Block                    Node
	Binary                   Node
	Break                    Node
	Call                     Node
	Cast                     Node
	Cond                     Node
	Continue                 Node
	Expr                     Node
	FnDecl                   Node
	ForEach                  Node
	ForRange                 Node
	InstanceOf               Node
	Ident                    Node
	If                       Node
	Index                    Node
	Lambda                   Node
	Literal                  Node
	MemberReference          Node
	Member                   Node
	NewArray                 Node
	NewClass                 Node
	NullLiteral              Node
	Print                    Node
	Return                   Node
	String                   Node
	Try                      Node
	Unary                    Node
	VarDecl                  Node
	While                    Node
	ArgumentList             Node
	ArrayCreationExpression  Node
	ArrayInitializer         Node
	ArrayType                Node
	AssignmentExpression     Node
	BinaryExpression         Node
	BlockStmt                Node
	CastExpression           Node
	ClassBody                Node
	ClassDeclaration         Node
	ConstructorBody          Node
	ConstructorDeclaration   Node
	DecimalIntegerLiteral    Node
	EnhancedForStatement     Node
	ExpressionStatement      Node
	False                    Node
	FieldAccess              Node
	FieldDeclaration         Node
	ForStatement             Node
	FormalParameter          Node
	FormalParameters         Node
	GenericType              Node
	Identifier               Node
	IfStatement              Node
	LocalVariableDeclaration Node
	MethodDeclaration        Node
	MethodInvocation         Node
	ObjectCreationExpression Node
	ParenthesizedExpression  Node
	ReturnStatement          Node
	ScopedTypeIdentifier     Node
	StringFragment           Node
	StringLiteral            Node
	This                     Node
	True                     Node
	TypeArguments            Node
	TypeIdentifier           Node
	UnaryExpression          Node
	UpdateExpression         Node
	VariableDeclarator       Node
)

// convert creates a Node from a tree-sitter node.
func convert(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Type()}
	if IncludePos {
		sp := n.StartPoint()
		ep := n.EndPoint()
		node.Start = int(sp.Row) + 1
		node.StartCol = int(sp.Column)
		node.End = int(ep.Row) + 1
		node.EndCol = int(ep.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(node.Kind) {
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

// isValueNode reports whether the given tree-sitter kind represents a leaf that
// carries meaningful text in the final JSON output. Pure syntax tokens are
// ignored to keep the representation compact.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "type_identifier", "decimal_integer_literal",
		"hex_integer_literal", "octal_integer_literal", "binary_integer_literal",
		"decimal_floating_point_literal", "hex_floating_point_literal",
		"string_literal", "string_fragment", "character_literal",
		"true", "false", "null", "this":
		return true
	default:
		return false
	}
}
