package java

import sitter "github.com/tree-sitter/go-tree-sitter"

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

// The following aliases mirror the node kinds that appear in the JSON output.
// Only nodes that may hold textual values are represented to keep the
// structure compact.
type (
	SourceFile               struct{ Node }
	ClassDeclaration         struct{ Node }
	ConstructorDeclaration   struct{ Node }
	ConstructorBody          struct{ Node }
	MethodDeclaration        struct{ Node }
	FormalParameters         struct{ Node }
	FormalParameter          struct{ Node }
	ArrayType                struct{ Node }
	ArrayCreationExpression  struct{ Node }
	ArrayInitializer         struct{ Node }
	VariableDeclarator       struct{ Node }
	AssignmentExpression     struct{ Node }
	UpdateExpression         struct{ Node }
	UnaryExpression          struct{ Node }
	BinaryExpression         struct{ Node }
	CastExpression           struct{ Node }
	ObjectCreationExpression struct{ Node }
	ScopedTypeIdentifier     struct{ Node }
	TypeArguments            struct{ Node }
	GenericType              struct{ Node }
	TypeIdentifier           struct{ Node }
	Identifier               struct{ Node }
	ClassBody                struct{ Node }
	Block                    struct{ Node }
	IfStatement              struct{ Node }
	ForStatement             struct{ Node }
	EnhancedForStatement     struct{ Node }
	ExpressionStatement      struct{ Node }
	ReturnStatement          struct{ Node }
	MethodInvocation         struct{ Node }
	FieldAccess              struct{ Node }
	ArgumentList             struct{ Node }
	StringLiteral            struct{ Node }
	StringFragment           struct{ Node }
	DecimalIntegerLiteral    struct{ Node }
	True                     struct{ Node }
	False                    struct{ Node }
	This                     struct{ Node }
)

// Option configures AST conversion behaviour.
type Option struct {
	// WithPositions includes source position information when true.
	WithPositions bool
}

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
		"true", "false", "null", "this":
		return true
	default:
		return false
	}
}
