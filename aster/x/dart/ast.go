package dart

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
)

// Node mirrors a tree-sitter node with optional position information. Only
// leaves that contain a real value (identifiers, literals, etc.) keep their raw
// text in the Text field so the JSON output stays compact.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Several typed aliases mimic common Dart AST structures so Program can expose
// a more descriptive type hierarchy while still relying on the generic Node
// internally. Only kinds that appear in the golden test are defined here.
type (
	ProgramNode                     Node
	Comment                         Node
	ClassDefinition                 Node
	ClassBody                       Node
	Declaration                     Node
	TypeIdentifier                  Node
	Identifier                      Node
	InitializedIdentifierList       Node
	InitializedIdentifier           Node
	ConstructorParam                Node
	FormalParameterList             Node
	FormalParameter                 Node
	FunctionSignature               Node
	FunctionBody                    Node
	Block                           Node
	ExpressionStatement             Node
	ForStatement                    Node
	ForLoopParts                    Node
	ForElement                      Node
	ListLiteral                     Node
	Arguments                       Node
	Argument                        Node
	NamedArgument                   Node
	ArgumentPart                    Node
	ConstantConstructorSignature    Node
	DecimalIntegerLiteral           Node
	StringLiteral                   Node
	EscapeSequence                  Node
	InitializedVariableDefinition   Node
	Label                           Node
	LocalVariableDeclaration        Node
	OptionalFormalParameters        Node
	Selector                        Node
	TypeArguments                   Node
	UnconditionalAssignableSelector Node
)

// Options controls how the AST is generated.
type Options struct {
	IncludePos bool
}

// isValueNode reports whether the given node type should keep its
// textual content. Keywords and pure syntax nodes are omitted.
func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "type_identifier", "decimal_integer_literal",
		"decimal_floating_point_literal",
		"string_literal", "escape_sequence", "comment",
		"true", "false", "void_type", "null",
		"final_builtin", "const_builtin", "inferred_type", "this",
		"required", "break_builtin", "continue_builtin",
		// operators that influence printing
		"additive_operator", "multiplicative_operator",
		"relational_operator", "equality_operator",
		"minus_operator", "negation_operator", "tilde_operator",
		"assignment_operator":
		return true
	default:
		return false
	}
}

// toNode converts the given tree-sitter node into our Node representation. Non
// semantic leaves are dropped so the resulting JSON stays small.
func toNode(n *sitter.Node, src []byte, pos bool) *Node {
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

	if isValueNode(n.Kind()) {
		node.Text = n.Utf8Text(src)
	} else if n.NamedChildCount() == 0 {
		// discard pure syntax leaves
		return nil
	}

	for i := uint(0); i < n.ChildCount(); i++ {
		child := n.Child(i)
		if !child.IsNamed() && !isValueNode(child.Kind()) {
			continue
		}
		if c := toNode(child, src, pos); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}

	return node
}
