package c

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
)

// IncludePos controls whether positional information should be recorded when
// converting tree-sitter nodes. When false (the default) the position fields
// remain zero and are omitted from the marshalled JSON due to the `omitempty`
// struct tags.
// IncludePos controls whether positional information is recorded when
// converting tree-sitter nodes. When false (the default) the position
// fields remain zero and are omitted from the JSON output due to the
// `omitempty` struct tags. Use InspectWithOption to override this
// behaviour on a per-call basis.
var IncludePos bool

// Option controls how the AST is generated. When Positions is true the
// Start/End fields of nodes are populated, otherwise they remain zero.
type Option struct {
	Positions bool
}

// Node represents a tree-sitter node with byte offsets and optional text.
// Node represents a simplified syntax tree node. Only nodes that carry useful
// textual values are kept in the tree. Position information is stored using
// line/column pairs for easier cross language comparison.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Typed aliases give a clearer structure when composing a Program. Only node
// kinds that hold values or appear in the golden tests are enumerated here.
// Additional kinds will still be represented by Node at runtime.
type (
	TranslationUnit      Node
	PreprocInclude       Node
	Declaration          Node
	FunctionDefinition   Node
	ParameterDeclaration Node
	ParameterList        Node
	CompoundStatement    Node
	ExpressionStatement  Node
	ReturnStatement      Node
	CallExpression       Node
	AssignmentExpression Node
	BinaryExpression     Node
	IfStatement          Node
	ForStatement         Node
	WhileStatement       Node
	StructSpecifier      Node
	Identifier           Node
	NumberLiteral        Node
	CharLiteral          Node
	StringLiteral        Node
	SystemLibString      Node
	PrimitiveType        Node
	TypeIdentifier       Node
	StringContent        Node
	EscapeSequence       Node
	Comment              Node
	BreakStatement       Node
	ContinueStatement    Node
)

// convert transforms a tree-sitter node into a *Node. When pos is true the
// resulting node includes positional information.
func convert(n *sitter.Node, src []byte, pos bool) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Kind()}

	// Capture operator text for expressions where tree-sitter stores it in an
	// unnamed child field. This information is required when reconstructing the
	// original source from the AST.
	switch n.Kind() {
	case "binary_expression", "assignment_expression", "update_expression", "unary_expression", "pointer_expression":
		if op := n.ChildByFieldName("operator"); op != nil {
			node.Text = op.Utf8Text(src)
		}
	}
	if pos {
		start := n.StartPosition()
		end := n.EndPosition()
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Kind()) || n.Kind() == "break_statement" || n.Kind() == "continue_statement" {
			node.Text = n.Utf8Text(src)
		} else if node.Kind == "comment" {
			node.Text = n.Utf8Text(src)
		} else {
			// skip pure syntax leaves
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(uint(i))
		if child == nil {
			continue
		}
		if c := convert(child, src, pos); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	switch node.Kind {
	case "declaration":
		// All pieces of a declaration are kept as children so the source
		// can be reconstructed without relying on the original text.
	case "function_definition":
		// Function definitions are printed from their children, so we no
		// longer fall back to storing the original text.
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "field_identifier", "number_literal", "char_literal", "string_literal",
		"system_lib_string", "primitive_type", "type_identifier",
		"storage_class_specifier", "type_qualifier", "sized_type_specifier",
		"string_content", "escape_sequence", "comment":
		return true
	default:
		return false
	}
}
