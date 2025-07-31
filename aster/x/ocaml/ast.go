package ocaml

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
)

// Node represents a simplified OCaml AST node. Only nodes that carry
// meaningful values keep their text to minimise the resulting JSON.
// Source positions include both line and column information.
// Options controls how the AST is produced.
// When IncludePositions is false, all position fields in the resulting JSON
// will be omitted.
type Options struct {
	IncludePositions bool
}

// Node represents a simplified OCaml AST node. Only nodes that carry
// meaningful values keep their text to minimise the resulting JSON.
// Source positions include both line and column information.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// File is the root syntactic element of an OCaml source file.
type (
	// CompilationUnit is the root syntactic element of an OCaml source file.
	CompilationUnit struct{ Node }

	ApplicationExpression   struct{ Node }
	Comment                 struct{ Node }
	ConsExpression          struct{ Node }
	ConsPattern             struct{ Node }
	ConstructorDeclaration  struct{ Node }
	ConstructorName         struct{ Node }
	ConstructorPath         struct{ Node }
	ConstructorPattern      struct{ Node }
	DoClause                struct{ Node }
	ElseClause              struct{ Node }
	ExceptionDefinition     struct{ Node }
	ForExpression           struct{ Node }
	FunExpression           struct{ Node }
	IfExpression            struct{ Node }
	InfixExpression         struct{ Node }
	LetBinding              struct{ Node }
	LetExpression           struct{ Node }
	LetOpenExpression       struct{ Node }
	ListExpression          struct{ Node }
	MatchCase               struct{ Node }
	MatchExpression         struct{ Node }
	ModuleName              struct{ Node }
	ModulePath              struct{ Node }
	Number                  struct{ Node }
	OpenModule              struct{ Node }
	Parameter               struct{ Node }
	ParenthesizedExpression struct{ Node }
	ParenthesizedPattern    struct{ Node }
	PrefixExpression        struct{ Node }
	ProductExpression       struct{ Node }
	RelOperator             struct{ Node }
	SequenceExpression      struct{ Node }
	String                  struct{ Node }
	StringContent           struct{ Node }
	ThenClause              struct{ Node }
	TryExpression           struct{ Node }
	TuplePattern            struct{ Node }
	TypedExpression         struct{ Node }
	Unit                    struct{ Node }
	ValueDefinition         struct{ Node }
	ValueName               struct{ Node }
	ValuePath               struct{ Node }
	ValuePattern            struct{ Node }
)

type File = CompilationUnit

// Program wraps a parsed OCaml source file.
type Program struct {
	File CompilationUnit `json:"file"`
}

// convert recursively converts a tree-sitter node into a Node.
// Leaf nodes keep their raw text while internal nodes only preserve
// their named children.
// convert recursively converts a tree-sitter node into our Node structure.
// Leaf nodes that don't carry useful values are skipped entirely.
func convert(n *sitter.Node, src []byte, pos bool) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Kind()}
	if pos {
		sp := n.StartPosition()
		ep := n.EndPosition()
		node.Start = int(sp.Row) + 1
		node.StartCol = int(sp.Column)
		node.End = int(ep.Row) + 1
		node.EndCol = int(ep.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Kind()) {
			node.Text = n.Utf8Text(src)
			return node
		}
		return nil
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := convert(n.NamedChild(uint(i)), src, pos)
		if child != nil {
			node.Children = append(node.Children, child)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

// isValueNode reports whether a node kind holds a textual value that should be kept.
func isValueNode(kind string) bool {
	switch kind {
	case "comment", "constructor_name", "module_name", "number",
		"rel_operator", "string", "string_content", "unit",
		"value_name", "value_pattern", "boolean",
		"add_operator", "sign_operator", "prefix_operator",
		"assign_operator":
		return true
	default:
		return false
	}
}
