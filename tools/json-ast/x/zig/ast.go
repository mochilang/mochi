package zig

import sitter "github.com/smacker/go-tree-sitter"

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

// SourceFile is the root of a Zig program.
type SourceFile struct{ Node }

// Additional concrete node types mirroring the Zig grammar. They simply embed
// Node so the JSON structure remains unchanged while giving Go callers richer
// types to work with.
type (
	LineComment         struct{ Node }
	ArrayTypeStart      struct{ Node }
	AssignExpr          struct{ Node }
	BuiltinIdentifier   struct{ Node }
	BinaryExpr          struct{ Node }
	Block               struct{ Node }
	BlockExpr           struct{ Node }
	BlockLabel          struct{ Node }
	BreakLabel          struct{ Node }
	BuildinTypeExpr     struct{ Node }
	ContainerDecl       struct{ Node }
	ContainerField      struct{ Node }
	ErrorUnionExpr      struct{ Node }
	EscapeSequence      struct{ Node }
	FieldInit           struct{ Node }
	FieldOrFnCall       struct{ Node }
	FnCallArguments     struct{ Node }
	FnProto             struct{ Node }
	ForPrefix           struct{ Node }
	ForStatement        struct{ Node }
	FormatSequence      struct{ Node }
	Identifier          struct{ Node }
	Integer             struct{ Node }
	InitList            struct{ Node }
	LabeledStatement    struct{ Node }
	LabeledTypeExpr     struct{ Node }
	LoopStatement       struct{ Node }
	PrefixTypeOp        struct{ Node }
	PtrIndexPayload     struct{ Node }
	StringLiteralSingle struct{ Node }
	Statement           struct{ Node }
	SuffixExpr          struct{ Node }
	TopLevelDecl        struct{ Node }
	UnaryExpr           struct{ Node }
	VarDecl             struct{ Node }
)

// Options control how the AST is produced.
type Options struct {
	// Positions includes line/column information when true.
	Positions bool
}

// valueKinds lists tree-sitter node types that contain a textual value we want
// to keep in the AST.
var valueKinds = map[string]struct{}{
	"IDENTIFIER":          {},
	"BUILTINIDENTIFIER":   {},
	"BuildinTypeExpr":     {},
	"STRINGLITERALSINGLE": {},
	"INTEGER":             {},
	"EscapeSequence":      {},
	"FormatSequence":      {},
	"line_comment":        {},
}

// convert transforms a tree-sitter node into our Node representation. It
// returns ok=false when the node holds no semantic value and should be skipped
// from the final JSON.
func convert(n *sitter.Node, src []byte, withPos bool) (node Node, ok bool) {
	node = Node{Kind: n.Type()}
	if withPos {
		start := n.StartPoint()
		end := n.EndPoint()
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if _, keep := valueKinds[n.Type()]; keep {
			node.Text = n.Content(src)
			return node, true
		}
		// skip pure syntax leaf
		return Node{}, false
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		if c, ok := convert(child, src, withPos); ok {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return Node{}, false
	}
	return node, true
}
