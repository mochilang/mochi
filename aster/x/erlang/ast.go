package erlang

import (
        sitter "github.com/tree-sitter/go-tree-sitter"
)

// Node represents an Erlang AST node produced from tree-sitter.  Leaf nodes
// that carry a meaningful value (identifiers, literals, comments) populate the
// Text field; purely syntactic leaves are omitted.  Start and End are 1-based
// line numbers while StartCol and EndCol are zero-based column positions.
// Node mirrors a tree-sitter node.  Only leaf nodes that carry a semantic
// value populate the Text field.  Position fields are omitted unless requested
// via Option.Positions.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// SourceFile is the root of an Erlang program.
// SourceFile is the root node for an Erlang source file.
type SourceFile Node

// The following aliases provide a slightly typed representation mirroring
// tree-sitter's node kinds that appear in the golden tests.  Only nodes that
// can carry a value are represented here to keep the structure minimal.
type (
	Error             Node
	AnonymousFun      Node
	Arity             Node
	Atom              Node
	BinaryOpExpr      Node
	Call              Node
	ClauseBody        Node
	Comment           Node
	ExportAttribute   Node
	ExprArgs          Node
	Fa                Node
	FunClause         Node
	FunDecl           Node
	FunctionClause    Node
	Generator         Node
	Integer           Node
	LcExprs           Node
	List              Node
	ListComprehension Node
	MapExpr           Node
	MapField          Node
	MatchExpr         Node
	ModuleAttribute   Node
	ParenExpr         Node
	Remote            Node
	RemoteModule      Node
	String            Node
	Var               Node
)

// Option controls how the AST is generated.
type Option struct {
	// Positions includes line/column information when true.
	Positions bool
}

// convert converts a tree-sitter Node into our Node representation.
func convert(n *sitter.Node, src []byte, opt Option) *Node {
	if n == nil {
		return nil
	}
	start := n.StartPoint()
	end := n.EndPoint()
	node := &Node{Kind: n.Type()}
	if opt.Positions {
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Type()) {
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
		if c := convert(child, src, opt); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

func isValueNode(kind string) bool {
	switch kind {
	case "atom", "integer", "string", "var", "comment":
		return true
	default:
		return false
	}
}
