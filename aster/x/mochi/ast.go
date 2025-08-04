package mochi

import (
	"fmt"

	mochiast "mochi/ast"
)

// Option controls how AST nodes are generated.
type Option struct {
	// WithPositions populates the positional fields when true. Since the
	// underlying AST currently lacks precise offsets these fields remain
	// zero even when requested.
	WithPositions bool
	// Filename associates parsed programs with a source file so diagnostics
	// can show snippets.
	Filename string
}

// Node represents a simplified Mochi AST node. Leaf nodes that carry a textual
// value populate the Text field. Position information follows 1-based line
// numbers and 0-based columns similar to tree-sitter.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Typed node aliases mirror the simplified Mochi grammar used in the tests.
// Only node kinds that carry values or appear in the golden files are
// enumerated here so Program can expose a slightly richer structure.
type (
	ProgramNode struct{ Node }
	Fun         struct{ Node }
	Param       struct{ Node }
	Type        struct{ Node }
	Let         struct{ Node }
	Call        struct{ Node }
	Selector    struct{ Node }
	List        struct{ Node }
	Map         struct{ Node }
	Entry       struct{ Node }
	Int         struct{ Node }
	String      struct{ Node }
	Range       struct{ Node }
	Block       struct{ Node }
	For         struct{ Node }
	Binary      struct{ Node }
	Index       struct{ Node }
	If          struct{ Node }
	Return      struct{ Node }
	Unary       struct{ Node }
	Query       struct{ Node }
	Select      struct{ Node }
	From        struct{ Node }
	In          struct{ Node }
	Source      struct{ Node }
)

// Program represents a parsed Mochi file. The root node is the program itself.
type Program struct {
	File     *ProgramNode `json:"file"`
	Source   string       `json:"-"`
	Filename string       `json:"-"`
}

// convert transforms a node from the generic ast package into our Node
// representation. When withPos is true the Start/End fields would be populated
// if the underlying AST exposed them. Non-value leaves are omitted.
func convert(n *mochiast.Node, withPos bool) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Kind}
	if n.Value != nil {
		node.Text = fmt.Sprint(n.Value)
	}
	if withPos {
		// The mochi/ast package does not currently retain precise
		// position information so the fields remain zero.
	}
	for _, c := range n.Children {
		if child := convert(c, withPos); child != nil {
			node.Children = append(node.Children, child)
		}
	}

	// Preserve control flow statements like break and continue even if they
	// have no text or children so the printed output can reconstruct the
	// original program structure.
	if len(node.Children) == 0 && node.Text == "" {
		switch node.Kind {
		case "break", "continue", "string":
			// keep empty literal or control flow marker
		default:
			return nil
		}
	}
	return node
}
