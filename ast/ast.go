package ast

import (
	"fmt"
	"regexp"
	"strings"

	"github.com/alecthomas/participle/v2/lexer"
)

// Node is a simplified, uniform AST representation (inspired by Lisp)
type Node struct {
	Kind  string
	Value any
	Pos   lexer.Position

	Children []*Node
}

// Print writes a compact Lisp-like tree to stdout
func (n *Node) Print(indent string) {
	fmt.Print(n.String())
}

// String returns the pretty-printed AST as a compact Lisp-like string
func (n *Node) String() string {
	var b strings.Builder
	write(&b, n, "")
	b.WriteString("\n")
	return b.String()
}

// write is a helper to recursively pretty-print the AST
func write(b *strings.Builder, n *Node, indent string) {
	b.WriteString(indent + "(" + n.Kind)
	if n.Value != nil {
		b.WriteString(" " + formatValue(n.Value))
	}

	if len(n.Children) == 0 {
		b.WriteString(")")
		return
	}

	// Check if all children are leaf nodes
	allLeaf := true
	for _, c := range n.Children {
		if len(c.Children) > 0 {
			allLeaf = false
			break
		}
	}

	if allLeaf {
		for _, c := range n.Children {
			b.WriteString(" (" + c.Kind)
			if c.Value != nil {
				b.WriteString(" " + formatValue(c.Value))
			}
			b.WriteString(")")
		}
		b.WriteString(")")
	} else {
		b.WriteString("\n")
		for _, c := range n.Children {
			write(b, c, indent+"  ")
			b.WriteString("\n")
		}
		b.WriteString(indent + ")")
	}
}

// safeIdent matches values that don't need quoting in AST output.
// Now includes common operators.
var safeIdent = regexp.MustCompile(`^[a-zA-Z0-9._\-+=*/<>!%&|^~]+$`)

// formatValue returns a string representation of a value,
// quoting only if necessary.
func formatValue(v any) string {
	switch s := v.(type) {
	case string:
		if safeIdent.MatchString(s) {
			return s
		}
		return fmt.Sprintf("%q", s)
	default:
		return fmt.Sprintf("%v", v)
	}
}
