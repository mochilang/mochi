package fs

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns F# source code for the given Program. The output is
// reconstructed from the AST and only supports a limited subset of
// constructs used in the golden tests.
func Print(p *Program) (string, error) {
	if p == nil || p.Root == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	writeNode(&b, (*Node)(p.Root), 0)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeNode(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "file", "ERROR":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('\n')
			}
			writeNode(b, c, indent)
		}
	case "line_comment":
		b.WriteString(n.Text)
	case "function_or_value_defn":
		b.WriteString(strings.Repeat("    ", indent))
		b.WriteString("let ")
		if len(n.Children) >= 1 {
			writeExpr(b, n.Children[0])
		}
		if len(n.Children) == 3 {
			b.WriteString(": ")
			writeExpr(b, n.Children[1])
			b.WriteString(" = ")
			writeExpr(b, n.Children[2])
		} else if len(n.Children) == 2 {
			b.WriteString(" = ")
			writeExpr(b, n.Children[1])
		}
	default:
		writeExpr(b, n)
	}
}

func writeExpr(b *bytes.Buffer, n *Node) {
	switch n.Kind {
	case "identifier":
		b.WriteString(n.Name)
	case "int", "string":
		b.WriteString(n.Text)
	case "long_identifier_or_op", "long_identifier":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('.')
			}
			writeExpr(b, c)
		}
	case "application_expression":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, c)
		}
	case "list_expression":
		b.WriteByte('[')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString("; ")
			}
			writeExpr(b, c)
		}
		b.WriteByte(']')
	case "index_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0])
			b.WriteString(".[")
			writeExpr(b, n.Children[1])
			b.WriteByte(']')
		}
	case "paren_expression":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, c)
		}
		b.WriteByte(')')
	case "infix_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0])
			b.WriteString(" + ")
			writeExpr(b, n.Children[1])
		}
	default:
		// unsupported nodes are ignored
	}
}
