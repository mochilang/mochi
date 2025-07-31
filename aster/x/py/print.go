//go:build slow

package py

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns Python source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil || p.File == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	writeModule(&b, p.File, 0)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeModule(b *bytes.Buffer, m *Module, indent int) {
	for _, c := range m.Children {
		writeStmt(b, c, indent)
	}
}

func writeStmt(b *bytes.Buffer, n *Node, indent int) {
	ind := strings.Repeat("    ", indent)
	switch n.Kind {
	case "comment":
		b.WriteString(ind)
		b.WriteString(n.Text)
		b.WriteByte('\n')
	case "expression_statement":
		if len(n.Children) > 0 {
			c := n.Children[0]
			if c.Kind == "assignment" {
				b.WriteString(ind)
				writeAssignment(b, c, indent)
			} else {
				b.WriteString(ind)
				writeExpr(b, c, indent)
			}
			b.WriteByte('\n')
		}
	case "assignment":
		b.WriteString(ind)
		writeAssignment(b, n, indent)
	case "return_statement":
		b.WriteString(ind)
		b.WriteString("return")
		if len(n.Children) > 0 {
			b.WriteByte(' ')
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteByte('\n')
	case "function_definition":
		if len(n.Children) >= 3 {
			b.WriteString(ind)
			b.WriteString("def ")
			writeExpr(b, n.Children[0], indent)
			writeParameters(b, n.Children[1])
			b.WriteString(":\n")
			writeBlock(b, n.Children[2], indent+1)
		}
	case "for_statement":
		if len(n.Children) >= 3 {
			b.WriteString(ind)
			b.WriteString("for ")
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" in ")
			writeExpr(b, n.Children[1], indent)
			b.WriteString(":\n")
			writeBlock(b, n.Children[2], indent+1)
		}
	case "if_statement":
		if len(n.Children) >= 2 {
			b.WriteString(ind)
			b.WriteString("if ")
			writeExpr(b, n.Children[0], indent)
			b.WriteString(":\n")
			writeBlock(b, n.Children[1], indent+1)
		}
	case "block":
		writeBlock(b, n, indent)
	default:
		// treat as expression statement
		b.WriteString(ind)
		writeExpr(b, n, indent)
		b.WriteByte('\n')
	}
}

func writeBlock(b *bytes.Buffer, n *Node, indent int) {
	for _, c := range n.Children {
		writeStmt(b, c, indent)
	}
}

func writeAssignment(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) != 2 {
		b.WriteString("assignment")
		return
	}
	writeExpr(b, n.Children[0], indent)
	b.WriteString(" = ")
	writeExpr(b, n.Children[1], indent)
}

func writeParameters(b *bytes.Buffer, n *Node) {
	b.WriteByte('(')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeExpr(b, c, 0)
	}
	b.WriteByte(')')
}

func writeExpr(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "identifier", "integer", "float", "true", "false", "none":
		b.WriteString(n.Text)
	case "string":
		if len(n.Children) > 0 {
			fmt.Fprintf(b, "%q", n.Children[0].Text)
		} else {
			b.WriteString("\"\"")
		}
	case "call":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('(')
			for i, arg := range n.Children[1].Children {
				if i > 0 {
					b.WriteString(", ")
				}
				writeExpr(b, arg, indent)
			}
			b.WriteByte(')')
		}
	case "argument_list":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(')')
	case "binary_operator":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" + ")
			writeExpr(b, n.Children[1], indent)
		}
	case "unary_operator":
		if len(n.Children) == 1 {
			b.WriteByte('-')
			writeExpr(b, n.Children[0], indent)
		}
	case "comparison_operator":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" == ")
			writeExpr(b, n.Children[1], indent)
		}
	case "list":
		b.WriteByte('[')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(']')
	case "subscript":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('[')
			writeExpr(b, n.Children[1], indent)
			b.WriteByte(']')
		}
	default:
		// unsupported; write kind
		b.WriteString(n.Kind)
	}
}
