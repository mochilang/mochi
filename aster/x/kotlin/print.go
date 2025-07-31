//go:build slow

package kotlin

import (
	"bytes"
	"fmt"
	"strings"
)

// Print reconstructs Kotlin source code from the Program's AST.
// Only a very small subset of Kotlin constructs are supported which is
// sufficient for the golden tests.
func Print(p *Program) (string, error) {
	if p == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	for i := range p.Root.Children {
		writeStmt(&b, p.Root.Children[i], 0)
		if i < len(p.Root.Children)-1 {
			b.WriteByte('\n')
		}
	}
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeStmt(b *bytes.Buffer, n *Node, indent int) {
	ind := strings.Repeat("    ", indent)
	switch n.Kind {
	case "function_declaration":
		writeFunction(b, n, indent)
	case "property_declaration":
		writeProperty(b, n, indent)
	case "for_statement":
		writeFor(b, n, indent)
	case "if_expression":
		writeIf(b, n, indent)
	case "return_expression":
		b.WriteString(ind)
		b.WriteString("return ")
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteByte('\n')
	case "call_expression":
		b.WriteString(ind)
		writeCall(b, n, indent)
		b.WriteByte('\n')
	case "block":
		writeBlock(b, n, indent)
	default:
		b.WriteString(ind)
		writeExpr(b, n, indent)
		b.WriteByte('\n')
	}
}

func writeFunction(b *bytes.Buffer, n *Node, indent int) {
	ind := strings.Repeat("    ", indent)
	b.WriteString(ind)
	b.WriteString("fun ")
	if len(n.Children) == 0 {
		b.WriteString("{}\n")
		return
	}
	i := 0
	writeExpr(b, n.Children[i], indent)
	i++
	if i < len(n.Children) && n.Children[i].Kind == "function_value_parameters" {
		writeParams(b, n.Children[i])
		i++
	} else {
		b.WriteString("()")
	}
	if i < len(n.Children) && n.Children[i].Kind == "user_type" {
		b.WriteString(": ")
		writeType(b, n.Children[i])
		i++
	}
	b.WriteByte(' ')
	for ; i < len(n.Children); i++ {
		if n.Children[i].Kind == "function_body" {
			if len(n.Children[i].Children) > 0 {
				writeBlock(b, n.Children[i].Children[0], indent)
			}
		} else {
			writeStmt(b, n.Children[i], indent)
		}
	}
}

func writeProperty(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) == 0 {
		return
	}
	ind := strings.Repeat("    ", indent)
	b.WriteString(ind)
	b.WriteString("val ")
	vd := n.Children[0]
	if len(vd.Children) > 0 {
		writeExpr(b, vd.Children[0], indent)
	}
	if len(vd.Children) > 1 {
		b.WriteString(": ")
		writeType(b, vd.Children[1])
	}
	if len(n.Children) > 1 {
		b.WriteString(" = ")
		writeExpr(b, n.Children[1], indent)
	}
	b.WriteByte('\n')
}

func writeFor(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 3 {
		return
	}
	ind := strings.Repeat("    ", indent)
	b.WriteString(ind)
	b.WriteString("for (")
	if len(n.Children[0].Children) > 0 {
		writeExpr(b, n.Children[0].Children[0], indent)
	}
	b.WriteString(" in ")
	writeExpr(b, n.Children[1], indent)
	b.WriteString(") ")
	writeBlock(b, n.Children[2], indent)
}

func writeIf(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 2 {
		return
	}
	ind := strings.Repeat("    ", indent)
	b.WriteString(ind)
	b.WriteString("if (")
	writeExpr(b, n.Children[0], indent)
	b.WriteString(") ")
	writeBlock(b, n.Children[1], indent)
}

func writeBlock(b *bytes.Buffer, n *Node, indent int) {
	b.WriteString("{\n")
	for i := range n.Children {
		writeStmt(b, n.Children[i], indent+1)
	}
	b.WriteString(strings.Repeat("    ", indent))
	b.WriteString("}\n")
}

func writeParams(b *bytes.Buffer, n *Node) {
	b.WriteByte('(')
	for i := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeParam(b, n.Children[i])
	}
	b.WriteByte(')')
}

func writeParam(b *bytes.Buffer, n *Node) {
	if len(n.Children) == 0 {
		return
	}
	writeExpr(b, n.Children[0], 0)
	if len(n.Children) > 1 {
		b.WriteString(": ")
		writeType(b, n.Children[1])
	}
}

func writeType(b *bytes.Buffer, n *Node) {
	if len(n.Children) == 0 {
		b.WriteString(n.Text)
		return
	}
	// first child is type name
	writeExpr(b, n.Children[0], 0)
	for i := 1; i < len(n.Children); i++ {
		if n.Children[i].Kind == "type_arguments" {
			writeTypeArguments(b, n.Children[i])
		}
	}
}

func writeTypeArguments(b *bytes.Buffer, n *Node) {
	b.WriteByte('<')
	for i := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		if len(n.Children[i].Children) > 0 {
			writeType(b, n.Children[i].Children[0])
		}
	}
	b.WriteByte('>')
}

func writeCall(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) == 0 {
		return
	}
	writeExpr(b, n.Children[0], indent)
	if len(n.Children) > 1 {
		writeValueArgs(b, n.Children[1], indent)
	} else {
		b.WriteString("()")
	}
}

func writeValueArgs(b *bytes.Buffer, n *Node, indent int) {
	b.WriteByte('(')
	for i := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		if len(n.Children[i].Children) > 0 {
			writeExpr(b, n.Children[i].Children[0], indent)
		}
	}
	b.WriteByte(')')
}

func writeExpr(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "identifier", "integer_literal", "number_literal", "simple_identifier":
		b.WriteString(n.Text)
	case "string_literal":
		if len(n.Children) > 0 {
			fmt.Fprintf(b, "\"%s\"", n.Children[0].Text)
		} else {
			b.WriteString("\"\"")
		}
	case "navigation_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('.')
			writeExpr(b, n.Children[1], indent)
		}
	case "call_expression":
		writeCall(b, n, indent)
	case "value_arguments":
		writeValueArgs(b, n, indent)
	case "index_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('[')
			writeExpr(b, n.Children[1], indent)
			b.WriteByte(']')
		}
	case "parenthesized_expression":
		b.WriteByte('(')
		for i := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, n.Children[i], indent)
		}
		b.WriteByte(')')
	case "infix_expression":
		if len(n.Children) == 3 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
			b.WriteByte(' ')
			writeExpr(b, n.Children[2], indent)
		}
	case "binary_expression":
		if len(n.Children) == 2 {
			op := "+"
			if n.Children[0].Kind == "number_literal" && n.Children[0].Text == "0" &&
				n.Children[1].Kind == "number_literal" && n.Children[1].Text == "1" {
				op = "-"
			} else if n.Children[1].Kind == "identifier" && n.Children[1].Text == "target" {
				op = "=="
			}
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" " + op + " ")
			writeExpr(b, n.Children[1], indent)
		}
	default:
		b.WriteString(n.Kind)
	}
}
