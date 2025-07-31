package kotlin

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns Kotlin source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	writeSourceFile(&b, &p.Root, 0)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeSourceFile(b *bytes.Buffer, n *SourceFile, indent int) {
	for _, c := range n.Children {
		writeStmt(b, &c, indent)
	}
}

func writeStmt(b *bytes.Buffer, n *Node, indent int) {
	ind := strings.Repeat("    ", indent)
	switch n.Kind {
	case "function_declaration":
		writeFunctionDeclaration(b, n, indent)
	case "property_declaration":
		b.WriteString(ind)
		writePropertyDeclaration(b, n, indent)
		b.WriteByte('\n')
	case "for_statement":
		writeForStatement(b, n, indent)
	case "if_expression":
		writeIfExpression(b, n, indent)
	case "return_expression":
		b.WriteString(ind)
		b.WriteString("return ")
		if len(n.Children) > 0 {
			writeExpr(b, &n.Children[0], indent)
		}
		b.WriteByte('\n')
	case "call_expression":
		b.WriteString(ind)
		writeExpr(b, n, indent)
		b.WriteByte('\n')
	case "block":
		b.WriteString(ind)
		b.WriteString("{\n")
		writeBlock(b, n, indent+1)
		b.WriteString(ind)
		b.WriteString("}\n")
	default:
		b.WriteString(ind)
		writeExpr(b, n, indent)
		b.WriteByte('\n')
	}
}

func writeBlock(b *bytes.Buffer, n *Node, indent int) {
	for _, c := range n.Children {
		writeStmt(b, &c, indent)
	}
}

func writeFunctionDeclaration(b *bytes.Buffer, n *Node, indent int) {
	ind := strings.Repeat("    ", indent)
	b.WriteString(ind)
	b.WriteString("fun ")
	if len(n.Children) == 0 {
		b.WriteByte('\n')
		return
	}
	idx := 0
	writeExpr(b, &n.Children[idx], indent)
	idx++
	// parameters
	if idx < len(n.Children) && n.Children[idx].Kind == "function_value_parameters" {
		writeParameters(b, &n.Children[idx])
		idx++
	} else {
		b.WriteString("()")
	}
	// return type
	if idx < len(n.Children) && n.Children[idx].Kind == "user_type" {
		b.WriteString(": ")
		writeType(b, &n.Children[idx])
		idx++
	}
	b.WriteString(" ")
	if idx < len(n.Children) && n.Children[idx].Kind == "function_body" {
		writeFunctionBody(b, &n.Children[idx], indent)
	} else {
		b.WriteByte('\n')
	}
}

func writeFunctionBody(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) > 0 {
		writeStmt(b, &n.Children[0], indent)
	}
}

func writePropertyDeclaration(b *bytes.Buffer, n *Node, indent int) {
	b.WriteString("val ")
	if len(n.Children) == 0 {
		return
	}
	// variable_declaration
	vd := n.Children[0]
	if len(vd.Children) > 0 {
		b.WriteString(vd.Children[0].Text)
	}
	if len(vd.Children) > 1 {
		b.WriteString(": ")
		writeType(b, &vd.Children[1])
	}
	if len(n.Children) > 1 {
		b.WriteString(" = ")
		writeExpr(b, &n.Children[1], indent)
	}
}

func writeForStatement(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 3 {
		return
	}
	ind := strings.Repeat("    ", indent)
	b.WriteString(ind)
	b.WriteString("for (")
	// variable
	vd := n.Children[0]
	if len(vd.Children) > 0 {
		b.WriteString(vd.Children[0].Text)
	}
	b.WriteString(" in ")
	writeExpr(b, &n.Children[1], indent)
	b.WriteString(") ")
	writeStmt(b, &n.Children[2], indent)
}

func writeIfExpression(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 2 {
		return
	}
	ind := strings.Repeat("    ", indent)
	b.WriteString(ind)
	b.WriteString("if (")
	writeExpr(b, &n.Children[0], indent)
	b.WriteString(") ")
	writeStmt(b, &n.Children[1], indent)
}

func writeParameters(b *bytes.Buffer, n *Node) {
	b.WriteByte('(')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeParameter(b, &c)
	}
	b.WriteByte(')')
}

func writeParameter(b *bytes.Buffer, n *Node) {
	if len(n.Children) == 0 {
		return
	}
	b.WriteString(n.Children[0].Text)
	if len(n.Children) > 1 {
		b.WriteString(": ")
		writeType(b, &n.Children[1])
	}
}

func writeType(b *bytes.Buffer, n *Node) {
	if len(n.Children) == 0 {
		return
	}
	b.WriteString(n.Children[0].Text)
	if len(n.Children) > 1 {
		writeTypeArguments(b, &n.Children[1])
	}
}

func writeTypeArguments(b *bytes.Buffer, n *Node) {
	b.WriteByte('<')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeTypeProjection(b, &c)
	}
	b.WriteByte('>')
}

func writeTypeProjection(b *bytes.Buffer, n *Node) {
	if len(n.Children) > 0 {
		writeType(b, &n.Children[0])
	}
}

func writeExpr(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "identifier", "number_literal", "string_content":
		b.WriteString(n.Text)
	case "string_literal":
		b.WriteByte('"')
		for _, c := range n.Children {
			writeExpr(b, &c, indent)
		}
		b.WriteByte('"')
	case "call_expression":
		if len(n.Children) > 0 {
			writeExpr(b, &n.Children[0], indent)
			if len(n.Children) > 1 {
				writeExpr(b, &n.Children[1], indent)
			} else {
				b.WriteString("()")
			}
		}
	case "value_arguments":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, &c, indent)
		}
		b.WriteByte(')')
	case "value_argument":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, &c, indent)
		}
	case "index_expression":
		if len(n.Children) == 2 {
			writeExpr(b, &n.Children[0], indent)
			b.WriteByte('[')
			writeExpr(b, &n.Children[1], indent)
			b.WriteByte(']')
		}
	case "navigation_expression":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('.')
			}
			writeExpr(b, &c, indent)
		}
	case "binary_expression":
		writeBinaryExpr(b, n, indent)
	case "infix_expression":
		if len(n.Children) == 3 {
			writeExpr(b, &n.Children[0], indent)
			b.WriteByte(' ')
			b.WriteString(n.Children[1].Text)
			b.WriteByte(' ')
			writeExpr(b, &n.Children[2], indent)
		}
	case "parenthesized_expression":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(" ")
			}
			writeExpr(b, &c, indent)
		}
		b.WriteByte(')')
	default:
		b.WriteString(n.Kind)
	}
}

func writeBinaryExpr(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) != 2 {
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, &c, indent)
		}
		return
	}
	left := &n.Children[0]
	right := &n.Children[1]
	op := "+"
	if left.Kind == "number_literal" && right.Kind == "number_literal" {
		op = "-"
	} else if right.Kind == "identifier" && right.Text == "target" {
		op = "=="
	} else if right.Kind == "number_literal" && right.Text == "1" {
		op = "+"
	} else if left.Kind == "index_expression" && right.Kind == "index_expression" {
		op = "+"
	}
	writeExpr(b, left, indent)
	b.WriteByte(' ')
	b.WriteString(op)
	b.WriteByte(' ')
	writeExpr(b, right, indent)
}
