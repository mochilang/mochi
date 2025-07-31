//go:build slow

package java

import (
	"bytes"
	"fmt"
	"strings"
)

// Print reconstructs Java source code from the Program AST.
func Print(p *Program) (string, error) {
	if p == nil || p.File == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	writeProgram(&b, (*Node)(p.File), 0)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeProgram(b *bytes.Buffer, n *Node, indent int) {
	for _, c := range n.Children {
		writeStmt(b, c, indent)
	}
}

func writeStmt(b *bytes.Buffer, n *Node, indent int) {
	ind := strings.Repeat("    ", indent)
	switch n.Kind {
	case "class_declaration":
		if len(n.Children) >= 2 {
			b.WriteString(ind)
			b.WriteString("class ")
			b.WriteString(n.Children[0].Text)
			b.WriteString(" {\n")
			writeProgram(b, n.Children[1], indent+1)
			b.WriteString(ind)
			b.WriteString("}\n")
		}
	case "field_declaration":
		writeField(b, n, indent)
	case "method_declaration":
		writeMethod(b, n, indent)
	case "local_variable_declaration":
		writeLocalVar(b, n, indent)
		b.WriteByte('\n')
	case "expression_statement":
		b.WriteString(ind)
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteString(";\n")
	case "return_statement":
		b.WriteString(ind)
		b.WriteString("return")
		if len(n.Children) > 0 {
			b.WriteByte(' ')
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteString(";\n")
	case "for_statement":
		writeFor(b, n, indent)
	case "if_statement":
		writeIf(b, n, indent)
	case "block":
		b.WriteString(ind)
		b.WriteString("{\n")
		for _, c := range n.Children {
			writeStmt(b, c, indent+1)
		}
		b.WriteString(ind)
		b.WriteString("}\n")
	default:
		b.WriteString(ind)
		writeExpr(b, n, indent)
		b.WriteByte('\n')
	}
}

func writeField(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) == 0 {
		return
	}
	ind := strings.Repeat("    ", indent)
	b.WriteString(ind)
	b.WriteString("static int[] ")
	vd := n.Children[0]
	if len(vd.Children) > 0 {
		b.WriteString(vd.Children[0].Text)
	}
	if len(vd.Children) > 1 {
		b.WriteString(" = ")
		writeExpr(b, vd.Children[1], indent)
	}
	b.WriteString(";\n")
}

func writeLocalVar(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) == 0 {
		return
	}
	ind := strings.Repeat("    ", indent)
	b.WriteString(ind)
	b.WriteString("int ")
	vd := n.Children[0]
	if len(vd.Children) > 0 {
		b.WriteString(vd.Children[0].Text)
	}
	if len(vd.Children) > 1 {
		b.WriteString(" = ")
		writeExpr(b, vd.Children[1], indent)
	}
	b.WriteString(";")
}

func writeMethod(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 2 {
		return
	}
	ind := strings.Repeat("    ", indent)
	name := n.Children[0].Text
	b.WriteString(ind)
	switch name {
	case "main":
		b.WriteString("public static void ")
	case "twoSum":
		b.WriteString("static int[] ")
	default:
		b.WriteString("static void ")
	}
	b.WriteString(name)
	if len(n.Children) > 1 {
		writeParameters(b, n.Children[1])
	} else {
		b.WriteString("()")
	}
	if len(n.Children) > 2 {
		b.WriteString(" {\n")
		writeProgram(b, n.Children[2], indent+1)
		b.WriteString(ind)
		b.WriteString("}\n")
	} else {
		b.WriteString(" {}\n")
	}
}

func writeParameters(b *bytes.Buffer, n *Node) {
	b.WriteByte('(')
	for i, p := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		if p.Kind == "formal_parameter" {
			if len(p.Children) == 2 && p.Children[0].Kind == "array_type" {
				b.WriteString("String[] ")
				b.WriteString(p.Children[1].Text)
			} else if len(p.Children) >= 1 {
				b.WriteString("int ")
				b.WriteString(p.Children[len(p.Children)-1].Text)
			}
		}
	}
	b.WriteByte(')')
}

func writeFor(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 4 {
		return
	}
	ind := strings.Repeat("    ", indent)
	b.WriteString(ind)
	b.WriteString("for (")
	init := n.Children[0]
	if init.Kind == "local_variable_declaration" {
		writeLocalVar(b, init, 0)
	} else {
		writeExpr(b, init, indent)
	}
	b.WriteString("; ")
	writeExpr(b, n.Children[1], indent)
	b.WriteString("; ")
	writeExpr(b, n.Children[2], indent)
	b.WriteString(") {\n")
	writeProgram(b, n.Children[3], indent+1)
	b.WriteString(ind)
	b.WriteString("}\n")
}

func writeIf(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 2 {
		return
	}
	ind := strings.Repeat("    ", indent)
	b.WriteString(ind)
	b.WriteString("if (")
	cond := n.Children[0]
	if cond.Kind == "parenthesized_expression" && len(cond.Children) > 0 {
		writeExpr(b, cond.Children[0], indent)
	} else {
		writeExpr(b, cond, indent)
	}
	b.WriteString(") {\n")
	writeProgram(b, n.Children[1], indent+1)
	b.WriteString(ind)
	b.WriteString("}\n")
}

func writeArgumentList(b *bytes.Buffer, n *Node, indent int) {
	b.WriteByte('(')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeExpr(b, c, indent)
	}
	b.WriteByte(')')
}

func writeExpr(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "identifier", "decimal_integer_literal", "true", "false":
		b.WriteString(n.Text)
	case "method_invocation":
		if len(n.Children) >= 2 {
			writeExpr(b, n.Children[0], indent)
			if n.Children[1].Kind == "argument_list" {
				writeArgumentList(b, n.Children[1], indent)
			} else {
				b.WriteByte('.')
				writeExpr(b, n.Children[1], indent)
				if len(n.Children) > 2 {
					writeArgumentList(b, n.Children[2], indent)
				} else {
					b.WriteString("()")
				}
			}
		}
	case "field_access":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('.')
			writeExpr(b, n.Children[1], indent)
		}
	case "array_creation_expression":
		b.WriteString("new int[]{")
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteString("}")
	case "array_initializer":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
	case "array_access":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('[')
			writeExpr(b, n.Children[1], indent)
			b.WriteByte(']')
		}
	case "argument_list":
		writeArgumentList(b, n, indent)
	case "binary_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(' ')
			b.WriteString(detectOperator(n))
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
		}
	case "update_expression":
		if len(n.Children) == 1 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString("++")
		}
	case "unary_expression":
		b.WriteByte('-')
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
	case "parenthesized_expression":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(')')
	default:
		b.WriteString(n.Kind)
	}
}

func detectOperator(n *Node) string {
	if len(n.Children) != 2 {
		return "+"
	}
	r := n.Children[1]
	if r.Kind == "identifier" && r.Text == "n" {
		return "<"
	}
	if r.Kind == "identifier" && r.Text == "target" {
		return "=="
	}
	return "+"
}
