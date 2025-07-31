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

func isTypeKind(kind string) bool {
	switch kind {
	case "integral_type", "floating_point_type", "void_type", "array_type", "type_identifier", "boolean_type", "primitive_type", "generic_type", "scoped_type_identifier":
		return true
	default:
		return false
	}
}

func writeType(b *bytes.Buffer, n *Node) {
	if n == nil {
		return
	}
	switch n.Kind {
	case "array_type":
		if len(n.Children) > 0 {
			writeType(b, n.Children[0])
		}
		b.WriteString("[]")
	case "generic_type":
		for _, c := range n.Children {
			if c.Kind == "type_arguments" {
				writeTypeArguments(b, c)
			} else {
				writeType(b, c)
			}
		}
	case "type_arguments":
		writeTypeArguments(b, n)
	case "scoped_type_identifier":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('.')
			}
			writeType(b, c)
		}
	case "integral_type", "floating_point_type", "void_type", "type_identifier", "boolean_type", "primitive_type":
		b.WriteString(n.Text)
	default:
		writeExpr(b, n, 0)
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
	case "constructor_declaration":
		writeConstructor(b, n, indent)
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
	case "enhanced_for_statement":
		writeEnhancedFor(b, n, indent)
	case "if_statement":
		writeIf(b, n, indent)
	case "try_statement":
		writeTry(b, n, indent)
	case "break_statement":
		b.WriteString(ind)
		b.WriteString("break;\n")
	case "continue_statement":
		b.WriteString(ind)
		b.WriteString("continue;\n")
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
	b.WriteString("static ")
	writeType(b, n.Children[0])
	b.WriteByte(' ')
	vd := n.Children[1]
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
	writeType(b, n.Children[0])
	b.WriteByte(' ')
	vd := n.Children[1]
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
	b.WriteString(ind)
	idx := 0
	var name string
	if isTypeKind(n.Children[0].Kind) {
		name = n.Children[1].Text
		if name == "main" {
			b.WriteString("public static ")
		} else {
			b.WriteString("static ")
		}
		writeType(b, n.Children[0])
		b.WriteByte(' ')
		idx = 2
	} else {
		name = n.Children[0].Text
		if name == "main" {
			b.WriteString("public static void ")
		} else {
			b.WriteString("static void ")
		}
		idx = 1
	}
	b.WriteString(name)
	if idx < len(n.Children) && n.Children[idx].Kind == "formal_parameters" {
		writeParameters(b, n.Children[idx])
		idx++
	} else {
		b.WriteString("()")
	}
	if idx < len(n.Children) {
		b.WriteString(" {\n")
		writeProgram(b, n.Children[idx], indent+1)
		b.WriteString(ind)
		b.WriteString("}\n")
	} else {
		b.WriteString(" {}\n")
	}
}

func writeConstructor(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 1 {
		return
	}
	ind := strings.Repeat("    ", indent)
	b.WriteString(ind)
	name := n.Children[0].Text
	b.WriteString(name)
	idx := 1
	if idx < len(n.Children) && n.Children[idx].Kind == "formal_parameters" {
		writeParameters(b, n.Children[idx])
		idx++
	} else {
		b.WriteString("()")
	}
	if idx < len(n.Children) {
		b.WriteString(" {\n")
		writeProgram(b, n.Children[idx], indent+1)
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
			if len(p.Children) == 2 {
				writeType(b, p.Children[0])
				b.WriteByte(' ')
				b.WriteString(p.Children[1].Text)
			} else if len(p.Children) == 1 {
				writeType(b, p.Children[0])
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
		var tmp bytes.Buffer
		writeLocalVar(&tmp, init, 0)
		s := strings.TrimSuffix(tmp.String(), ";")
		b.WriteString(s)
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
	b.WriteString(") {")
	if n.Children[1].Kind == "block" {
		b.WriteByte('\n')
		writeProgram(b, n.Children[1], indent+1)
	} else {
		b.WriteByte('\n')
		writeStmt(b, n.Children[1], indent+1)
	}
	b.WriteString(ind)
	b.WriteString("}\n")
}

func writeEnhancedFor(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 4 {
		return
	}
	ind := strings.Repeat("    ", indent)
	b.WriteString(ind)
	b.WriteString("for (")
	// children: type, identifier, iterable, block
	writeType(b, n.Children[0])
	b.WriteByte(' ')
	writeExpr(b, n.Children[1], indent)
	b.WriteString(" : ")
	writeExpr(b, n.Children[2], indent)
	b.WriteString(") {\n")
	writeProgram(b, n.Children[3], indent+1)
	b.WriteString(ind)
	b.WriteString("}\n")
}

func writeTry(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) == 0 {
		return
	}
	ind := strings.Repeat("    ", indent)
	b.WriteString(ind)
	b.WriteString("try {\n")
	writeProgram(b, n.Children[0], indent+1)
	b.WriteString(ind)
	b.WriteString("}")
	if len(n.Children) > 1 {
		for _, c := range n.Children[1:] {
			if c.Kind == "catch_clause" {
				b.WriteString(" catch (")
				if len(c.Children) > 0 {
					p := c.Children[0]
					if len(p.Children) >= 2 {
						writeType(b, p.Children[0].Children[0])
						b.WriteByte(' ')
						b.WriteString(p.Children[1].Text)
					}
				}
				b.WriteString(") {\n")
				if len(c.Children) > 1 {
					writeProgram(b, c.Children[1], indent+1)
				}
				b.WriteString(ind)
				b.WriteString("}")
			}
		}
	}
	b.WriteByte('\n')
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

func writeTypeArguments(b *bytes.Buffer, n *Node) {
	b.WriteByte('<')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeType(b, c)
	}
	b.WriteByte('>')
}

func writeExpr(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "identifier", "decimal_integer_literal", "true", "false",
		"integral_type", "floating_point_type", "void_type", "type_identifier", "string_fragment", "null_literal":
		b.WriteString(n.Text)
	case "string_literal":
		if n.Text != "" {
			b.WriteString(n.Text)
		} else if len(n.Children) > 0 {
			var sb strings.Builder
			for _, c := range n.Children {
				if c.Kind == "string_fragment" {
					sb.WriteString(c.Text)
				}
			}
			b.WriteByte('"')
			b.WriteString(sb.String())
			b.WriteByte('"')
		}
	case "method_invocation":
		if len(n.Children) == 1 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString("()")
		} else if len(n.Children) >= 2 {
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
	case "scoped_type_identifier":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('.')
			}
			writeExpr(b, c, indent)
		}
	case "generic_type":
		for _, c := range n.Children {
			if c.Kind == "type_arguments" {
				writeTypeArguments(b, c)
			} else {
				writeExpr(b, c, indent)
			}
		}
	case "type_arguments":
		writeTypeArguments(b, n)
	case "array_creation_expression":
		if len(n.Children) > 0 {
			b.WriteString("new ")
			writeType(b, n.Children[0])
			b.WriteString("[]{")
			if len(n.Children) > 1 {
				writeExpr(b, n.Children[1], indent)
			}
			b.WriteString("}")
		}
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
	case "object_creation_expression":
		if len(n.Children) >= 1 {
			b.WriteString("new ")
			writeType(b, n.Children[0])
			idx := 1
			if idx < len(n.Children) && n.Children[idx].Kind == "argument_list" {
				writeArgumentList(b, n.Children[idx], indent)
				idx++
			} else {
				b.WriteString("()")
			}
			if idx < len(n.Children) && n.Children[idx].Kind == "class_body" {
				b.WriteString(" {")
				if len(n.Children[idx].Children) > 0 {
					b.WriteByte('{')
					b.WriteByte('\n')
					writeProgram(b, n.Children[idx].Children[0], indent+1)
					b.WriteString(strings.Repeat("    ", indent))
					b.WriteString("}")
				}
				b.WriteString("}")
			}
		}
	case "assignment_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" = ")
			writeExpr(b, n.Children[1], indent)
		}
	case "cast_expression":
		if len(n.Children) == 2 {
			b.WriteByte('(')
			writeType(b, n.Children[0])
			b.WriteByte(')')
			writeExpr(b, n.Children[1], indent)
		}
	case "instanceof_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" instanceof ")
			writeType(b, n.Children[1])
		}
	case "binary_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			op := "+"
			if n.Text != "" {
				op = n.Text
			}
			b.WriteByte(' ')
			b.WriteString(op)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
		}
	case "ternary_expression":
		if len(n.Children) == 3 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" ? ")
			writeExpr(b, n.Children[1], indent)
			b.WriteString(" : ")
			writeExpr(b, n.Children[2], indent)
		}
	case "lambda_expression":
		if len(n.Children) == 2 {
			writeParameters(b, n.Children[0])
			b.WriteString(" -> ")
			writeExpr(b, n.Children[1], indent)
		}
	case "update_expression":
		if len(n.Children) == 1 {
			writeExpr(b, n.Children[0], indent)
			op := n.Text
			if op == "" {
				op = "++"
			}
			b.WriteString(op)
		}
	case "unary_expression":
		op := n.Text
		if op == "" {
			op = "-"
		}
		b.WriteString(op)
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
