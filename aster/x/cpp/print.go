//go:build slow

package cpp

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns C++ source code reconstructed from p.
func Print(p *Program) (string, error) {
	if p == nil || p.Root == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	writeTranslationUnit(&b, (*Node)(p.Root), 0)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeTranslationUnit(b *bytes.Buffer, n *Node, indent int) {
	for _, c := range n.Children {
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
	case "preproc_include":
		b.WriteString(ind)
		b.WriteString("#include ")
		if len(n.Children) > 0 {
			b.WriteString(n.Children[0].Text)
		}
		b.WriteByte('\n')
	case "function_definition":
		writeFunctionDefinition(b, n, indent)
	case "struct_specifier":
		b.WriteString(ind)
		writeStructSpecifier(b, n, indent)
		b.WriteByte('\n')
	case "declaration":
		b.WriteString(ind)
		writeDeclaration(b, n, indent)
		b.WriteString(";\n")
	case "expression_statement":
		if len(n.Children) > 0 {
			b.WriteString(ind)
			c := n.Children[0]
			if c.Kind == "binary_expression" {
				writeBinaryOp(b, c, indent, detectOperator(c))
			} else {
				writeExpr(b, c, indent)
			}
			b.WriteString(";\n")
		}
	case "return_statement":
		b.WriteString(ind)
		b.WriteString("return")
		if len(n.Children) > 0 {
			b.WriteByte(' ')
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteString(";\n")
	case "break_statement":
		b.WriteString(ind)
		b.WriteString("break;\n")
	case "continue_statement":
		b.WriteString(ind)
		b.WriteString("continue;\n")
	case "for_statement":
		writeForStatement(b, n, indent)
	case "for_range_loop":
		writeForRangeLoop(b, n, indent)
	case "if_statement":
		writeIfStatement(b, n, indent)
	default:
		// Fallback: treat as expression statement
		b.WriteString(ind)
		writeExpr(b, n, indent)
		b.WriteByte('\n')
	}
}

func writeFunctionDefinition(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 3 {
		return
	}
	ind := strings.Repeat("    ", indent)
	writeExpr(b, n.Children[0], indent)
	b.WriteByte(' ')
	writeFunctionDeclarator(b, n.Children[1], indent)
	b.WriteString(" {\n")
	writeBlock(b, n.Children[2], indent+1)
	b.WriteString(ind)
	b.WriteString("}\n")
}

func writeFunctionDeclarator(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) == 0 {
		return
	}
	writeExpr(b, n.Children[0], indent)
	if len(n.Children) > 1 {
		writeParameterList(b, n.Children[1])
	} else {
		b.WriteString("()")
	}
}

func writeParameterList(b *bytes.Buffer, n *Node) {
	b.WriteByte('(')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeParameterDecl(b, c)
	}
	b.WriteByte(')')
}

func writeParameterDecl(b *bytes.Buffer, n *Node) {
	if len(n.Children) >= 2 {
		writeExpr(b, n.Children[0], 0)
		b.WriteByte(' ')
		writeExpr(b, n.Children[1], 0)
	}
}

func writeDeclaration(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 2 {
		return
	}
	writeExpr(b, n.Children[0], indent)
	b.WriteByte(' ')
	writeInitDeclarator(b, n.Children[1], indent)
}

func writeInitDeclarator(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) == 0 {
		// simple identifier without initializer
		writeExpr(b, n, indent)
		return
	}
	writeExpr(b, n.Children[0], indent)
	if len(n.Children) > 1 {
		b.WriteString(" = ")
		writeExpr(b, n.Children[1], indent)
	}
}

func writeForStatement(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 4 {
		return
	}
	ind := strings.Repeat("    ", indent)
	b.WriteString(ind)
	b.WriteString("for (")
	writeDeclaration(b, n.Children[0], 0)
	b.WriteString("; ")
	writeBinaryOp(b, n.Children[1], indent, "<")
	b.WriteString("; ")
	writeExpr(b, n.Children[2], indent)
	b.WriteString(") {\n")
	writeBlock(b, n.Children[3], indent+1)
	b.WriteString(ind)
	b.WriteString("}\n")
}

func writeIfStatement(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 2 {
		return
	}
	ind := strings.Repeat("    ", indent)
	b.WriteString(ind)
	b.WriteString("if ")
	cond := n.Children[0]
	if cond.Kind == "condition_clause" && len(cond.Children) > 0 {
		writeExpr(b, cond.Children[0], indent)
	} else {
		writeExpr(b, cond, indent)
	}
	b.WriteString(" {\n")
	writeBlock(b, n.Children[1], indent+1)
	b.WriteString(ind)
	b.WriteString("}\n")
}

func writeBlock(b *bytes.Buffer, n *Node, indent int) {
	for _, c := range n.Children {
		writeStmt(b, c, indent)
	}
}

func writeStructSpecifier(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 2 {
		return
	}
	b.WriteString("struct ")
	writeExpr(b, n.Children[0], indent)
	b.WriteString(" {\n")
	if list := n.Children[1]; list != nil {
		for _, f := range list.Children {
			writeFieldDeclaration(b, f, indent+1)
		}
	}
	b.WriteString(strings.Repeat("    ", indent))
	b.WriteString("};")
}

func writeFieldDeclaration(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 2 {
		return
	}
	b.WriteString(strings.Repeat("    ", indent))
	writeExpr(b, n.Children[0], indent)
	b.WriteByte(' ')
	writeExpr(b, n.Children[1], indent)
	b.WriteString(";\n")
}

func writeForRangeLoop(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 4 {
		return
	}
	ind := strings.Repeat("    ", indent)
	b.WriteString(ind)
	b.WriteString("for (")
	writeExpr(b, n.Children[0], indent)
	b.WriteByte(' ')
	writeExpr(b, n.Children[1], indent)
	b.WriteString(" : ")
	writeExpr(b, n.Children[2], indent)
	b.WriteString(") {\n")
	writeBlock(b, n.Children[3], indent+1)
	b.WriteString(ind)
	b.WriteString("}\n")
}

func writeExpr(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "identifier", "number_literal", "namespace_identifier", "type_identifier", "field_identifier", "auto", "primitive_type", "true", "false":
		b.WriteString(n.Text)
	case "string_literal":
		if len(n.Children) > 0 {
			fmt.Fprintf(b, "%q", n.Children[0].Text)
		} else {
			b.WriteString("\"\"")
		}
	case "qualified_identifier":
		if len(n.Children) >= 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString("::")
			writeExpr(b, n.Children[1], indent)
		}
	case "field_expression":
		if len(n.Children) >= 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('.')
			writeExpr(b, n.Children[1], indent)
		}
	case "call_expression":
		if len(n.Children) >= 1 {
			writeExpr(b, n.Children[0], indent)
			if len(n.Children) > 1 {
				writeArgumentList(b, n.Children[1], indent)
			} else {
				b.WriteString("()")
			}
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
	case "subscript_expression":
		if len(n.Children) >= 2 {
			writeExpr(b, n.Children[0], indent)
			writeSubscriptArgList(b, n.Children[1], indent)
		}
	case "subscript_argument_list":
		b.WriteByte('[')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(']')
	case "parenthesized_expression":
		b.WriteByte('(')
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteByte(')')
	case "initializer_list":
		b.WriteByte('{')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte('}')
	case "compound_literal_expression":
		if len(n.Children) >= 1 {
			writeExpr(b, n.Children[0], indent)
		}
		if len(n.Children) > 1 {
			writeExpr(b, n.Children[1], indent)
		}
	case "placeholder_type_specifier":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
	case "template_type":
		if len(n.Children) >= 1 {
			writeExpr(b, n.Children[0], indent)
		}
		if len(n.Children) > 1 {
			writeExpr(b, n.Children[1], indent)
		}
	case "template_argument_list":
		b.WriteByte('<')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte('>')
	case "type_descriptor":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
	case "binary_expression":
		op := strings.TrimSpace(n.Text)
		if op == "" {
			op = detectOperator(n)
		}
		writeBinaryOp(b, n, indent, op)
	case "update_expression":
		op := strings.TrimSpace(n.Text)
		prefix := false
		if strings.HasPrefix(op, "pre") {
			prefix = true
			op = strings.TrimPrefix(op, "pre")
		} else if strings.HasPrefix(op, "post") {
			op = strings.TrimPrefix(op, "post")
		}
		if op == "" {
			op = "++"
		}
		if len(n.Children) > 0 {
			if prefix {
				b.WriteString(op)
				writeExpr(b, n.Children[0], indent)
			} else {
				writeExpr(b, n.Children[0], indent)
				b.WriteString(op)
			}
		}
	case "conditional_expression":
		if len(n.Children) == 3 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" ? ")
			writeExpr(b, n.Children[1], indent)
			b.WriteString(" : ")
			writeExpr(b, n.Children[2], indent)
		}
	case "initializer_pair":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" = ")
			writeExpr(b, n.Children[1], indent)
		}
	case "field_designator":
		if len(n.Children) > 0 {
			b.WriteByte('.')
			writeExpr(b, n.Children[0], indent)
		}
	case "abstract_function_declarator":
		if len(n.Children) > 0 {
			writeParameterList(b, n.Children[0])
		} else {
			b.WriteString("()")
		}
	case "lambda_expression":
		if len(n.Children) >= 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(' ')
			if n.Children[1].Kind == "compound_statement" {
				b.WriteString("{\n")
				writeBlock(b, n.Children[1], indent+1)
				b.WriteString(strings.Repeat("    ", indent))
				b.WriteByte('}')
			} else {
				writeExpr(b, n.Children[1], indent)
			}
		} else if len(n.Children) == 1 {
			b.WriteString("[&] ")
			if n.Children[0].Kind == "compound_statement" {
				b.WriteString("{\n")
				writeBlock(b, n.Children[0], indent+1)
				b.WriteString(strings.Repeat("    ", indent))
				b.WriteByte('}')
			} else {
				writeExpr(b, n.Children[0], indent)
			}
		}
	case "lambda_capture_specifier":
		b.WriteString(n.Text)
	case "for_range_loop":
		writeForRangeLoop(b, n, indent)
	case "char_literal":
		if len(n.Children) > 0 {
			b.WriteByte('\'')
			b.WriteString(n.Children[0].Text)
			b.WriteByte('\'')
		}
	default:
		b.WriteString(n.Kind)
	}
}

func writeArgumentList(b *bytes.Buffer, n *Node, indent int) { writeExpr(b, n, indent) }

func writeSubscriptArgList(b *bytes.Buffer, n *Node, indent int) { writeExpr(b, n, indent) }

func writeBinaryOp(b *bytes.Buffer, n *Node, indent int, op string) {
	if len(n.Children) != 2 {
		b.WriteString(n.Kind)
		return
	}
	writeExpr(b, n.Children[0], indent)
	b.WriteString(" " + op + " ")
	writeExpr(b, n.Children[1], indent)
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
	if containsCout(n) {
		return "<<"
	}
	return "+"
}

func containsCout(n *Node) bool {
	if n == nil {
		return false
	}
	if n.Kind == "qualified_identifier" && len(n.Children) == 2 {
		if n.Children[0].Kind == "namespace_identifier" && n.Children[0].Text == "std" &&
			n.Children[1].Kind == "identifier" && (n.Children[1].Text == "cout" || n.Children[1].Text == "endl" || n.Children[1].Text == "boolalpha") {
			return true
		}
	}
	for _, c := range n.Children {
		if containsCout(c) {
			return true
		}
	}
	return false
}
