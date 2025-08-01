//go:build slow

package c

import (
	"bytes"
	"fmt"
	"os/exec"
	"strings"
)

// Print returns C source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil || p.Root == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	writeTranslationUnit(&b, p.Root, 0)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeTranslationUnit(b *bytes.Buffer, n *TranslationUnit, indent int) {
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
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteByte('\n')
	case "type_definition":
		if len(n.Children) >= 2 {
			b.WriteString(ind)
			b.WriteString("typedef ")
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
			b.WriteString(";\n")
		}
	case "struct_specifier":
		if len(n.Children) >= 2 {
			b.WriteString(ind)
			b.WriteString("struct ")
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" {\n")
			writeFieldDeclList(b, n.Children[1], indent+1)
			b.WriteString(ind)
			b.WriteString("};\n")
		}
	case "function_definition":
		if len(n.Children) >= 2 {
			body := n.Children[len(n.Children)-1]
			decl := n.Children[len(n.Children)-2]
			specs := n.Children[:len(n.Children)-2]
			b.WriteString(ind)
			for i, s := range specs {
				if i > 0 {
					b.WriteByte(' ')
				}
				writeExpr(b, s, indent)
			}
			if len(specs) > 0 {
				b.WriteByte(' ')
			}
			writeFunctionDeclarator(b, decl)
			b.WriteString(" {\n")
			writeBlock(b, body, indent+1)
			b.WriteString(ind)
			b.WriteString("}\n")
		}
	case "declaration":
		b.WriteString(ind)
		writeDeclaration(b, n)
		b.WriteString(";\n")
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
	case "break_statement":
		b.WriteString(ind)
		b.WriteString("break;\n")
	case "continue_statement":
		b.WriteString(ind)
		b.WriteString("continue;\n")
	case "for_statement":
		if len(n.Children) >= 4 {
			b.WriteString(ind)
			b.WriteString("for (")
			switch n.Children[0].Kind {
			case "declaration":
				writeDeclaration(b, n.Children[0])
			default:
				writeExpr(b, n.Children[0], 0)
			}
			b.WriteString("; ")
			writeExpr(b, n.Children[1], 0)
			b.WriteString("; ")
			writeExpr(b, n.Children[2], 0)
			b.WriteString(") {\n")
			writeBlock(b, n.Children[3], indent+1)
			b.WriteString(ind)
			b.WriteString("}\n")
		}
	case "while_statement":
		if len(n.Children) >= 2 {
			b.WriteString(ind)
			b.WriteString("while ")
			writeExpr(b, n.Children[0], 0)
			b.WriteString(" {\n")
			writeBlock(b, n.Children[1], indent+1)
			b.WriteString(ind)
			b.WriteString("}\n")
		}
	case "if_statement":
		if len(n.Children) >= 2 {
			b.WriteString(ind)
			b.WriteString("if (")
			writeExpr(b, n.Children[0], 0)
			b.WriteString(") ")
			writeBlockOrStmt(b, n.Children[1], indent)
			if len(n.Children) >= 3 {
				b.WriteString(" else ")
				writeBlockOrStmt(b, n.Children[2], indent)
			}
			b.WriteByte('\n')
		}
	case "compound_statement":
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
		writeStmt(b, c, indent)
	}
}

func writeBlockOrStmt(b *bytes.Buffer, n *Node, indent int) {
	if n == nil {
		b.WriteString("{}")
		return
	}
	if n.Kind == "else_clause" && len(n.Children) > 0 {
		writeBlockOrStmt(b, n.Children[0], indent)
		return
	}
	if n.Kind == "compound_statement" {
		b.WriteString("{\n")
		writeBlock(b, n, indent+1)
		b.WriteString(strings.Repeat("    ", indent))
		b.WriteString("}")
		return
	}
	b.WriteString("{\n")
	writeStmt(b, n, indent+1)
	b.WriteString(strings.Repeat("    ", indent))
	b.WriteString("}")
}

func writeFieldDeclList(b *bytes.Buffer, n *Node, indent int) {
	for _, c := range n.Children {
		writeFieldDecl(b, c, indent)
	}
}

func writeFieldDecl(b *bytes.Buffer, n *Node, indent int) {
	ind := strings.Repeat("    ", indent)
	if len(n.Children) == 0 {
		return
	}
	b.WriteString(ind)
	if len(n.Children) == 1 {
		writeExpr(b, n.Children[0], indent)
		b.WriteString(";\n")
		return
	}
	decl := n.Children[len(n.Children)-1]
	specs := n.Children[:len(n.Children)-1]
	for i, s := range specs {
		if i > 0 {
			b.WriteByte(' ')
		}
		writeExpr(b, s, indent)
	}
	b.WriteByte(' ')
	writeDeclarator(b, decl)
	b.WriteString(";\n")
}

func writeFunctionDeclarator(b *bytes.Buffer, n *Node) {
	switch len(n.Children) {
	case 1:
		writeExpr(b, n.Children[0], 0)
		b.WriteString("()")
	case 2:
		writeExpr(b, n.Children[0], 0)
		writeParameterList(b, n.Children[1])
	}
}

func writeParameterList(b *bytes.Buffer, n *Node) {
	b.WriteByte('(')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeParameter(b, c)
	}
	b.WriteByte(')')
}

func writeParameter(b *bytes.Buffer, n *Node) {
	if len(n.Children) == 0 {
		return
	}
	if len(n.Children) == 1 {
		writeExpr(b, n.Children[0], 0)
		return
	}
	decl := n.Children[len(n.Children)-1]
	specs := n.Children[:len(n.Children)-1]
	for i, s := range specs {
		if i > 0 {
			b.WriteByte(' ')
		}
		writeExpr(b, s, 0)
	}
	b.WriteByte(' ')
	writeExpr(b, decl, 0)
}

func writeDeclaration(b *bytes.Buffer, n *Node) {
	if len(n.Children) == 0 {
		b.WriteString(n.Kind)
		return
	}

	decl := n.Children[len(n.Children)-1]
	specs := n.Children[:len(n.Children)-1]

	for i, s := range specs {
		if i > 0 {
			b.WriteByte(' ')
		}
		writeExpr(b, s, 0)
	}
	if len(specs) > 0 {
		b.WriteByte(' ')
	}
	if decl.Kind == "init_declarator" {
		writeInitDeclarator(b, decl)
	} else {
		writeDeclarator(b, decl)
	}
}

func writeInitDeclarator(b *bytes.Buffer, n *Node) {
	if len(n.Children) == 0 {
		return
	}
	writeDeclarator(b, n.Children[0])
	if len(n.Children) > 1 {
		b.WriteString(" = ")
		writeExpr(b, n.Children[1], 0)
	}
}

func writeDeclarator(b *bytes.Buffer, n *Node) {
	switch n.Kind {
	case "identifier":
		b.WriteString(n.Text)
	case "array_declarator":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], 0)
		}
		b.WriteByte('[')
		if len(n.Children) > 1 {
			writeExpr(b, n.Children[1], 0)
		}
		b.WriteByte(']')
	case "pointer_declarator":
		b.WriteByte('*')
		if len(n.Children) > 0 {
			writeDeclarator(b, n.Children[0])
		}
	default:
		writeExpr(b, n, 0)
	}
}

func writeExpr(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "identifier", "field_identifier", "type_identifier", "number_literal", "char_literal", "primitive_type", "system_lib_string", "string_content", "escape_sequence", "storage_class_specifier", "type_qualifier", "sized_type_specifier":
		b.WriteString(n.Text)
	case "string_literal":
		b.WriteByte('"')
		for _, c := range n.Children {
			writeExpr(b, c, indent)
		}
		b.WriteByte('"')
	case "call_expression":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
			if len(n.Children) > 1 {
				writeArgumentList(b, n.Children[1])
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
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('[')
			writeExpr(b, n.Children[1], indent)
			b.WriteByte(']')
		}
	case "field_expression":
		if len(n.Children) >= 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('.')
			writeExpr(b, n.Children[1], indent)
		}
	case "parenthesized_expression":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				writeExpr(b, c, indent)
			} else {
				writeExpr(b, c, indent)
			}
		}
		b.WriteByte(')')
	case "binary_expression":
		writeBinaryExpr(b, n, indent)
	case "assignment_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			op := n.Text
			if op == "" {
				op = "="
			}
			b.WriteByte(' ')
			b.WriteString(op)
			b.WriteByte(' ')
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
	case "initializer_list":
		b.WriteByte('{')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(",")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte('}')
	case "initializer_pair":
		if len(n.Children) == 2 {
			if n.Children[0].Kind == "field_designator" && len(n.Children[0].Children) > 0 {
				b.WriteByte('.')
				writeExpr(b, n.Children[0].Children[0], indent)
			} else {
				writeExpr(b, n.Children[0], indent)
			}
			b.WriteString(" = ")
			writeExpr(b, n.Children[1], indent)
		}
	case "compound_literal_expression":
		if len(n.Children) >= 1 {
			b.WriteByte('(')
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(')')
		}
		if len(n.Children) > 1 {
			writeExpr(b, n.Children[1], indent)
		}
	case "sizeof_expression":
		b.WriteString("sizeof(")
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteByte(')')
	case "type_descriptor":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, c, indent)
		}
	case "struct_specifier":
		if len(n.Children) >= 1 {
			b.WriteString("struct ")
			writeExpr(b, n.Children[0], indent)
		}
	case "pointer_declarator":
		b.WriteByte('*')
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
	case "pointer_expression":
		if len(n.Children) > 0 {
			op := n.Text
			if op == "" {
				op = "*"
			}
			b.WriteString(op)
			writeExpr(b, n.Children[0], indent)
		}
	case "array_declarator":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteByte('[')
		if len(n.Children) > 1 {
			writeExpr(b, n.Children[1], indent)
		}
		b.WriteByte(']')
	case "unary_expression":
		if len(n.Children) == 1 {
			op := n.Text
			if op == "" {
				op = "-"
			}
			b.WriteString(op)
			writeExpr(b, n.Children[0], indent)
		}
	case "conditional_expression":
		if len(n.Children) == 3 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" ? ")
			writeExpr(b, n.Children[1], indent)
			b.WriteString(" : ")
			writeExpr(b, n.Children[2], indent)
		}
	case "cast_expression":
		if len(n.Children) == 2 {
			b.WriteByte('(')
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(')')
			writeExpr(b, n.Children[1], indent)
		}
	default:
		b.WriteString(n.Kind)
	}
}

func writeArgumentList(b *bytes.Buffer, n *Node) {
	b.WriteByte('(')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeExpr(b, c, 0)
	}
	b.WriteByte(')')
}

func writeBinaryExpr(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) == 1 {
		writeExpr(b, n.Children[0], indent)
		op := n.Text
		if op == "" {
			op = "+"
		}
		b.WriteByte(' ')
		b.WriteString(op)
		b.WriteByte(' ')
		b.WriteString("0")
		return
	}
	if len(n.Children) != 2 {
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, c, indent)
		}
		return
	}
	left, right := n.Children[0], n.Children[1]
	op := n.Text
	if op == "" {
		op = "+"
	}
	writeExpr(b, left, indent)
	b.WriteByte(' ')
	b.WriteString(op)
	b.WriteByte(' ')
	writeExpr(b, right, indent)
}

// ensure we're not accidentally importing exec without using it
var _ = exec.Cmd{}
