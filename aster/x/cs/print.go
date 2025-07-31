package cs

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns C# source code reconstructed from the Program AST.
func Print(p *Program) (string, error) {
	if p == nil || p.File == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	writeCompilationUnit(&b, p.File, 0)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func indent(n int) string { return strings.Repeat("    ", n) }

func writeCompilationUnit(b *bytes.Buffer, n *CompilationUnit, indentLevel int) {
	for _, c := range n.Children {
		switch c.Kind {
		case "comment":
			b.WriteString(strings.Repeat("", indentLevel))
			b.WriteString(c.Text)
			b.WriteByte('\n')
		case "using_directive":
			b.WriteString(indent(indentLevel))
			b.WriteString("using ")
			if len(c.Children) > 0 {
				writeExpr(b, c.Children[0], indentLevel)
			}
			b.WriteString(";\n")
		case "class_declaration":
			writeClassDecl(b, c, indentLevel)
		case "struct_declaration":
			writeStructDecl(b, c, indentLevel)
		default:
			writeStmt(b, c, indentLevel)
		}
	}
}

func writeClassDecl(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) == 0 {
		return
	}
	b.WriteString(indent(indentLevel))
	b.WriteString("class ")
	if len(n.Children) > 0 && n.Children[0].Kind == "identifier" {
		b.WriteString(n.Children[0].Text)
	}
	b.WriteString(" {\n")
	if len(n.Children) > 1 {
		if n.Children[1].Kind == "declaration_list" {
			for _, m := range n.Children[1].Children {
				writeMemberDecl(b, m, indentLevel+1)
			}
		}
	}
	b.WriteString(indent(indentLevel))
	b.WriteString("}\n")
}

func writeStructDecl(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) == 0 {
		return
	}
	b.WriteString(indent(indentLevel))
	b.WriteString("struct ")
	if len(n.Children) > 0 && n.Children[0].Kind == "identifier" {
		b.WriteString(n.Children[0].Text)
	}
	b.WriteString(" {\n")
	if len(n.Children) > 1 {
		if n.Children[1].Kind == "declaration_list" {
			for _, m := range n.Children[1].Children {
				writeMemberDecl(b, m, indentLevel+1)
			}
		}
	}
	b.WriteString(indent(indentLevel))
	b.WriteString("}\n")
}

func writeMemberDecl(b *bytes.Buffer, n *Node, indentLevel int) {
	switch n.Kind {
	case "field_declaration":
		writeFieldDecl(b, n, indentLevel)
	case "method_declaration":
		writeMethodDecl(b, n, indentLevel)
	default:
		writeStmt(b, n, indentLevel)
	}
}

func writeFieldDecl(b *bytes.Buffer, n *Node, indentLevel int) {
	b.WriteString(indent(indentLevel))
	idx := 0
	for idx < len(n.Children) && n.Children[idx].Kind == "modifier" {
		b.WriteString(n.Children[idx].Text)
		b.WriteByte(' ')
		idx++
	}
	if idx < len(n.Children) && n.Children[idx].Kind == "variable_declaration" {
		writeVarDecl(b, n.Children[idx], indentLevel)
	}
	b.WriteString(";\n")
}

func writeVarDecl(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) == 0 {
		return
	}
	writeExpr(b, n.Children[0], indentLevel)
	if len(n.Children) > 1 {
		b.WriteByte(' ')
		for i, d := range n.Children[1:] {
			if i > 0 {
				b.WriteString(", ")
			}
			writeVarDeclarator(b, d, indentLevel)
		}
	}
}

func writeVarDeclarator(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) == 0 {
		return
	}
	if n.Children[0].Kind == "identifier" {
		b.WriteString(n.Children[0].Text)
	}
	if len(n.Children) > 1 {
		b.WriteString(" = ")
		writeExpr(b, n.Children[1], indentLevel)
	}
}

func writeMethodDecl(b *bytes.Buffer, n *Node, indentLevel int) {
	b.WriteString(indent(indentLevel))
	idx := 0
	for idx < len(n.Children) && n.Children[idx].Kind == "modifier" {
		b.WriteString(n.Children[idx].Text)
		b.WriteByte(' ')
		idx++
	}
	if idx < len(n.Children) {
		writeExpr(b, n.Children[idx], indentLevel)
		b.WriteByte(' ')
		idx++
	}
	if idx < len(n.Children) && n.Children[idx].Kind == "identifier" {
		b.WriteString(n.Children[idx].Text)
		idx++
	}
	if idx < len(n.Children) && n.Children[idx].Kind == "parameter_list" {
		writeParameterList(b, n.Children[idx])
		idx++
	} else {
		b.WriteString("()")
	}
	if idx < len(n.Children) && n.Children[idx].Kind == "arrow_expression_clause" {
		writeArrowClause(b, n.Children[idx], indentLevel)
		b.WriteString(";\n")
		return
	}
	b.WriteString(" {\n")
	if idx < len(n.Children) {
		if n.Children[idx].Kind == "block" {
			writeBlock(b, n.Children[idx], indentLevel+1)
		}
	}
	b.WriteString(indent(indentLevel))
	b.WriteString("}\n")
}

func writeArrowClause(b *bytes.Buffer, n *Node, indentLevel int) {
	b.WriteString(" => ")
	for _, c := range n.Children {
		writeExpr(b, c, indentLevel)
	}
}

func writeParameterList(b *bytes.Buffer, n *Node) {
	b.WriteByte('(')
	for i, p := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeParameter(b, p)
	}
	b.WriteByte(')')
}

func writeParameter(b *bytes.Buffer, n *Node) {
	if len(n.Children) >= 2 {
		writeExpr(b, n.Children[0], 0)
		b.WriteByte(' ')
		writeExpr(b, n.Children[1], 0)
	}
}

func writeBlock(b *bytes.Buffer, n *Node, indentLevel int) {
	for _, c := range n.Children {
		writeStmt(b, c, indentLevel)
	}
}

func writeStmt(b *bytes.Buffer, n *Node, indentLevel int) {
	switch n.Kind {
	case "comment":
		b.WriteString(indent(indentLevel))
		b.WriteString(n.Text)
		b.WriteByte('\n')
	case "local_declaration_statement":
		b.WriteString(indent(indentLevel))
		if len(n.Children) > 0 {
			writeVarDecl(b, n.Children[0], indentLevel)
		}
		b.WriteString(";\n")
	case "expression_statement":
		b.WriteString(indent(indentLevel))
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indentLevel)
		}
		b.WriteString(";\n")
	case "return_statement":
		b.WriteString(indent(indentLevel))
		b.WriteString("return")
		if len(n.Children) > 0 {
			b.WriteByte(' ')
			writeExpr(b, n.Children[0], indentLevel)
		}
		b.WriteString(";\n")
	case "for_statement":
		writeForStatement(b, n, indentLevel)
	case "foreach_statement":
		writeForeachStatement(b, n, indentLevel)
	case "break_statement":
		b.WriteString(indent(indentLevel))
		b.WriteString("break;\n")
	case "continue_statement":
		b.WriteString(indent(indentLevel))
		b.WriteString("continue;\n")
	case "if_statement":
		writeIfStatement(b, n, indentLevel)
	case "block":
		writeBlock(b, n, indentLevel)
	default:
		b.WriteString(indent(indentLevel))
		writeExpr(b, n, indentLevel)
		b.WriteByte('\n')
	}
}

func writeForStatement(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) < 4 {
		return
	}
	b.WriteString(indent(indentLevel))
	b.WriteString("for (")
	writeVarDecl(b, n.Children[0], indentLevel)
	b.WriteString("; ")
	writeExpr(b, n.Children[1], indentLevel)
	b.WriteString("; ")
	writeExpr(b, n.Children[2], indentLevel)
	b.WriteString(") {\n")
	writeBlock(b, n.Children[3], indentLevel+1)
	b.WriteString(indent(indentLevel))
	b.WriteString("}\n")
}

func writeForeachStatement(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) < 4 {
		return
	}
	b.WriteString(indent(indentLevel))
	b.WriteString("foreach (")
	writeExpr(b, n.Children[0], indentLevel)
	b.WriteByte(' ')
	writeExpr(b, n.Children[1], indentLevel)
	b.WriteString(" in ")
	writeExpr(b, n.Children[2], indentLevel)
	b.WriteString(") {\n")
	writeBlock(b, n.Children[3], indentLevel+1)
	b.WriteString(indent(indentLevel))
	b.WriteString("}\n")
}

func writeIfStatement(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) < 2 {
		return
	}
	b.WriteString(indent(indentLevel))
	b.WriteString("if (")
	writeExpr(b, n.Children[0], indentLevel)
	b.WriteString(") {\n")
	writeBlock(b, n.Children[1], indentLevel+1)
	b.WriteString(indent(indentLevel))
	b.WriteString("}\n")
}

func writeExpr(b *bytes.Buffer, n *Node, indentLevel int) {
	switch n.Kind {
	case "identifier", "integer_literal", "real_literal", "boolean_literal", "implicit_type", "predefined_type", "modifier":
		b.WriteString(n.Text)
	case "string_literal":
		b.WriteByte('"')
		for _, c := range n.Children {
			b.WriteString(c.Text)
		}
		if n.Text != "" && len(n.Children) == 0 {
			b.WriteString(n.Text)
		}
		b.WriteByte('"')
	case "member_access_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indentLevel)
			b.WriteByte('.')
			writeExpr(b, n.Children[1], indentLevel)
		}
	case "qualified_name":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('.')
			}
			writeExpr(b, c, indentLevel)
		}
	case "invocation_expression":
		if len(n.Children) >= 1 {
			writeExpr(b, n.Children[0], indentLevel)
			if len(n.Children) > 1 {
				writeExpr(b, n.Children[1], indentLevel)
			} else {
				b.WriteString("()")
			}
		}
	case "argument_list":
		b.WriteByte('(')
		for i, arg := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			if len(arg.Children) > 0 {
				writeExpr(b, arg.Children[0], indentLevel)
			}
		}
		b.WriteByte(')')
	case "array_creation_expression":
		b.WriteString("new ")
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indentLevel)
		}
		if len(n.Children) > 1 {
			writeExpr(b, n.Children[1], indentLevel)
		}
	case "array_type":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indentLevel)
		}
		b.WriteString("[]")
	case "initializer_expression":
		b.WriteByte('{')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indentLevel)
		}
		b.WriteByte('}')
	case "variable_declarator":
		writeVarDeclarator(b, n, indentLevel)
	case "parenthesized_expression":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(" ")
			}
			writeExpr(b, c, indentLevel)
		}
		b.WriteByte(')')
	case "binary_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indentLevel)
			op := n.Text
			if op == "" {
				op = "+"
			}
			b.WriteByte(' ')
			b.WriteString(op)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indentLevel)
		}
	case "assignment_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indentLevel)
			op := n.Text
			if op == "" {
				op = "="
			}
			b.WriteByte(' ')
			b.WriteString(op)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indentLevel)
		}
	case "prefix_unary_expression":
		op := n.Text
		if op == "" {
			op = "-"
		}
		b.WriteString(op)
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indentLevel)
		}
	case "postfix_unary_expression":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indentLevel)
		}
		op := n.Text
		if op == "" {
			op = "++"
		}
		b.WriteString(op)
	case "element_access_expression":
		if len(n.Children) >= 2 {
			writeExpr(b, n.Children[0], indentLevel)
			writeExpr(b, n.Children[1], indentLevel)
		}
	case "generic_name":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indentLevel)
		}
		if len(n.Children) > 1 {
			writeExpr(b, n.Children[1], indentLevel)
		}
	case "type_argument_list":
		b.WriteByte('<')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indentLevel)
		}
		b.WriteByte('>')
	case "cast_expression":
		if len(n.Children) == 2 {
			b.WriteByte('(')
			writeExpr(b, n.Children[0], indentLevel)
			b.WriteByte(')')
			writeExpr(b, n.Children[1], indentLevel)
		}
	case "bracketed_argument_list":
		b.WriteByte('[')
		for i, arg := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			if len(arg.Children) > 0 {
				writeExpr(b, arg.Children[0], indentLevel)
			}
		}
		b.WriteByte(']')
	case "object_creation_expression":
		b.WriteString("new ")
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indentLevel)
		}
		for i, c := range n.Children[1:] {
			if i == 0 && c.Kind != "initializer_expression" {
				writeExpr(b, c, indentLevel)
				continue
			}
			writeExpr(b, c, indentLevel)
		}
	case "lambda_expression":
		if len(n.Children) == 2 {
			writeParameterList(b, n.Children[0])
			b.WriteString(" => ")
			writeExpr(b, n.Children[1], indentLevel)
		}
	case "conditional_expression":
		if len(n.Children) == 3 {
			writeExpr(b, n.Children[0], indentLevel)
			b.WriteString(" ? ")
			writeExpr(b, n.Children[1], indentLevel)
			b.WriteString(" : ")
			writeExpr(b, n.Children[2], indentLevel)
		}
	case "interpolated_string_expression":
		b.WriteString("$\"")
		for _, c := range n.Children {
			switch c.Kind {
			case "string_content", "string_literal_content":
				b.WriteString(c.Text)
			case "interpolation":
				b.WriteByte('{')
				for _, e := range c.Children {
					writeExpr(b, e, indentLevel)
				}
				b.WriteByte('}')
			}
		}
		b.WriteByte('"')
	case "query_expression":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, c, indentLevel)
		}
	case "from_clause":
		if len(n.Children) == 2 {
			b.WriteString("from ")
			writeExpr(b, n.Children[0], indentLevel)
			b.WriteString(" in ")
			writeExpr(b, n.Children[1], indentLevel)
		}
	case "select_clause":
		if len(n.Children) == 1 {
			b.WriteString("select ")
			writeExpr(b, n.Children[0], indentLevel)
		}
	default:
		for _, c := range n.Children {
			writeExpr(b, c, indentLevel)
		}
	}
}
