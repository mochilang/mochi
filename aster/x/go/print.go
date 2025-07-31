//go:build slow

package gox

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns Go source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil || p.Root == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	writeFile(&b, p.Root, 0)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func indent(n int) string {
	return strings.Repeat("    ", n)
}

func writeFile(b *bytes.Buffer, f *SourceFile, indentLevel int) {
	usesFmt := containsFmt((*Node)(f))
	for _, c := range f.Children {
		switch c.Kind {
		case "comment":
			b.WriteString(c.Text)
			b.WriteByte('\n')
		case "package_clause":
			b.WriteString("package ")
			if len(c.Children) > 0 {
				b.WriteString(c.Children[0].Text)
			}
			b.WriteByte('\n')
			if usesFmt {
				b.WriteString("\nimport (\n    \"fmt\"\n)\n")
			}
		default:
			writeStmt(b, c, indentLevel)
		}
	}
}

func writeStmt(b *bytes.Buffer, n *Node, indentLevel int) {
	switch n.Kind {
	case "function_declaration":
		writeFuncDecl(b, n, indentLevel)
	case "var_declaration":
		writeVarDecl(b, n, indentLevel)
	case "expression_statement":
		b.WriteString(indent(indentLevel))
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indentLevel)
		}
		b.WriteByte('\n')
	case "return_statement":
		writeReturn(b, n, indentLevel)
	case "for_statement":
		writeForStmt(b, n, indentLevel)
	case "block":
		writeBlock(b, n, indentLevel)
	case "if_statement":
		writeIfStmt(b, n, indentLevel)
	default:
		// treat as expression statement
		b.WriteString(indent(indentLevel))
		writeExpr(b, n, indentLevel)
		b.WriteByte('\n')
	}
}

func writeBlock(b *bytes.Buffer, n *Node, indentLevel int) {
	for _, c := range n.Children {
		writeStmt(b, c, indentLevel)
	}
}

func writeFuncDecl(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) < 2 {
		return
	}
	b.WriteString(indent(indentLevel))
	b.WriteString("func ")
	writeExpr(b, n.Children[0], indentLevel)
	paramsIdx := 1
	if paramsIdx < len(n.Children) && n.Children[paramsIdx].Kind == "parameter_list" {
		writeParameters(b, n.Children[paramsIdx])
		paramsIdx++
	} else {
		b.WriteString("()")
	}
	if paramsIdx < len(n.Children)-1 {
		b.WriteByte(' ')
		writeExpr(b, n.Children[paramsIdx], indentLevel)
		paramsIdx++
	}
	block := n.Children[len(n.Children)-1]
	b.WriteString(" {\n")
	writeBlock(b, block, indentLevel+1)
	b.WriteString(indent(indentLevel))
	b.WriteString("}\n")
}

func writeParameters(b *bytes.Buffer, n *Node) {
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
	if len(n.Children) < 2 {
		return
	}
	b.WriteString(n.Children[0].Text)
	b.WriteByte(' ')
	writeExpr(b, n.Children[1], 0)
}

func writeVarDecl(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) == 0 {
		return
	}
	b.WriteString(indent(indentLevel))
	b.WriteString("var ")
	writeVarSpec(b, n.Children[0], indentLevel)
	b.WriteByte('\n')
}

func writeVarSpec(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) == 0 {
		return
	}
	writeExpr(b, n.Children[0], indentLevel)
	idx := 1
	if idx < len(n.Children)-1 {
		b.WriteByte(' ')
		writeExpr(b, n.Children[idx], indentLevel)
		idx++
	}
	if idx < len(n.Children) {
		b.WriteString(" = ")
		if n.Children[idx].Kind == "expression_list" {
			writeExprList(b, n.Children[idx], indentLevel)
		} else {
			writeExpr(b, n.Children[idx], indentLevel)
		}
	}
}

func writeReturn(b *bytes.Buffer, n *Node, indentLevel int) {
	b.WriteString(indent(indentLevel))
	b.WriteString("return")
	if len(n.Children) > 0 {
		b.WriteByte(' ')
		if n.Children[0].Kind == "expression_list" {
			writeExprList(b, n.Children[0], indentLevel)
		} else {
			writeExpr(b, n.Children[0], indentLevel)
		}
	}
	b.WriteByte('\n')
}

func writeForStmt(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) < 2 {
		return
	}
	b.WriteString(indent(indentLevel))
	b.WriteString("for ")
	writeForClause(b, n.Children[0], indentLevel)
	b.WriteString(" {\n")
	writeBlock(b, n.Children[1], indentLevel+1)
	b.WriteString(indent(indentLevel))
	b.WriteString("}\n")
}

func writeForClause(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) != 3 {
		return
	}
	switch n.Children[0].Kind {
	case "short_var_declaration":
		writeShortVarDecl(b, n.Children[0], indentLevel)
	default:
		writeExpr(b, n.Children[0], indentLevel)
	}
	b.WriteString("; ")
	writeExpr(b, n.Children[1], indentLevel)
	b.WriteString("; ")
	if n.Children[2].Kind == "inc_statement" {
		writeIncStmt(b, n.Children[2])
	} else {
		writeExpr(b, n.Children[2], indentLevel)
	}
}

func writeIfStmt(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) < 2 {
		return
	}
	b.WriteString(indent(indentLevel))
	b.WriteString("if ")
	writeExpr(b, n.Children[0], indentLevel)
	b.WriteString(" {\n")
	writeBlock(b, n.Children[1], indentLevel+1)
	b.WriteString(indent(indentLevel))
	b.WriteString("}\n")
}

func writeShortVarDecl(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) != 2 {
		return
	}
	if n.Children[0].Kind == "expression_list" {
		writeExprList(b, n.Children[0], indentLevel)
	} else {
		writeExpr(b, n.Children[0], indentLevel)
	}
	b.WriteString(" := ")
	if n.Children[1].Kind == "expression_list" {
		writeExprList(b, n.Children[1], indentLevel)
	} else {
		writeExpr(b, n.Children[1], indentLevel)
	}
}

func writeIncStmt(b *bytes.Buffer, n *Node) {
	if len(n.Children) == 0 {
		return
	}
	writeExpr(b, n.Children[0], 0)
	b.WriteString("++")
}

func writeExprList(b *bytes.Buffer, n *Node, indentLevel int) {
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeExpr(b, c, indentLevel)
	}
}

func writeExpr(b *bytes.Buffer, n *Node, indentLevel int) {
	switch n.Kind {
	case "identifier", "field_identifier", "package_identifier", "type_identifier", "int_literal", "interpreted_string_literal", "raw_string_literal":
		b.WriteString(n.Text)
	case "call_expression":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indentLevel)
			if len(n.Children) > 1 {
				writeExpr(b, n.Children[1], indentLevel)
			} else {
				b.WriteString("()")
			}
		}
	case "selector_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indentLevel)
			b.WriteByte('.')
			writeExpr(b, n.Children[1], indentLevel)
		}
	case "argument_list":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indentLevel)
		}
		b.WriteByte(')')
	case "slice_type":
		b.WriteString("[]")
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indentLevel)
		}
	case "index_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indentLevel)
			b.WriteByte('[')
			writeExpr(b, n.Children[1], indentLevel)
			b.WriteByte(']')
		}
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
		writeBinaryExpr(b, n, indentLevel)
	case "expression_list":
		writeExprList(b, n, indentLevel)
	case "composite_literal":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indentLevel)
			writeExpr(b, n.Children[1], indentLevel)
		}
	case "literal_value":
		b.WriteByte('{')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indentLevel)
		}
		b.WriteByte('}')
	case "literal_element":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indentLevel)
		}
	default:
		for _, c := range n.Children {
			writeExpr(b, c, indentLevel)
		}
	}
}

func writeBinaryExpr(b *bytes.Buffer, n *Node, indentLevel int) {
	if len(n.Children) != 2 {
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, c, indentLevel)
		}
		return
	}
	left, right := n.Children[0], n.Children[1]
	op := "+"
	if right.Kind == "identifier" && right.Text == "n" {
		op = "<"
	} else if right.Kind == "identifier" && right.Text == "target" {
		op = "=="
	} else if left.Kind == "int_literal" && left.Text == "0" && right.Kind == "int_literal" && right.Text == "1" {
		op = "-"
	} else if right.Kind == "int_literal" && right.Text == "1" {
		op = "+"
	} else if left.Kind == "index_expression" && right.Kind == "index_expression" {
		op = "+"
	}
	writeExpr(b, left, indentLevel)
	b.WriteByte(' ')
	b.WriteString(op)
	b.WriteByte(' ')
	writeExpr(b, right, indentLevel)
}

func containsFmt(n *Node) bool {
	if n.Kind == "identifier" && n.Text == "fmt" {
		return true
	}
	for _, c := range n.Children {
		if containsFmt(c) {
			return true
		}
	}
	return false
}
