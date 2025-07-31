package lua

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns Lua source code for the given Program constructed from its AST.
func Print(p *Program) (string, error) {
	if p == nil || p.Root == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	writeChunk(&b, p.Root, 0)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeChunk(b *bytes.Buffer, n *ProgramNode, indent int) {
	for _, c := range n.Children {
		writeStmt(b, c, indent)
	}
}

func writeStmt(b *bytes.Buffer, n *Node, indent int) {
	ind := strings.Repeat("  ", indent)
	switch n.Kind {
	case "function_declaration":
		if len(n.Children) >= 1 {
			b.WriteString(ind)
			b.WriteString("function ")
			writeExpr(b, n.Children[0], indent)
			idx := 1
			if idx < len(n.Children) && n.Children[idx].Kind == "parameters" {
				writeParams(b, n.Children[idx])
				idx++
			} else {
				b.WriteString("()")
			}
			b.WriteByte('\n')
			if idx < len(n.Children) {
				writeBlock(b, n.Children[idx], indent+1)
			}
			b.WriteString(ind)
			b.WriteString("end\n")
		}
	case "assignment_statement":
		b.WriteString(ind)
		writeAssignment(b, n)
		b.WriteByte('\n')
	case "variable_declaration":
		b.WriteString(ind)
		b.WriteString("local ")
		if len(n.Children) > 0 {
			writeAssignment(b, n.Children[0])
		}
		b.WriteByte('\n')
	case "return_statement":
		b.WriteString(ind)
		b.WriteString("return")
		if len(n.Children) > 0 {
			b.WriteByte(' ')
			writeExprList(b, n.Children[0], indent)
		}
		b.WriteByte('\n')
	case "for_statement":
		if len(n.Children) >= 2 {
			b.WriteString(ind)
			b.WriteString("for ")
			writeClause(b, n.Children[0])
			b.WriteString(" do\n")
			writeBlock(b, n.Children[1], indent+1)
			b.WriteString(ind)
			b.WriteString("end\n")
		}
	case "if_statement":
		writeIfStatement(b, n, indent)
	case "do_statement":
		if len(n.Children) > 0 {
			b.WriteString(ind)
			b.WriteString("do\n")
			writeBlock(b, n.Children[0], indent+1)
			b.WriteString(ind)
			b.WriteString("end\n")
		}
	case "while_statement":
		if len(n.Children) >= 2 {
			b.WriteString(ind)
			b.WriteString("while ")
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" do\n")
			writeBlock(b, n.Children[1], indent+1)
			b.WriteString(ind)
			b.WriteString("end\n")
		}
	case "function_call":
		b.WriteString(ind)
		writeExpr(b, n, indent)
		b.WriteByte('\n')
	case "block":
		writeBlock(b, n, indent)
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

func writeIfStatement(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 2 {
		return
	}
	ind := strings.Repeat("  ", indent)
	b.WriteString(ind)
	b.WriteString("if ")
	writeExpr(b, n.Children[0], indent)
	b.WriteString(" then\n")
	writeBlock(b, n.Children[1], indent+1)
	for i := 2; i < len(n.Children); i++ {
		c := n.Children[i]
		switch c.Kind {
		case "elseif_statement":
			b.WriteString(ind)
			b.WriteString("elseif ")
			if len(c.Children) >= 2 {
				writeExpr(b, c.Children[0], indent)
				b.WriteString(" then\n")
				writeBlock(b, c.Children[1], indent+1)
			}
		case "else_statement":
			b.WriteString(ind)
			b.WriteString("else\n")
			if len(c.Children) > 0 {
				writeBlock(b, c.Children[0], indent+1)
			}
		}
	}
	b.WriteString(ind)
	b.WriteString("end\n")
}

func writeAssignment(b *bytes.Buffer, n *Node) {
	if len(n.Children) != 2 {
		return
	}
	writeVarList(b, n.Children[0])
	b.WriteString(" = ")
	writeExprList(b, n.Children[1], 0)
}

func writeClause(b *bytes.Buffer, n *Node) {
	switch n.Kind {
	case "for_generic_clause":
		if len(n.Children) >= 2 {
			writeVarList(b, n.Children[0])
			b.WriteString(" in ")
			writeExprList(b, n.Children[1], 0)
		}
	case "for_numeric_clause":
		if len(n.Children) >= 3 {
			writeExpr(b, n.Children[0], 0)
			b.WriteString(" = ")
			writeExpr(b, n.Children[1], 0)
			b.WriteString(", ")
			writeExpr(b, n.Children[2], 0)
			if len(n.Children) > 3 {
				b.WriteString(", ")
				writeExpr(b, n.Children[3], 0)
			}
		}
	}
}

func writeParams(b *bytes.Buffer, n *Node) {
	b.WriteByte('(')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeExpr(b, c, 0)
	}
	b.WriteByte(')')
}

func writeVarList(b *bytes.Buffer, n *Node) {
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeExpr(b, c, 0)
	}
}

func writeExprList(b *bytes.Buffer, n *Node, indent int) {
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeExpr(b, c, indent)
	}
}

func writeExpr(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "identifier", "number", "string":
		b.WriteString(n.Text)
	case "function_call":
		if len(n.Children) >= 1 {
			writeExpr(b, n.Children[0], indent)
			if len(n.Children) > 1 {
				writeExpr(b, n.Children[1], indent)
			} else {
				b.WriteString("()")
			}
		}
	case "arguments":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(')')
	case "dot_index_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('.')
			writeExpr(b, n.Children[1], indent)
		}
	case "bracket_index_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('[')
			writeExpr(b, n.Children[1], indent)
			b.WriteByte(']')
		}
	case "binary_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(' ')
			b.WriteString(n.Text)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
		}
	case "unary_expression":
		if len(n.Children) == 1 {
			b.WriteString(n.Text)
			writeExpr(b, n.Children[0], indent)
		}
	case "parenthesized_expression":
		if len(n.Children) > 0 {
			b.WriteByte('(')
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(')')
		}
	case "table_constructor":
		b.WriteByte('{')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte('}')
	case "field":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
	case "function_definition":
		b.WriteString("function")
		if len(n.Children) >= 2 {
			writeParams(b, n.Children[0])
			b.WriteByte('\n')
			writeBlock(b, n.Children[1], indent+1)
			b.WriteString(strings.Repeat("  ", indent))
			b.WriteString("end")
		}
	default:
		b.WriteString(n.Kind)
	}
}
