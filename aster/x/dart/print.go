//go:build slow

package dart

import (
	"bytes"
	"fmt"
)

// Print returns Dart source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil || p.Root == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	writeProgram(&b, p.Root, 0)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func indent(b *bytes.Buffer, n int) {
	for i := 0; i < n; i++ {
		b.WriteString("  ")
	}
}

func writeProgram(b *bytes.Buffer, n *ProgramNode, indentLevel int) {
	for i := 0; i < len(n.Children); i++ {
		c := n.Children[i]
		if c.Kind == "function_signature" && i+1 < len(n.Children) && n.Children[i+1].Kind == "function_body" {
			writeFunction(b, c, n.Children[i+1], indentLevel)
			i++
			continue
		}
		writeStmt(b, c, indentLevel)
	}
}

func writeFunction(b *bytes.Buffer, sig *Node, body *Node, indentLevel int) {
	indent(b, indentLevel)
	writeFuncSig(b, sig)
	b.WriteString(" {")
	b.WriteByte('\n')
	if len(body.Children) > 0 {
		writeBlock(b, body.Children[0], indentLevel+1)
	}
	indent(b, indentLevel)
	b.WriteString("}")
	b.WriteByte('\n')
}

func writeFuncSig(b *bytes.Buffer, n *Node) {
	idx := 0
	if idx < len(n.Children) && (n.Children[idx].Kind == "type_identifier" || n.Children[idx].Kind == "void_type") {
		writeExpr(b, n.Children[idx])
		idx++
		if idx < len(n.Children) && n.Children[idx].Kind == "type_arguments" {
			writeExpr(b, n.Children[idx])
			idx++
		}
		b.WriteByte(' ')
	}
	if idx < len(n.Children) && n.Children[idx].Kind == "identifier" {
		writeExpr(b, n.Children[idx])
		idx++
	}
	if idx < len(n.Children) && n.Children[idx].Kind == "formal_parameter_list" {
		writeParameters(b, n.Children[idx])
	} else {
		b.WriteString("()")
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
		indent(b, indentLevel)
		b.WriteString(n.Text)
		b.WriteByte('\n')
	case "expression_statement":
		if len(n.Children) > 0 {
			indent(b, indentLevel)
			writeExpr(b, n.Children[0])
			for i := 1; i < len(n.Children); i++ {
				writeExpr(b, n.Children[i])
			}
			b.WriteString(";")
			b.WriteByte('\n')
		}
	case "return_statement":
		indent(b, indentLevel)
		b.WriteString("return")
		if len(n.Children) > 0 {
			b.WriteByte(' ')
			writeExpr(b, n.Children[0])
		}
		b.WriteString(";")
		b.WriteByte('\n')
	case "local_variable_declaration":
		indent(b, indentLevel)
		if len(n.Children) > 0 {
			writeVarDef(b, n.Children[0])
		}
		b.WriteString(";")
		b.WriteByte('\n')
	case "for_statement":
		if len(n.Children) >= 2 {
			indent(b, indentLevel)
			b.WriteString("for (")
			writeForParts(b, n.Children[0])
			b.WriteString(") {")
			b.WriteByte('\n')
			writeBlock(b, n.Children[1], indentLevel+1)
			indent(b, indentLevel)
			b.WriteString("}")
			b.WriteByte('\n')
		}
	case "if_statement":
		if len(n.Children) >= 2 {
			indent(b, indentLevel)
			b.WriteString("if (")
			writeExpr(b, n.Children[0])
			b.WriteString(") {")
			b.WriteByte('\n')
			writeBlock(b, n.Children[1], indentLevel+1)
			indent(b, indentLevel)
			b.WriteString("}")
			b.WriteByte('\n')
		}
	default:
		// fall back to expression statement
		indent(b, indentLevel)
		writeExpr(b, n)
		b.WriteString(";")
		b.WriteByte('\n')
	}
}

func writeVarDef(b *bytes.Buffer, n *Node) {
	idx := 0
	if idx < len(n.Children) && n.Children[idx].Kind == "type_identifier" {
		writeExpr(b, n.Children[idx])
		idx++
		if idx < len(n.Children) && n.Children[idx].Kind == "type_arguments" {
			writeExpr(b, n.Children[idx])
			idx++
		}
		b.WriteByte(' ')
	} else {
		b.WriteString("var ")
	}
	if idx < len(n.Children) && n.Children[idx].Kind == "identifier" {
		writeExpr(b, n.Children[idx])
		idx++
	}
	if idx < len(n.Children) {
		b.WriteString(" = ")
		writeExpr(b, n.Children[idx])
		for i := idx + 1; i < len(n.Children); i++ {
			writeExpr(b, n.Children[i])
		}
	}
}

func writeForParts(b *bytes.Buffer, n *Node) {
	// expecting init, condition, update
	if len(n.Children) >= 1 {
		switch n.Children[0].Kind {
		case "local_variable_declaration":
			writeVarDef(b, n.Children[0].Children[0])
		default:
			writeExpr(b, n.Children[0])
		}
	}
	b.WriteString("; ")
	if len(n.Children) >= 2 {
		writeExpr(b, n.Children[1])
	}
	b.WriteString("; ")
	if len(n.Children) >= 3 {
		writeExpr(b, n.Children[2])
	}
}

func writeParameters(b *bytes.Buffer, n *Node) {
	b.WriteByte('(')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		if len(c.Children) > 0 {
			writeExpr(b, c.Children[0])
		}
	}
	b.WriteByte(')')
}

func writeExpr(b *bytes.Buffer, n *Node) {
	switch n.Kind {
	case "identifier", "decimal_integer_literal", "string_literal", "comment", "type_identifier", "void_type", "true", "false", "null":
		b.WriteString(n.Text)
	case "type_arguments":
		b.WriteByte('<')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c)
		}
		b.WriteByte('>')
	case "list_literal":
		b.WriteByte('[')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c)
		}
		b.WriteByte(']')
	case "additive_expression":
		if len(n.Children) >= 2 {
			for i := 0; i < len(n.Children)-1; i++ {
				writeExpr(b, n.Children[i])
			}
			b.WriteString(" + ")
			writeExpr(b, n.Children[len(n.Children)-1])
		}
	case "unary_expression":
		if len(n.Children) == 1 {
			b.WriteByte('-')
			writeExpr(b, n.Children[0])
		}
	case "relational_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0])
			b.WriteString(" < ")
			writeExpr(b, n.Children[1])
		}
	case "equality_expression":
		if len(n.Children) >= 2 {
			writeExpr(b, n.Children[0])
			for i := 1; i < len(n.Children)-1; i++ {
				writeExpr(b, n.Children[i])
			}
			b.WriteString(" == ")
			writeExpr(b, n.Children[len(n.Children)-1])
		}
	case "postfix_expression":
		if len(n.Children) == 1 {
			writeExpr(b, n.Children[0])
			b.WriteString("++")
		}
	case "assignable_expression":
		if len(n.Children) == 1 {
			writeExpr(b, n.Children[0])
		}
	case "selector":
		if len(n.Children) == 1 {
			child := n.Children[0]
			switch child.Kind {
			case "argument_part":
				writeExpr(b, child)
			case "unconditional_assignable_selector":
				if len(child.Children) > 0 && child.Children[0].Kind == "index_selector" {
					writeExpr(b, child.Children[0])
				} else {
					b.WriteByte('.')
					writeExpr(b, child)
				}
			}
		}
	case "argument_part":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0])
		}
	case "arguments":
		b.WriteByte('(')
		for i, arg := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, arg)
		}
		b.WriteByte(')')
	case "argument":
		for _, c := range n.Children {
			writeExpr(b, c)
		}
	case "unconditional_assignable_selector":
		if len(n.Children) > 0 {
			switch n.Children[0].Kind {
			case "identifier":
				b.WriteString(n.Children[0].Text)
			case "index_selector":
				writeExpr(b, n.Children[0])
			}
		}
	case "index_selector":
		b.WriteByte('[')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c)
		}
		b.WriteByte(']')
	default:
		// fallback: print kind
		b.WriteString(n.Kind)
	}
}
