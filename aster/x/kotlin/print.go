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
	case "while_statement":
		writeWhile(b, n, indent)
	case "assignment":
		b.WriteString(ind)
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" = ")
			writeExpr(b, n.Children[1], indent)
		}
		b.WriteByte('\n')
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
	case "class_declaration":
		writeClass(b, n, indent)
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
	if i < len(n.Children) && (n.Children[i].Kind == "user_type" || n.Children[i].Kind == "function_type") {
		b.WriteString(": ")
		if n.Children[i].Kind == "function_type" {
			writeFunctionType(b, n.Children[i])
		} else {
			writeType(b, n.Children[i])
		}
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
		if vd.Children[1].Kind == "function_type" {
			writeFunctionType(b, vd.Children[1])
		} else {
			writeType(b, vd.Children[1])
		}
	}
	if len(n.Children) > 1 {
		b.WriteString(" = ")
		writeExpr(b, n.Children[1], indent)
	}
	b.WriteByte('\n')
}

func writeClass(b *bytes.Buffer, n *Node, indent int) {
	ind := strings.Repeat("    ", indent)
	b.WriteString(ind)
	b.WriteString("data class ")
	if len(n.Children) == 0 {
		b.WriteByte('\n')
		return
	}
	i := 0
	if n.Children[0].Kind == "modifiers" {
		// modifiers already include 'data'
		i++
	}
	if i < len(n.Children) && n.Children[i].Kind == "identifier" {
		b.WriteString(n.Children[i].Text)
		i++
	}
	if i < len(n.Children) && n.Children[i].Kind == "primary_constructor" {
		b.WriteByte('(')
		if len(n.Children[i].Children) > 0 {
			params := n.Children[i].Children[0]
			for j := range params.Children {
				if j > 0 {
					b.WriteString(", ")
				}
				p := params.Children[j]
				if len(p.Children) >= 1 {
					b.WriteString("val ")
					writeExpr(b, p.Children[0], indent)
					if len(p.Children) > 1 {
						b.WriteString(": ")
						writeType(b, p.Children[1])
					}
				}
			}
		}
		b.WriteByte(')')
		i++
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
		if n.Children[0].Kind == "multi_variable_declaration" {
			for i, c := range n.Children[0].Children {
				if i > 0 {
					b.WriteString(", ")
				}
				if len(c.Children) > 0 {
					writeExpr(b, c.Children[0], indent)
				}
			}
		} else {
			writeExpr(b, n.Children[0].Children[0], indent)
		}
	}
	b.WriteString(" in ")
	writeExpr(b, n.Children[1], indent)
	b.WriteString(") ")
	writeBlock(b, n.Children[2], indent)
}

func writeWhile(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 2 {
		return
	}
	ind := strings.Repeat("    ", indent)
	b.WriteString(ind)
	b.WriteString("while (")
	writeExpr(b, n.Children[0], indent)
	b.WriteString(") ")
	if n.Children[1].Kind == "block" {
		writeBlock(b, n.Children[1], indent)
	} else {
		b.WriteString("{\n")
		writeStmt(b, n.Children[1], indent+1)
		b.WriteString(ind)
		b.WriteString("}\n")
	}
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
	if len(n.Children) >= 3 {
		b.WriteString(ind)
		b.WriteString("else ")
		if n.Children[2].Kind == "if_expression" {
			writeIf(b, n.Children[2], indent)
		} else {
			writeBlock(b, n.Children[2], indent)
		}
	}
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

func writeFunctionType(b *bytes.Buffer, n *Node) {
	if len(n.Children) == 0 {
		return
	}
	b.WriteByte('(')
	var idx int
	if n.Children[0].Kind == "function_type_parameters" {
		for i := range n.Children[0].Children {
			if i > 0 {
				b.WriteString(", ")
			}
			if len(n.Children[0].Children[i].Children) > 0 {
				writeType(b, n.Children[0].Children[i].Children[0])
			}
		}
		idx = 1
	}
	b.WriteString(") -> ")
	if idx < len(n.Children) {
		writeType(b, n.Children[idx])
	}
}

func writeLambdaParams(b *bytes.Buffer, n *Node) {
	for i := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		if len(n.Children[i].Children) > 0 {
			writeExpr(b, n.Children[i].Children[0], 0)
			if len(n.Children[i].Children) > 1 {
				b.WriteString(": ")
				writeType(b, n.Children[i].Children[1])
			}
		}
	}
}

func writeLambdaLiteral(b *bytes.Buffer, n *Node, indent int) {
	i := 0
	singleLine := true
	if len(n.Children) > 0 && n.Children[0].Kind == "lambda_parameters" {
		i = 1
	}
	if len(n.Children)-i > 1 {
		singleLine = false
	} else if len(n.Children) > i {
		switch n.Children[i].Kind {
		case "property_declaration", "for_statement", "assignment", "block":
			singleLine = false
		}
	}
	if singleLine {
		b.WriteByte('{')
		if i == 1 {
			writeLambdaParams(b, n.Children[0])
			b.WriteString(" -> ")
		}
		for ; i < len(n.Children); i++ {
			if i > 1 {
				b.WriteByte(' ')
			}
			writeExpr(b, n.Children[i], indent)
		}
		b.WriteByte('}')
		return
	}
	b.WriteString("{\n")
	if i == 1 {
		writeLambdaParams(b, n.Children[0])
		b.WriteString(" ->\n")
	}
	for ; i < len(n.Children); i++ {
		writeStmt(b, n.Children[i], indent+1)
	}
	b.WriteString(strings.Repeat("    ", indent))
	b.WriteByte('}')
}

func writeAnnotatedLambda(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) > 0 {
		writeLambdaLiteral(b, n.Children[0], indent)
	}
}

func writeCall(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) == 0 {
		return
	}
	writeExpr(b, n.Children[0], indent)
	if len(n.Children) > 1 {
		switch n.Children[1].Kind {
		case "value_arguments":
			writeValueArgs(b, n.Children[1], indent)
		case "annotated_lambda":
			b.WriteByte(' ')
			writeAnnotatedLambda(b, n.Children[1], indent)
		default:
			writeExpr(b, n.Children[1], indent)
		}
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
		if len(n.Children[i].Children) == 1 {
			writeExpr(b, n.Children[i].Children[0], indent)
		} else if len(n.Children[i].Children) == 2 {
			writeExpr(b, n.Children[i].Children[0], indent)
			b.WriteString(" = ")
			writeExpr(b, n.Children[i].Children[1], indent)
		}
	}
	b.WriteByte(')')
}

func writeWhenExpr(b *bytes.Buffer, n *Node, indent int) {
	ind := strings.Repeat("    ", indent)
	b.WriteString("when (")
	idx := 0
	if len(n.Children) > 0 && n.Children[0].Kind == "when_subject" {
		if len(n.Children[0].Children) > 0 {
			writeExpr(b, n.Children[0].Children[0], indent)
		}
		idx = 1
	}
	b.WriteString(") {\n")
	for ; idx < len(n.Children); idx++ {
		c := n.Children[idx]
		b.WriteString(ind)
		b.WriteString("    ")
		if len(c.Children) == 2 {
			writeExpr(b, c.Children[0], indent)
			b.WriteString(" -> ")
			writeExpr(b, c.Children[1], indent)
		} else if len(c.Children) == 1 {
			b.WriteString("else -> ")
			writeExpr(b, c.Children[0], indent)
		}
		b.WriteByte('\n')
	}
	b.WriteString(ind)
	b.WriteByte('}')
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
	case "when_expression":
		writeWhenExpr(b, n, indent)
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
	case "lambda_literal":
		writeLambdaLiteral(b, n, indent)
	case "function_type":
		writeFunctionType(b, n)
	case "in_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" in ")
			writeExpr(b, n.Children[1], indent)
		}
	case "if_expression":
		writeIfExpr(b, n, indent)
	case "unary_expression":
		if len(n.Children) == 1 {
			if n.Text != "" {
				b.WriteString(n.Text)
			} else {
				b.WriteByte('!')
			}
			writeExpr(b, n.Children[0], indent)
		}
	case "binary_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			if n.Text != "" {
				b.WriteString(" " + n.Text + " ")
			} else {
				b.WriteByte(' ')
			}
			writeExpr(b, n.Children[1], indent)
		}
	case "as_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			op := n.Text
			if op == "" {
				op = "as"
			}
			b.WriteString(" ")
			b.WriteString(op)
			b.WriteString(" ")
			writeExpr(b, n.Children[1], indent)
		}
	default:
		if n.Text != "" && len(n.Children) == 0 {
			b.WriteString(n.Text)
		} else if len(n.Children) > 0 {
			for i, c := range n.Children {
				if i > 0 {
					b.WriteByte(' ')
				}
				writeExpr(b, c, indent)
			}
		} else {
			b.WriteString(n.Kind)
		}
	}
}

func writeIfExpr(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 2 {
		return
	}
	b.WriteString("if (")
	writeExpr(b, n.Children[0], indent)
	b.WriteString(") ")
	writeExpr(b, n.Children[1], indent)
	if len(n.Children) >= 3 {
		b.WriteString(" else ")
		writeExpr(b, n.Children[2], indent)
	}
}
