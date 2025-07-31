package ts

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns TypeScript source code constructed from the Program AST.
func Print(p *Program) (string, error) {
	if p == nil || p.Root == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	writeProgram(&b, (*Node)(p.Root), 0)
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
	case "comment":
		b.WriteString(ind)
		b.WriteString(n.Text)
		b.WriteByte('\n')
	case "lexical_declaration":
		kw := n.Text
		if kw == "" {
			kw = "const"
		}
		b.WriteString(ind)
		b.WriteString(kw)
		b.WriteByte(' ')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeVarDeclarator(b, c, indent)
		}
		b.WriteString(";\n")
	case "expression_statement":
		if len(n.Children) > 0 {
			b.WriteString(ind)
			writeExpr(b, n.Children[0], indent)
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
	case "statement_block":
		b.WriteString(ind)
		writeBlock(b, n, indent)
		b.WriteByte('\n')
	default:
		b.WriteString(ind)
		writeExpr(b, n, indent)
		b.WriteByte('\n')
	}
}

func writeBlock(b *bytes.Buffer, n *Node, indent int) {
	b.WriteByte('{')
	if len(n.Children) > 0 {
		b.WriteByte('\n')
		for _, c := range n.Children {
			writeStmt(b, c, indent+1)
		}
		b.WriteString(strings.Repeat("    ", indent))
	}
	b.WriteByte('}')
}

func writeVarDeclarator(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) == 0 {
		return
	}
	writeExpr(b, n.Children[0], indent)
	i := 1
	if len(n.Children) > 1 && n.Children[1].Kind == "type_annotation" {
		writeTypeAnnotation(b, n.Children[1], indent)
		i = 2
	}
	if len(n.Children) > i {
		b.WriteString(" = ")
		writeExpr(b, n.Children[i], indent)
	}
}

func writeTypeAnnotation(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) == 0 {
		return
	}
	b.WriteString(": ")
	writeExpr(b, n.Children[0], indent)
}

func writeExpr(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "identifier", "property_identifier", "type_identifier", "predefined_type", "number":
		b.WriteString(n.Text)
	case "string":
		b.WriteByte('"')
		if len(n.Children) > 0 {
			for _, c := range n.Children {
				b.WriteString(c.Text)
			}
		} else {
			b.WriteString(n.Text)
		}
		b.WriteByte('"')
	case "call_expression":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
			if len(n.Children) > 1 {
				writeExpr(b, n.Children[1], indent)
			} else {
				b.WriteString("()")
			}
		}
	case "member_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('.')
			writeExpr(b, n.Children[1], indent)
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
	case "binary_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			op := n.Text
			if op == "" {
				op = "+"
			}
			b.WriteByte(' ')
			b.WriteString(op)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
		}
	case "unary_expression":
		if len(n.Children) == 1 {
			op := n.Text
			if op == "" {
				op = "+"
			}
			b.WriteString(op)
			writeExpr(b, n.Children[0], indent)
		}
	case "parenthesized_expression":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(" ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(')')
	case "array":
		b.WriteByte('[')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(']')
	case "spread_element":
		b.WriteString("...")
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
	case "array_type":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteString("[]")
	case "subscript_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('[')
			writeExpr(b, n.Children[1], indent)
			b.WriteByte(']')
		}
	case "arrow_function":
		var params *Node
		var body *Node
		if len(n.Children) == 1 {
			body = n.Children[0]
		} else if len(n.Children) >= 2 {
			if n.Children[0].Kind == "formal_parameters" {
				params = n.Children[0]
				body = n.Children[1]
			} else {
				params = nil
				body = n.Children[len(n.Children)-1]
			}
		}
		if params != nil {
			writeParameters(b, params, indent)
		} else {
			b.WriteString("()")
		}
		b.WriteString(" => ")
		if body != nil {
			if body.Kind == "statement_block" {
				writeBlock(b, body, indent)
			} else {
				writeExpr(b, body, indent)
			}
		}
	case "formal_parameters":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(')')
	case "required_parameter":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
	default:
		// Fallback: print children in order
		for _, c := range n.Children {
			writeExpr(b, c, indent)
		}
	}
}

func writeParameters(b *bytes.Buffer, n *Node, indent int) {
	writeExpr(b, n, indent)
}
