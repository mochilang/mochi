package rs

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns Rust source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil || p.Root == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	for _, c := range p.Root.Children {
		writeStmt(&b, c, 0)
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
	case "line_comment":
		b.WriteString(ind)
		b.WriteString(n.Text)
		b.WriteByte('\n')
	case "use_declaration":
		b.WriteString(ind)
		b.WriteString("use ")
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString("::")
			}
			writeExpr(b, c, indent)
		}
		b.WriteString(";\n")
	case "attribute_item":
		b.WriteString(ind)
		b.WriteString("#[")
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteString("]\n")
	case "struct_item":
		b.WriteString(ind)
		b.WriteString("struct ")
		idx := 0
		if len(n.Children) > idx {
			writeExpr(b, n.Children[idx], indent)
			idx++
		}
		if idx < len(n.Children) && n.Children[idx].Kind == "type_parameters" {
			writeExpr(b, n.Children[idx], indent)
			idx++
		}
		b.WriteString(" {\n")
		if idx < len(n.Children) {
			for _, f := range n.Children[idx].Children {
				b.WriteString(strings.Repeat("    ", indent+1))
				writeFieldDecl(b, f)
				b.WriteByte('\n')
			}
		}
		b.WriteString(ind)
		b.WriteString("}\n")
	case "impl_item":
		b.WriteString(ind)
		b.WriteString("impl ")
		if len(n.Children) >= 1 {
			writeExpr(b, n.Children[0], indent)
		}
		if len(n.Children) >= 2 {
			b.WriteString(" for ")
			writeExpr(b, n.Children[1], indent)
		}
		b.WriteString(" {\n")
		if len(n.Children) > 2 {
			for _, d := range n.Children[2].Children {
				writeStmt(b, d, indent+1)
			}
		}
		b.WriteString(ind)
		b.WriteString("}\n")
	case "function_item":
		b.WriteString(ind)
		b.WriteString("fn ")
		if len(n.Children) >= 2 {
			writeExpr(b, n.Children[0], 0)
			writeParameters(b, n.Children[1])
			idx := 2
			if len(n.Children) > idx && n.Children[idx].Kind != "block" {
				b.WriteString(" -> ")
				writeExpr(b, n.Children[idx], 0)
				idx++
			}
			if len(n.Children) > idx {
				b.WriteByte(' ')
				writeBlock(b, n.Children[idx], indent)
			}
		}
		b.WriteByte('\n')
	case "let_declaration":
		b.WriteString(ind)
		b.WriteString("let ")
		idx := 0
		if idx < len(n.Children) && n.Children[idx].Kind == "mutable_specifier" {
			writeExpr(b, n.Children[idx], indent)
			b.WriteByte(' ')
			idx++
		}
		if idx < len(n.Children) {
			writeExpr(b, n.Children[idx], indent)
			idx++
		}
		if len(n.Children)-idx == 1 {
			b.WriteString(" = ")
			writeExpr(b, n.Children[idx], indent)
		} else if len(n.Children)-idx >= 2 {
			b.WriteString(": ")
			writeExpr(b, n.Children[idx], indent)
			idx++
			b.WriteString(" = ")
			writeExpr(b, n.Children[idx], indent)
		}
		b.WriteString(";\n")
	case "expression_statement":
		if len(n.Children) == 0 {
			return
		}
		b.WriteString(ind)
		expr := n.Children[0]
		writeExpr(b, expr, indent)
		if expr.Kind != "for_expression" && expr.Kind != "if_expression" && expr.Kind != "block" {
			b.WriteString(";")
		}
		b.WriteByte('\n')
	case "return_expression":
		b.WriteString(ind)
		b.WriteString("return ")
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteByte('\n')
	case "static_item":
		b.WriteString(ind)
		b.WriteString("static ")
		if len(n.Children) >= 1 {
			writeExpr(b, n.Children[0], indent)
		}
		if len(n.Children) >= 2 {
			b.WriteString(": ")
			writeExpr(b, n.Children[1], indent)
		}
		if len(n.Children) >= 3 {
			b.WriteString(" = ")
			writeExpr(b, n.Children[2], indent)
		}
		b.WriteString(";\n")
	default:
		b.WriteString(ind)
		writeExpr(b, n, indent)
		b.WriteByte('\n')
	}
}

func writeBlock(b *bytes.Buffer, n *Node, indent int) {
	b.WriteString("{\n")
	for _, c := range n.Children {
		writeStmt(b, c, indent+1)
	}
	b.WriteString(strings.Repeat("    ", indent))
	b.WriteString("}")
}

func writeFieldDecl(b *bytes.Buffer, n *Node) {
	if len(n.Children) == 2 {
		writeExpr(b, n.Children[0], 0)
		b.WriteString(": ")
		writeExpr(b, n.Children[1], 0)
		b.WriteByte(',')
	}
}

func writeFieldInit(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) == 2 {
		writeExpr(b, n.Children[0], indent)
		b.WriteString(": ")
		writeExpr(b, n.Children[1], indent)
	}
}

func writeParameters(b *bytes.Buffer, n *Node) {
	b.WriteByte('(')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeParameter(b, c)
	}
	if len(n.Children) == 0 && n.Text == "()" {
		// keep empty
	}
	b.WriteByte(')')
}

func writeParameter(b *bytes.Buffer, n *Node) {
	switch n.Kind {
	case "self_parameter":
		writeExpr(b, n, 0)
	default:
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], 0)
			b.WriteString(": ")
			writeExpr(b, n.Children[1], 0)
		} else {
			writeExpr(b, n, 0)
		}
	}
}

func writeExpr(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "identifier", "field_identifier", "type_identifier", "primitive_type", "integer_literal", "string_content", "mutable_specifier", "escape_sequence":
		b.WriteString(n.Text)
	case "wildcard_pattern":
		b.WriteByte('_')
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
				writeArguments(b, n.Children[1], indent)
			} else {
				b.WriteString("()")
			}
		}
	case "field_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('.')
			writeExpr(b, n.Children[1], indent)
		}
	case "arguments":
		writeArguments(b, n, indent)
	case "array_expression":
		b.WriteByte('[')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(']')
	case "tuple_expression":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(')')
	case "tuple_type":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(')')
	case "tuple_pattern":
		b.WriteByte('(')
		if len(n.Children) == 1 {
			b.WriteString("_, ")
			writeExpr(b, n.Children[0], indent)
		} else {
			for i, c := range n.Children {
				if i > 0 {
					b.WriteString(", ")
				}
				writeExpr(b, c, indent)
			}
		}
		b.WriteByte(')')
	case "abstract_type":
		if len(n.Children) == 1 {
			b.WriteString("impl ")
			writeExpr(b, n.Children[0], indent)
		}
	case "function_type":
		if len(n.Children) >= 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('(')
			for i, c := range n.Children[1].Children {
				if i > 0 {
					b.WriteString(", ")
				}
				writeExpr(b, c, indent)
			}
			b.WriteByte(')')
			if len(n.Children) > 2 {
				b.WriteString(" -> ")
				writeExpr(b, n.Children[2], indent)
			}
		}
	case "closure_expression":
		idx := 0
		if len(n.Children) > 0 && n.Children[0].Kind == "closure_parameters" {
			writeExpr(b, n.Children[0], indent)
			idx = 1
		} else {
			b.WriteString("||")
		}
		if len(n.Children)-idx >= 2 {
			b.WriteString(" -> ")
			writeExpr(b, n.Children[idx], indent)
			idx++
		}
		if idx < len(n.Children) {
			b.WriteByte(' ')
			if n.Children[idx].Kind == "block" {
				writeBlock(b, n.Children[idx], indent)
			} else {
				writeExpr(b, n.Children[idx], indent)
			}
		}
	case "closure_parameters":
		b.WriteByte('|')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeParameter(b, c)
		}
		b.WriteByte('|')
	case "generic_type":
		if len(n.Children) >= 1 {
			writeExpr(b, n.Children[0], indent)
			if len(n.Children) > 1 {
				writeTypeArguments(b, n.Children[1])
			}
		}
	case "generic_function":
		if len(n.Children) >= 1 {
			writeExpr(b, n.Children[0], indent)
			if len(n.Children) > 1 {
				b.WriteString("::")
				writeTypeArguments(b, n.Children[1])
			}
		}
	case "generic_type_with_turbofish":
		if len(n.Children) >= 1 {
			writeExpr(b, n.Children[0], indent)
			if len(n.Children) > 1 {
				b.WriteString("::<")
				for i, c := range n.Children[1].Children {
					if i > 0 {
						b.WriteString(", ")
					}
					writeExpr(b, c, indent)
				}
				b.WriteByte('>')
			}
		}
	case "type_arguments":
		writeTypeArguments(b, n)
	case "type_parameters":
		writeTypeParameters(b, n)
	case "type_parameter":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
	case "range_expression":
		switch len(n.Children) {
		case 1:
			c := n.Children[0]
			if c.StartCol == n.StartCol {
				writeExpr(b, c, indent)
				b.WriteString("..")
			} else {
				b.WriteString("..")
				writeExpr(b, c, indent)
			}
		case 2:
			writeExpr(b, n.Children[0], indent)
			b.WriteString("..")
			writeExpr(b, n.Children[1], indent)
		}
	case "assignment_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" = ")
			writeExpr(b, n.Children[1], indent)
		}
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
	case "parenthesized_expression":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(',')
				b.WriteByte(' ')
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(')')
	case "type_cast_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" as ")
			writeExpr(b, n.Children[1], indent)
		}
	case "index_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('[')
			writeExpr(b, n.Children[1], indent)
			b.WriteByte(']')
		}
	case "unary_expression":
		if len(n.Children) == 1 {
			op := n.Text
			if op == "" {
				op = "!"
			}
			b.WriteString(op)
			writeExpr(b, n.Children[0], indent)
		}
	case "reference_expression":
		if len(n.Children) == 1 {
			b.WriteByte('&')
			writeExpr(b, n.Children[0], indent)
		}
	case "reference_type":
		b.WriteByte('&')
		if len(n.Children) > 0 && n.Children[0].Kind == "mutable_specifier" {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(' ')
			if len(n.Children) > 1 {
				writeExpr(b, n.Children[1], indent)
			}
		} else if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
	case "self_parameter":
		b.WriteString("&self")
	case "lifetime":
		b.WriteByte('\'')
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		} else {
			b.WriteString(n.Text)
		}
	case "scoped_identifier":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString("::")
			}
			writeExpr(b, c, indent)
		}
	case "scoped_type_identifier":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString("::")
			}
			writeExpr(b, c, indent)
		}
	case "scoped_use_list":
		if len(n.Children) >= 1 {
			writeExpr(b, n.Children[0], indent)
			if len(n.Children) > 1 {
				b.WriteString("::{")
				for i, c := range n.Children[1].Children {
					if i > 0 {
						b.WriteString(", ")
					}
					writeExpr(b, c, indent)
				}
				b.WriteByte('}')
			}
		}
	case "use_list":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
	case "struct_expression":
		if len(n.Children) >= 1 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteString(" {")
		if len(n.Children) > 1 {
			for i, f := range n.Children[1].Children {
				if i > 0 {
					b.WriteString(", ")
				}
				writeFieldInit(b, f, indent)
			}
		}
		b.WriteString("}")
	case "tuple_struct_pattern":
		if len(n.Children) >= 1 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteByte('(')
		for i, c := range n.Children[1:] {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(')')
	case "field_initializer_list":
		b.WriteString("{")
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeFieldInit(b, c, indent)
		}
		b.WriteString("}")
	case "field_initializer":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(": ")
			writeExpr(b, n.Children[1], indent)
		}
	case "try_expression":
		if len(n.Children) == 1 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('?')
		}
	case "for_expression":
		if len(n.Children) == 3 {
			b.WriteString("for ")
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" in ")
			writeExpr(b, n.Children[1], indent)
			b.WriteByte(' ')
			writeBlock(b, n.Children[2], indent)
		}
	case "break_expression":
		b.WriteString("break")
	case "continue_expression":
		b.WriteString("continue")
	case "if_expression":
		if len(n.Children) >= 2 {
			b.WriteString("if ")
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(' ')
			writeBlock(b, n.Children[1], indent)
			if len(n.Children) > 2 {
				writeExpr(b, n.Children[2], indent)
			}
		}
	case "let_condition":
		if len(n.Children) == 2 {
			b.WriteString("let ")
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" = ")
			writeExpr(b, n.Children[1], indent)
		}
	case "block":
		writeBlock(b, n, indent)
	case "macro_invocation":
		if len(n.Children) == 0 {
			return
		}
		writeExpr(b, n.Children[0], indent)
		b.WriteByte('!')
		if len(n.Children) > 1 {
			writeExpr(b, n.Children[1], indent)
		}
	case "else_clause":
		b.WriteString(" else ")
		if len(n.Children) == 1 {
			if n.Children[0].Kind == "block" {
				writeBlock(b, n.Children[0], indent)
			} else {
				writeExpr(b, n.Children[0], indent)
			}
		}
	case "token_tree":
		b.WriteString(n.Text)
	case "attribute":
		for i, c := range n.Children {
			if i > 0 {
				// no separator
			}
			writeExpr(b, c, indent)
		}
	default:
		if n.Text != "" {
			b.WriteString(n.Text)
		} else {
			b.WriteString(n.Kind)
		}
	}
}

func writeArguments(b *bytes.Buffer, n *Node, indent int) {
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
		writeExpr(b, c, 0)
	}
	b.WriteByte('>')
}

func writeTypeParameters(b *bytes.Buffer, n *Node) {
	b.WriteByte('<')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeExpr(b, c, 0)
	}
	b.WriteByte('>')
}
