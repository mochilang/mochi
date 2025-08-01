//go:build slow

package fs

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns F# source code for the given Program. The output is
// reconstructed from the AST and only supports a limited subset of
// constructs used in the golden tests.
func Print(p *Program) (string, error) {
	if p == nil || p.Root == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	writeNode(&b, (*Node)(p.Root), 0)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeNode(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "import_decl":
		if len(n.Children) > 0 {
			b.WriteString(strings.Repeat("    ", indent))
			b.WriteString("open ")
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('\n')
		}
	case "module_defn":
		var name string
		if len(n.Children) > 0 && n.Children[0].Kind == "ERROR" && len(n.Children[0].Children) > 0 {
			name = n.Children[0].Children[0].Name
		}
		if name != "" {
			b.WriteString("module ")
			b.WriteString(name)
			b.WriteByte('\n')
		}
		for i := 1; i < len(n.Children); i++ {
			writeNode(b, n.Children[i], indent)
		}
	case "file", "ERROR":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('\n')
			}
			writeNode(b, c, indent)
		}
	case "sequential_expression":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('\n')
			}
			writeNode(b, c, indent)
		}
	case "type_definition":
		if len(n.Children) > 0 {
			writeNode(b, n.Children[0], indent)
		}
		b.WriteByte('\n')
	case "record_type_defn":
		if len(n.Children) >= 2 {
			b.WriteString("type ")
			writeNode(b, n.Children[0], indent)
			b.WriteString(" = {")
			if len(n.Children) > 1 {
				b.WriteByte('\n')
				writeNode(b, n.Children[1], indent+1)
				b.WriteByte('\n')
				b.WriteString(strings.Repeat("    ", indent))
			}
			b.WriteByte('}')
		}
		b.WriteByte('\n')
	case "record_fields":
		for i, f := range n.Children {
			if i > 0 {
				b.WriteByte('\n')
			}
			b.WriteString(strings.Repeat("    ", indent))
			writeNode(b, f, indent)
		}
	case "record_field":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(": ")
			writeExpr(b, n.Children[1], indent)
		}
	case "type_name":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
	case "field_initializers":
		for i, f := range n.Children {
			if i > 0 {
				b.WriteString("; ")
			}
			writeNode(b, f, indent)
		}
	case "field_initializer":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" = ")
			writeExpr(b, n.Children[1], indent)
		}
	case "line_comment":
		b.WriteString(n.Text)
		b.WriteByte('\n')
	case "value_declaration":
		if len(n.Children) > 0 {
			writeNode(b, n.Children[0], indent)
		}
	case "function_or_value_defn":
		b.WriteString(strings.Repeat("    ", indent))
		b.WriteString("let ")
		if len(n.Children) >= 1 {
			left := n.Children[0]
			if left.Kind == "function_declaration_left" {
				if len(left.Children) > 0 {
					writeExpr(b, left.Children[0], indent)
				}
				if len(left.Children) > 1 {
					for i, a := range left.Children[1].Children {
						b.WriteByte(' ')
						if i > 0 {
							// parameters separated by space
						}
						writeExpr(b, a, indent)
					}
				}
				if len(n.Children) > 1 {
					b.WriteString(" = ")
					writeExpr(b, n.Children[1], indent)
				}
			} else {
				writeExpr(b, left, indent)
				if len(n.Children) == 3 {
					b.WriteString(": ")
					writeExpr(b, n.Children[1], indent)
					b.WriteString(" = ")
					writeExpr(b, n.Children[2], indent)
				} else if len(n.Children) == 2 {
					b.WriteString(" = ")
					writeExpr(b, n.Children[1], indent)
				}
			}
		}
		b.WriteByte('\n')
	default:
		writeExpr(b, n, indent)
		b.WriteByte('\n')
	}
}

func writeExpr(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "identifier":
		b.WriteString(n.Name)
	case "int", "string", "float":
		b.WriteString(n.Text)
	case "long_identifier_or_op", "long_identifier":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('.')
			}
			writeExpr(b, c, indent)
		}
	case "identifier_pattern", "value_declaration_left":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, c, indent)
		}
	case "type_name", "simple_type":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
	case "const":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		} else {
			b.WriteString(n.Text)
		}
	case "range_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" .. ")
			writeExpr(b, n.Children[1], indent)
		}
	case "application_expression":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, c, indent)
		}
	case "list_expression":
		b.WriteByte('[')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString("; ")
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
	case "brace_expression":
		b.WriteByte('{')
		if len(n.Children) > 0 {
			if len(n.Children[0].Children) > 0 {
				b.WriteByte(' ')
				for i, f := range n.Children[0].Children {
					if i > 0 {
						b.WriteString("; ")
					}
					writeNode(b, f, indent)
				}
				b.WriteByte(' ')
			}
		}
		b.WriteByte('}')
	case "index_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(".[")
			writeExpr(b, n.Children[1], indent)
			b.WriteByte(']')
		}
	case "paren_expression":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(')')
	case "infix_expression":
		if len(n.Children) == 3 && n.Children[1].Kind == "infix_op" {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(' ')
			b.WriteString(strings.TrimSpace(n.Children[1].Text))
			b.WriteByte(' ')
			writeExpr(b, n.Children[2], indent)
		} else if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" + ")
			writeExpr(b, n.Children[1], indent)
		}
	case "prefixed_expression":
		if len(n.Children) == 2 && n.Children[0].Kind == "prefix_op" {
			b.WriteString(strings.TrimSpace(n.Children[0].Text))
			writeExpr(b, n.Children[1], indent)
		}
	case "function_declaration_left":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		if len(n.Children) > 1 {
			for i, p := range n.Children[1].Children {
				if i > 0 {
					b.WriteByte(' ')
				} else {
					b.WriteByte(' ')
				}
				writeExpr(b, p, indent)
			}
		}
	case "argument_patterns":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, c, indent)
		}
	case "fun_expression":
		b.WriteString("fun ")
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteString(" -> ")
		if len(n.Children) > 1 {
			writeExpr(b, n.Children[1], indent)
		}
	case "mutate_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" <- ")
			writeExpr(b, n.Children[1], indent)
		}
	case "yield_expression":
		b.WriteString("yield ")
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
	case "if_expression":
		if len(n.Children) >= 2 {
			b.WriteString("if ")
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" then ")
			writeExpr(b, n.Children[1], indent)
			if len(n.Children) > 2 {
				b.WriteString(" else ")
				writeExpr(b, n.Children[2], indent)
			}
		}
	case "for_expression":
		if len(n.Children) >= 3 {
			b.WriteString("for ")
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" in ")
			writeExpr(b, n.Children[1], indent)
			b.WriteString(" do")
			body := n.Children[2]
			if body.Kind == "yield_expression" {
				b.WriteByte(' ')
				writeExpr(b, body, indent)
			} else {
				b.WriteByte('\n')
				writeNode(b, body, indent+1)
			}
		}
	case "postfix_type":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, c, indent)
		}
	default:
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeExpr(b, c, indent)
		}
	}
}
