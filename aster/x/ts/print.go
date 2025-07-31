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
	case "variable_declaration":
		kw := n.Text
		if kw == "" {
			kw = "var"
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
	case "for_statement":
		if len(n.Children) >= 4 {
			b.WriteString(ind)
			b.WriteString("for (")
			writeExpr(b, n.Children[0], indent)
			b.WriteString("; ")
			writeExpr(b, n.Children[1], indent)
			b.WriteString("; ")
			writeExpr(b, n.Children[2], indent)
			b.WriteString(")")
			body := n.Children[3]
			if body.Kind == "statement_block" {
				b.WriteByte(' ')
				writeBlock(b, body, indent)
				b.WriteByte('\n')
			} else {
				b.WriteByte('\n')
				writeStmt(b, body, indent+1)
			}
		}
	case "for_in_statement":
		if len(n.Children) >= 3 {
			b.WriteString(ind)
			b.WriteString("for (const ")
			writeExpr(b, n.Children[0], indent)
			kw := n.Text
			if kw == "" {
				kw = "of"
			}
			b.WriteByte(' ')
			b.WriteString(kw)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
			b.WriteString(")")
			body := n.Children[2]
			if body.Kind == "statement_block" {
				b.WriteByte(' ')
				writeBlock(b, body, indent)
				b.WriteByte('\n')
			} else {
				b.WriteByte('\n')
				writeStmt(b, body, indent+1)
			}
		}
        case "if_statement":
                if len(n.Children) >= 2 {
                        b.WriteString(ind)
                        b.WriteString("if (")
                        writeExpr(b, n.Children[0], indent)
                        b.WriteString(")")
                        body := n.Children[1]
                        if body.Kind == "statement_block" {
                                b.WriteByte(' ')
                                writeBlock(b, body, indent)
                                b.WriteByte('\n')
                        } else {
                                b.WriteByte('\n')
                                writeStmt(b, body, indent+1)
                        }
                        for _, c := range n.Children[2:] {
                                if c.Kind != "else_clause" || len(c.Children) == 0 {
                                        continue
                                }
                                b.WriteString(ind)
                                b.WriteString("else ")
                                child := c.Children[0]
                                if child.Kind == "statement_block" {
                                        writeBlock(b, child, indent)
                                        b.WriteByte('\n')
                                } else {
                                        b.WriteByte('\n')
                                        writeStmt(b, child, indent+1)
                                }
                        }
                }
	case "break_statement":
		b.WriteString(ind)
		b.WriteString("break\n")
	case "continue_statement":
		b.WriteString(ind)
		b.WriteString("continue\n")
	case "export_statement":
		if len(n.Children) > 0 {
			b.WriteString(ind)
			b.WriteString("export ")
			child := n.Children[0]
			switch child.Kind {
			case "interface_declaration":
				writeInterfaceDecl(b, child, indent)
				b.WriteByte('\n')
			default:
				writeStmt(b, child, indent)
			}
		}
	case "interface_declaration":
		b.WriteString(ind)
		writeInterfaceDecl(b, n, indent)
		b.WriteByte('\n')
	case "function_declaration":
		if len(n.Children) >= 2 {
			b.WriteString(ind)
			b.WriteString("function ")
			writeExpr(b, n.Children[0], indent)
			idx := 1
			if idx < len(n.Children) && n.Children[idx].Kind == "formal_parameters" {
				writeParameters(b, n.Children[idx], indent)
				idx++
			} else {
				b.WriteString("()")
			}
			if idx < len(n.Children) && n.Children[idx].Kind == "type_annotation" {
				writeTypeAnnotation(b, n.Children[idx], indent)
				idx++
			}
			b.WriteByte(' ')
			if idx < len(n.Children) {
				if n.Children[idx].Kind == "statement_block" {
					writeBlock(b, n.Children[idx], indent)
				} else {
					writeExpr(b, n.Children[idx], indent)
				}
			}
			b.WriteByte('\n')
		}
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
	case "identifier", "property_identifier", "type_identifier", "predefined_type", "number", "true", "false", "null", "undefined":
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
				if isJSONStringify(n.Children[0]) && len(n.Children[1].Children) == 2 && n.Children[1].Children[1].Kind == "string" {
					b.WriteByte('(')
					writeExpr(b, n.Children[1].Children[0], indent)
					b.WriteString(", null, ")
					writeExpr(b, n.Children[1].Children[1], indent)
					b.WriteByte(')')
				} else {
					writeExpr(b, n.Children[1], indent)
				}
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
			if len(op) > 0 && ((op[0] >= 'a' && op[0] <= 'z') || (op[0] >= 'A' && op[0] <= 'Z')) {
				b.WriteByte(' ')
			}
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
	case "shorthand_property_identifier":
		b.WriteString(n.Text)
	case "array_pattern":
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
	case "object":
		b.WriteByte('{')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte('}')
	case "generic_type":
		for _, c := range n.Children {
			writeExpr(b, c, indent)
		}
	case "type_arguments":
		b.WriteByte('<')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte('>')
	case "new_expression":
		if len(n.Children) > 0 {
			b.WriteString("new ")
			writeExpr(b, n.Children[0], indent)
			for _, c := range n.Children[1:] {
				writeExpr(b, c, indent)
			}
		}
	case "object_type":
		writeInterfaceBody(b, n, indent)
	case "pair":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(": ")
			writeExpr(b, n.Children[1], indent)
		}
	case "property_signature":
		if len(n.Children) >= 2 {
			writeExpr(b, n.Children[0], indent)
			if n.Children[1].Kind == "type_annotation" {
				writeTypeAnnotation(b, n.Children[1], indent)
				if len(n.Children) > 2 {
					b.WriteString(" = ")
					writeExpr(b, n.Children[2], indent)
				}
			} else {
				b.WriteString(": ")
				writeExpr(b, n.Children[1], indent)
			}
		} else if len(n.Children) == 1 {
			writeExpr(b, n.Children[0], indent)
		}
	case "lexical_declaration":
		kw := n.Text
		if kw == "" {
			kw = "const"
		}
		b.WriteString(kw)
		b.WriteByte(' ')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeVarDeclarator(b, c, indent)
		}
	case "variable_declaration":
		kw := n.Text
		if kw == "" {
			kw = "var"
		}
		b.WriteString(kw)
		b.WriteByte(' ')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeVarDeclarator(b, c, indent)
		}
	case "subscript_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('[')
			writeExpr(b, n.Children[1], indent)
			b.WriteByte(']')
		}
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
			op := n.Text
			if op == "" {
				op = "++"
			}
			b.WriteString(op)
			writeExpr(b, n.Children[0], indent)
		}
	case "ternary_expression":
		if len(n.Children) == 3 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" ? ")
			writeExpr(b, n.Children[1], indent)
			b.WriteString(" : ")
			writeExpr(b, n.Children[2], indent)
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
				params = n.Children[0]
				body = n.Children[1]
			}
		}
		if params != nil {
			if params.Kind == "formal_parameters" {
				writeParameters(b, params, indent)
			} else {
				writeExpr(b, params, indent)
			}
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
	case "optional_parameter":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteByte('?')
		if len(n.Children) > 1 && n.Children[1].Kind == "type_annotation" {
			writeTypeAnnotation(b, n.Children[1], indent)
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

func writeInterfaceDecl(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 2 {
		return
	}
	b.WriteString("interface ")
	writeExpr(b, n.Children[0], indent)
	b.WriteByte(' ')
	writeInterfaceBody(b, n.Children[1], indent)
}

func writeInterfaceBody(b *bytes.Buffer, n *Node, indent int) {
	b.WriteByte('{')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString("; ")
		} else if len(n.Children) > 0 {
			b.WriteByte(' ')
		}
		writeExpr(b, c, indent)
	}
	if len(n.Children) > 0 {
		b.WriteByte(' ')
	}
	b.WriteByte('}')
}

func isJSONStringify(n *Node) bool {
	if n == nil || n.Kind != "member_expression" || len(n.Children) != 2 {
		return false
	}
	return n.Children[0].Kind == "identifier" && n.Children[0].Text == "JSON" &&
		n.Children[1].Kind == "property_identifier" && n.Children[1].Text == "stringify"
}
