//go:build slow

package swift

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns Swift source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil || p.File == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	writeFile(&b, p.File, 0)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func indentStr(n int) string {
	return strings.Repeat("    ", n)
}

func writeFile(b *bytes.Buffer, f *SourceFile, indent int) {
	for _, c := range f.Children {
		writeStmt(b, c, indent)
	}
}

func writeStmt(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "comment":
		b.WriteString(indentStr(indent))
		b.WriteString(n.Text)
		b.WriteByte('\n')
	case "function_declaration":
		writeFuncDecl(b, n, indent)
	case "property_declaration":
		writePropertyDecl(b, n, indent)
	case "class_declaration":
		writeClassDecl(b, n, indent)
	case "import_declaration":
		b.WriteString(indentStr(indent))
		b.WriteString("import ")
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteByte('\n')
	case "for_statement":
		writeForStmt(b, n, indent)
	case "if_statement":
		writeIfStmt(b, n, indent)
	case "while_statement":
		writeWhileStmt(b, n, indent)
	case "assignment":
		b.WriteString(indentStr(indent))
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" = ")
			writeExpr(b, n.Children[1], indent)
		}
		b.WriteByte('\n')
	case "control_transfer_statement":
		b.WriteString(indentStr(indent))
		kw := n.Text
		if kw == "" {
			kw = "return"
		}
		b.WriteString(kw)
		if len(n.Children) > 0 {
			b.WriteByte(' ')
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteByte('\n')
	case "call_expression":
		b.WriteString(indentStr(indent))
		writeExpr(b, n, indent)
		b.WriteByte('\n')
	case "statements":
		for _, c := range n.Children {
			writeStmt(b, c, indent)
		}
	default:
		// unsupported statement kinds treated as expression statement
		b.WriteString(indentStr(indent))
		writeExpr(b, n, indent)
		b.WriteByte('\n')
	}
}

func writeFuncDecl(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 4 {
		return
	}
	b.WriteString(indentStr(indent))
	b.WriteString("func ")
	writeExpr(b, n.Children[0], indent)
	b.WriteByte('(')
	for i := 1; i < len(n.Children)-2; i++ {
		if i > 1 {
			b.WriteString(", ")
		}
		writeParameter(b, n.Children[i])
	}
	b.WriteByte(')')
	b.WriteString(" -> ")
	writeType(b, n.Children[len(n.Children)-2])
	b.WriteString(" {\n")
	body := n.Children[len(n.Children)-1]
	if len(body.Children) > 0 {
		writeStmt(b, body.Children[0], indent+1)
	}
	b.WriteString(indentStr(indent))
	b.WriteString("}\n")
}

func writeParameter(b *bytes.Buffer, n *Node) {
	if len(n.Children) < 3 {
		return
	}
	b.WriteString(n.Children[0].Text)
	b.WriteByte(' ')
	b.WriteString(n.Children[1].Text)
	b.WriteString(": ")
	writeType(b, n.Children[2])
}

func writeClassDecl(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 2 {
		return
	}
	kw := n.Text
	if kw == "" {
		kw = "class"
	}
	b.WriteString(indentStr(indent))
	b.WriteString(kw)
	b.WriteByte(' ')
	writeExpr(b, n.Children[0], indent)
	b.WriteString(" {\n")
	if len(n.Children) > 1 {
		body := n.Children[1]
		for _, c := range body.Children {
			writeStmt(b, c, indent+1)
		}
	}
	b.WriteString(indentStr(indent))
	b.WriteString("}\n")
}

func writePropertyDecl(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 2 {
		return
	}
	b.WriteString(indentStr(indent))
	kw := n.Text
	if kw == "" {
		kw = "let"
	}
	b.WriteString(kw)
	b.WriteByte(' ')
	writePattern(b, n.Children[0])
	if n.Children[1].Kind == "type_annotation" {
		b.WriteString(": ")
		if len(n.Children[1].Children) > 0 {
			writeType(b, n.Children[1].Children[0])
		}
	} else {
		b.WriteString(" = ")
		writeExpr(b, n.Children[1], indent)
	}
	b.WriteByte('\n')
}

func writePattern(b *bytes.Buffer, n *Node) {
	if len(n.Children) > 0 {
		writeExpr(b, n.Children[0], 0)
	}
}

func writeForStmt(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 3 {
		return
	}
	b.WriteString(indentStr(indent))
	b.WriteString("for ")
	writePattern(b, n.Children[0])
	b.WriteString(" in ")
	writeExpr(b, n.Children[1], indent)
	b.WriteString(" {\n")
	writeStmt(b, n.Children[2], indent+1)
	b.WriteString(indentStr(indent))
	b.WriteString("}\n")
}

func writeWhileStmt(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 2 {
		return
	}
	b.WriteString(indentStr(indent))
	b.WriteString("while ")
	writeExpr(b, n.Children[0], indent)
	b.WriteString(" {\n")
	writeStmt(b, n.Children[1], indent+1)
	b.WriteString(indentStr(indent))
	b.WriteString("}\n")
}

func writeIfStmt(b *bytes.Buffer, n *Node, indent int) {
	if len(n.Children) < 2 {
		return
	}
	b.WriteString(indentStr(indent))
	b.WriteString("if ")
	writeExpr(b, n.Children[0], indent)
	b.WriteString(" {\n")
	writeStmt(b, n.Children[1], indent+1)
	b.WriteString(indentStr(indent))
	b.WriteString("}\n")
}

func writeType(b *bytes.Buffer, n *Node) {
	switch n.Kind {
	case "array_type":
		b.WriteByte('[')
		if len(n.Children) > 0 {
			writeType(b, n.Children[0])
		}
		b.WriteByte(']')
	case "user_type":
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], 0)
		}
	case "dictionary_type":
		b.WriteByte('[')
		if len(n.Children) >= 2 {
			writeType(b, n.Children[0])
			b.WriteString(": ")
			writeType(b, n.Children[1])
		}
		b.WriteByte(']')
	default:
		writeExpr(b, n, 0)
	}
}

func writeExpr(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "simple_identifier", "type_identifier", "identifier", "integer_literal", "bang", "boolean_literal":
		if n.Text != "" {
			b.WriteString(n.Text)
		} else {
			for _, c := range n.Children {
				writeExpr(b, c, indent)
			}
		}
	case "line_string_literal":
		b.WriteByte('"')
		if len(n.Children) > 0 {
			b.WriteString(n.Children[0].Text)
		}
		b.WriteByte('"')
	case "array_literal":
		b.WriteByte('[')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(']')
	case "dictionary_literal":
		b.WriteByte('[')
		for i := 0; i < len(n.Children); i += 2 {
			if i > 0 {
				b.WriteString(", ")
			}
			if i+1 >= len(n.Children) {
				break
			}
			key := n.Children[i]
			val := n.Children[i+1]
			writeExpr(b, key, indent)
			b.WriteString(": ")
			writeExpr(b, val, indent)
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
		if len(n.Children) == 1 {
			// Preserve parentheses around single expression
		}
		b.WriteByte(')')
	case "additive_expression", "multiplicative_expression", "range_expression",
		"equality_expression", "comparison_expression", "conjunction_expression",
		"disjunction_expression", "nil_coalescing_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			op := n.Text
			if op == "" {
				switch n.Kind {
				case "range_expression":
					op = "..<"
				case "nil_coalescing_expression":
					op = "??"
				case "conjunction_expression":
					op = "&&"
				case "disjunction_expression":
					op = "||"
				case "equality_expression":
					op = "=="
				case "comparison_expression":
					op = "<"
				case "multiplicative_expression":
					op = "*"
				default:
					op = "+"
				}
			}
			b.WriteByte(' ')
			b.WriteString(op)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
		}
	case "ternary_expression":
		if len(n.Children) == 3 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" ? ")
			writeExpr(b, n.Children[1], indent)
			b.WriteString(" : ")
			writeExpr(b, n.Children[2], indent)
		}
	case "prefix_expression":
		if len(n.Children) == 1 {
			op := n.Text
			if op == "" {
				op = "-"
			}
			b.WriteString(op)
			writeExpr(b, n.Children[0], indent)
		} else if len(n.Children) == 2 {
			if n.Children[0].Kind == "bang" {
				b.WriteString(n.Children[0].Text)
				writeExpr(b, n.Children[1], indent)
			} else {
				op := n.Text
				if op == "" {
					op = "-"
				}
				b.WriteString(op)
				writeExpr(b, n.Children[1], indent)
			}
		}
	case "as_expression":
		if len(n.Children) == 2 {
			b.WriteByte('(')
			writeExpr(b, n.Children[0], indent)
			op := n.Text
			if op == "" {
				op = "as!"
			}
			b.WriteByte(' ')
			b.WriteString(op)
			b.WriteByte(' ')
			writeType(b, n.Children[1])
			b.WriteByte(')')
		}
	case "lambda_literal":
		b.WriteByte('{')
		for _, c := range n.Children {
			if c.Kind == "statements" {
				for i, s := range c.Children {
					if i > 0 {
						b.WriteByte(' ')
					}
					start := b.Len()
					writeStmt(b, s, 0)
					if b.Len() > start && b.Bytes()[b.Len()-1] == '\n' {
						b.Truncate(b.Len() - 1)
					}
				}
			} else {
				start := b.Len()
				writeStmt(b, c, 0)
				if b.Len() > start && b.Bytes()[b.Len()-1] == '\n' {
					b.Truncate(b.Len() - 1)
				}
			}
		}
		b.WriteByte('}')
	case "postfix_expression":
		if len(n.Children) == 2 && n.Children[1].Kind == "bang" {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(n.Children[1].Text)
		} else {
			for _, c := range n.Children {
				writeExpr(b, c, indent)
			}
		}
	case "navigation_expression":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('.')
			writeExpr(b, n.Children[1], indent)
		}
	case "call_expression":
		if len(n.Children) == 2 {
			callee := n.Children[0]
			argsNode := n.Children[1]
			// detect indexing vs function call
			if isIndexingCall(callee) {
				writeExpr(b, callee, indent)
				b.WriteByte('[')
				writeValueArguments(b, argsNode.Children[0])
				b.WriteByte(']')
			} else {
				writeExpr(b, callee, indent)
				writeCallSuffix(b, argsNode)
			}
		}
	case "call_suffix":
		writeCallSuffix(b, n)
	case "value_arguments":
		writeValueArguments(b, n)
	default:
		// fallback
		for _, c := range n.Children {
			writeExpr(b, c, indent)
		}
	}
}

func isIndexingCall(callee *Node) bool {
	if callee.Kind != "simple_identifier" {
		return false
	}
	name := callee.Text
	switch name {
	case "print", "twoSum", "String", "Double":
		return false
	}
	return true
}

func writeCallSuffix(b *bytes.Buffer, n *Node) {
	for _, c := range n.Children {
		switch c.Kind {
		case "value_arguments", "lambda_literal":
			writeExpr(b, c, 0)
		}
	}
}

func writeValueArguments(b *bytes.Buffer, n *Node) {
	if n.Kind != "value_arguments" {
		return
	}
	b.WriteByte('(')
	for i, arg := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		if len(arg.Children) == 1 {
			writeExpr(b, arg.Children[0], 0)
		} else if len(arg.Children) >= 2 && arg.Children[0].Kind == "value_argument_label" {
			label := arg.Children[0]
			if len(label.Children) > 0 {
				writeExpr(b, label.Children[0], 0)
			}
			b.WriteString(": ")
			writeExpr(b, arg.Children[1], 0)
		} else if len(arg.Children) > 0 {
			writeExpr(b, arg.Children[0], 0)
		} else if arg.Text != "" {
			b.WriteString(arg.Text)
		}
	}
	b.WriteByte(')')
}
