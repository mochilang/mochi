package ruby

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns Ruby source code for the given Program.
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
	ind := strings.Repeat("  ", indent)
	switch n.Kind {
	case "comment":
		b.WriteString(ind)
		b.WriteString(n.Text)
		b.WriteByte('\n')
	case "next":
		b.WriteString(ind)
		b.WriteString("next\n")
	case "break":
		b.WriteString(ind)
		b.WriteString("break\n")
	case "assignment":
		b.WriteString(ind)
		if len(n.Children) >= 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" = ")
			writeExpr(b, n.Children[1], indent)
		}
		b.WriteByte('\n')
	case "call":
		b.WriteString(ind)
		writeExpr(b, n, indent)
		b.WriteByte('\n')
	case "return":
		b.WriteString(ind)
		b.WriteString("return")
		if len(n.Children) > 0 {
			b.WriteByte(' ')
			writeReturnArgs(b, n.Children[0], indent)
		}
		b.WriteByte('\n')
	case "method":
		if len(n.Children) >= 3 {
			b.WriteString(ind)
			b.WriteString("def ")
			writeExpr(b, n.Children[0], indent)
			writeParameters(b, n.Children[1])
			b.WriteByte('\n')
			writeBody(b, n.Children[2], indent+1)
			b.WriteString(ind)
			b.WriteString("end\n")
		}
	case "if":
		if len(n.Children) >= 2 {
			b.WriteString(ind)
			b.WriteString("if ")
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('\n')
			writeBody(b, n.Children[1], indent+1)
			idx := 2
			var elseNode *Node
			for idx < len(n.Children) && n.Children[idx].Kind == "elsif" {
				e := n.Children[idx]
				if len(e.Children) > 0 {
					b.WriteString(ind)
					b.WriteString("elsif ")
					writeExpr(b, e.Children[0], indent)
					b.WriteByte('\n')
					if len(e.Children) > 1 {
						writeBody(b, e.Children[1], indent+1)
					}
					if len(e.Children) > 2 && e.Children[2].Kind == "else" {
						elseNode = e.Children[2]
					}
				}
				idx++
			}
			if idx < len(n.Children) && n.Children[idx].Kind == "else" {
				elseNode = n.Children[idx]
			}
			if elseNode != nil {
				b.WriteString(ind)
				b.WriteString("else\n")
				writeBody(b, elseNode, indent+1)
			}
			b.WriteString(ind)
			b.WriteString("end\n")
		}
	default:
		b.WriteString(ind)
		writeExpr(b, n, indent)
		b.WriteByte('\n')
	}
}

func writeBody(b *bytes.Buffer, n *Node, indent int) {
	if n.Kind == "then" || n.Kind == "else" {
		for _, c := range n.Children {
			writeStmt(b, c, indent)
		}
		return
	}
	for _, c := range n.Children {
		writeStmt(b, c, indent)
	}
}

func writeParameters(b *bytes.Buffer, n *Node) {
	b.WriteByte('(')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeExpr(b, c, 0)
	}
	b.WriteByte(')')
}

func writeBlockParameters(b *bytes.Buffer, n *Node) {
	b.WriteByte('|')
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString(", ")
		}
		writeExpr(b, c, 0)
	}
	b.WriteByte('|')
}

func writeDoBlock(b *bytes.Buffer, n *Node, indent int) {
	b.WriteString(" do")
	if len(n.Children) > 0 && n.Children[0].Kind == "block_parameters" {
		b.WriteByte(' ')
		writeBlockParameters(b, n.Children[0])
	}
	b.WriteByte('\n')
	if len(n.Children) > 1 {
		writeBody(b, n.Children[1], indent+1)
	}
	b.WriteString(strings.Repeat("  ", indent))
	b.WriteString("end")
}

func writeBegin(b *bytes.Buffer, n *Node, indent int) {
	b.WriteString("begin\n")
	writeBody(b, n, indent+1)
	b.WriteString(strings.Repeat("  ", indent))
	b.WriteString("end")
}

func writeExpr(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "identifier", "constant", "integer", "hash_key_symbol", "simple_symbol", "true", "false", "global_variable":
		b.WriteString(n.Text)
	case "string":
		b.WriteByte('"')
		if len(n.Children) > 0 {
			b.WriteString(n.Children[0].Text)
		}
		b.WriteByte('"')
	case "string_content":
		b.WriteString(n.Text)
	case "call":
		if len(n.Children) == 0 {
			return
		}
		// function call without receiver
		if len(n.Children) >= 2 && n.Children[1].Kind == "argument_list" {
			writeExpr(b, n.Children[0], indent)
			writeExpr(b, n.Children[1], indent)
			if len(n.Children) > 2 && n.Children[2].Kind == "do_block" {
				writeDoBlock(b, n.Children[2], indent)
			}
			return
		}

		// method call with receiver
		writeExpr(b, n.Children[0], indent)
		if len(n.Children) >= 2 && n.Children[1].Kind == "identifier" {
			b.WriteByte('.')
			writeExpr(b, n.Children[1], indent)
			idx := 2
			if len(n.Children) > idx && n.Children[idx].Kind == "argument_list" {
				writeExpr(b, n.Children[idx], indent)
				idx++
			}
			if len(n.Children) > idx && n.Children[idx].Kind == "do_block" {
				writeDoBlock(b, n.Children[idx], indent)
			}
		}
	case "argument_list":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
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
	case "hash":
		b.WriteByte('{')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte('}')
	case "scope_resolution":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString("::")
			writeExpr(b, n.Children[1], indent)
		}
	case "pair":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(": ")
			writeExpr(b, n.Children[1], indent)
		}
	case "binary":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			op := "+"
			if n.Text != "" {
				op = strings.TrimSpace(n.Text)
			}
			b.WriteByte(' ')
			b.WriteString(op)
			b.WriteByte(' ')
			writeExpr(b, n.Children[1], indent)
		}
	case "unary":
		if len(n.Children) > 0 {
			op := "-"
			if n.Text != "" {
				op = strings.TrimSpace(n.Text)
			}
			b.WriteString(op)
			writeExpr(b, n.Children[0], indent)
		}
	case "element_reference":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('[')
			writeExpr(b, n.Children[1], indent)
			b.WriteByte(']')
		}
	case "lambda":
		b.WriteString("->")
		if len(n.Children) > 0 && n.Children[0].Kind == "lambda_parameters" {
			writeExpr(b, n.Children[0], indent)
			if len(n.Children) > 1 {
				b.WriteByte(' ')
				writeExpr(b, n.Children[1], indent)
			}
		}
	case "lambda_parameters":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
		b.WriteByte(')')
	case "block":
		b.WriteString("{")
		if len(n.Children) > 0 {
			b.WriteByte(' ')
			writeExpr(b, n.Children[0], indent)
			b.WriteByte(' ')
		}
		b.WriteString("}")
	case "block_body":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString("; ")
			}
			writeExpr(b, c, indent)
		}
	case "parenthesized_statements":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(';')
			}
			if c.Kind == "begin" {
				writeBegin(b, c, indent)
			} else {
				writeExpr(b, c, indent)
			}
		}
		b.WriteByte(')')
	case "begin":
		writeBegin(b, n, indent)
	case "do_block":
		writeDoBlock(b, n, indent)
	case "block_parameters":
		writeBlockParameters(b, n)
	case "body_statement":
		writeBody(b, n, indent)
	case "method_parameters":
		writeParameters(b, n)
	case "conditional":
		if len(n.Children) == 3 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" ? ")
			writeExpr(b, n.Children[1], indent)
			b.WriteString(" : ")
			writeExpr(b, n.Children[2], indent)
		}
	case "range":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			op := "..."
			if n.Text != "" {
				op = strings.TrimSpace(n.Text)
			}
			b.WriteString(op)
			writeExpr(b, n.Children[1], indent)
		}
	default:
		b.WriteString(n.Kind)
	}
}

func writeReturnArgs(b *bytes.Buffer, n *Node, indent int) {
	if n.Kind == "argument_list" {
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeExpr(b, c, indent)
		}
	} else {
		writeExpr(b, n, indent)
	}
}
