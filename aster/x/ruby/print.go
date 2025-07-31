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
	case "identifier", "constant", "integer", "float", "hash_key_symbol", "simple_symbol", "true", "false", "global_variable":
		b.WriteString(n.Text)
	case "string":
		b.WriteByte('"')
		for _, c := range n.Children {
			switch c.Kind {
			case "string_content":
				b.WriteString(c.Text)
			case "interpolation":
				writeExpr(b, c, indent)
			default:
				writeExpr(b, c, indent)
			}
		}
		b.WriteByte('"')
	case "string_content":
		b.WriteString(n.Text)
	case "interpolation":
		b.WriteString("#{")
		if len(n.Children) > 0 {
			writeExpr(b, n.Children[0], indent)
		}
		b.WriteByte('}')
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
		if len(n.Children) >= 2 && (n.Children[1].Kind == "identifier" || n.Children[1].Kind == "constant") {
			b.WriteByte('.')
			writeExpr(b, n.Children[1], indent)
			idx := 2
			if len(n.Children) > idx && n.Children[idx].Kind == "argument_list" {
				writeExpr(b, n.Children[idx], indent)
				idx++
			}
			if len(n.Children) > idx && n.Children[idx].Kind == "do_block" {
				writeDoBlock(b, n.Children[idx], indent)
				idx++
			}
			if len(n.Children) > idx && n.Children[idx].Kind == "block" {
				writeExpr(b, n.Children[idx], indent)
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
			sep := ": "
			switch n.Children[0].Kind {
			case "string", "integer", "float":
				sep = " => "
			}
			b.WriteString(sep)
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
	case "destructured_parameter":
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
			idx := 0
			if n.Children[0].Kind == "block_parameters" {
				writeBlockParameters(b, n.Children[0])
				idx = 1
				if idx < len(n.Children) {
					b.WriteByte(' ')
				}
			}
			for i := idx; i < len(n.Children); i++ {
				if i > idx {
					b.WriteString("; ")
				}
				writeExprForBlock(b, n.Children[i], indent)
			}
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
	case "class":
		if len(n.Children) >= 2 {
			b.WriteString("class ")
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('\n')
			writeBody(b, n.Children[1], indent+1)
			b.WriteString(strings.Repeat("  ", indent))
			b.WriteString("end")
		}
	case "block_parameters":
		writeBlockParameters(b, n)
	case "body_statement":
		writeBody(b, n, indent)
	case "method_parameters":
		writeParameters(b, n)
	case "operator_assignment":
		if len(n.Children) == 2 {
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" ||= ")
			writeExpr(b, n.Children[1], indent)
		}
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
	case "module":
		if len(n.Children) >= 2 {
			b.WriteString("module ")
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('\n')
			writeBody(b, n.Children[1], indent+1)
			b.WriteString(strings.Repeat("  ", indent))
			b.WriteString("end")
		}
	case "singleton_method":
		if len(n.Children) > 0 {
			b.WriteString("def self.")
			writeExpr(b, n.Children[0], indent)
			idx := 1
			if idx < len(n.Children) && n.Children[idx].Kind == "method_parameters" {
				writeParameters(b, n.Children[idx])
				idx++
			} else {
				b.WriteString("()")
			}
			b.WriteByte('\n')
			if idx < len(n.Children) {
				writeBody(b, n.Children[idx], indent+1)
			}
			b.WriteString(strings.Repeat("  ", indent))
			b.WriteString("end")
		}
	case "for":
		if len(n.Children) >= 3 {
			b.WriteString("for ")
			writeExpr(b, n.Children[0], indent)
			b.WriteString(" in ")
			if len(n.Children[1].Children) > 0 {
				writeExpr(b, n.Children[1].Children[0], indent)
			}
			b.WriteByte('\n')
			writeBody(b, n.Children[2], indent+1)
			b.WriteString(strings.Repeat("  ", indent))
			b.WriteString("end")
		}
	case "unless":
		if len(n.Children) >= 2 {
			b.WriteString(strings.Repeat("  ", indent))
			b.WriteString("unless ")
			writeExpr(b, n.Children[0], indent)
			b.WriteByte('\n')
			writeBody(b, n.Children[1], indent+1)
			b.WriteString(strings.Repeat("  ", indent))
			b.WriteString("end")
		}
	case "do":
		writeBody(b, n, indent)
	default:
		b.WriteString(n.Kind)
	}
}

func writeExprForBlock(b *bytes.Buffer, n *Node, indent int) {
	switch n.Kind {
	case "if":
		writeInlineIf(b, n)
	case "block_body":
		if len(n.Children) == 1 && n.Children[0].Kind == "if" {
			writeInlineIf(b, n.Children[0])
		} else {
			writeBodyInline(b, n)
		}
	default:
		writeExpr(b, n, indent)
	}
}

func writeInlineIf(b *bytes.Buffer, n *Node) {
	writeInlineIfChain(b, n, "if ")
	b.WriteString(" end")
}

func writeInlineIfChain(b *bytes.Buffer, n *Node, prefix string) {
	if len(n.Children) < 2 {
		return
	}
	b.WriteString(prefix)
	writeExpr(b, n.Children[0], 0)
	b.WriteString(" then ")
	writeBodyInline(b, n.Children[1])
	if len(n.Children) > 2 {
		c := n.Children[2]
		if c.Kind == "elsif" {
			b.WriteString(" elsif ")
			writeInlineIfChain(b, c, "")
		} else if c.Kind == "else" {
			b.WriteString(" else ")
			writeBodyInline(b, c)
		}
	}
}

func writeBodyInline(b *bytes.Buffer, n *Node) {
	for i, c := range n.Children {
		if i > 0 {
			b.WriteString("; ")
		}
		writeExpr(b, c, 0)
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
