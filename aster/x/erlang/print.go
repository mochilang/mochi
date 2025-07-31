//go:build slow

package erlang

import (
	"bytes"
	"fmt"
	"strings"
)

// Print reconstructs Erlang source code from the Program AST. The output is
// generated solely from the structured nodes rather than reusing the original
// text so that formatting is consistent.
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
	if n == nil {
		return
	}
	if n.Text != "" && len(n.Children) == 0 {
		b.WriteString(n.Text)
		return
	}
	switch n.Kind {
	case "source_file":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte('\n')
			}
			writeNode(b, c, indent)
		}
	case "module_attribute":
		b.WriteString("-module(")
		if len(n.Children) > 0 {
			writeNode(b, n.Children[0], indent)
		}
		b.WriteString(").")
		b.WriteByte('\n')
	case "export_attribute":
		b.WriteString("-export([")
		if len(n.Children) > 0 {
			fa := n.Children[0]
			if len(fa.Children) >= 2 {
				writeNode(b, fa.Children[0], indent)
				b.WriteByte('/')
				writeNode(b, fa.Children[1].Children[0], indent)
			}
		}
		b.WriteString("]).")
		b.WriteByte('\n')
	case "fun_decl":
		for _, c := range n.Children {
			writeNode(b, c, indent)
		}
	case "function_clause":
		if len(n.Children) < 3 {
			return
		}
		writeNode(b, n.Children[0], indent)
		writeNode(b, n.Children[1], indent)
		b.WriteString(" ->\n")
		writeNode(b, n.Children[2], indent+1)
		b.WriteByte('.')
		b.WriteByte('\n')
	case "expr_args":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeNode(b, c, indent)
		}
		b.WriteByte(')')
	case "clause_body":
		for i, c := range n.Children {
			writeIndent(b, indent)
			writeNode(b, c, indent)
			if i < len(n.Children)-1 {
				b.WriteString(",\n")
			} else {
				b.WriteByte('\n')
			}
		}
	case "call":
		if len(n.Children) >= 2 {
			writeNode(b, n.Children[0], indent)
			writeNode(b, n.Children[1], indent)
		}
	case "remote":
		if len(n.Children) == 2 {
			writeNode(b, n.Children[0], indent)
			b.WriteByte(':')
			writeNode(b, n.Children[1], indent)
		}
	case "remote_module":
		if len(n.Children) > 0 {
			writeNode(b, n.Children[0], indent)
		}
	case "list":
		b.WriteByte('[')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeNode(b, c, indent)
		}
		b.WriteByte(']')
	case "tuple":
		b.WriteByte('{')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeNode(b, c, indent)
		}
		b.WriteByte('}')
	case "comment":
		writeIndent(b, indent)
		b.WriteString(n.Text)
		b.WriteByte('\n')
	case "atom", "integer", "string", "var":
		b.WriteString(n.Text)
	case "shebang":
		b.WriteString(n.Text)
		b.WriteByte('\n')
	case "paren_expr":
		b.WriteByte('(')
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeNode(b, c, indent)
		}
		b.WriteByte(')')
	case "binary_op_expr":
		if len(n.Children) == 2 {
			writeNode(b, n.Children[0], indent)
			op := extractBinaryOp(n)
			if op != "" {
				b.WriteByte(' ')
				b.WriteString(op)
				b.WriteByte(' ')
			}
			writeNode(b, n.Children[1], indent)
		}
	case "unary_op_expr":
		if len(n.Children) == 1 {
			op := strings.TrimSpace(strings.TrimSuffix(n.Text, n.Children[0].Text))
			b.WriteString(op)
			b.WriteByte(' ')
			writeNode(b, n.Children[0], indent)
		}
	case "anonymous_fun":
		b.WriteString("fun ")
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString("; ")
			}
			writeNode(b, c, indent)
		}
		b.WriteString(" end")
	case "fun_clause":
		if len(n.Children) == 3 {
			writeNode(b, n.Children[0], indent)
			writeNode(b, n.Children[1], indent)
			b.WriteString(" -> ")
			writeNode(b, n.Children[2], indent)
		} else if len(n.Children) == 2 {
			writeNode(b, n.Children[0], indent)
			b.WriteString(" -> ")
			writeNode(b, n.Children[1], indent)
		}
	case "case_expr":
		if len(n.Children) >= 2 {
			b.WriteString("case ")
			writeNode(b, n.Children[0], indent)
			b.WriteString(" of\n")
			for i, c := range n.Children[1:] {
				writeIndent(b, indent+1)
				writeNode(b, c, indent+1)
				if i < len(n.Children[1:])-1 {
					b.WriteString(";\n")
				} else {
					b.WriteByte('\n')
				}
			}
			writeIndent(b, indent)
			b.WriteString("end")
		}
	case "cr_clause":
		if len(n.Children) >= 2 {
			writeNode(b, n.Children[0], indent)
			b.WriteString(" -> ")
			writeNode(b, n.Children[1], indent)
		}
	case "map_expr":
		b.WriteString("#{")
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeNode(b, c, indent)
		}
		b.WriteByte('}')
	case "map_field":
		if len(n.Children) == 2 {
			writeNode(b, n.Children[0], indent)
			b.WriteString(" => ")
			writeNode(b, n.Children[1], indent)
		}
	case "list_comprehension":
		b.WriteByte('[')
		if len(n.Children) > 0 {
			writeNode(b, n.Children[0], indent)
		}
		if len(n.Children) > 1 {
			b.WriteString(" || ")
			for i, c := range n.Children[1:] {
				if i > 0 {
					b.WriteString(", ")
				}
				writeNode(b, c, indent)
			}
		}
		b.WriteByte(']')
	case "lc_exprs":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeNode(b, c, indent)
		}
	case "lc_or_zc_expr":
		for i, c := range n.Children {
			if i > 0 {
				b.WriteString(", ")
			}
			writeNode(b, c, indent)
		}
	case "try_expr":
		var i int
		b.WriteString("try\n")
		for i = 0; i < len(n.Children); i++ {
			c := n.Children[i]
			if c.Kind == "catch_clause" {
				break
			}
			writeIndent(b, indent+1)
			writeNode(b, c, indent+1)
			if i < len(n.Children)-1 && n.Children[i+1].Kind != "catch_clause" {
				b.WriteString(",\n")
			} else {
				b.WriteByte('\n')
			}
		}
		if i < len(n.Children) && n.Children[i].Kind == "catch_clause" {
			writeIndent(b, indent+1)
			b.WriteString("catch\n")
			for j := i; j < len(n.Children); j++ {
				if j > i {
					b.WriteString(";\n")
				}
				writeIndent(b, indent+2)
				writeNode(b, n.Children[j], indent+2)
			}
			b.WriteByte('\n')
		}
		writeIndent(b, indent)
		b.WriteString("end")
	case "catch_clause":
		if len(n.Children) == 2 {
			writeNode(b, n.Children[0], indent)
			b.WriteString(" -> ")
			writeNode(b, n.Children[1], indent)
		}
	case "generator":
		if len(n.Children) == 2 {
			writeNode(b, n.Children[0], indent)
			b.WriteString(" <- ")
			writeNode(b, n.Children[1], indent)
		}
	case "match_expr":
		if len(n.Children) == 2 {
			writeNode(b, n.Children[0], indent)
			b.WriteString(" = ")
			writeNode(b, n.Children[1], indent)
		}
	default:
		if n.Text != "" {
			b.WriteString(n.Text)
			return
		}
		for i, c := range n.Children {
			if i > 0 {
				b.WriteByte(' ')
			}
			writeNode(b, c, indent)
		}
	}
}

func writeIndent(b *bytes.Buffer, indent int) {
	for i := 0; i < indent; i++ {
		b.WriteString("    ")
	}
}

func extractBinaryOp(n *Node) string {
	if len(n.Children) < 2 {
		return ""
	}
	t := strings.TrimSpace(n.Text)
	left := strings.TrimSpace(n.Children[0].Text)
	right := strings.TrimSpace(n.Children[1].Text)
	if i := strings.Index(t, left); i >= 0 {
		t = t[i+len(left):]
	}
	if j := strings.LastIndex(t, right); j >= 0 {
		t = t[:j]
	}
	return strings.TrimSpace(t)
}
