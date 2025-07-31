//go:build slow

package rkt

import (
	"bytes"
	"fmt"
	"strings"
)

// Print returns Racket source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil || p.Root == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	lines := strings.Split(p.Source, "\n")
	writeProgram(&b, p.Root, lines, 0)
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeProgram(b *bytes.Buffer, n *ProgramNode, lines []string, indent int) {
	prev := 0
	for i, c := range n.Children {
		if i > 0 {
			if c.Start > prev && prev != 0 {
				b.WriteByte('\n')
			}
			b.WriteByte('\n')
		}
		writeTop(b, c, lines, indent)
		prev = c.End
	}
}

func writeTop(b *bytes.Buffer, n *Node, lines []string, indent int) {
	writeNode(b, n, lines, indent)
	if b.Len() == 0 || b.Bytes()[b.Len()-1] != '\n' {
		b.WriteByte('\n')
	}
}

func writeNode(b *bytes.Buffer, n *Node, lines []string, indent int) {
	switch n.Kind {
	case "comment", "number", "string", "symbol":
		b.WriteString(n.Text)
	case "extension":
		b.WriteString("#lang ")
		if len(n.Children) > 0 {
			b.WriteString(n.Children[0].Text)
		}
	case "lang_name":
		b.WriteString(n.Text)
	case "list":
		if len(n.Children) == 0 {
			if n.Text != "" {
				b.WriteString(n.Text)
			} else {
				b.WriteString("()")
			}
			return
		}
		b.WriteByte('(')
		prev := 0
		for i, c := range n.Children {
			if i > 0 {
				if c.Start > prev && prev != 0 {
					b.WriteByte('\n')
					b.WriteString(strings.Repeat("  ", indent+1))
				} else if c.Kind == "list" {
					b.WriteByte('\n')
					b.WriteString(strings.Repeat("  ", indent+1))
				} else {
					b.WriteByte(' ')
				}
			}
			writeNode(b, c, lines, indent+1)
			prev = c.End
		}
		b.WriteByte(')')
	case "quote":
		b.WriteByte('\'')
		if len(n.Children) > 0 {
			writeNode(b, n.Children[0], lines, indent)
		} else if n.Text != "" {
			b.WriteString(n.Text)
		}
	default:
		// unknown; write kind or text
		if n.Text != "" {
			b.WriteString(n.Text)
		} else if s := originalText(lines, n); s != "" {
			b.WriteString(s)
		}
	}
}

func originalText(lines []string, n *Node) string {
	if n.Start == 0 || n.End == 0 || n.Start > len(lines) || n.End > len(lines) {
		return ""
	}
	if n.Start == n.End {
		line := lines[n.Start-1]
		if n.EndCol > len(line) {
			return line
		}
		return line[n.StartCol:n.EndCol]
	}
	var b strings.Builder
	line := lines[n.Start-1]
	if n.StartCol < len(line) {
		b.WriteString(line[n.StartCol:])
	} else {
		b.WriteString(line)
	}
	for i := n.Start; i < n.End-1; i++ {
		b.WriteByte('\n')
		b.WriteString(lines[i])
	}
	b.WriteByte('\n')
	endLine := lines[n.End-1]
	if n.EndCol <= len(endLine) {
		b.WriteString(endLine[:n.EndCol])
	} else {
		b.WriteString(endLine)
	}
	return b.String()
}
