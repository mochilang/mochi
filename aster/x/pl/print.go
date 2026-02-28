package pl

import (
	"bytes"
	"fmt"
	"strings"
)

// Print reconstructs Prolog source code from the given Program.
func Print(p *Program) (string, error) {
	if p == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	for i, c := range p.Clauses {
		if i > 0 {
			b.WriteByte('\n')
		}
		writeClause(&b, c)
	}
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeClause(b *bytes.Buffer, c Clause) {
	if c.Name == ":-" {
		b.WriteString(":- ")
		for i, p := range c.Params {
			if i > 0 {
				b.WriteString(", ")
			}
			b.WriteString(fmt.Sprint(p))
		}
		b.WriteString(".\n")
		return
	}
	b.WriteString(c.Name)
	if len(c.Params) > 0 {
		b.WriteByte('(')
		for i, p := range c.Params {
			if i > 0 {
				b.WriteString(", ")
			}
			b.WriteString(fmt.Sprint(p))
		}
		b.WriteByte(')')
	}
	b.WriteString(" :-\n    ")
	b.WriteString(strings.TrimSpace(c.Body))
	b.WriteString(".\n")
}
