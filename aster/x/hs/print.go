//go:build slow

package hs

import (
	"bytes"
	"fmt"
	"strings"

	hsparse "mochi/tools/a2mochi/x/hs"
)

// Print reconstructs Haskell source code from the simplified Program.
func Print(p *Program) (string, error) {
	if p == nil {
		return "", fmt.Errorf("nil program")
	}
	var b bytes.Buffer
	for _, l := range p.Pragmas {
		b.WriteString(l)
		if !strings.HasSuffix(l, "\n") {
			b.WriteByte('\n')
		}
	}
	for _, l := range p.Imports {
		b.WriteString(l)
		if !strings.HasSuffix(l, "\n") {
			b.WriteByte('\n')
		}
	}
	for _, it := range p.Items {
		writeItem(&b, it)
	}
	out := b.String()
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out += "\n"
	}
	return out, nil
}

func writeItem(b *bytes.Buffer, it hsparse.Item) {
	switch it.Kind {
	case "var":
		b.WriteString(it.Name)
		b.WriteString(" = ")
		b.WriteString(it.Body)
		if !strings.HasSuffix(it.Body, "\n") {
			b.WriteByte('\n')
		}
	case "func":
		b.WriteString(it.Name)
		if len(it.Params) > 0 {
			b.WriteByte(' ')
			b.WriteString(strings.Join(it.Params, " "))
		}
		b.WriteString(" = ")
		b.WriteString(it.Body)
		if !strings.HasSuffix(it.Body, "\n") {
			b.WriteByte('\n')
		}
	case "sig":
		b.WriteString(it.Name)
		b.WriteString(" :: ")
		b.WriteString(strings.TrimSpace(it.Type))
		b.WriteByte('\n')
	case "struct":
		b.WriteString("data ")
		b.WriteString(it.Name)
		b.WriteString(" = ")
		b.WriteString(it.Name)
		if len(it.Fields) > 0 {
			b.WriteString(" {")
			for i, f := range it.Fields {
				if i > 0 {
					b.WriteString(", ")
				}
				b.WriteString(f.Name)
				b.WriteString(" :: ")
				b.WriteString(f.Type)
			}
			b.WriteString(" }")
		}
		b.WriteByte('\n')
	default:
		// unknown item kind
	}
}
