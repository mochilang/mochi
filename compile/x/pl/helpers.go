package plcode

import "strings"

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

var plReserved = map[string]bool{
	"is":    true,
	"true":  true,
	"false": true,
}

func sanitizeVar(name string) string {
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	sanitized := b.String()
	if sanitized == "" || !((sanitized[0] >= 'A' && sanitized[0] <= 'Z') || (sanitized[0] >= 'a' && sanitized[0] <= 'z') || sanitized[0] == '_') {
		sanitized = "_" + sanitized
	}
	if plReserved[sanitized] {
		sanitized = "_" + sanitized
	}
	if sanitized[0] >= 'a' && sanitized[0] <= 'z' {
		sanitized = strings.ToUpper(sanitized[:1]) + sanitized[1:]
	}
	return sanitized
}

func sanitizeAtom(name string) string {
	name = strings.ReplaceAll(name, "-", "_")
	return strings.ToLower(name)
}
