package tscode

import (
	"bytes"
	"strings"
)

// emitter handles code generation with indentation support.
type emitter struct {
	buf    bytes.Buffer
	indent int
}

// writeln writes a line with the current indentation.
func (e *emitter) writeln(s string) {
	e.writeIndent()
	e.buf.WriteString(s)
	e.buf.WriteByte('\n')
}

func (e *emitter) writeIndent() {
	for i := 0; i < e.indent; i++ {
		e.buf.WriteByte('\t')
	}
}

func indentBlock(s string, depth int) string {
	if s == "" {
		return s
	}
	prefix := strings.Repeat("\t", depth)
	lines := strings.Split(strings.TrimRight(s, "\n"), "\n")
	for i, line := range lines {
		lines[i] = prefix + line
	}
	return strings.Join(lines, "\n") + "\n"
}
