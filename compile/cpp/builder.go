package cppcode

import "bytes"

// codeBuilder assists in generating C++ source with indentation.
type codeBuilder struct {
	buf    bytes.Buffer
	indent int
}

// writeln writes a line with the current indentation.
func (b *codeBuilder) writeln(s string) {
	for i := 0; i < b.indent; i++ {
		b.buf.WriteByte('\t')
	}
	b.buf.WriteString(s)
	b.buf.WriteByte('\n')
}

// writeIndent writes the current indentation without a newline.
func (b *codeBuilder) writeIndent() {
	for i := 0; i < b.indent; i++ {
		b.buf.WriteByte('\t')
	}
}
