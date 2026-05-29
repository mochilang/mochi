package gotree

import (
	"bytes"
	"go/format"
	"strings"
)

// Writer is a thin wrapper around bytes.Buffer that tracks
// indentation depth. Node.write methods append source text
// through Writer; Render then runs the result through
// go/format.Source so depth-tracking is a hint, not a contract.
type Writer struct {
	buf   *bytes.Buffer
	depth int
}

// NewWriter constructs a Writer backed by a fresh bytes.Buffer.
func NewWriter() *Writer { return &Writer{buf: new(bytes.Buffer)} }

// Indent increases the indentation depth by one tab.
func (w *Writer) Indent() { w.depth++ }

// Dedent decreases the indentation depth by one tab.
func (w *Writer) Dedent() {
	if w.depth > 0 {
		w.depth--
	}
}

// Line writes s at the current indentation followed by a newline.
// Embedded newlines in s are written verbatim; only the leading
// indent and trailing newline are added by Line.
func (w *Writer) Line(s string) {
	w.buf.WriteString(strings.Repeat("\t", w.depth))
	w.buf.WriteString(s)
	w.buf.WriteByte('\n')
}

// Raw writes s with no indentation, no newline. Use for partial
// lines (e.g. building up an expression piece by piece).
func (w *Writer) Raw(s string) { w.buf.WriteString(s) }

// Newline writes a bare newline (no indent).
func (w *Writer) Newline() { w.buf.WriteByte('\n') }

// Bytes returns the accumulated buffer contents. Callers should
// pass the result through Format to canonicalise whitespace and
// import ordering before writing to disk.
func (w *Writer) Bytes() []byte { return w.buf.Bytes() }

// Format runs src through go/format.Source. It is the canonical
// post-processor for any byte slice the lowerer assembles.
func Format(src []byte) ([]byte, error) { return format.Source(src) }
