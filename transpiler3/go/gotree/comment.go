package gotree

import "strings"

// Comment is a single source comment. Text is the raw payload
// without the leading "//" or surrounding "/* */"; the writer
// re-adds the syntax based on Style.
type Comment struct {
	Text  string
	Style CommentStyle
}

// CommentStyle selects line vs block comment syntax.
type CommentStyle int

const (
	// LineComment renders as "// Text" on a single source line.
	LineComment CommentStyle = iota
	// BlockComment renders as "/* Text */".
	BlockComment
)

// CommentGroup is an attached run of comments. Doc comments on
// declarations are CommentGroups so multi-line documentation
// renders as consecutive "//" lines preserved by gofmt.
type CommentGroup struct {
	List []Comment
}

func (g *CommentGroup) write(w *Writer) {
	if g == nil {
		return
	}
	for _, c := range g.List {
		switch c.Style {
		case BlockComment:
			w.Line("/* " + c.Text + " */")
		default:
			for line := range strings.SplitSeq(c.Text, "\n") {
				w.Line("// " + line)
			}
		}
	}
}
