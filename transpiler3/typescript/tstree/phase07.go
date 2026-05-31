// Phase 7 adds RawExpr and RawDecl: verbatim TypeScript escape hatches.
// RawExpr is used by Datalog lowering; RawDecl by stream/channel class
// helpers and other multi-line runtime prelude blocks.
package tstree

import "strings"

// RawExpr emits Text verbatim as a TypeScript expression.
// The Text must be a syntactically valid TS expression.
type RawExpr struct {
	Text string
}

func (e *RawExpr) exprNode()             {}
func (e *RawExpr) TsString(_ int) string { return e.Text }

// RawDecl emits Text verbatim as a top-level TypeScript declaration.
// Each line is indented by indent spaces when printed; the Text itself
// must NOT include a leading indent (TsString handles it).
type RawDecl struct {
	Text string
}

func (d *RawDecl) declNode() {}
func (d *RawDecl) TsString(indent int) string {
	if indent == 0 {
		return d.Text
	}
	pad := strings.Repeat("  ", indent)
	lines := strings.Split(d.Text, "\n")
	for i, line := range lines {
		if line != "" {
			lines[i] = pad + line
		}
	}
	return strings.Join(lines, "\n")
}
