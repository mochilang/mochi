package rktcode

import (
	"strings"

	"mochi/parser"
)

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

func sanitizeName(name string) string {
	if name == "" {
		return ""
	}
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	s := b.String()
	if s == "" || !((s[0] >= 'A' && s[0] <= 'Z') || (s[0] >= 'a' && s[0] <= 'z') || s[0] == '_') {
		s = "_" + s
	}
	return s
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return false
	}
	return p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0
}

// isListPushCall returns the variable name and argument if the expression is a
// simple list.push(x) call.
func isListPushCall(e *parser.Expr) (string, *parser.Expr, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return "", nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", nil, false
	}
	p := u.Value
	if p.Target.Selector == nil || len(p.Target.Selector.Tail) != 1 {
		return "", nil, false
	}
	if p.Target.Selector.Tail[0] != "push" {
		return "", nil, false
	}
	if len(p.Ops) != 1 || p.Ops[0].Call == nil || len(p.Ops[0].Call.Args) != 1 {
		return "", nil, false
	}
	return p.Target.Selector.Root, p.Ops[0].Call.Args[0], true
}
