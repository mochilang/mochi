//go:build slow

package hscode

import (
	"strings"

	"mochi/parser"
	"mochi/types"
)

var hsReserved = map[string]bool{
	"case": true, "class": true, "data": true, "default": true,
	"deriving": true, "do": true, "else": true, "foreign": true,
	"if": true, "import": true, "in": true, "infix": true,
	"infixl": true, "infixr": true, "instance": true, "let": true,
	"module": true, "newtype": true, "of": true, "then": true,
	"type": true, "where": true,
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
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
	if hsReserved[s] {
		s = "_" + s
	}
	return s
}

func simpleStringKey(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	return "", false
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil {
		return "", false
	}
	if len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func wrapAnyValue(t types.Type, v string) string {
	switch t.(type) {
	case types.IntType, types.Int64Type:
		return "VInt (" + v + ")"
	case types.FloatType:
		return "VDouble (" + v + ")"
	case types.StringType:
		return "VString (" + v + ")"
	case types.BoolType:
		return "VBool (" + v + ")"
	default:
		return "VString (" + v + ")"
	}
}
