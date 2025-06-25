package rbcode

import (
	"strings"

	"mochi/parser"
	"mochi/types"
)

var rbReserved = map[string]struct{}{
	// Ruby keywords
	"BEGIN": {}, "END": {}, "alias": {}, "and": {}, "begin": {}, "break": {},
	"case": {}, "class": {}, "def": {}, "defined?": {}, "do": {}, "else": {},
	"elsif": {}, "end": {}, "ensure": {}, "false": {}, "for": {}, "if": {},
	"in": {}, "module": {}, "next": {}, "nil": {}, "not": {}, "or": {},
	"redo": {}, "rescue": {}, "retry": {}, "return": {}, "self": {},
	"super": {}, "then": {}, "true": {}, "undef": {}, "unless": {},
	"until": {}, "when": {}, "while": {}, "yield": {},
}

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
	if _, ok := rbReserved[s]; ok {
		return "_" + s
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
	if p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0 {
		return true
	}
	return false
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil {
		return nil, false
	}
	if len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil {
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

func isStringLiteral(e *parser.Expr) bool {
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
	return p.Target != nil && p.Target.Lit != nil && p.Target.Lit.Str != nil
}

func (c *Compiler) isStringPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Str != nil {
		return true
	}
	if p.Call != nil && p.Call.Func == "str" {
		return true
	}
	if p.Selector != nil && len(p.Selector.Tail) == 0 && c.env != nil {
		if t, err := c.env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.StringType); ok {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) isStringPostfix(p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	return c.isStringPrimary(p.Target)
}

func (c *Compiler) isListExpr(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if name, ok := identName(e); ok && c.env != nil {
		if t, err := c.env.GetVar(name); err == nil {
			if _, ok := t.(types.ListType); ok {
				return true
			}
		}
	}
	if len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 0 {
			p := u.Value
			if len(p.Ops) == 0 && p.Target.List != nil {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) isMapExpr(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if name, ok := identName(e); ok && c.env != nil {
		if t, err := c.env.GetVar(name); err == nil {
			if _, ok := t.(types.MapType); ok {
				return true
			}
		}
	}
	if len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 0 {
			p := u.Value
			if len(p.Ops) == 0 && p.Target.Map != nil {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	return types.ResolveTypeRef(t, c.env)
}
