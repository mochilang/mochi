package pascode

import (
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
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

var pasReserved = map[string]struct{}{
	"and": {}, "array": {}, "begin": {}, "case": {}, "const": {}, "div": {},
	"do": {}, "downto": {}, "else": {}, "end": {}, "file": {}, "for": {},
	"function": {}, "goto": {}, "if": {}, "in": {}, "label": {}, "mod": {},
	"nil": {}, "not": {}, "of": {}, "or": {}, "packed": {}, "procedure": {},
	"program": {}, "record": {}, "repeat": {}, "set": {}, "then": {}, "to": {},
	"type": {}, "until": {}, "var": {}, "while": {}, "with": {},
	"result": {},
	"length": {},
}

func sanitizeName(name string) string {
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	if b.Len() == 0 {
		return "_"
	}
	res := b.String()
	if _, ok := pasReserved[strings.ToLower(res)]; ok {
		return "_" + res
	}
	return res
}

func (c *Compiler) newVar() string {
	name := fmt.Sprintf("_tmp%d", c.tempVarCount)
	c.tempVarCount++
	if c.tempVars == nil {
		c.tempVars = make(map[string]bool)
	}
	c.tempVars[name] = true
	return name
}

func isListLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil || u.Value.Target == nil {
		return false
	}
	return u.Value.Target.List != nil
}

func (c *Compiler) isListExpr(e *parser.Expr) bool {
	if isListLiteral(e) {
		return true
	}
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	return c.isListUnary(e.Binary.Left)
}

func (c *Compiler) isListUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) > 0 {
		return false
	}
	return c.isListPostfix(u.Value)
}

func (c *Compiler) isListPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return c.isListPrimary(p.Target)
}

func (c *Compiler) isListPrimary(p *parser.Primary) bool {
	switch {
	case p == nil:
		return false
	case p.List != nil:
		return true
	case p.Selector != nil:
		if c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				if _, ok := t.(types.ListType); ok {
					return true
				}
			}
		}
	}
	return false
}

func contains(list []string, s string) bool {
	for _, v := range list {
		if v == s {
			return true
		}
	}
	return false
}

func (c *Compiler) isFloatExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if c.isFloatUnary(e.Binary.Left) {
		return true
	}
	for _, part := range e.Binary.Right {
		if c.isFloatPostfix(part.Right) {
			return true
		}
	}
	return false
}

func (c *Compiler) isFloatUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isFloatPostfix(u.Value)
}

func (c *Compiler) isFloatPostfix(p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) > 0 {
		last := p.Ops[len(p.Ops)-1]
		if last.Cast != nil && last.Cast.Type != nil && last.Cast.Type.Simple != nil && *last.Cast.Type.Simple == "float" {
			return true
		}
	}
	return c.isFloatPrimary(p.Target)
}

func (c *Compiler) isFloatPrimary(p *parser.Primary) bool {
	switch {
	case p == nil:
		return false
	case p.Lit != nil && p.Lit.Float != nil:
		return true
	case p.Selector != nil:
		if c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				if _, ok := t.(types.FloatType); ok {
					return true
				}
			}
		}
	}
	return false
}
