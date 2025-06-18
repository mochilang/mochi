package pascode

import (
	"fmt"
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

var pasReserved = map[string]struct{}{
	"and": {}, "array": {}, "begin": {}, "case": {}, "const": {}, "div": {},
	"do": {}, "downto": {}, "else": {}, "end": {}, "file": {}, "for": {},
	"function": {}, "goto": {}, "if": {}, "in": {}, "label": {}, "mod": {},
	"nil": {}, "not": {}, "of": {}, "or": {}, "packed": {}, "procedure": {},
	"program": {}, "record": {}, "repeat": {}, "set": {}, "then": {}, "to": {},
	"type": {}, "until": {}, "var": {}, "while": {}, "with": {},
	"result": {},
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
