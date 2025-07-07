//go:build archived

package plcode

import (
	"strings"

	"mochi/parser"
	"mochi/types"
)

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

var plReserved = map[string]bool{
	"is":    true,
	"true":  true,
	"false": true,
}

func sanitizeVar(name string) string {
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	sanitized := b.String()
	if sanitized == "" || !((sanitized[0] >= 'A' && sanitized[0] <= 'Z') || (sanitized[0] >= 'a' && sanitized[0] <= 'z') || sanitized[0] == '_') {
		sanitized = "_" + sanitized
	}
	if plReserved[sanitized] {
		sanitized = "_" + sanitized
	}
	if sanitized[0] >= 'a' && sanitized[0] <= 'z' {
		sanitized = strings.ToUpper(sanitized[:1]) + sanitized[1:]
	}
	return sanitized
}

func sanitizeAtom(name string) string {
	name = strings.ReplaceAll(name, "-", "_")
	clean := make([]rune, len(name))
	for i, r := range name {
		if (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || (r >= '0' && r <= '9') || r == '_' {
			clean[i] = r
		} else {
			clean[i] = '_'
		}
	}
	name = string(clean)
	if len(name) > 0 && (name[0] < 'a' || name[0] > 'z') {
		name = "p_" + name
	}
	return strings.ToLower(name)
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
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
	if e == nil || len(e.Binary.Right) != 0 {
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

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = c.resolveTypeRef(p)
		}
		var ret types.Type = types.VoidType{}
		if t.Fun.Return != nil {
			ret = c.resolveTypeRef(t.Fun.Return)
		}
		return types.FuncType{Params: params, Return: ret}
	}
	if t.Generic != nil {
		name := t.Generic.Name
		args := t.Generic.Args
		switch name {
		case "list":
			if len(args) == 1 {
				return types.ListType{Elem: c.resolveTypeRef(args[0])}
			}
		case "map":
			if len(args) == 2 {
				return types.MapType{Key: c.resolveTypeRef(args[0]), Value: c.resolveTypeRef(args[1])}
			}
		}
		return types.AnyType{}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "float":
			return types.FloatType{}
		case "string":
			return types.StringType{}
		case "bool":
			return types.BoolType{}
		default:
			if c != nil && c.env != nil {
				if st, ok := c.env.GetStruct(*t.Simple); ok {
					return st
				}
				if ut, ok := c.env.GetUnion(*t.Simple); ok {
					return ut
				}
			}
			return types.AnyType{}
		}
	}
	return types.AnyType{}
}

func (c *Compiler) isStringExpr(e *parser.Expr) bool {
	_, ok := types.TypeOfExprBasic(e, c.env).(types.StringType)
	return ok
}

func (c *Compiler) isStringUnary(u *parser.Unary) bool {
	_, ok := types.TypeOfPostfixBasic(u, c.env).(types.StringType)
	return ok
}

func (c *Compiler) isStringPostfix(p *parser.PostfixExpr) bool {
	_, ok := types.TypeOfPostfixBasic(&parser.Unary{Value: p}, c.env).(types.StringType)
	return ok
}
