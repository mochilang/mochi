package luacode

import (
	"strings"

	"mochi/parser"
	"mochi/types"
)

// inferExprType performs a very small amount of type inference.
// It only distinguishes lists, maps and strings which is
// sufficient for generating idiomatic loops.
func (c *Compiler) inferExprType(e *parser.Expr) types.Type {
	if e == nil {
		return types.AnyType{}
	}
	if id, ok := identName(e); ok && c.env != nil {
		if t, err := c.env.GetVar(id); err == nil {
			return t
		}
	}
	return c.inferPrimaryType(e.Binary.Left.Value.Target)
}

func (c *Compiler) inferPrimaryType(p *parser.Primary) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	switch {
	case p.Lit != nil:
		switch {
		case p.Lit.Str != nil:
			return types.StringType{}
		case p.Lit.Int != nil:
			return types.IntType{}
		case p.Lit.Float != nil:
			return types.FloatType{}
		case p.Lit.Bool != nil:
			return types.BoolType{}
		}
	case p.List != nil:
		return types.ListType{Elem: types.AnyType{}}
	case p.Map != nil:
		return types.MapType{Key: types.AnyType{}, Value: types.AnyType{}}
	case p.Selector != nil:
		if c.env != nil {
			if len(p.Selector.Tail) > 0 {
				full := p.Selector.Root + "." + strings.Join(p.Selector.Tail, ".")
				if t, err := c.env.GetVar(full); err == nil {
					return t
				}
			}
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				return t
			}
		}
	}
	return types.AnyType{}
}

func isList(t types.Type) bool {
	_, ok := t.(types.ListType)
	return ok
}

func isMap(t types.Type) bool {
	_, ok := t.(types.MapType)
	return ok
}

func isString(t types.Type) bool {
	_, ok := t.(types.StringType)
	return ok
}
