package plcode

import (
	"strings"

	"mochi/parser"
	"mochi/types"
)

// inferExprType performs minimal type inference used by the code generator.
// It distinguishes literals, lists, maps and known variable references.
func (c *Compiler) inferExprType(e *parser.Expr) types.Type {
	if e == nil {
		return types.AnyType{}
	}
	if id, ok := identName(e); ok && c.env != nil {
		if t, err := c.env.GetVar(id); err == nil {
			return t
		}
	}
	return c.inferUnaryType(e.Binary.Left)
}

func (c *Compiler) inferPostfixType(p *parser.PostfixExpr) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	t := c.inferPrimaryType(p.Target)
	for _, op := range p.Ops {
		if op.Cast != nil {
			t = c.resolveTypeRef(op.Cast.Type)
		} else if op.Index != nil && op.Index.Colon == nil {
			switch tt := t.(type) {
			case types.ListType:
				t = tt.Elem
			case types.MapType:
				t = tt.Value
			case types.StringType:
				t = types.StringType{}
			default:
				t = types.AnyType{}
			}
		} else if op.Index != nil {
			switch tt := t.(type) {
			case types.ListType:
				t = tt
			case types.StringType:
				t = types.StringType{}
			default:
				t = types.AnyType{}
			}
		} else if op.Call != nil {
			if ft, ok := t.(types.FuncType); ok {
				t = ft.Return
			} else {
				t = types.AnyType{}
			}
		}
	}
	return t
}

func (c *Compiler) inferUnaryType(u *parser.Unary) types.Type {
	if u == nil {
		return types.AnyType{}
	}
	return c.inferPostfixType(u.Value)
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
