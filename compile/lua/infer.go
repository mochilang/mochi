package luacode

import (
	"mochi/parser"
	"mochi/types"
)

func (c *Compiler) inferExprType(e *parser.Expr) types.Type {
	if e == nil {
		return types.AnyType{}
	}
	return c.inferBinaryType(e.Binary)
}

func (c *Compiler) inferBinaryType(b *parser.BinaryExpr) types.Type {
	if b == nil {
		return types.AnyType{}
	}
	return c.inferUnaryType(b.Left)
}

func (c *Compiler) inferUnaryType(u *parser.Unary) types.Type {
	if u == nil {
		return types.AnyType{}
	}
	return c.inferPostfixType(u.Value)
}

func (c *Compiler) inferPostfixType(p *parser.PostfixExpr) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	t := c.inferPrimaryType(p.Target)
	for _, op := range p.Ops {
		if op.Index != nil && op.Index.Colon == nil {
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
		}
	}
	return t
}

func (c *Compiler) inferPrimaryType(p *parser.Primary) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	switch {
	case p.Selector != nil:
		if c.env != nil {
			name := p.Selector.Root
			t, err := c.env.GetVar(name)
			if err == nil {
				cur := t
				for _, field := range p.Selector.Tail {
					st, ok := cur.(types.StructType)
					if !ok {
						return types.AnyType{}
					}
					ft, ok := st.Fields[field]
					if !ok {
						return types.AnyType{}
					}
					cur = ft
				}
				return cur
			}
		}
		return types.AnyType{}
	case p.List != nil:
		return types.ListType{Elem: types.AnyType{}}
	case p.Map != nil:
		return types.MapType{Key: types.AnyType{}, Value: types.AnyType{}}
	case p.Lit != nil:
		switch {
		case p.Lit.Int != nil:
			return types.IntType{}
		case p.Lit.Float != nil:
			return types.FloatType{}
		case p.Lit.Str != nil:
			return types.StringType{}
		case p.Lit.Bool != nil:
			return types.BoolType{}
		}
	}
	return types.AnyType{}
}
