package erlcode

import (
	"strings"

	"mochi/parser"
	"mochi/types"
)

func (c *Compiler) inferExprType(e *parser.Expr) types.Type {
	if e == nil {
		return types.AnyType{}
	}
	return c.inferBinaryType(e.Binary)
}

func (c *Compiler) inferExprTypeHint(e *parser.Expr, hint types.Type) types.Type {
	if e == nil {
		return types.AnyType{}
	}
	if lt, ok := hint.(types.ListType); ok {
		if e.Binary != nil && len(e.Binary.Right) == 0 {
			if ll := e.Binary.Left.Value.Target.List; ll != nil {
				if len(ll.Elems) == 0 {
					return types.ListType{Elem: lt.Elem}
				}
				elem := c.inferExprTypeHint(ll.Elems[0], lt.Elem)
				for _, el := range ll.Elems[1:] {
					t := c.inferExprTypeHint(el, lt.Elem)
					if !equalTypes(elem, t) {
						elem = types.AnyType{}
						break
					}
				}
				return types.ListType{Elem: elem}
			}
		}
	}
	return c.inferExprType(e)
}

func (c *Compiler) inferBinaryType(b *parser.BinaryExpr) types.Type {
	if b == nil {
		return types.AnyType{}
	}
	t := c.inferUnaryType(b.Left)
	for _, op := range b.Right {
		rt := c.inferPostfixType(op.Right)
		switch op.Op {
		case "+", "-", "*", "/", "%":
			if isInt64(t) {
				if isInt64(rt) || isInt(rt) {
					t = types.Int64Type{}
					continue
				}
			}
			if _, ok := t.(types.IntType); ok {
				if _, ok := rt.(types.IntType); ok {
					t = types.IntType{}
					continue
				}
			}
			if _, ok := t.(types.FloatType); ok {
				if _, ok := rt.(types.FloatType); ok {
					t = types.FloatType{}
					continue
				}
			}
			if op.Op == "+" {
				if llist, ok := t.(types.ListType); ok {
					if rlist, ok := rt.(types.ListType); ok && equalTypes(llist.Elem, rlist.Elem) {
						t = llist
						continue
					}
				}
				if _, ok := t.(types.StringType); ok {
					if _, ok := rt.(types.StringType); ok {
						t = types.StringType{}
						continue
					}
				}
			}
			t = types.AnyType{}
		case "==", "!=", "<", "<=", ">", ">=":
			t = types.BoolType{}
		case "&&", "||":
			if isBool(t) && isBool(rt) {
				t = types.BoolType{}
			} else {
				t = types.AnyType{}
			}
		case "in":
			switch rt.(type) {
			case types.MapType, types.ListType, types.StringType:
				t = types.BoolType{}
			default:
				t = types.AnyType{}
			}
		default:
			t = types.AnyType{}
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
		} else if op.Cast != nil {
			t = c.resolveTypeRef(op.Cast.Type)
		}
	}
	return t
}

func (c *Compiler) inferPrimaryType(p *parser.Primary) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	switch {
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
	case p.Selector != nil:
		if c.env != nil {
			if len(p.Selector.Tail) > 0 {
				full := p.Selector.Root + "." + strings.Join(p.Selector.Tail, ".")
				if t, err := c.env.GetVar(full); err == nil {
					return t
				}
			}
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				if len(p.Selector.Tail) == 0 {
					return t
				}
				if st, ok := t.(types.StructType); ok {
					cur := st
					for idx, field := range p.Selector.Tail {
						ft, ok := cur.Fields[field]
						if !ok {
							return types.AnyType{}
						}
						if idx == len(p.Selector.Tail)-1 {
							return ft
						}
						if next, ok := ft.(types.StructType); ok {
							cur = next
						} else {
							return types.AnyType{}
						}
					}
				}
				if ut, ok := t.(types.UnionType); ok {
					if ft, ok := unionFieldPathType(ut, p.Selector.Tail); ok {
						return ft
					}
				}
			}
		}
		return types.AnyType{}
	case p.Struct != nil:
		if c.env != nil {
			if st, ok := c.env.GetStruct(p.Struct.Name); ok {
				return st
			}
		}
		return types.AnyType{}
	case p.FunExpr != nil:
		params := make([]types.Type, len(p.FunExpr.Params))
		for i, par := range p.FunExpr.Params {
			if par.Type != nil {
				params[i] = c.resolveTypeRef(par.Type)
			} else {
				params[i] = types.AnyType{}
			}
		}
		var ret types.Type = types.VoidType{}
		if p.FunExpr.Return != nil {
			ret = c.resolveTypeRef(p.FunExpr.Return)
		} else if p.FunExpr.ExprBody != nil {
			ret = c.inferExprType(p.FunExpr.ExprBody)
		} else {
			ret = types.AnyType{}
		}
		return types.FuncType{Params: params, Return: ret}
	case p.Generate != nil:
		switch p.Generate.Target {
		case "text":
			return types.StringType{}
		case "embedding":
			return types.ListType{Elem: types.FloatType{}}
		default:
			if c.env != nil {
				if st, ok := c.env.GetStruct(p.Generate.Target); ok {
					return st
				}
			}
			return types.AnyType{}
		}
	case p.Call != nil:
		switch p.Call.Func {
		case "len":
			return types.IntType{}
		case "str":
			return types.StringType{}
		case "count":
			return types.IntType{}
		case "avg":
			return types.FloatType{}
		case "now":
			return types.Int64Type{}
		default:
			if c.env != nil {
				if t, err := c.env.GetVar(p.Call.Func); err == nil {
					if ft, ok := t.(types.FuncType); ok {
						return ft.Return
					}
				}
			}
			return types.AnyType{}
		}
	case p.Group != nil:
		return c.inferExprType(p.Group)
	case p.List != nil:
		var elemType types.Type = types.AnyType{}
		if len(p.List.Elems) > 0 {
			elemType = c.inferExprType(p.List.Elems[0])
			for _, e := range p.List.Elems[1:] {
				t := c.inferExprType(e)
				if !equalTypes(elemType, t) {
					elemType = types.AnyType{}
					break
				}
			}
		}
		return types.ListType{Elem: elemType}
	case p.Load != nil:
		var elem types.Type = types.MapType{Key: types.StringType{}, Value: types.AnyType{}}
		if p.Load.Type != nil {
			elem = c.resolveTypeRef(p.Load.Type)
			if st, ok := c.env.GetStruct(*p.Load.Type.Simple); elem == (types.AnyType{}) && ok {
				elem = st
			}
		}
		return types.ListType{Elem: elem}
	case p.Save != nil:
		return types.VoidType{}
	case p.Query != nil:
		srcType := c.inferExprType(p.Query.Source)
		var elemType types.Type = types.AnyType{}
		if lt, ok := srcType.(types.ListType); ok {
			elemType = lt.Elem
		}
		child := types.NewEnv(c.env)
		child.SetVar(p.Query.Var, elemType, true)
		for _, f := range p.Query.Froms {
			ft := c.inferExprType(f.Src)
			var fe types.Type = types.AnyType{}
			if lt, ok := ft.(types.ListType); ok {
				fe = lt.Elem
			}
			child.SetVar(f.Var, fe, true)
		}
		for _, j := range p.Query.Joins {
			jt := c.inferExprType(j.Src)
			var je types.Type = types.AnyType{}
			if lt, ok := jt.(types.ListType); ok {
				je = lt.Elem
			}
			child.SetVar(j.Var, je, true)
		}
		orig := c.env
		c.env = child
		elem := c.inferExprType(p.Query.Select)
		c.env = orig
		return types.ListType{Elem: elem}
	case p.Map != nil:
		var keyType types.Type = types.AnyType{}
		var valType types.Type = types.AnyType{}
		if len(p.Map.Items) > 0 {
			if _, ok := simpleStringKey(p.Map.Items[0].Key); ok {
				keyType = types.StringType{}
			} else {
				keyType = c.inferExprType(p.Map.Items[0].Key)
			}
			valType = c.inferExprType(p.Map.Items[0].Value)
			for _, it := range p.Map.Items[1:] {
				var kt types.Type
				if _, ok := simpleStringKey(it.Key); ok {
					kt = types.StringType{}
				} else {
					kt = c.inferExprType(it.Key)
				}
				vt := c.inferExprType(it.Value)
				if !equalTypes(keyType, kt) {
					keyType = types.AnyType{}
				}
				if !equalTypes(valType, vt) {
					valType = types.AnyType{}
				}
			}
		}
		return types.MapType{Key: keyType, Value: valType}
	case p.Match != nil:
		var rType types.Type
		for _, cs := range p.Match.Cases {
			t := c.inferExprType(cs.Result)
			if rType == nil {
				rType = t
			} else if !equalTypes(rType, t) {
				rType = types.AnyType{}
			}
		}
		if rType == nil {
			rType = types.AnyType{}
		}
		return rType
	}
	return types.AnyType{}
}

func resultType(op string, left, right types.Type) types.Type {
	switch op {
	case "+", "-", "*", "/", "%":
		if _, ok := left.(types.IntType); ok {
			if _, ok := right.(types.IntType); ok {
				return types.IntType{}
			}
		}
		if _, ok := left.(types.FloatType); ok {
			if _, ok := right.(types.FloatType); ok {
				return types.FloatType{}
			}
		}
		if op == "+" {
			if _, ok := left.(types.StringType); ok {
				if _, ok := right.(types.StringType); ok {
					return types.StringType{}
				}
			}
		}
		return types.AnyType{}
	case "==", "!=", "<", "<=", ">", ">=":
		return types.BoolType{}
	default:
		return types.AnyType{}
	}
}
