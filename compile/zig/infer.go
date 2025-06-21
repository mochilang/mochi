package zigcode

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
			if isInt(t) && isInt(rt) {
				t = types.IntType{}
				continue
			}
			if isFloat(t) && isFloat(rt) {
				t = types.FloatType{}
				continue
			}
			if op.Op == "+" {
				if llist, ok := t.(types.ListType); ok {
					if rlist, ok := rt.(types.ListType); ok && equalTypes(llist.Elem, rlist.Elem) {
						t = llist
						continue
					}
				}
				if isString(t) && isString(rt) {
					t = types.StringType{}
					continue
				}
			}
			t = types.AnyType{}
		case "==", "!=", "<", "<=", ">", ">=":
			t = types.BoolType{}
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
		case "reduce":
			if len(p.Call.Args) == 3 {
				return c.inferExprType(p.Call.Args[2])
			}
			return types.AnyType{}
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
	case p.Map != nil:
		var keyType types.Type = types.AnyType{}
		var valType types.Type = types.AnyType{}
		if len(p.Map.Items) > 0 {
			keyType = c.inferExprType(p.Map.Items[0].Key)
			valType = c.inferExprType(p.Map.Items[0].Value)
			for _, it := range p.Map.Items[1:] {
				kt := c.inferExprType(it.Key)
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
	case p.Query != nil:
		return types.ListType{Elem: c.inferExprType(p.Query.Select)}
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
	case p.If != nil:
		return c.inferIfExprType(p.If)
	}
	return types.AnyType{}
}

func (c *Compiler) inferIfExprType(ie *parser.IfExpr) types.Type {
	t := c.inferExprType(ie.Then)
	var elseT types.Type
	if ie.ElseIf != nil {
		elseT = c.inferIfExprType(ie.ElseIf)
	} else if ie.Else != nil {
		elseT = c.inferExprType(ie.Else)
	}
	if elseT != nil && !equalTypes(t, elseT) {
		return types.AnyType{}
	}
	return t
}
