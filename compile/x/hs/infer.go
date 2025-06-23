package hscode

import (
	"reflect"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// inferExprType returns the static type of expression e using a
// simplified version of the Go compiler's inference logic.
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
			if isInt(t) {
				if isInt(rt) {
					t = types.IntType{}
					continue
				}
			}
			if isFloat(t) {
				if isFloat(rt) {
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
	case p.Call != nil:
		switch p.Call.Func {
		case "len", "count":
			return types.IntType{}
		case "str":
			return types.StringType{}
		case "avg":
			return types.FloatType{}
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
	case p.Query != nil:
		srcType := c.inferExprType(p.Query.Source)
		var elem types.Type = types.AnyType{}
		if lt, ok := srcType.(types.ListType); ok {
			elem = lt.Elem
		}
		orig := c.env
		child := types.NewEnv(c.env)
		child.SetVar(p.Query.Var, elem, true)
		for _, f := range p.Query.Froms {
			ft := c.inferExprType(f.Src)
			var fe types.Type = types.AnyType{}
			if lt, ok := ft.(types.ListType); ok {
				fe = lt.Elem
			}
			child.SetVar(f.Var, fe, true)
		}
		c.env = child
		ret := c.inferExprType(p.Query.Select)
		c.env = orig
		return types.ListType{Elem: ret}
	case p.Group != nil:
		return c.inferExprType(p.Group)
	case p.List != nil:
		var elem types.Type = types.AnyType{}
		if len(p.List.Elems) > 0 {
			elem = c.inferExprType(p.List.Elems[0])
			for _, e := range p.List.Elems[1:] {
				t := c.inferExprType(e)
				if !equalTypes(elem, t) {
					elem = types.AnyType{}
					break
				}
			}
		}
		return types.ListType{Elem: elem}
	case p.Map != nil:
		var keyT types.Type = types.AnyType{}
		var valT types.Type = types.AnyType{}
		if len(p.Map.Items) > 0 {
			keyT = c.inferExprType(p.Map.Items[0].Key)
			valT = c.inferExprType(p.Map.Items[0].Value)
			for _, it := range p.Map.Items[1:] {
				kt := c.inferExprType(it.Key)
				vt := c.inferExprType(it.Value)
				if !equalTypes(keyT, kt) {
					keyT = types.AnyType{}
				}
				if !equalTypes(valT, vt) {
					valT = types.AnyType{}
				}
			}
		}
		return types.MapType{Key: keyT, Value: valT}
	}
	return types.AnyType{}
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
			if c.env != nil {
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

func equalTypes(a, b types.Type) bool {
	if _, ok := a.(types.AnyType); ok {
		return true
	}
	if _, ok := b.(types.AnyType); ok {
		return true
	}
	if isInt(a) && isInt(b) {
		return true
	}
	if la, ok := a.(types.ListType); ok {
		if lb, ok := b.(types.ListType); ok {
			return equalTypes(la.Elem, lb.Elem)
		}
	}
	if ma, ok := a.(types.MapType); ok {
		if mb, ok := b.(types.MapType); ok {
			return equalTypes(ma.Key, mb.Key) && equalTypes(ma.Value, mb.Value)
		}
	}
	return reflect.DeepEqual(a, b)
}

func isInt(t types.Type) bool {
	switch t.(type) {
	case types.IntType, types.Int64Type:
		return true
	default:
		return false
	}
}

func isFloat(t types.Type) bool {
	_, ok := t.(types.FloatType)
	return ok
}

func isString(t types.Type) bool {
	_, ok := t.(types.StringType)
	return ok
}
func (c *Compiler) exprIsString(e *parser.Expr) bool {
	_, ok := c.inferExprType(e).(types.StringType)
	return ok
}

func (c *Compiler) exprIsInt(e *parser.Expr) bool {
	return isInt(c.inferExprType(e))
}

func (c *Compiler) unaryIsInt(u *parser.Unary) bool {
	return isInt(c.inferUnaryType(u))
}

func (c *Compiler) postfixIsInt(p *parser.PostfixExpr) bool {
	return isInt(c.inferPostfixType(p))
}

func (c *Compiler) primaryIsInt(p *parser.Primary) bool {
	return isInt(c.inferPrimaryType(p))
}

func (c *Compiler) isMapExpr(p *parser.Primary) bool {
	_, ok := c.inferPrimaryType(p).(types.MapType)
	return ok
}

func (c *Compiler) isStringExpr(p *parser.Primary) bool {
	_, ok := c.inferPrimaryType(p).(types.StringType)
	return ok
}

func (c *Compiler) exprIsList(e *parser.Expr) bool {
	_, ok := c.inferExprType(e).(types.ListType)
	return ok
}

func (c *Compiler) postfixIsMap(p *parser.PostfixExpr) bool {
	_, ok := c.inferPostfixType(p).(types.MapType)
	return ok
}
