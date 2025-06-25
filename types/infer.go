package types

import (
	"reflect"
	"strings"

	"mochi/parser"
)

// ResolveTypeRef exposes resolveTypeRef for external packages.
func ResolveTypeRef(t *parser.TypeRef, env *Env) Type {
	return resolveTypeRef(t, env)
}

// ExprType returns the static type of expression e using env.
func ExprType(e *parser.Expr, env *Env) Type {
	if e == nil {
		return AnyType{}
	}
	return binaryType(env, e.Binary)
}

// ExprTypeHint infers the type of e using a hint for list literals.
func ExprTypeHint(e *parser.Expr, hint Type, env *Env) Type {
	if e == nil {
		return AnyType{}
	}
	if lt, ok := hint.(ListType); ok {
		if e.Binary != nil && len(e.Binary.Right) == 0 {
			if ll := e.Binary.Left.Value.Target.List; ll != nil {
				if len(ll.Elems) == 0 {
					return ListType{Elem: lt.Elem}
				}
				elem := ExprTypeHint(ll.Elems[0], lt.Elem, env)
				for _, el := range ll.Elems[1:] {
					t := ExprTypeHint(el, lt.Elem, env)
					if !equalTypes(elem, t) {
						elem = AnyType{}
						break
					}
				}
				return ListType{Elem: elem}
			}
		}
	}
	return ExprType(e, env)
}

func binaryType(env *Env, b *parser.BinaryExpr) Type {
	if b == nil {
		return AnyType{}
	}
	t := unaryType(env, b.Left)
	for _, op := range b.Right {
		rt := postfixType(env, op.Right)
		switch op.Op {
		case "+", "-", "*", "/", "%":
			if isInt64(t) {
				if isInt64(rt) || isInt(rt) {
					t = Int64Type{}
					continue
				}
			}
			if _, ok := t.(IntType); ok {
				if _, ok := rt.(IntType); ok {
					t = IntType{}
					continue
				}
			}
			if _, ok := t.(FloatType); ok {
				if _, ok := rt.(FloatType); ok {
					t = FloatType{}
					continue
				}
			}
			if op.Op == "+" {
				if llist, ok := t.(ListType); ok {
					if rlist, ok := rt.(ListType); ok && equalTypes(llist.Elem, rlist.Elem) {
						t = llist
						continue
					}
				}
				if _, ok := t.(StringType); ok {
					if _, ok := rt.(StringType); ok {
						t = StringType{}
						continue
					}
				}
			}
			t = AnyType{}
		case "==", "!=", "<", "<=", ">", ">=":
			t = BoolType{}
		case "&&", "||":
			if isBool(t) && isBool(rt) {
				t = BoolType{}
			} else {
				t = AnyType{}
			}
		case "in":
			switch rt.(type) {
			case MapType, ListType, StringType:
				t = BoolType{}
			default:
				t = AnyType{}
			}
		default:
			t = AnyType{}
		}
	}
	return t
}

func unaryType(env *Env, u *parser.Unary) Type {
	if u == nil {
		return AnyType{}
	}
	return postfixType(env, u.Value)
}

func postfixType(env *Env, p *parser.PostfixExpr) Type {
	if p == nil {
		return AnyType{}
	}
	t := primaryType(env, p.Target)
	for _, op := range p.Ops {
		if op.Index != nil && op.Index.Colon == nil {
			switch tt := t.(type) {
			case ListType:
				t = tt.Elem
			case MapType:
				t = tt.Value
			case StringType:
				t = StringType{}
			default:
				t = AnyType{}
			}
		} else if op.Index != nil {
			switch tt := t.(type) {
			case ListType:
				t = tt
			case StringType:
				t = StringType{}
			default:
				t = AnyType{}
			}
		} else if op.Call != nil {
			if sel := p.Target.Selector; sel != nil && len(sel.Tail) == 1 {
				switch sel.Tail[0] {
				case "keys":
					t = ListType{Elem: AnyType{}}
					continue
				}
			}
			if ft, ok := t.(FuncType); ok {
				t = ft.Return
			} else {
				t = AnyType{}
			}
		} else if op.Cast != nil {
			t = ResolveTypeRef(op.Cast.Type, env)
		}
	}
	return t
}

func primaryType(env *Env, p *parser.Primary) Type {
	if p == nil {
		return AnyType{}
	}
	switch {
	case p.Lit != nil:
		switch {
		case p.Lit.Int != nil:
			return IntType{}
		case p.Lit.Float != nil:
			return FloatType{}
		case p.Lit.Str != nil:
			return StringType{}
		case p.Lit.Bool != nil:
			return BoolType{}
		}
	case p.Selector != nil:
		if env != nil {
			if len(p.Selector.Tail) > 0 {
				full := p.Selector.Root + "." + strings.Join(p.Selector.Tail, ".")
				if t, err := env.GetVar(full); err == nil {
					return t
				}
			}
			if t, err := env.GetVar(p.Selector.Root); err == nil {
				if len(p.Selector.Tail) == 0 {
					return t
				}
				if st, ok := t.(StructType); ok {
					cur := st
					for idx, field := range p.Selector.Tail {
						ft, ok := cur.Fields[field]
						if !ok {
							return AnyType{}
						}
						if idx == len(p.Selector.Tail)-1 {
							return ft
						}
						if next, ok := ft.(StructType); ok {
							cur = next
						} else {
							return AnyType{}
						}
					}
				}
				if ut, ok := t.(UnionType); ok {
					if ft, ok := unionFieldPathType(ut, p.Selector.Tail); ok {
						return ft
					}
				}
			}
		}
		return AnyType{}
	case p.Struct != nil:
		if env != nil {
			if st, ok := env.GetStruct(p.Struct.Name); ok {
				return st
			}
		}
		return AnyType{}
	case p.FunExpr != nil:
		params := make([]Type, len(p.FunExpr.Params))
		for i, par := range p.FunExpr.Params {
			if par.Type != nil {
				params[i] = ResolveTypeRef(par.Type, env)
			} else {
				params[i] = AnyType{}
			}
		}
		var ret Type = VoidType{}
		if p.FunExpr.Return != nil {
			ret = ResolveTypeRef(p.FunExpr.Return, env)
		} else if p.FunExpr.ExprBody != nil {
			ret = ExprType(p.FunExpr.ExprBody, env)
		} else {
			ret = AnyType{}
		}
		return FuncType{Params: params, Return: ret}
	case p.Generate != nil:
		switch p.Generate.Target {
		case "text":
			return StringType{}
		case "embedding":
			return ListType{Elem: FloatType{}}
		default:
			if env != nil {
				if st, ok := env.GetStruct(p.Generate.Target); ok {
					return st
				}
			}
			return AnyType{}
		}
	case p.Call != nil:
		switch p.Call.Func {
		case "len":
			return IntType{}
		case "str":
			return StringType{}
		case "count":
			return IntType{}
		case "avg":
			return FloatType{}
		case "now":
			return Int64Type{}
		case "keys":
			return ListType{Elem: AnyType{}}
		default:
			if env != nil {
				if t, err := env.GetVar(p.Call.Func); err == nil {
					if ft, ok := t.(FuncType); ok {
						return ft.Return
					}
				}
			}
			return AnyType{}
		}
	case p.Group != nil:
		return ExprType(p.Group, env)
	case p.List != nil:
		var elemType Type = AnyType{}
		if len(p.List.Elems) > 0 {
			elemType = ExprType(p.List.Elems[0], env)
			for _, e := range p.List.Elems[1:] {
				t := ExprType(e, env)
				if !equalTypes(elemType, t) {
					elemType = AnyType{}
					break
				}
			}
		}
		return ListType{Elem: elemType}
	case p.Load != nil:
		var elem Type = MapType{Key: StringType{}, Value: AnyType{}}
		if p.Load.Type != nil {
			elem = ResolveTypeRef(p.Load.Type, env)
			if st, ok := env.GetStruct(*p.Load.Type.Simple); elem == (AnyType{}) && ok {
				elem = st
			}
		}
		return ListType{Elem: elem}
	case p.Save != nil:
		return VoidType{}
	case p.Query != nil:
		srcType := ExprType(p.Query.Source, env)
		var elemType Type = AnyType{}
		if lt, ok := srcType.(ListType); ok {
			elemType = lt.Elem
		}
		child := NewEnv(env)
		child.SetVar(p.Query.Var, elemType, true)
		for _, f := range p.Query.Froms {
			ft := ExprType(f.Src, env)
			var fe Type = AnyType{}
			if lt, ok := ft.(ListType); ok {
				fe = lt.Elem
			}
			child.SetVar(f.Var, fe, true)
		}
		for _, j := range p.Query.Joins {
			jt := ExprType(j.Src, env)
			var je Type = AnyType{}
			if lt, ok := jt.(ListType); ok {
				je = lt.Elem
			}
			child.SetVar(j.Var, je, true)
		}
		orig := env
		env = child
		elem := ExprType(p.Query.Select, env)
		env = orig
		return ListType{Elem: elem}
	case p.Map != nil:
		var keyType Type = AnyType{}
		var valType Type = AnyType{}
		if len(p.Map.Items) > 0 {
			if _, ok := simpleStringKey(p.Map.Items[0].Key); ok {
				keyType = StringType{}
			} else {
				keyType = ExprType(p.Map.Items[0].Key, env)
			}
			valType = ExprType(p.Map.Items[0].Value, env)
			for _, it := range p.Map.Items[1:] {
				var kt Type
				if _, ok := simpleStringKey(it.Key); ok {
					kt = StringType{}
				} else {
					kt = ExprType(it.Key, env)
				}
				vt := ExprType(it.Value, env)
				if !equalTypes(keyType, kt) {
					keyType = AnyType{}
				}
				if !equalTypes(valType, vt) {
					valType = AnyType{}
				}
			}
		}
		return MapType{Key: keyType, Value: valType}
	case p.Match != nil:
		var rType Type
		for _, cs := range p.Match.Cases {
			t := ExprType(cs.Result, env)
			if rType == nil {
				rType = t
			} else if !equalTypes(rType, t) {
				rType = AnyType{}
			}
		}
		if rType == nil {
			rType = AnyType{}
		}
		return rType
	}
	return AnyType{}
}

// ResultType returns the resulting type of applying op to left and right.
func ResultType(op string, left, right Type) Type {
	switch op {
	case "+", "-", "*", "/", "%":
		if _, ok := left.(IntType); ok {
			if _, ok := right.(IntType); ok {
				return IntType{}
			}
		}
		if _, ok := left.(FloatType); ok {
			if _, ok := right.(FloatType); ok {
				return FloatType{}
			}
		}
		if op == "+" {
			if _, ok := left.(StringType); ok {
				if _, ok := right.(StringType); ok {
					return StringType{}
				}
			}
		}
		return AnyType{}
	case "==", "!=", "<", "<=", ">", ">=":
		return BoolType{}
	default:
		return AnyType{}
	}
}

// unionFieldPathType attempts to resolve a field path across all variants of a union.
// It returns the type if every variant has the field path with the same type.
func unionFieldPathType(ut UnionType, tail []string) (Type, bool) {
	var result Type
	for _, variant := range ut.Variants {
		cur := Type(variant)
		for _, field := range tail {
			st, ok := cur.(StructType)
			if !ok {
				return nil, false
			}
			ft, ok := st.Fields[field]
			if !ok {
				return nil, false
			}
			cur = ft
		}
		if result == nil {
			result = cur
		} else if !equalTypes(result, cur) {
			return nil, false
		}
	}
	if result == nil {
		return nil, false
	}
	return result, true
}

func equalTypes(a, b Type) bool {
	if _, ok := a.(AnyType); ok {
		return true
	}
	if _, ok := b.(AnyType); ok {
		return true
	}
	if la, ok := a.(ListType); ok {
		if lb, ok := b.(ListType); ok {
			return equalTypes(la.Elem, lb.Elem)
		}
	}
	if ma, ok := a.(MapType); ok {
		if mb, ok := b.(MapType); ok {
			return equalTypes(ma.Key, mb.Key) && equalTypes(ma.Value, mb.Value)
		}
	}
	if ua, ok := a.(UnionType); ok {
		if sb, ok := b.(StructType); ok {
			if _, ok := ua.Variants[sb.Name]; ok {
				return true
			}
		}
	}
	if ub, ok := b.(UnionType); ok {
		if sa, ok := a.(StructType); ok {
			if _, ok := ub.Variants[sa.Name]; ok {
				return true
			}
		}
	}
	if isInt64(a) && (isInt64(b) || isInt(b)) {
		return true
	}
	if isInt64(b) && (isInt64(a) || isInt(a)) {
		return true
	}
	if isInt(a) && isInt(b) {
		return true
	}
	return reflect.DeepEqual(a, b)
}

func isInt64(t Type) bool  { _, ok := t.(Int64Type); return ok }
func isInt(t Type) bool    { _, ok := t.(IntType); return ok }
func isFloat(t Type) bool  { _, ok := t.(FloatType); return ok }
func isBool(t Type) bool   { _, ok := t.(BoolType); return ok }
func isString(t Type) bool { _, ok := t.(StringType); return ok }
func isList(t Type) bool   { _, ok := t.(ListType); return ok }

func simpleStringKey(e *parser.Expr) (string, bool) {
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
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	return "", false
}
