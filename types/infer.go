package types

import (
	"reflect"
	"strings"

	"mochi/ast"
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
	return inferBinaryType(env, e.Binary)
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
	if mt, ok := hint.(MapType); ok {
		if e.Binary != nil && len(e.Binary.Right) == 0 {
			if ml := e.Binary.Left.Value.Target.Map; ml != nil {
				if len(ml.Items) == 0 {
					return MapType{Key: mt.Key, Value: mt.Value}
				}
				key := ExprTypeHint(ml.Items[0].Key, mt.Key, env)
				val := ExprTypeHint(ml.Items[0].Value, mt.Value, env)
				for _, it := range ml.Items[1:] {
					kt := ExprTypeHint(it.Key, mt.Key, env)
					vt := ExprTypeHint(it.Value, mt.Value, env)
					if !equalTypes(key, kt) {
						key = AnyType{}
					}
					if !equalTypes(val, vt) {
						val = AnyType{}
					}
				}
				return MapType{Key: key, Value: val}
			}
		}
	}
	return ExprType(e, env)
}

func inferBinaryType(env *Env, b *parser.BinaryExpr) Type {
	if b == nil {
		return AnyType{}
	}

	operands := []Type{inferUnaryType(env, b.Left)}
	ops := []string{}

	for _, part := range b.Right {
		op := part.Op
		if op == "union" && part.All {
			op = "union_all"
		}
		ops = append(ops, op)
		operands = append(operands, inferPostfixType(env, part.Right))
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}

	contains := func(list []string, op string) bool {
		for _, s := range list {
			if s == op {
				return true
			}
		}
		return false
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				left := operands[i]
				right := operands[i+1]
				var res Type
				switch ops[i] {
				case "+", "-", "*", "/", "%":
					if ops[i] == "/" && isInt(left) && isInt(right) {
						res = FloatType{}
						break
					}
					if (isInt64(left) && (isInt64(right) || isInt(right))) ||
						(isInt64(right) && isInt(left)) {
						res = Int64Type{}
						break
					}
					if (isFloat(left) || isFloat(right)) &&
						(isInt(left) || isFloat(left) || isInt64(left)) &&
						(isInt(right) || isFloat(right) || isInt64(right)) {
						res = FloatType{}
						break
					}
					if isInt(left) && isInt(right) {
						res = IntType{}
						break
					}
					if ops[i] == "+" {
						if ll, ok := left.(ListType); ok {
							if rl, ok := right.(ListType); ok && equalTypes(ll.Elem, rl.Elem) {
								res = ll
								break
							}
							res = ListType{Elem: AnyType{}}
							break
						}
						if _, ok := left.(StringType); ok {
							if _, ok := right.(StringType); ok {
								res = StringType{}
								break
							}
						}
					}
					res = AnyType{}
				case "==", "!=", "<", "<=", ">", ">=":
					res = BoolType{}
				case "&&", "||":
					if isBool(left) && isBool(right) {
						res = BoolType{}
					} else {
						res = AnyType{}
					}
				case "in":
					switch right.(type) {
					case MapType, ListType, StringType:
						res = BoolType{}
					default:
						res = AnyType{}
					}
				case "union", "union_all", "except", "intersect":
					if ll, ok := left.(ListType); ok {
						if rl, ok := right.(ListType); ok {
							if equalTypes(ll.Elem, rl.Elem) {
								res = ll
							} else {
								res = ListType{Elem: AnyType{}}
							}
						} else {
							res = ListType{Elem: AnyType{}}
						}
					} else {
						res = AnyType{}
					}
				default:
					res = AnyType{}
				}
				operands[i] = res
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return AnyType{}
	}
	return operands[0]
}

func inferUnaryType(env *Env, u *parser.Unary) Type {
	if u == nil {
		return AnyType{}
	}
	return inferPostfixType(env, u.Value)
}

func inferPostfixType(env *Env, p *parser.PostfixExpr) Type {
	if p == nil {
		return AnyType{}
	}
	t := inferPrimaryType(env, p.Target)
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
					if mt, ok := t.(MapType); ok {
						t = ListType{Elem: mt.Key}
					} else {
						t = ListType{Elem: AnyType{}}
					}
					continue
				case "values":
					if mt, ok := t.(MapType); ok {
						t = ListType{Elem: mt.Value}
					} else {
						t = ListType{Elem: AnyType{}}
					}
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

func inferPrimaryType(env *Env, p *parser.Primary) Type {
	if p == nil {
		return AnyType{}
	}
	switch {
	case p.Lit != nil:
		switch {
		case p.Lit.Int != nil:
			return IntType{}
		case p.Lit.BigInt != nil:
			return Int64Type{}
		case p.Lit.Float != nil:
			return FloatType{}
		case p.Lit.Str != nil:
			return StringType{}
		case p.Lit.Bool != nil:
			return BoolType{}
		case p.Lit.Null:
			return AnyType{}
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
				cur := t
				for i, field := range p.Selector.Tail {
					last := i == len(p.Selector.Tail)-1
					switch tt := cur.(type) {
					case StructType:
						ft, ok := tt.Fields[field]
						if !ok {
							return AnyType{}
						}
						cur = ft
					case MapType:
						cur = tt.Value
					case ListType:
						if field == "contains" {
							cur = FuncType{Params: []Type{tt.Elem}, Return: BoolType{}}
						} else {
							cur = tt.Elem
						}
					case GroupType:
						if field == "key" {
							cur = tt.Key
						} else if field == "items" {
							cur = ListType{Elem: tt.Elem}
						} else {
							cur = AnyType{}
						}
					case StringType:
						if field == "contains" {
							cur = FuncType{Params: []Type{StringType{}}, Return: BoolType{}}
						} else {
							return AnyType{}
						}
					default:
						return AnyType{}
					}
					if last {
						return cur
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
		case "str", "input":
			return StringType{}
		case "count":
			return IntType{}
		case "avg":
			return FloatType{}
		case "sum":
			if len(p.Call.Args) == 1 {
				t := ExprType(p.Call.Args[0], env)
				switch tt := t.(type) {
				case ListType:
					if _, ok := tt.Elem.(FloatType); ok {
						return FloatType{}
					}
					return IntType{}
				case GroupType:
					if _, ok := tt.Elem.(FloatType); ok {
						return FloatType{}
					}
					return IntType{}
				}
			}
			return IntType{}
		case "min", "max":
			if len(p.Call.Args) == 1 {
				t := ExprType(p.Call.Args[0], env)
				switch tt := t.(type) {
				case ListType:
					return tt.Elem
				case GroupType:
					return tt.Elem
				}
			}
			return AnyType{}
		case "first":
			if len(p.Call.Args) == 1 {
				t := ExprType(p.Call.Args[0], env)
				if lt, ok := t.(ListType); ok {
					return lt.Elem
				}
			}
			return AnyType{}
		case "reduce":
			if len(p.Call.Args) == 3 {
				return ExprType(p.Call.Args[2], env)
			}
			return AnyType{}
		case "keys":
			if len(p.Call.Args) == 1 {
				argType := ExprType(p.Call.Args[0], env)
				if mt, ok := argType.(MapType); ok {
					return ListType{Elem: mt.Key}
				}
			}
			return ListType{Elem: AnyType{}}
		case "values":
			if len(p.Call.Args) == 1 {
				argType := ExprType(p.Call.Args[0], env)
				if mt, ok := argType.(MapType); ok {
					return ListType{Elem: mt.Value}
				}
			}
			return ListType{Elem: AnyType{}}
		case "now":
			return Int64Type{}
		case "to_json":
			return StringType{}
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
	case p.If != nil:
		return inferIfExprType(p.If, env)
	case p.Group != nil:
		return ExprType(p.Group, env)
	case p.List != nil:
		var elemType Type = AnyType{}
		if len(p.List.Elems) > 0 {
			first := p.List.Elems[0]
			// Attempt to infer a struct type when all elements are map
			// literals with matching keys and value types.
			if ml := first.Binary.Left.Value.Target.Map; ml != nil && len(first.Binary.Right) == 0 {
				fields := map[string]Type{}
				order := make([]string, len(ml.Items))
				valid := true
				for i, it := range ml.Items {
					key, ok := SimpleStringKey(it.Key)
					if !ok {
						valid = false
						break
					}
					order[i] = key
					fields[key] = ExprType(it.Value, env)
				}
				if valid {
					for _, e := range p.List.Elems[1:] {
						if e.Binary == nil || len(e.Binary.Right) != 0 {
							valid = false
							break
						}
						ml2 := e.Binary.Left.Value.Target.Map
						if ml2 == nil || len(ml2.Items) != len(order) {
							valid = false
							break
						}
						for i, it := range ml2.Items {
							key, ok := SimpleStringKey(it.Key)
							if !ok || key != order[i] {
								valid = false
								break
							}
							vt := ExprType(it.Value, env)
							ft := fields[key]
							if !equalTypes(ft, vt) {
								fields[key] = AnyType{}
							}
						}
						if !valid {
							break
						}
					}
				}
				if valid {
					elemType = StructType{Fields: fields, Order: order}
				}
			}
			if _, ok := elemType.(AnyType); ok {
				elemType = ExprType(first, env)
				for _, e := range p.List.Elems[1:] {
					t := ExprType(e, env)
					if !equalTypes(elemType, t) {
						elemType = AnyType{}
						break
					}
				}
			}
		}
		return ListType{Elem: elemType}
	case p.Load != nil:
		var elem Type = MapType{Key: StringType{}, Value: AnyType{}}
		if p.Load.Type != nil {
			elem = ResolveTypeRef(p.Load.Type, env)
			if p.Load.Type.Simple != nil {
				if st, ok := env.GetStruct(*p.Load.Type.Simple); elem == (AnyType{}) && ok {
					elem = st
				}
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
		if p.Query.Group != nil {
			keyT := ExprType(p.Query.Group.Exprs[0], child)
			genv := NewEnv(child)
			genv.SetVar(p.Query.Group.Name, GroupType{Key: keyT, Elem: elemType}, true)
			env = genv
		} else {
			env = child
		}
		elem := ExprType(p.Query.Select, env)
		env = orig
		return ListType{Elem: elem}
	case p.Map != nil:
		if len(p.Map.Items) > 0 {
			st := StructType{Fields: make(map[string]Type), Order: make([]string, len(p.Map.Items))}
			allId := true
			for i, it := range p.Map.Items {
				key, ok := identName(it.Key)
				if !ok {
					allId = false
					break
				}
				st.Fields[key] = ExprType(it.Value, env)
				st.Order[i] = key
			}
			if allId {
				return st
			}
		}

		var keyType Type = AnyType{}
		var valType Type = AnyType{}
		if len(p.Map.Items) > 0 {
			if _, ok := SimpleStringKey(p.Map.Items[0].Key); ok {
				keyType = StringType{}
			} else {
				keyType = ExprType(p.Map.Items[0].Key, env)
			}
			valType = ExprType(p.Map.Items[0].Value, env)
			for _, it := range p.Map.Items[1:] {
				var kt Type
				if _, ok := SimpleStringKey(it.Key); ok {
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

// IfExprType returns the static type of an if-expression.
func IfExprType(ie *parser.IfExpr, env *Env) Type {
	return inferIfExprType(ie, env)
}

func inferIfExprType(ie *parser.IfExpr, env *Env) Type {
	if ie == nil {
		return AnyType{}
	}
	thenT := ExprType(ie.Then, env)
	var elseT Type = AnyType{}
	if ie.ElseIf != nil {
		elseT = inferIfExprType(ie.ElseIf, env)
	} else if ie.Else != nil {
		elseT = ExprType(ie.Else, env)
	}
	if equalTypes(thenT, elseT) {
		return thenT
	}
	if isInt64(thenT) && (isInt64(elseT) || isInt(elseT)) {
		return Int64Type{}
	}
	if isInt64(elseT) && (isInt64(thenT) || isInt(thenT)) {
		return Int64Type{}
	}
	if isInt(thenT) && isInt(elseT) {
		return IntType{}
	}
	if isFloat(thenT) && isFloat(elseT) {
		return FloatType{}
	}
	if isString(thenT) && isString(elseT) {
		return StringType{}
	}
	if lt1, ok1 := thenT.(ListType); ok1 {
		if lt2, ok2 := elseT.(ListType); ok2 && equalTypes(lt1.Elem, lt2.Elem) {
			return lt1
		}
	}
	if isBool(thenT) && isBool(elseT) {
		return BoolType{}
	}
	return AnyType{}
}

// ResultType returns the resulting type of applying op to left and right.
func ResultType(op string, left, right Type) Type {
	switch op {
	case "+", "-", "*", "/", "%":
		if isNumeric(left) && isNumeric(right) {
			if isFloat(left) || isFloat(right) {
				return FloatType{}
			}
			return IntType{}
		}
		if op == "+" {
			if isString(left) || isString(right) {
				return StringType{}
			}
			if ll, ok := left.(ListType); ok {
				if rl, ok := right.(ListType); ok {
					if equalTypes(ll.Elem, rl.Elem) {
						return ll
					}
					return ListType{Elem: AnyType{}}
				}
				return ListType{Elem: AnyType{}}
			}
		}
		return AnyType{}
	case "==", "!=", "<", "<=", ">", ">=", "in", "&&", "||":
		return BoolType{}
	case "union", "union_all", "except", "intersect":
		if llist, ok := left.(ListType); ok {
			if rlist, ok := right.(ListType); ok {
				if equalTypes(llist.Elem, rlist.Elem) {
					return llist
				}
				return ListType{Elem: AnyType{}}
			}
			return ListType{Elem: AnyType{}}
		}
		return AnyType{}
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
		_, ok2 := b.(AnyType)
		return ok2
	}
	if _, ok := b.(AnyType); ok {
		_, ok2 := a.(AnyType)
		return ok2
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

// SimpleStringKey returns the string value of e if it is a simple
// string key expression like a bare identifier or string literal.
func SimpleStringKey(e *parser.Expr) (string, bool) {
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

// NodeType returns the static type of the given AST node using env when
// available. It handles only a subset of node kinds sufficient for the COBOL
// backend.
func NodeType(n *ast.Node, env *Env) Type {
	if n == nil {
		return AnyType{}
	}
	switch n.Kind {
	case "int":
		return IntType{}
	case "float":
		return FloatType{}
	case "string":
		return StringType{}
	case "bool":
		return BoolType{}
	case "list":
		var elem Type = AnyType{}
		if len(n.Children) > 0 {
			elem = NodeType(n.Children[0], env)
		}
		return ListType{Elem: elem}
	case "fetch":
		return StringType{}
	case "selector":
		if env != nil {
			if t, err := env.GetVar(n.Value.(string)); err == nil {
				return t
			}
		}
		return AnyType{}
	case "call":
		if env != nil {
			if t, err := env.GetVar(n.Value.(string)); err == nil {
				if ft, ok := t.(FuncType); ok {
					return ft.Return
				}
			}
		}
		return AnyType{}
	case "binary":
		lt := NodeType(n.Children[0], env)
		rt := NodeType(n.Children[1], env)
		op := n.Value.(string)
		switch op {
		case "+", "-", "*", "/", "%":
			if isFloat(lt) || isFloat(rt) {
				return FloatType{}
			}
			if isInt(lt) && isInt(rt) {
				return IntType{}
			}
			if op == "+" && isString(lt) && isString(rt) {
				return StringType{}
			}
			if op == "+" && isList(lt) && isList(rt) {
				llt := lt.(ListType)
				rlt := rt.(ListType)
				if equalTypes(llt.Elem, rlt.Elem) {
					return llt
				}
			}
			return AnyType{}
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			return BoolType{}
		}
	case "group":
		return NodeType(n.Children[0], env)
	case "unary":
		return NodeType(n.Children[0], env)
	case "index":
		base := NodeType(n.Children[0], env)
		if lt, ok := base.(ListType); ok {
			if len(n.Children) > 1 && (n.Children[1].Kind == "start" || n.Children[1].Kind == "end" || len(n.Children) > 2) {
				return lt
			}
			return lt.Elem
		}
		if isString(base) {
			return StringType{}
		}
	case "if_expr":
		thenT := NodeType(n.Children[1], env)
		if len(n.Children) > 2 {
			elseT := NodeType(n.Children[2], env)
			if equalTypes(thenT, elseT) {
				return thenT
			}
			return AnyType{}
		}
		return thenT
	case "match":
		var t Type
		for _, cs := range n.Children[1:] {
			r := NodeType(cs.Children[1], env)
			if t == nil {
				t = r
				continue
			}
			if !equalTypes(t, r) {
				t = AnyType{}
			}
		}
		if t == nil {
			return AnyType{}
		}
		return t
	}
	return AnyType{}
}

// IsString reports whether n has static type string.
func IsString(n *ast.Node, env *Env) bool {
	_, ok := NodeType(n, env).(StringType)
	return ok
}

// IsFloat reports whether n has static type float.
func IsFloat(n *ast.Node, env *Env) bool {
	_, ok := NodeType(n, env).(FloatType)
	return ok
}

// IsList reports whether n has static list type.
func IsList(n *ast.Node, env *Env) bool {
	_, ok := NodeType(n, env).(ListType)
	return ok
}

// TypeOfExprBasic performs lightweight type inference for dynamic backends.
// It distinguishes literals, lists, maps and known variable references.
func TypeOfExprBasic(e *parser.Expr, env *Env) Type {
	if e == nil {
		return AnyType{}
	}
	if id, ok := identName(e); ok && env != nil {
		if t, err := env.GetVar(id); err == nil {
			return t
		}
	}
	return TypeOfPostfixBasic(e.Binary.Left, env)
}

// TypeOfPostfixBasic infers the type of a unary expression using simple rules.
func TypeOfPostfixBasic(u *parser.Unary, env *Env) Type {
	if u == nil {
		return AnyType{}
	}
	t := TypeOfPrimaryBasic(u.Value.Target, env)
	for _, op := range u.Value.Ops {
		if op.Cast != nil {
			t = ResolveTypeRef(op.Cast.Type, env)
		}
	}
	return t
}

// TypeOfPrimaryBasic infers the type of a primary expression using simple rules.
func TypeOfPrimaryBasic(p *parser.Primary, env *Env) Type {
	if p == nil {
		return AnyType{}
	}
	switch {
	case p.Lit != nil:
		switch {
		case p.Lit.Str != nil:
			return StringType{}
		case p.Lit.Int != nil:
			return IntType{}
		case p.Lit.BigInt != nil:
			return Int64Type{}
		case p.Lit.Float != nil:
			return FloatType{}
		case p.Lit.Bool != nil:
			return BoolType{}
		case p.Lit.Null:
			return AnyType{}
		}
	case p.List != nil:
		return ListType{Elem: AnyType{}}
	case p.Map != nil:
		return MapType{Key: AnyType{}, Value: AnyType{}}
	case p.Selector != nil:
		if env != nil {
			if len(p.Selector.Tail) > 0 {
				full := p.Selector.Root + "." + strings.Join(p.Selector.Tail, ".")
				if t, err := env.GetVar(full); err == nil {
					return t
				}
			}
			if t, err := env.GetVar(p.Selector.Root); err == nil {
				return t
			}
		}
	}
	return AnyType{}
}

// IsListType reports whether t is a list type.
func IsListType(t Type) bool { return isList(t) }

// IsMapType reports whether t is a map type.
func IsMapType(t Type) bool { _, ok := t.(MapType); return ok }

// IsStringType reports whether t is a string type.
func IsStringType(t Type) bool { return isString(t) }
