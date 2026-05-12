package types

import (
	"strings"


	"mochi/parser"
)

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
		operands = append(operands, inferUnaryType(env, part.Right))
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
					// MEP-10 B2: numeric mix is the lattice join. Replaces
					// the prior order-dependent cascade so `bigint - float`
					// and `float - bigint` produce the same result type.
					if joined, ok := numericJoin(left, right); ok {
						res = joined
						break
					}
					if ops[i] == "+" {
						if ll, ok := left.(ListType); ok {
							if rl, ok := right.(ListType); ok {
								// Match applyBinaryType: unify on elem types
								// so empty-list concatenation ([1] + []) keeps
								// the concrete element type, but a genuine
								// mismatch ([int] + [string]) infers AnyType
								// rather than silently widening to [any]. The
								// authoritative error is raised by
								// applyBinaryType in types/check.go.
								// See MEP 4 §6 problem 6.
								if unify(ll.Elem, rl.Elem, nil) {
									if IsAnyType(ll.Elem) {
										res = rl
									} else {
										res = ll
									}
									break
								}
								res = AnyType{}
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
					// MEP 4 P9: keep inference honest with the check
					// pass. `any` keeps its escape-hatch behaviour;
					// otherwise comparable operands must be equal-kinded
					// or both numeric.
					// Comparisons and equality are always bool-typed; the
					// checker rejects nonsensical operand pairs separately
					// (MEP-5 T-Eq / T-Cmp). The inferrer never widens to any.
					res = BoolType{}
				case "&&", "||":
					// Boolean connectives are always bool-typed; the checker
					// rejects non-bool operands (MEP-5 P16).
					res = BoolType{}
				case "in":
					// `in` is always bool-typed; the checker rejects non-iterable
					// right operands separately.
					res = BoolType{}
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
	t := inferPostfixType(env, u.Value)
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "!":
			// logical negation always yields a boolean result
			t = BoolType{}
		case "-":
			// numeric negation preserves numeric type when known
			switch t.(type) {
			case IntType, Int64Type, BigIntType, FloatType, BigRatType:
				// keep t as is
			default:
				t = AnyType{}
			}
		}
	}
	return t
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
				t = OptionType{Elem: tt.Value}
			case StructType:
				if key, ok := SimpleStringKey(op.Index.Start); ok {
					if ft, ok2 := tt.FieldType(key); ok2 {
						t = ft
					} else {
						t = AnyType{}
					}
					continue
				}
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
			t = resolveTypeRef(op.Cast.Type, env)
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
				tail := p.Selector.Tail
				for i := 0; i < len(tail); i++ {
					field := tail[i]
					last := i == len(tail)-1
					if ot, ok := cur.(OptionType); ok {
						cur = ot.Elem
						i--
						continue
					}
					switch tt := cur.(type) {
					case StructType:
						ft, ok := tt.FieldType(field)
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
				params[i] = resolveTypeRef(par.Type, env)
			} else {
				params[i] = AnyType{}
			}
		}
		var ret Type = UnitType{}
		if p.FunExpr.Return != nil {
			ret = resolveTypeRef(p.FunExpr.Return, env)
		} else if p.FunExpr.ExprBody != nil {
			ret = ExprType(p.FunExpr.ExprBody, env)
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
				var elem Type
				switch tt := t.(type) {
				case ListType:
					elem = tt.Elem
				case GroupType:
					elem = tt.Elem
				}
				if elem != nil && isNumeric(elem) {
					return elem
				}
			}
			return FloatType{}
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
		case "append":
			if len(p.Call.Args) == 2 {
				t := ExprType(p.Call.Args[0], env)
				if lt, ok := t.(ListType); ok {
					elem := lt.Elem
					argT := ExprType(p.Call.Args[1], env)
					if _, ok := elem.(AnyType); ok {
						elem = argT
					} else if !equalTypes(elem, argT) {
						elem = AnyType{}
					}
					return ListType{Elem: elem}
				}
			}
			return ListType{Elem: AnyType{}}
		case "now":
			return Int64Type{}
		case "to_string":
			return StringType{}
		case "to_json":
			return StringType{}
		default:
			if env != nil {
				if t, err := env.GetVar(p.Call.Func); err == nil {
					if ft, ok := t.(FuncType); ok {
						if len(p.Call.Args) < len(ft.Params) {
							remain := append([]Type(nil), ft.Params[len(p.Call.Args):]...)
							return FuncType{Params: remain, Return: ft.Return}
						}
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
				fields := make([]StructField, len(ml.Items))
				valid := true
				for i, it := range ml.Items {
					key, ok := SimpleStringKey(it.Key)
					if !ok {
						valid = false
						break
					}
					fields[i] = StructField{Name: key, Type: ExprType(it.Value, env)}
				}
				if valid {
					for _, e := range p.List.Elems[1:] {
						if e.Binary == nil || len(e.Binary.Right) != 0 {
							valid = false
							break
						}
						ml2 := e.Binary.Left.Value.Target.Map
						if ml2 == nil || len(ml2.Items) != len(fields) {
							valid = false
							break
						}
						for i, it := range ml2.Items {
							key, ok := SimpleStringKey(it.Key)
							if !ok || key != fields[i].Name {
								valid = false
								break
							}
							vt := ExprType(it.Value, env)
							if !equalTypes(fields[i].Type, vt) {
								fields[i].Type = AnyType{}
							}
						}
						if !valid {
							break
						}
					}
				}
				if valid {
					elemType = StructType{Fields: fields}
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
			elem = resolveTypeRef(p.Load.Type, env)
			if p.Load.Type.Simple != nil {
				if st, ok := env.GetStruct(*p.Load.Type.Simple); elem == (AnyType{}) && ok {
					elem = st
				}
			}
		}
		return ListType{Elem: elem}
	case p.Save != nil:
		return UnitType{}
	case p.Query != nil:
		srcType := ExprType(p.Query.Source, env)
		var elemType Type = AnyType{}
		if lt, ok := srcType.(ListType); ok {
			elemType = lt.Elem
		} else if gt, ok := srcType.(GroupType); ok {
			elemType = gt.Elem
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
			fields := make([]StructField, len(p.Map.Items))
			allConst := true
			for i, it := range p.Map.Items {
				key, ok := stringKey(it.Key)
				if !ok {
					allConst = false
					break
				}
				fields[i] = StructField{Name: key, Type: ExprType(it.Value, env)}
			}
			if allConst {
				return StructType{Fields: fields}
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
				if !unify(keyType, kt, nil) {
					keyType = AnyType{}
				}
				if !unify(valType, vt, nil) {
					valType = AnyType{}
				}
			}
		}
		return MapType{Key: keyType, Value: valType}
	case p.Match != nil:
		// MEP-5 §Match [T-Match]: the principal type is the first arm's
		// type. Disagreement is rejected by the checker (T008); the
		// inferrer must not silently widen to AnyType.
		var rType Type
		for _, cs := range p.Match.Cases {
			rType = ExprType(cs.Result, env)
			break
		}
		if rType == nil {
			rType = AnyType{}
		}
		return rType
	}
	return AnyType{}
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
			ft, ok := st.FieldType(field)
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

// equalTypes is the structural-equality relation on Type kinds. Each kind
// in the type-system table (types/check.go:16-140) gets its own case so a
// new kind cannot be silently treated as equal by reflect.DeepEqual.
// Equality is symmetric, so each case only needs to handle the matching
// kind on the other side (with the explicit struct/union carve-out so a
// struct counts as equal to a union it is a variant of).
//
// The int/int64 carve-out is preserved verbatim: until MEP 5 lands the
// final numeric tower decision, the checker needs int and int64 to be
// interchangeable. Tracked separately under MEP 4 §6 problem 1.
//
// See MEP 4 §6 problem 15.
func equalTypes(a, b Type) bool {
	switch at := a.(type) {
	case IntType:
		_, ok := b.(IntType)
		return ok
	case Int64Type:
		_, ok := b.(Int64Type)
		return ok
	case FloatType:
		_, ok := b.(FloatType)
		return ok
	case BigIntType:
		_, ok := b.(BigIntType)
		return ok
	case BigRatType:
		_, ok := b.(BigRatType)
		return ok
	case StringType:
		_, ok := b.(StringType)
		return ok
	case BoolType:
		_, ok := b.(BoolType)
		return ok
	case UnitType:
		_, ok := b.(UnitType)
		return ok
	case AnyType:
		_, ok := b.(AnyType)
		return ok
	case ListType:
		bt, ok := b.(ListType)
		if !ok {
			return false
		}
		return equalTypes(at.Elem, bt.Elem)
	case MapType:
		bt, ok := b.(MapType)
		if !ok {
			return false
		}
		return equalTypes(at.Key, bt.Key) && equalTypes(at.Value, bt.Value)
	case OptionType:
		bt, ok := b.(OptionType)
		if !ok {
			return false
		}
		return equalTypes(at.Elem, bt.Elem)
	case GroupType:
		bt, ok := b.(GroupType)
		if !ok {
			return false
		}
		return equalTypes(at.Key, bt.Key) && equalTypes(at.Elem, bt.Elem)
	case StructType:
		switch bt := b.(type) {
		case StructType:
			return at.Name == bt.Name
		case UnionType:
			_, ok := bt.Variants[at.Name]
			return ok
		}
		return false
	case UnionType:
		switch bt := b.(type) {
		case UnionType:
			return at.Name == bt.Name
		case StructType:
			_, ok := at.Variants[bt.Name]
			return ok
		}
		return false
	case FuncType:
		bt, ok := b.(FuncType)
		if !ok {
			return false
		}
		if len(at.Params) != len(bt.Params) {
			return false
		}
		if (at.Variadic == nil) != (bt.Variadic == nil) {
			return false
		}
		for i := range at.Params {
			if !equalTypes(at.Params[i], bt.Params[i]) {
				return false
			}
		}
		if at.Variadic != nil && !equalTypes(at.Variadic, bt.Variadic) {
			return false
		}
		return equalTypes(at.Return, bt.Return)
	case *TypeVar:
		bt, ok := b.(*TypeVar)
		if !ok {
			return false
		}
		// TypeVar identity is by pointer (MEP 4 §6 problem 14).
		return at == bt
	}
	return false
}

// numericJoin returns the lattice join of two numeric kinds, or
// (nil, false) if either operand is not numeric. The lattice is:
//
//	int <: int64 <: bigint <: bigrat
//	             float <: bigrat
//
// The two integer-side and float-side chains meet at bigrat. This is
// the soundness fix for MEP-10 B2: the prior cascade was order-
// dependent so `bigint - float` and `float - bigint` could produce
// different result types. The lattice is symmetric by construction.
func numericJoin(a, b Type) (Type, bool) {
	ra, ok := numericRank(a)
	if !ok {
		return nil, false
	}
	rb, ok := numericRank(b)
	if !ok {
		return nil, false
	}
	const (
		rInt    = 0
		rInt64  = 1
		rBigInt = 2
		rFloat  = 3
		rBigRat = 4
	)
	// Float absorbs int and int64 (matches runtime coercion) but
	// promotes to bigrat when paired with bigint or bigrat so we do
	// not silently lose precision on a value too large for IEEE 754.
	if ra == rFloat || rb == rFloat {
		if ra == rBigInt || rb == rBigInt || ra == rBigRat || rb == rBigRat {
			return BigRatType{}, true
		}
		return FloatType{}, true
	}
	if ra > rb {
		ra, rb = rb, ra
	}
	switch rb {
	case rInt:
		return IntType{}, true
	case rInt64:
		return Int64Type{}, true
	case rBigInt:
		return BigIntType{}, true
	case rBigRat:
		return BigRatType{}, true
	}
	return nil, false
}

func numericRank(t Type) (int, bool) {
	switch t.(type) {
	case IntType:
		return 0, true
	case Int64Type:
		return 1, true
	case BigIntType:
		return 2, true
	case FloatType:
		return 3, true
	case BigRatType:
		return 4, true
	}
	return 0, false
}

func isInt64(t Type) bool  { _, ok := t.(Int64Type); return ok }
func isInt(t Type) bool    { _, ok := t.(IntType); return ok }
func isBigInt(t Type) bool { _, ok := t.(BigIntType); return ok }
func isBigRat(t Type) bool { _, ok := t.(BigRatType); return ok }
func isFloat(t Type) bool {
	_, ok := t.(FloatType)
	if ok {
		return true
	}
	_, ok2 := t.(BigRatType)
	return ok2
}
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

