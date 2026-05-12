package types

import (
	"fmt"
	"strings"

	"github.com/alecthomas/participle/v2/lexer"
	"mochi/parser"
)

func checkExpr(e *parser.Expr, env *Env) (Type, error) {
	return checkExprWithExpected(e, env, nil)
}

func checkExprWithExpected(e *parser.Expr, env *Env, expected Type) (Type, error) {
	actual, err := checkBinaryExpr(e.Binary, env, expected)
	if err != nil {
		return nil, err
	}
	if expected != nil && !unify(actual, expected, nil) {
		return nil, errTypeMismatch(e.Pos, expected, actual)
	}
	return actual, nil
}
func checkBinaryExpr(b *parser.BinaryExpr, env *Env, expected Type) (Type, error) {
	left, err := checkUnary(b.Left, env, expected)
	if err != nil {
		return nil, err
	}

	type token struct {
		pos lexer.Position
		op  string
	}

	operands := []Type{left}
	operators := []token{}

	for _, part := range b.Right {
		typ, err := checkUnary(part.Right, env, nil)
		if err != nil {
			return nil, err
		}
		operands = append(operands, typ)
		op := part.Op
		if part.Op == "union" && part.All {
			op = "union_all"
		}
		operators = append(operators, token{part.Pos, op})
	}

	for _, level := range [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	} {
		for i := 0; i < len(operators); {
			op := operators[i].op
			if contains(level, op) {
				l := operands[i]
				r := operands[i+1]
				res, err := applyBinaryType(operators[i].pos, op, l, r)
				if err != nil {
					return nil, err
				}
				operands[i] = res
				operands = append(operands[:i+1], operands[i+2:]...)
				operators = append(operators[:i], operators[i+1:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return nil, fmt.Errorf("unexpected state after binary type eval")
	}
	return operands[0], nil
}

func applyBinaryType(pos lexer.Position, op string, left, right Type) (Type, error) {
	if IsAnyType(left) || IsAnyType(right) {
		if op == "+" && (unify(left, StringType{}, nil) || unify(right, StringType{}, nil)) {
			return StringType{}, nil
		}
		switch op {
		case "+", "-", "*", "/", "%":
			return AnyType{}, nil
		case "<", "<=", ">", ">=", "==", "!=", "&&", "||", "in":
			return BoolType{}, nil
		default:
			return AnyType{}, nil
		}
	}
	if op == "+" || op == "union" || op == "union_all" || op == "except" || op == "intersect" {
		if llist, ok := left.(ListType); ok {
			if rlist, ok := right.(ListType); ok {
				if !unify(llist.Elem, rlist.Elem, nil) {
					return nil, errOperatorMismatch(pos, op, left, right)
				}
				return ListType{Elem: llist.Elem}, nil
			}
		}
	}
	switch op {
	case "+", "-", "*", "/", "%":
		switch {
		case isNumeric(left) && isNumeric(right):
			// MEP-10 B2: numeric mix is the lattice join. The same
			// helper drives inferBinaryType so the inferrer and the
			// checker agree on the result type kind. Without this,
			// inferring `int + int = int` while the checker reports
			// `bigint` produced spurious "expected int, got bigint"
			// errors at let-binding sites once MEP-11.2 routed those
			// through Subtype.
			if joined, ok := numericJoin(left, right); ok {
				return joined, nil
			}
			return IntType{}, nil
		case op == "+" && (unify(left, StringType{}, nil) || unify(right, StringType{}, nil)):
			return StringType{}, nil
		default:
			return nil, errOperatorMismatch(pos, op, left, right)
		}
	case "==", "!=", "<", "<=", ">", ">=":
		// MEP 4 P9. The `any` short-circuit at the top of the function
		// already handles the soundness escape hatch. Here, `unify` is
		// used (not `equalTypes`) because empty-list literals carry
		// element type `any`, and comparing `xs == []` is a common
		// surface idiom that must continue to type-check.
		if unify(left, right, nil) {
			return BoolType{}, nil
		}
		if isNumeric(left) && isNumeric(right) {
			return BoolType{}, nil
		}
		return nil, errIncompatibleComparison(pos)
	case "in":
		switch rt := right.(type) {
		case MapType:
			if !unify(left, rt.Key, nil) {
				return nil, errOperatorMismatch(pos, op, left, right)
			}
			return BoolType{}, nil
		case ListType:
			if !unify(left, rt.Elem, nil) {
				return nil, errOperatorMismatch(pos, op, left, right)
			}
			return BoolType{}, nil
		default:
			if !(unify(left, StringType{}, nil) && unify(right, StringType{}, nil)) {
				return nil, errOperatorMismatch(pos, op, left, right)
			}
			return BoolType{}, nil
		}
	case "&&", "||":
		if !(unify(left, BoolType{}, nil) && unify(right, BoolType{}, nil)) {
			return nil, errOperatorMismatch(pos, op, left, right)
		}
		return BoolType{}, nil
	default:
		return nil, errUnsupportedOperator(pos, op)
	}
}

func contains(ops []string, op string) bool {
	for _, o := range ops {
		if o == op {
			return true
		}
	}
	return false
}

func checkUnary(u *parser.Unary, env *Env, expected Type) (Type, error) {
	return checkPostfix(u.Value, env, expected)
}

func checkPostfix(p *parser.PostfixExpr, env *Env, expected Type) (Type, error) {
	exp := expected
	if len(p.Ops) > 0 {
		exp = nil
	}
	typ, err := checkPrimary(p.Target, env, exp)
	if err != nil {
		return nil, err
	}

	for _, op := range p.Ops {
		if idx := op.Index; idx != nil {
			switch t := typ.(type) {
			case ListType:
				if idx.Colon == nil {
					// list[i]
					if idx.Start == nil {
						return nil, errMissingIndex(idx.Pos)
					}
					startType, err := checkExpr(idx.Start, env)
					if err != nil {
						return nil, err
					}
					if !(unify(startType, IntType{}, nil) || unify(startType, Int64Type{}, nil)) {
						return nil, errIndexNotInteger(idx.Pos)
					}
					typ = t.Elem
				} else {
					// list[i:j], list[:j], list[i:], list[:]
					if idx.Start != nil {
						startType, err := checkExpr(idx.Start, env)
						if err != nil {
							return nil, err
						}
						if !(unify(startType, IntType{}, nil) || unify(startType, Int64Type{}, nil)) {
							return nil, errIndexNotInteger(idx.Pos)
						}
					}
					if idx.End != nil {
						endType, err := checkExpr(idx.End, env)
						if err != nil {
							return nil, err
						}
						if !(unify(endType, IntType{}, nil) || unify(endType, Int64Type{}, nil)) {
							return nil, errIndexNotInteger(idx.Pos)
						}
					}
					typ = t // list slice returns same list type
				}

			case MapType:
				if idx.Colon != nil {
					return nil, errInvalidMapSlice(idx.Pos)
				}
				if idx.Start == nil {
					return nil, errMissingIndex(idx.Pos)
				}
				keyType, err := checkExpr(idx.Start, env)
				if err != nil {
					return nil, err
				}
				if !unify(keyType, t.Key, nil) {
					return nil, errIndexTypeMismatch(idx.Pos, t.Key, keyType)
				}
				typ = OptionType{Elem: t.Value}

			case StringType:
				if idx.Start == nil && idx.Colon == nil {
					return nil, errMissingIndex(idx.Pos)
				}
				if idx.Start != nil {
					startType, err := checkExpr(idx.Start, env)
					if err != nil {
						return nil, err
					}
					if !(unify(startType, IntType{}, nil) || unify(startType, Int64Type{}, nil)) {
						return nil, errIndexNotInteger(idx.Pos)
					}
				}
				if idx.End != nil {
					endType, err := checkExpr(idx.End, env)
					if err != nil {
						return nil, err
					}
					if !(unify(endType, IntType{}, nil) || unify(endType, Int64Type{}, nil)) {
						return nil, errIndexNotInteger(idx.Pos)
					}
				}
				typ = StringType{}

			case AnyType:
				typ = AnyType{}
			default:
				if IsAnyType(typ) {
					// Allow dynamic indexing on values of unknown
					// type, propagating the `any` type forward.
					typ = AnyType{}
				} else {
					return nil, errNotIndexable(p.Target.Pos, typ)
				}
			}
		} else if call := op.Call; call != nil {
			ft, ok := typ.(FuncType)
			if !ok {
				if _, isAny := typ.(AnyType); isAny {
					// dynamic call, assume any
					for _, arg := range call.Args {
						if _, err := checkExpr(arg, env); err != nil {
							return nil, err
						}
					}
					typ = AnyType{}
					continue
				}
				return nil, errNotFunction(call.Pos, "")
			}
			argCount := len(call.Args)
			paramCount := len(ft.Params)
			if argCount > paramCount {
				return nil, errTooManyArgs(call.Pos, paramCount, argCount)
			}
			for i := 0; i < argCount; i++ {
				at, err := checkExprWithExpected(call.Args[i], env, ft.Params[i])
				if err != nil {
					return nil, err
				}
				if !unify(at, ft.Params[i], nil) {
					return nil, errArgTypeMismatch(call.Pos, i, ft.Params[i], at)
				}
			}
			if argCount == paramCount {
				typ = ft.Return
			} else {
				typ = curryFuncType(ft.Params[argCount:], ft.Return)
			}
		} else if cast := op.Cast; cast != nil {
			target := resolveTypeRef(cast.Type, env)
			if !castOk(typ, target) {
				return nil, errInvalidCast(op.Pos, typ, target)
			}
			typ = target
		}
	}

	return typ, nil
}

func checkPrimary(p *parser.Primary, env *Env, expected Type) (Type, error) {
	switch {
	case p.Lit != nil:
		switch {
		case p.Lit.Int != nil:
			return IntType{}, nil
		case p.Lit.Float != nil:
			return FloatType{}, nil
		case p.Lit.Str != nil:
			return StringType{}, nil
		case p.Lit.Bool != nil:
			return BoolType{}, nil
		case p.Lit.Null:
			// MEP-10 A2: `null` is the lone value of `option[any]`. It
			// unifies with any other option type via the top-level any
			// case in `unify`, and against `any` itself via the top
			// short-circuit. A non-option target like `int` no longer
			// accepts `null` without an explicit option-typed slot.
			return OptionType{Elem: AnyType{}}, nil
		}

	case p.Selector != nil:
		typ, err := env.GetVar(p.Selector.Root)
		if err != nil {
			return nil, errUnknownVariable(p.Pos, p.Selector.Root)
		}
		prefix := p.Selector.Root
		for _, field := range p.Selector.Tail {
			if t, err := env.GetVar(prefix + "." + field); err == nil {
				typ = t
				prefix = prefix + "." + field
				continue
			}
			// MEP-16 R4 / T058: a field access on an Option<T> binding
			// is rejected unless the binding has been narrowed (e.g.
			// inside `if x != null { ... }`). The narrowed env shadows
			// the binding with T, so the OptionType reaches here only
			// when no guard is in scope.
			if _, opt := typ.(OptionType); opt {
				return nil, errOptionalDeref(p.Pos, prefix)
			}
			switch t := typ.(type) {
			case StructType:
				if ft, ok := t.FieldType(field); ok {
					typ = ft
					continue
				}
				if m, ok := t.Methods[field]; ok {
					typ = m.Type
					continue
				}
				return nil, errUnknownField(p.Pos, field, t)
			case GroupType:
				if field == "key" {
					typ = t.Key
					continue
				}
				if field == "items" {
					typ = ListType{Elem: t.Elem}
					continue
				}
				typ = AnyType{}
				continue
			case StringType:
				if field == "contains" {
					typ = FuncType{Params: []Type{StringType{}}, Return: BoolType{}}
					continue
				}
				if field == "padStart" {
					typ = FuncType{Params: []Type{IntType{}, StringType{}}, Return: StringType{}, Pure: true}
					continue
				}
				return nil, errNotStruct(p.Pos, typ)
			case MapType:
				switch field {
				case "keys":
					typ = FuncType{Params: []Type{}, Return: ListType{Elem: AnyType{}}, Pure: true}
					continue
				case "get":
					typ = FuncType{Params: []Type{t.Key, t.Value}, Return: t.Value, Pure: true}
					continue
				}
				if unify(t.Key, StringType{}, nil) {
					typ = t.Value
					continue
				}
				return nil, errNotStruct(p.Pos, typ)
			case AnyType:
				typ = AnyType{}
				continue
			default:
				return nil, errNotStruct(p.Pos, typ)
			}
		}
		return typ, nil

	case p.Call != nil:
		fnType, err := env.GetVar(p.Call.Func)
		if err != nil {
			return nil, errUnknownFunction(p.Pos, p.Call.Func)
		}

		ft, ok := fnType.(FuncType)
		if !ok {
			return nil, errNotFunction(p.Pos, p.Call.Func)
		}
		// MEP-12.2: instantiate TypeParams with fresh vars so distinct
		// call sites of the same generic function get distinct vars. The
		// returned `callSubst` accumulates per-arg bindings; it is
		// applied to the return type after all args are typed so the
		// caller sees a concrete result (e.g. `min(list<int>) → int`).
		callSubst := Subst{}
		if len(ft.TypeParams) > 0 {
			inst, sub := Instantiate(ft, ft.TypeParams)
			ft = inst.(FuncType)
			for k, v := range sub {
				callSubst[k] = v
			}
		}
		argCount := len(p.Call.Args)
		paramCount := len(ft.Params)

		if exp, ok := builtinArity[p.Call.Func]; ok && ft.Variadic == nil {
			if _, defined := env.GetFunc(p.Call.Func); !defined {
				if argCount != exp {
					return nil, errArgCount(p.Pos, p.Call.Func, exp, argCount)
				}
			}
		}

		if ft.Variadic == nil && argCount > paramCount {
			return nil, errTooManyArgs(p.Pos, paramCount, argCount)
		}

		// Params is the fixed prefix; ft.Variadic (if non-nil) types the
		// trailing varargs sequence (MEP 4 P13).
		fixed := paramCount

		argTypes := make([]Type, argCount)
		for i := 0; i < argCount && i < fixed; i++ {
			expected := callSubst.Apply(ft.Params[i])
			// MEP-12.2/12.3: when the original parameter mentions any
			// TypeVar quantified by this signature, never propagate the
			// hint. Two cases:
			//   (1) the var is still unbound: Subtype has no TypeVar
			//       rule, so the hint-driven check would mishandle it.
			//   (2) the var was bound by an earlier argument: forcing
			//       the bound type as a hint would surface a T008 in
			//       place of the more informative T047 unify-conflict
			//       diagnostic the call-site Unify produces below.
			hint := expected
			if len(FreeTypeVars(ft.Params[i], Subst{})) > 0 {
				hint = nil
			}
			at, err := checkExprWithExpected(p.Call.Args[i], env, hint)
			if err != nil {
				return nil, err
			}
			argTypes[i] = at
			if next, err := Unify(at, expected, callSubst); err == nil {
				callSubst = next
			} else if !unify(at, expected, nil) {
				// MEP-12.3: when a generic call fails to unify, surface
				// T047 with the offending type parameter rather than the
				// generic T007 mismatch. The legacy fallback above keeps
				// non-generic calls on T007.
				if len(ft.TypeParams) > 0 {
					// Use the structural TypeVar name from the
					// pre-substitution param so we can report the
					// original declared name (e.g. `T`) even after
					// Instantiate freshened it to `T#1`.
					if name := structuralTypeVarName(ft.Params[i]); name != "" {
						return nil, errTypeParamConflict(p.Pos, name, expected, at)
					}
				}
				return nil, errArgTypeMismatch(p.Pos, i, expected, at)
			}
			// MEP-10 B3b / B3c: when the argument names live
			// aggregate storage (bare ident, `rows[i]`, `obj.f`)
			// and the parameter slot is the same aggregate kind
			// with a widened element / key / value type, the
			// callee can deposit a value the source's static type
			// rejects through the alias. Require structural
			// equality. The check is narrow: it skips
			// `any`-typed parameters (the design-loose interop
			// position) because no structural write through the
			// parameter is reachable without an explicit `as` cast,
			// which the runtime guard from MEP-11.7 checks at the
			// boundary.
			if src := aliasSourceLabel(p.Call.Args[i]); src != "" {
				paramFinal := callSubst.Apply(ft.Params[i])
				if isAliasableAggregate(at) && isAliasableAggregate(paramFinal) && !equalKinds(at, paramFinal) {
					return nil, errAliasWidensElement(p.Pos, src, at, paramFinal)
				}
			}
		}

		if ft.Variadic != nil {
			variadicType := ft.Variadic
			variadicIsGeneric := len(FreeTypeVars(ft.Variadic, Subst{})) > 0
			for i := fixed; i < argCount; i++ {
				expected := callSubst.Apply(variadicType)
				hint := expected
				if variadicIsGeneric {
					hint = nil
				}
				at, err := checkExprWithExpected(p.Call.Args[i], env, hint)
				if err != nil {
					return nil, err
				}
				argTypes[i] = at
				if next, err := Unify(at, expected, callSubst); err == nil {
					callSubst = next
				} else if !unify(at, expected, nil) {
					if len(ft.TypeParams) > 0 {
						if name := structuralTypeVarName(variadicType); name != "" {
							return nil, errTypeParamConflict(p.Pos, name, expected, at)
						}
					}
					return nil, errArgTypeMismatch(p.Pos, i, expected, at)
				}
			}
			if _, defined := env.GetFunc(p.Call.Func); !defined {
				if err := checkBuiltinCall(p.Call.Func, argTypes, p.Pos); err != nil {
					return nil, err
				}
			}
			ret := callSubst.Apply(ft.Return)
			if p.Call.Func == "keys" && len(argTypes) == 1 {
				if mt, ok := argTypes[0].(MapType); ok {
					ret = ListType{Elem: mt.Key}
				}
			}
			if p.Call.Func == "values" && len(argTypes) == 1 {
				if mt, ok := argTypes[0].(MapType); ok {
					ret = ListType{Elem: mt.Value}
				}
			}
			// MEP-12.4: reverse mirrors its argument's static shape.
			// The declared signature stays loose (any -> any) so the
			// list-or-string discriminator in checkBuiltinCall still
			// applies; the post-process here pins the return type at
			// the call site, so reverse([1,2,3]) types as list<int>
			// rather than any.
			if p.Call.Func == "reverse" && len(argTypes) == 1 {
				switch at := argTypes[0].(type) {
				case ListType:
					ret = at
				case StringType:
					ret = StringType{}
				}
			}
			return ret, nil
		}
		if _, defined := env.GetFunc(p.Call.Func); !defined {
			if err := checkBuiltinCall(p.Call.Func, argTypes, p.Pos); err != nil {
				return nil, err
			}
		}
		ret := callSubst.Apply(ft.Return)
		if p.Call.Func == "keys" && len(argTypes) == 1 {
			if mt, ok := argTypes[0].(MapType); ok {
				ret = ListType{Elem: mt.Key}
			}
		}
		if p.Call.Func == "values" && len(argTypes) == 1 {
			if mt, ok := argTypes[0].(MapType); ok {
				ret = ListType{Elem: mt.Value}
			}
		}
		// MEP-12.4: reverse mirrors its argument's static shape.
		if p.Call.Func == "reverse" && len(argTypes) == 1 {
			switch at := argTypes[0].(type) {
			case ListType:
				ret = at
			case StringType:
				ret = StringType{}
			}
		}
		// MEP-12.3: T048 when the declared generic result still mentions
		// an unbound type parameter after argument unification. Only
		// fire on full applications; partial application still carries
		// the unbound var through the curried result.
		if len(ft.TypeParams) > 0 && argCount == paramCount {
			if name := firstFreeTypeVar(ret, callSubst); name != "" {
				return nil, errTypeParamEscapes(p.Pos, name)
			}
		}
		if argCount == paramCount {
			return ret, nil
		}
		return curryFuncType(ft.Params[argCount:], ret), nil

	case p.Struct != nil:
		st, ok := env.GetStruct(p.Struct.Name)
		if !ok {
			// treat unknown struct literal as map for tool specs
			for _, field := range p.Struct.Fields {
				if _, err := checkExpr(field.Value, env); err != nil {
					return nil, err
				}
			}
			return MapType{Key: StringType{}, Value: AnyType{}}, nil
		}
		provided := make(map[string]bool, len(p.Struct.Fields))
		for _, field := range p.Struct.Fields {
			ft, ok := st.FieldType(field.Name)
			if !ok {
				return nil, errUnknownField(p.Pos, field.Name, st)
			}
			valT, err := checkExpr(field.Value, env)
			if err != nil {
				return nil, err
			}
			if !unify(ft, valT, nil) {
				return nil, errTypeMismatch(field.Value.Pos, ft, valT)
			}
			provided[field.Name] = true
		}
		// MEP-13 §Struct typing: every declared field must be provided.
		// There are no field defaults today, so an omitted field would
		// leave the struct value with a missing slot that downstream
		// reads observe as nil.
		var missing []string
		for _, f := range st.Fields {
			if !provided[f.Name] {
				missing = append(missing, f.Name)
			}
		}
		if len(missing) > 0 {
			return nil, errStructMissingField(p.Pos, st.Name, missing)
		}
		return st, nil

	case p.List != nil:
		// MEP-5 §Collections [T-List]: every element must unify with the
		// principal element type. The prior rule widened to AnyType when
		// elements disagreed, masking the heterogeneity at type-check
		// time. We now reject with T008.
		var elemType Type = nil
		for _, elem := range p.List.Elems {
			t, err := checkExpr(elem, env)
			if err != nil {
				return nil, err
			}
			if elemType == nil {
				elemType = t
				continue
			}
			if !unify(elemType, t, nil) {
				return nil, errTypeMismatch(elem.Pos, elemType, t)
			}
		}
		if elemType == nil {
			elemType = AnyType{}
		}
		return ListType{Elem: elemType}, nil

	case p.Map != nil:
		var keyT, valT Type
		for _, item := range p.Map.Items {
			var kt Type
			if _, ok := stringKey(item.Key); ok {
				kt = StringType{}
			} else {
				var err error
				kt, err = checkExpr(item.Key, env)
				if err != nil {
					return nil, err
				}
			}
			vt, err := checkExpr(item.Value, env)
			if err != nil {
				return nil, err
			}
			if keyT == nil {
				keyT = kt
			} else if !unify(keyT, kt, nil) {
				keyT = AnyType{}
			}
			if valT == nil {
				valT = vt
			} else if !unify(valT, vt, nil) {
				valT = AnyType{}
			}
		}
		if keyT == nil {
			keyT = AnyType{}
		}
		if valT == nil {
			valT = AnyType{}
		}
		return MapType{Key: keyT, Value: valT}, nil

	case p.Query != nil:
		return checkQueryExpr(p.Query, env, expected)

	case p.LogicQuery != nil:
		return ListType{Elem: MapType{Key: StringType{}, Value: AnyType{}}}, nil

	case p.Fetch != nil:
		urlT, err := checkExpr(p.Fetch.URL, env)
		if err != nil {
			return nil, err
		}
		if !unify(urlT, StringType{}, nil) {
			return nil, errFetchURLString(p.Pos)
		}
		if p.Fetch.With != nil {
			wt, err := checkExpr(p.Fetch.With, env)
			if err != nil {
				return nil, err
			}
			mt, ok := wt.(MapType)
			if !ok || !unify(mt.Key, StringType{}, nil) {
				return nil, errFetchOptsMap(p.Pos)
			}
			if withMl := p.Fetch.With.Binary.Left.Value.Target.Map; withMl != nil {
				for _, item := range withMl.Items {
					if key, ok := stringKey(item.Key); ok {
						var expect Type
						switch key {
						case "method":
							expect = StringType{}
						case "headers":
							expect = MapType{Key: StringType{}, Value: StringType{}}
						case "body":
							expect = nil
						case "query":
							expect = MapType{Key: StringType{}, Value: StringType{}}
						case "timeout":
							expect = FloatType{}
						default:
							expect = nil
						}
						if expect != nil {
							vt, err := checkExpr(item.Value, env)
							if err != nil {
								return nil, err
							}
							if !unify(vt, expect, nil) {
								return nil, errFetchOptType(item.Value.Pos, key, expect, vt)
							}
						} else {
							if _, err := checkExpr(item.Value, env); err != nil {
								return nil, err
							}
						}
					} else {
						if _, err := checkExpr(item.Value, env); err != nil {
							return nil, err
						}
					}
				}
			}
		}
		if expected != nil {
			return expected, nil
		}
		return AnyType{}, nil

	case p.Load != nil:
		var elem Type = AnyType{}
		if p.Load.Type != nil {
			elem = resolveTypeRef(p.Load.Type, env)
		}
		return ListType{Elem: elem}, nil

	case p.Save != nil:
		if _, err := checkExpr(p.Save.Src, env); err != nil {
			return UnitType{}, err
		}
		if p.Save.With != nil {
			if _, err := checkExpr(p.Save.With, env); err != nil {
				return UnitType{}, err
			}
		}
		return UnitType{}, nil

	case p.Match != nil:
		return checkMatchExpr(p.Match, env, expected)

	case p.Generate != nil:
		for _, f := range p.Generate.Fields {
			var expect Type
			switch f.Name {
			case "prompt", "model", "text":
				expect = StringType{}
			case "temperature", "top_p":
				expect = FloatType{}
			case "max_tokens":
				expect = IntType{}
			case "stop":
				expect = ListType{Elem: StringType{}}
			case "normalize":
				expect = BoolType{}
			case "args", "tools", "tool_choice":
				expect = nil
			}
			var err error
			if expect != nil {
				_, err = checkExprWithExpected(f.Value, env, expect)
			} else {
				_, err = checkExpr(f.Value, env)
			}
			if err != nil {
				return nil, err
			}
		}
		if p.Generate.Target == "text" {
			return StringType{}, nil
		}
		if p.Generate.Target == "embedding" {
			return ListType{Elem: FloatType{}}, nil
		}
		st, ok := env.GetStruct(p.Generate.Target)
		if !ok {
			return nil, errUnknownType(p.Pos, p.Generate.Target)
		}
		return st, nil

	case p.If != nil:
		return checkIfExpr(p.If, env, expected)
	case p.FunExpr != nil:
		return checkFunExpr(p.FunExpr, env, expected, p.Pos)

	case p.Group != nil:
		return checkExprWithExpected(p.Group, env, expected)
	}

	return nil, errInvalidPrimary(p.Pos)
}

func checkFunExpr(f *parser.FunExpr, env *Env, expected Type, pos lexer.Position) (Type, error) {
	var expectedFunc *FuncType
	if ft, ok := expected.(FuncType); ok {
		expectedFunc = &ft
	}

	paramTypes := make([]Type, len(f.Params))
	for i, p := range f.Params {
		if p.Type == nil {
			// Default missing parameter types to `any` rather than
			// failing. This allows algorithms that omit explicit
			// parameter annotations to type-check, falling back to
			// dynamic behaviour.
			paramTypes[i] = AnyType{}
		} else {
			paramTypes[i] = resolveTypeRef(p.Type, env)
		}
	}

	var declaredRet Type
	if f.Return != nil {
		declaredRet = resolveTypeRef(f.Return, env)
	} else if expectedFunc != nil {
		declaredRet = expectedFunc.Return
	} else {
		declaredRet = &TypeVar{Name: "R"}
	}

	child := NewEnv(env)
	for i, p := range f.Params {
		child.SetVar(p.Name, paramTypes[i], true)
	}

	subst := Subst{}
	var actualRet Type
	var err error

	if f.ExprBody != nil {
		actualRet, err = checkExpr(f.ExprBody, child)
		if err != nil {
			return nil, err
		}
	} else {
		// Block body
		for _, stmt := range f.BlockBody {
			if err := checkStmt(stmt, child, declaredRet, false); err != nil {
				return nil, err
			}
		}
		actualRet = declaredRet
	}

	if !unify(declaredRet, actualRet, subst) {
		return nil, errTypeMismatch(pos, declaredRet, actualRet)
	}

	// Final substitution: resolve any type variable that was inferred
	if tv, ok := declaredRet.(*TypeVar); ok {
		if resolved, ok := subst[tv.Name]; ok {
			declaredRet = resolved
		}
	}

	return FuncType{Params: paramTypes, Return: declaredRet}, nil
}

// bareIdentName returns the source identifier if e is a single
// identifier reference with no operators, indices, or selectors. It
// drives the MEP-10 A3 alias-into-var check; only a bare reference
// can share storage with the source binding.
func bareIdentName(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return ""
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 || u.Value == nil {
		return ""
	}
	px := u.Value
	if len(px.Ops) != 0 || px.Target == nil {
		return ""
	}
	sel := px.Target.Selector
	if sel == nil || len(sel.Tail) != 0 {
		return ""
	}
	return sel.Root
}

// aliasSourceLabel returns a short human-readable name for the
// aliasable source expression e: a bare identifier, an index chain
// (`rows[0]`), or a field chain (`obj.f`). It returns "" when the
// expression cannot name live storage (literals, calls, casts,
// computed values). MEP-10 B3c uses this to widen B3 beyond bare
// identifiers so index/field reads of aliasable aggregates also
// reject element-widening alias targets.
func aliasSourceLabel(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return ""
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 || u.Value == nil {
		return ""
	}
	px := u.Value
	if px.Target == nil {
		return ""
	}
	sel := px.Target.Selector
	if sel == nil {
		return ""
	}
	label := sel.Root
	for _, t := range sel.Tail {
		label += "." + t
	}
	for _, op := range px.Ops {
		switch {
		case op.Field != nil:
			label += "." + op.Field.Name
		case op.Index != nil:
			label += "[...]"
		default:
			return ""
		}
	}
	return label
}

// checkLiteralAliasElements walks list and map literals in e and
// rejects element expressions that name live aggregate storage
// (via aliasSourceLabel) when the expected element / value type is
// a structural aggregate widening the source's element type.
// MEP-10 B3e: `var bag: list<list<any>> = [xs]` where `xs : list<int>`
// stores a reference to xs in bag; a later `bag[0][i] = ...` would
// corrupt reads through xs. Recurses into nested literals so deeper
// shapes (`list<list<list<any>>>` from `[[xs]]`) are caught too.
func checkLiteralAliasElements(e *parser.Expr, env *Env, expected Type) error {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 || u.Value == nil {
		return nil
	}
	px := u.Value
	if len(px.Ops) != 0 || px.Target == nil {
		return nil
	}
	target := px.Target
	switch et := expected.(type) {
	case ListType:
		if target.List == nil {
			return nil
		}
		for _, el := range target.List.Elems {
			if err := checkOneLiteralAliasElem(el, env, et.Elem); err != nil {
				return err
			}
			if err := checkLiteralAliasElements(el, env, et.Elem); err != nil {
				return err
			}
		}
	case MapType:
		if target.Map == nil {
			return nil
		}
		for _, kv := range target.Map.Items {
			if err := checkOneLiteralAliasElem(kv.Value, env, et.Value); err != nil {
				return err
			}
			if err := checkLiteralAliasElements(kv.Value, env, et.Value); err != nil {
				return err
			}
		}
	}
	return nil
}

func checkOneLiteralAliasElem(e *parser.Expr, env *Env, expected Type) error {
	src := aliasSourceLabel(e)
	if src == "" {
		return nil
	}
	if !isAliasableAggregate(expected) {
		return nil
	}
	actT, err := checkExpr(e, env)
	if err != nil {
		return nil
	}
	if isAliasableAggregate(actT) && !equalKinds(actT, expected) {
		return errAliasWidensElement(e.Pos, src, actT, expected)
	}
	return nil
}

// firstFreeTypeVar returns the lexicographically first free TypeVar
// name in t after applying sub, or "" if none. It powers the T047 and
// T048 messages so the user sees the offending parameter rather than
// the substitution-internal label.
func firstFreeTypeVar(t Type, sub Subst) string {
	free := FreeTypeVars(t, sub)
	if len(free) == 0 {
		return ""
	}
	name := free[0]
	if i := strings.IndexByte(name, '#'); i >= 0 {
		return name[:i]
	}
	return name
}

// structuralTypeVarName returns the first TypeVar's declared name found
// in t, ignoring substitutions. The "T#N" suffix introduced by
// FreshTypeVar is stripped so the caller surfaces the user-declared
// parameter name (e.g. "T") in diagnostics.
func structuralTypeVarName(t Type) string {
	free := FreeTypeVars(t, Subst{})
	if len(free) == 0 {
		return ""
	}
	name := free[0]
	if i := strings.IndexByte(name, '#'); i >= 0 {
		return name[:i]
	}
	return name
}

// isAliasableAggregate reports whether t is a value kind whose storage
// is shared on copy (list, map, struct). These are the kinds where an
// alias from an immutable binding into a mutable one would let writes
// through the alias mutate the original.
func isAliasableAggregate(t Type) bool {
	switch t.(type) {
	case ListType, MapType, StructType:
		return true
	}
	return false
}

func curryFuncType(params []Type, ret Type) Type {
	if len(params) == 0 {
		return ret
	}
	return FuncType{
		Params: []Type{params[0]},
		Return: curryFuncType(params[1:], ret),
	}
}

func checkIfExpr(ie *parser.IfExpr, env *Env, expected Type) (Type, error) {
	condT, err := checkExpr(ie.Cond, env)
	if err != nil {
		return nil, err
	}
	if !unify(condT, BoolType{}, nil) {
		return nil, errIfCondBoolean(ie.Cond.Pos)
	}

	// MEP-16 N1: tighten Option<T> bindings to T inside the branch
	// that proves the value is present. `if x != null { ... }` narrows
	// x in the then-branch; `if x == null { ... } else { ... }`
	// narrows x in the else-branch.
	truthy, falsy := optionNarrowing(ie.Cond, env)
	thenEnv := narrowedEnv(env, truthy)
	elseEnv := narrowedEnv(env, falsy)

	thenT, err := checkExprWithExpected(ie.Then, thenEnv, expected)
	if err != nil {
		return nil, err
	}

	var elseT Type = AnyType{}
	if ie.ElseIf != nil {
		elseT, err = checkIfExpr(ie.ElseIf, elseEnv, thenT)
		if err != nil {
			return nil, err
		}
	} else if ie.Else != nil {
		elseT, err = checkExprWithExpected(ie.Else, elseEnv, thenT)
		if err != nil {
			return nil, err
		}
	}

	result := thenT
	if !unify(result, elseT, nil) {
		result = AnyType{}
	}
	if expected != nil && !unify(result, expected, nil) {
		return nil, errTypeMismatch(ie.Pos, expected, result)
	}
	return result, nil
}

func checkMatchExpr(m *parser.MatchExpr, env *Env, expected Type) (Type, error) {
	targetType, err := checkExpr(m.Target, env)
	if err != nil {
		return nil, err
	}
	var resultType Type
	// MEP-10 A4: track variant coverage when the scrutinee is a union.
	// A wildcard `_` (or an identifier binding that does not name a
	// variant) covers the remainder; otherwise every variant must be
	// matched explicitly.
	matchedVariants := map[string]bool{}
	hasCatchAll := false
	// MEP-13 §Irredundancy: a literal pattern that repeats an earlier
	// one is rejected, as are any arms that follow a catch-all. We do
	// not have pattern guards, so two arms with the same pattern can
	// never both fire.
	seenLiterals := map[string]bool{}
	for _, c := range m.Cases {
		caseEnv := env
		if hasCatchAll {
			return nil, errMatchArmRedundant(c.Pos, "arm after catch-all is unreachable")
		}
		if call, ok := callPattern(c.Pattern); ok {
			if ut, ok := env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				if len(call.Args) != len(st.Fields) {
					return nil, errTypeMismatch(c.Pos, targetType, st)
				}
				if !unify(targetType, st, nil) {
					return nil, errTypeMismatch(c.Pos, targetType, st)
				}
				if matchedVariants[call.Func] {
					return nil, errMatchArmRedundant(c.Pos, "duplicate variant `"+call.Func+"`")
				}
				matchedVariants[call.Func] = true
				child := NewEnv(env)
				for idx, arg := range call.Args {
					if name, ok := identName(arg); ok {
						child.SetVar(name, st.Fields[idx].Type, true)
					}
				}
				caseEnv = child
			}
		} else if ident, ok := identName(c.Pattern); ok {
			if ut, ok := env.FindUnionByVariant(ident); ok {
				st := ut.Variants[ident]
				if !unify(targetType, st, nil) {
					return nil, errTypeMismatch(c.Pos, targetType, st)
				}
				if matchedVariants[ident] {
					return nil, errMatchArmRedundant(c.Pos, "duplicate variant `"+ident+"`")
				}
				matchedVariants[ident] = true
			} else if !isUnderscoreExpr(c.Pattern) {
				pType, err := checkExpr(c.Pattern, env)
				if err != nil {
					return nil, err
				}
				if !unify(targetType, pType, nil) {
					return nil, errTypeMismatch(c.Pos, targetType, pType)
				}
			} else {
				hasCatchAll = true
			}
		} else if !isUnderscoreExpr(c.Pattern) {
			pType, err := checkExpr(c.Pattern, env)
			if err != nil {
				return nil, err
			}
			if !unify(targetType, pType, nil) {
				return nil, errTypeMismatch(c.Pos, targetType, pType)
			}
			if key := literalPatternKey(c.Pattern); key != "" {
				if seenLiterals[key] {
					return nil, errMatchArmRedundant(c.Pos, "duplicate literal pattern")
				}
				seenLiterals[key] = true
			}
		} else {
			hasCatchAll = true
		}
		if c.Result == nil {
			for _, st := range c.Block {
				if err := checkStmt(st, caseEnv, expected, false); err != nil {
					return nil, err
				}
			}
			continue
		}

		rType, err := checkExprWithExpected(c.Result, caseEnv, expected)
		if err != nil {
			return nil, err
		}
		if resultType == nil {
			resultType = rType
			continue
		}
		// MEP-5 §Match [T-Match]: every arm's result type must unify with
		// the principal type. Heterogeneous arms are T008; the prior rule
		// silently widened to AnyType.
		if !unify(resultType, rType, nil) {
			return nil, errTypeMismatch(c.Pos, resultType, rType)
		}
	}
	// MEP-10 A4: when the scrutinee is a union, every variant must be
	// covered by an explicit arm or a wildcard `_` arm. The check runs
	// after all arms have been typed so the error pinpoints the match
	// site rather than an arbitrary arm.
	if ut, ok := targetType.(UnionType); ok && !hasCatchAll {
		var missing []string
		for _, name := range ut.Order {
			if !matchedVariants[name] {
				missing = append(missing, name)
			}
		}
		if len(missing) > 0 {
			return nil, errMatchNonExhaustive(m.Pos, ut.Name, missing)
		}
	}
	if resultType == nil {
		resultType = AnyType{}
	}
	if expected != nil && !unify(resultType, expected, nil) {
		return nil, errTypeMismatch(m.Pos, expected, resultType)
	}
	return resultType, nil
}

func checkQueryExpr(q *parser.QueryExpr, env *Env, expected Type) (Type, error) {
	srcT, err := checkExpr(q.Source, env)
	if err != nil {
		return nil, err
	}
	var elemT Type
	switch t := srcT.(type) {
	case ListType:
		elemT = t.Elem
	case GroupType:
		elemT = t.Elem
	default:
		return nil, errQuerySourceList(q.Pos)
	}
	child := NewEnv(env)
	child.SetVar(q.Var, elemT, true)

	for _, f := range q.Froms {
		ft, err := checkExpr(f.Src, child)
		if err != nil {
			return nil, err
		}
		var fe Type
		switch t := ft.(type) {
		case ListType:
			fe = t.Elem
		case GroupType:
			fe = t.Elem
		default:
			return nil, errJoinSourceList(f.Pos)
		}
		child.SetVar(f.Var, fe, true)
	}

	for _, j := range q.Joins {
		jt, err := checkExpr(j.Src, child)
		if err != nil {
			return nil, err
		}
		var je Type
		switch t := jt.(type) {
		case ListType:
			je = t.Elem
		case GroupType:
			je = t.Elem
		default:
			return nil, errJoinSourceList(j.Pos)
		}
		child.SetVar(j.Var, je, true)
		onT, err := checkExpr(j.On, child)
		if err != nil {
			return nil, err
		}
		if !unify(onT, BoolType{}, nil) {
			return nil, errJoinOnBoolean(j.On.Pos)
		}
	}

	if q.Where != nil {
		wt, err := checkExpr(q.Where, child)
		if err != nil {
			return nil, err
		}
		if _, ok := wt.(AnyType); !ok && !unify(wt, BoolType{}, nil) {
			return nil, errWhereBoolean(q.Where.Pos)
		}
		if name, pos, ok := firstImpureCall(q.Where, child); ok {
			return nil, errImpurePredicate(pos, name, "where")
		}
	}

	if q.Skip != nil {
		skipT, err := checkExpr(q.Skip, child)
		if err != nil {
			return nil, err
		}
		if _, ok := skipT.(AnyType); !ok && !unify(skipT, IntType{}, nil) {
			return nil, errSkipTakeIntOperand(q.Skip.Pos, "skip", skipT)
		}
	}
	if q.Take != nil {
		takeT, err := checkExpr(q.Take, child)
		if err != nil {
			return nil, err
		}
		if _, ok := takeT.(AnyType); !ok && !unify(takeT, IntType{}, nil) {
			return nil, errSkipTakeIntOperand(q.Take.Pos, "take", takeT)
		}
	}

	armEnv := child
	if q.Group != nil {
		var keyT Type
		if len(q.Group.Exprs) == 1 {
			var err error
			keyT, err = checkExpr(q.Group.Exprs[0], child)
			if err != nil {
				return nil, err
			}
		} else if len(q.Group.Exprs) == 2 {
			k1, err := checkExpr(q.Group.Exprs[0], child)
			if err != nil {
				return nil, err
			}
			k2, err := checkExpr(q.Group.Exprs[1], child)
			if err != nil {
				return nil, err
			}
			if _, ok := k1.(StringType); ok {
				if _, ok2 := k2.(StringType); ok2 {
					keyT = StructType{Name: "pair_string", Fields: []StructField{{Name: "a", Type: StringType{}}, {Name: "b", Type: StringType{}}}}
				}
			}
			if keyT == nil {
				keyT = AnyType{}
			}
		} else {
			var err error
			keyT, err = checkExpr(q.Group.Exprs[0], child)
			if err != nil {
				return nil, err
			}
		}
		genv := NewEnv(child)
		gStruct := GroupType{Key: keyT, Elem: elemT}
		genv.SetVar(q.Group.Name, gStruct, true)
		armEnv = genv
		if q.Group.Having != nil {
			ht, err := checkExpr(q.Group.Having, armEnv)
			if err != nil {
				return nil, err
			}
			if _, ok := ht.(AnyType); !ok && !unify(ht, BoolType{}, nil) {
				return nil, errHavingBoolean(q.Group.Having.Pos)
			}
			if name, pos, ok := firstImpureCall(q.Group.Having, armEnv); ok {
				return nil, errImpurePredicate(pos, name, "having")
			}
		}
	}

	if q.Sort != nil {
		// Type-check the sort expression best-effort: existing dataset
		// queries (TPC-H q7 et al.) reference group-by key fields as bare
		// names that the type environment does not currently expose, so
		// any check error here is downgraded to "unknown" rather than a
		// hard failure. The ordered constraint (T056) only fires when the
		// expression has a concrete, non-`any` type.
		if st, err := checkExpr(q.Sort, armEnv); err == nil {
			if _, ok := st.(AnyType); !ok && !isOrdered(st) {
				return nil, errSortByOrdered(q.Sort.Pos, st)
			}
		}
	}

	var selT Type
	if q.Group != nil {
		selT, err = checkExpr(q.Select, armEnv)
	} else {
		if name, arg, ok := aggregateCallName(q.Select); ok {
			at, err := checkExpr(arg, armEnv)
			if err != nil {
				return nil, err
			}
			switch name {
			case "sum":
				if _, ok := at.(AnyType); !ok && !isNumeric(at) {
					return nil, errSumOperand(q.Select.Pos, at)
				}
				// Preserve the element type per MEP-5 P2; widening to int/float
				// lost precision for bigint/bigrat/int64 summands.
				selT = at
			case "avg":
				if _, ok := at.(AnyType); !ok && !isNumeric(at) {
					return nil, errSumOperand(q.Select.Pos, at)
				}
				selT = FloatType{}
			case "min", "max":
				if _, ok := at.(AnyType); !ok && !isNumeric(at) {
					return nil, fmt.Errorf("min/max() expects numeric expression")
				}
				selT = at
			case "count":
				selT = IntType{}
			}
		} else {
			selT, err = checkExpr(q.Select, armEnv)
			if err != nil {
				return nil, err
			}
		}
	}
	if err != nil {
		return nil, err
	}
	if q.Distinct {
		if !isHashable(selT) {
			return nil, errDistinctHashable(q.Select.Pos, selT)
		}
	}
	if q.Group == nil {
		if _, _, ok := aggregateCallName(q.Select); ok {
			if expected != nil && !unify(selT, expected, nil) {
				return nil, errTypeMismatch(q.Pos, expected, selT)
			}
			return selT, nil
		}
	}
	result := ListType{Elem: selT}
	if expected != nil && !unify(result, expected, nil) {
		return nil, errTypeMismatch(q.Pos, expected, result)
	}
	return result, nil
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
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
	if p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0 {
		return true
	}
	return false
}

func identName(e *parser.Expr) (string, bool) {
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
	return "", false
}

func stringKey(e *parser.Expr) (string, bool) {
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

func aggregateCallName(e *parser.Expr) (string, *parser.Expr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", nil, false
	}
	p := u.Value
	if p == nil || len(p.Ops) != 0 || p.Target == nil || p.Target.Call == nil {
		return "", nil, false
	}
	call := p.Target.Call
	if len(call.Args) != 1 {
		return "", nil, false
	}
	if isQueryExpr(call.Args[0]) {
		return "", nil, false
	}
	switch call.Func {
	case "sum", "avg", "min", "max", "count":
		return call.Func, call.Args[0], true
	default:
		return "", nil, false
	}
}

func isQueryExpr(e *parser.Expr) bool {
	for e != nil {
		if e.Binary == nil || len(e.Binary.Right) != 0 {
			return false
		}
		u := e.Binary.Left
		if len(u.Ops) != 0 {
			return false
		}
		p := u.Value
		if p == nil || len(p.Ops) != 0 {
			return false
		}
		prim := p.Target
		if prim == nil {
			return false
		}
		if prim.Query != nil || prim.LogicQuery != nil {
			return true
		}
		if prim.Group != nil {
			e = prim.Group
			continue
		}
		return false
	}
	return false
}

// castOk reports whether an `e as T` expression should be accepted by
// the type checker. The policy is deliberately narrow: every accepted
// cast must have a well-defined runtime semantics. String parsing and
// arbitrary cross-kind casts are rejected; use a parsing builtin
// (`parseIntStr`, `int`, `str`, ...) for those.
func castOk(from, to Type) bool {
	if equalTypes(from, to) {
		return true
	}
	if _, ok := from.(AnyType); ok {
		return true
	}
	if _, ok := to.(AnyType); ok {
		return true
	}
	if isNumeric(from) && isNumeric(to) {
		return true
	}
	if u, ok := from.(UnionType); ok {
		if s, ok := to.(StructType); ok {
			if _, in := u.Variants[s.Name]; in {
				return true
			}
		}
	}
	if _, ok := from.(MapType); ok {
		if _, ok := to.(StructType); ok {
			return true
		}
	}
	return false
}

func isNumeric(t Type) bool {
	switch t.(type) {
	case IntType, Int64Type, FloatType, BigIntType, BigRatType:
		return true
	default:
		return false
	}
}

func isOrdered(t Type) bool {
	switch v := t.(type) {
	case IntType, Int64Type, FloatType, BigIntType, BigRatType, StringType, BoolType:
		return true
	case ListType:
		if _, ok := v.Elem.(AnyType); ok {
			return true
		}
		return isOrdered(v.Elem)
	default:
		return false
	}
}

func isHashable(t Type) bool {
	switch v := t.(type) {
	case IntType, Int64Type, FloatType, BigIntType, BigRatType, StringType, BoolType, UnitType, AnyType:
		return true
	case ListType:
		return isHashable(v.Elem)
	case MapType:
		return isHashable(v.Key) && isHashable(v.Value)
	case OptionType:
		return isHashable(v.Elem)
	case StructType:
		for _, f := range v.Fields {
			if !isHashable(f.Type) {
				return false
			}
		}
		return true
	case UnionType:
		for _, s := range v.Variants {
			if !isHashable(s) {
				return false
			}
		}
		return true
	case FuncType, GroupType:
		return false
	default:
		return false
	}
}

var builtinArity = map[string]int{
	"now":         0,
	"input":       0,
	"json":        1,
	"to_json":     1,
	"str":         1,
	"parseIntStr": 2,
	"int":         1,
	"upper":       1,
	"lower":       1,
	"reverse":     1,
	"distinct":    1,
	"trim":        1,
	"contains":    2,
	"split":       2,
	"join":        2,
	"eval":        1,
	"len":         1,
	"count":       1,
	"exists":      1,
	"avg":         1,
	"abs":         1,
	"ceil":        1,
	"floor":       1,
	"sum":         1,
	"min":         1,
	"max":         1,
	"keys":        1,
	"values":      1,
	"reduce":      3,
	"append":      2,
	"push":        2,
	"first":       1,
	"substring":   3,
	"padStart":    3,
	"indexOf":     2,
	"repeat":      2,
	"sha256":      1,
	"num":         1,
	"denom":       1,
}

func checkBuiltinCall(name string, args []Type, pos lexer.Position) error {
	switch name {
	case "now", "input":
		if len(args) != 0 {
			return errArgCount(pos, name, 0, len(args))
		}
		return nil
	case "json", "to_json", "str", "upper", "lower", "int", "eval":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch name {
		case "eval":
			if _, ok := args[0].(StringType); !ok {
				return errArgTypeMismatch(pos, 0, StringType{}, args[0])
			}
		case "int":
			switch args[0].(type) {
			case StringType, IntType, FloatType, AnyType:
				// ok
			default:
				return fmt.Errorf("int() expects numeric or string")
			}
		}
		return nil
	case "parseIntStr":
		if len(args) != 2 {
			return errArgCount(pos, name, 2, len(args))
		}
		if _, ok := args[0].(StringType); !ok {
			if _, ok := args[0].(AnyType); !ok {
				return errArgTypeMismatch(pos, 0, StringType{}, args[0])
			}
		}
		switch args[1].(type) {
		case IntType, AnyType:
			// ok
		case BigIntType:
			args[1] = IntType{}
		default:
			return errArgTypeMismatch(pos, 1, IntType{}, args[1])
		}
		return nil
	case "len":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch args[0].(type) {
		case ListType, MapType, StringType, AnyType:
			return nil
		default:
			return errLenOperand(pos, args[0])
		}
	case "count":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch args[0].(type) {
		case ListType, GroupType, AnyType:
			return nil
		default:
			return errCountOperand(pos, args[0])
		}
	case "exists":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch args[0].(type) {
		case ListType, MapType, StringType, AnyType, GroupType:
			return nil
		default:
			return fmt.Errorf("exists expects list, map or string")
		}
	case "avg":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch a := args[0].(type) {
		case ListType:
			if _, ok := a.Elem.(AnyType); ok || isNumeric(a.Elem) {
				return nil
			}
			if _, ok := a.Elem.(StringType); ok {
				return nil
			}
			if _, ok := a.Elem.(BoolType); ok {
				return nil
			}
			return errAvgOperand(pos, a.Elem)
		case GroupType:
			if _, ok := a.Elem.(AnyType); ok || isNumeric(a.Elem) {
				return nil
			}
			if _, ok := a.Elem.(StringType); ok {
				return nil
			}
			if _, ok := a.Elem.(BoolType); ok {
				return nil
			}
			return errAvgOperand(pos, a.Elem)
		case AnyType:
			return nil
		default:
			return errAvgOperand(pos, a)
		}
	case "sum":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch a := args[0].(type) {
		case ListType:
			if _, ok := a.Elem.(AnyType); ok || isNumeric(a.Elem) {
				return nil
			}
			if _, ok := a.Elem.(StringType); ok {
				return nil
			}
			if _, ok := a.Elem.(BoolType); ok {
				return nil
			}
			return errSumOperand(pos, a.Elem)
		case GroupType:
			if _, ok := a.Elem.(AnyType); ok || isNumeric(a.Elem) {
				return nil
			}
			if _, ok := a.Elem.(StringType); ok {
				return nil
			}
			if _, ok := a.Elem.(BoolType); ok {
				return nil
			}
			return errSumOperand(pos, a.Elem)
		case AnyType:
			return nil
		default:
			return errSumOperand(pos, a)
		}
	case "abs", "ceil", "floor":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		if !isNumeric(args[0]) {
			return fmt.Errorf("%s() expects numeric", name)
		}
		return nil
	case "min", "max":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch a := args[0].(type) {
		case ListType:
			if _, ok := a.Elem.(AnyType); ok || isNumeric(a.Elem) {
				return nil
			}
			if _, ok := a.Elem.(StringType); ok {
				return nil
			}
			if _, ok := a.Elem.(BoolType); ok {
				return nil
			}
			return fmt.Errorf("%s() expects numeric list", name)
		case GroupType:
			if _, ok := a.Elem.(AnyType); ok || isNumeric(a.Elem) {
				return nil
			}
			if _, ok := a.Elem.(StringType); ok {
				return nil
			}
			if _, ok := a.Elem.(BoolType); ok {
				return nil
			}
			return fmt.Errorf("%s() expects numeric list", name)
		case AnyType:
			return nil
		default:
			return fmt.Errorf("%s() expects list", name)
		}
	case "keys", "values":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch args[0].(type) {
		case MapType, AnyType:
			return nil
		default:
			return fmt.Errorf("%s() expects map", name)
		}
	case "collect":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch args[0].(type) {
		case ListType, GroupType, AnyType:
			return nil
		default:
			return fmt.Errorf("collect() expects list or group")
		}
	case "reduce":
		if len(args) != 3 {
			return errArgCount(pos, name, 3, len(args))
		}
		// first argument should be list
		if _, ok := args[0].(ListType); !ok {
			if _, ok := args[0].(AnyType); !ok {
				return fmt.Errorf("reduce() expects list, got %v", args[0])
			}
		}
		return nil
	case "concat":
		for _, a := range args {
			if _, ok := a.(ListType); !ok {
				if _, ok := a.(AnyType); !ok {
					return fmt.Errorf("concat() expects list, got %v", a)
				}
			}
		}
		return nil
	case "append", "push":
		if len(args) != 2 {
			return errArgCount(pos, name, 2, len(args))
		}
		if _, ok := args[0].(ListType); !ok {
			if _, ok := args[0].(AnyType); !ok {
				return fmt.Errorf("%s() expects list, got %v", name, args[0])
			}
		}
		return nil
	case "range":
		if len(args) < 1 || len(args) > 3 {
			return errArgCount(pos, name, 1, len(args))
		}
		for i, a := range args {
			if _, ok := a.(IntType); !ok {
				if _, ok := a.(AnyType); !ok {
					return errArgTypeMismatch(pos, i, IntType{}, a)
				}
			}
		}
		return nil
	case "first":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		if _, ok := args[0].(ListType); !ok {
			if _, ok := args[0].(AnyType); !ok {
				return fmt.Errorf("first() expects list, got %v", args[0])
			}
		}
		return nil
	case "reverse":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch args[0].(type) {
		case ListType, StringType, AnyType:
			return nil
		default:
			return fmt.Errorf("reverse() expects list or string, got %v", args[0])
		}
	case "distinct":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch args[0].(type) {
		case ListType, AnyType:
			return nil
		default:
			return fmt.Errorf("distinct() expects list, got %v", args[0])
		}
	case "substring":
		if len(args) != 3 {
			return errArgCount(pos, name, 3, len(args))
		}
		if _, ok := args[0].(StringType); !ok {
			if _, ok := args[0].(AnyType); !ok {
				return fmt.Errorf("substring() expects string, got %v", args[0])
			}
		}
		for i := 1; i < 3; i++ {
			switch args[i].(type) {
			case IntType, AnyType:
				// ok
			case BigIntType:
				// allow bigint indices, treat as int
				args[i] = IntType{}
			default:
				return errArgTypeMismatch(pos, i, IntType{}, args[i])
			}
		}
		return nil
	case "padStart":
		if len(args) != 3 {
			return errArgCount(pos, name, 3, len(args))
		}
		if _, ok := args[0].(StringType); !ok {
			if _, ok := args[0].(AnyType); !ok {
				return fmt.Errorf("padStart() expects string, got %v", args[0])
			}
		}
		switch args[1].(type) {
		case IntType, AnyType:
			// ok
		case BigIntType:
			args[1] = IntType{}
		default:
			return errArgTypeMismatch(pos, 1, IntType{}, args[1])
		}
		if _, ok := args[2].(StringType); !ok {
			if _, ok := args[2].(AnyType); !ok {
				return errArgTypeMismatch(pos, 2, StringType{}, args[2])
			}
		}
		return nil
	case "indexOf":
		if len(args) != 2 {
			return errArgCount(pos, name, 2, len(args))
		}
		for i := 0; i < 2; i++ {
			if _, ok := args[i].(StringType); !ok {
				if _, ok := args[i].(AnyType); !ok {
					return errArgTypeMismatch(pos, i, StringType{}, args[i])
				}
			}
		}
		return nil
	case "repeat":
		if len(args) != 2 {
			return errArgCount(pos, name, 2, len(args))
		}
		if _, ok := args[0].(StringType); !ok {
			if _, ok := args[0].(AnyType); !ok {
				return errArgTypeMismatch(pos, 0, StringType{}, args[0])
			}
		}
		switch args[1].(type) {
		case IntType, AnyType:
			// ok
		case BigIntType:
			args[1] = IntType{}
		default:
			return errArgTypeMismatch(pos, 1, IntType{}, args[1])
		}
		return nil
	case "sha256":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch args[0].(type) {
		case StringType, ListType, AnyType:
			return nil
		default:
			return fmt.Errorf("sha256 expects string or list")
		}
	case "num", "denom":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		if !isNumeric(args[0]) {
			return fmt.Errorf("%s() expects numeric", name)
		}
		return nil
	}
	return nil
}

// literalPatternKey returns a canonical string key for a literal pattern
// in a match arm, or "" if e is not a literal. The key is used by the
// MEP-13 §Irredundancy check to detect a duplicate literal pattern
// across arms; the position is irrelevant for equality, only the
// literal value matters.
func literalPatternKey(e *parser.Expr) string {
	if e == nil {
		return ""
	}
	if len(e.Binary.Right) != 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return ""
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Lit == nil {
		return ""
	}
	lit := p.Target.Lit
	switch {
	case lit.Int != nil:
		return fmt.Sprintf("int:%d", int(*lit.Int))
	case lit.Float != nil:
		return fmt.Sprintf("float:%v", *lit.Float)
	case lit.Str != nil:
		return fmt.Sprintf("str:%q", *lit.Str)
	case lit.Bool != nil:
		return fmt.Sprintf("bool:%v", bool(*lit.Bool))
	case lit.Null:
		return "null"
	}
	return ""
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil {
		return nil, false
	}
	if len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}
