package types

import (
	"fmt"

	"mochi/parser"
)

func Check(prog *parser.Program, env *Env) []error {
	registerPrelude(env)

	var errs []error

	// Pre-pass: register struct and union name stubs so the function pass
	// below can resolve forward references in parameter and return types.
	// The dedicated type-declaration pass that follows replaces each stub
	// with the fully populated shape.
	for _, stmt := range prog.Statements {
		if stmt.Type == nil {
			continue
		}
		if len(stmt.Type.Members) > 0 {
			stub := StructType{Name: stmt.Type.Name}
			env.SetStruct(stmt.Type.Name, stub)
			env.types[stmt.Type.Name] = stub
			continue
		}
		if len(stmt.Type.Variants) > 0 {
			variants := map[string]StructType{}
			for _, v := range stmt.Type.Variants {
				vs := StructType{Name: v.Name}
				variants[v.Name] = vs
				env.SetStruct(v.Name, vs)
			}
			stub := UnionType{Name: stmt.Type.Name, Variants: variants}
			env.SetUnion(stmt.Type.Name, stub)
			env.types[stmt.Type.Name] = stub
		}
		// Type aliases are handled during the dedicated pass; the right
		// hand side may itself be a forward reference.
	}

	// First pass: gather all function declarations so methods defined in types
	// can reference them regardless of order in the source file. MEP-15 Stage
	// 2b: a non-empty Effects annotation on FunStmt seeds the signature with
	// the declared upper bound; T064 fires here for unknown labels.
	declared := map[string]EffectSet{}
	for _, stmt := range prog.Statements {
		if stmt.Fun != nil {
			sigEnv := env
			if len(stmt.Fun.TypeParams) > 0 {
				sigEnv = NewEnv(env)
				for _, tp := range stmt.Fun.TypeParams {
					sigEnv.SetTypeParam(tp, &TypeVar{Name: tp})
				}
			}
			params := make([]Type, len(stmt.Fun.Params))
			for i, p := range stmt.Fun.Params {
				if p.Type != nil {
					params[i] = resolveTypeRef(p.Type, sigEnv)
				} else {
					params[i] = AnyType{}
				}
			}
			var ret Type = UnitType{}
			if stmt.Fun.Return != nil {
				ret = resolveTypeRef(stmt.Fun.Return, sigEnv)
			}
			seed := EmptyEffects
			if len(stmt.Fun.Effects) > 0 {
				d, labelErrs := parseDeclaredEffects(stmt.Fun)
				errs = append(errs, labelErrs...)
				declared[stmt.Fun.Name] = d
				seed = d
			}
			env.SetVar(stmt.Fun.Name, FuncType{
				Params:     params,
				Return:     ret,
				Effects:    seed,
				TypeParams: append([]string(nil), stmt.Fun.TypeParams...),
			}, false)
			env.SetFunc(stmt.Fun.Name, stmt.Fun)
		}
	}

	// MEP-15 Stage 2: iterate body-walked effect inference to a
	// fixpoint over the top-level function signatures. The bitset
	// lattice has at most 1<<effectMax states; each function climbs
	// monotonically so the loop terminates after at most effectMax
	// sweeps of the dependency graph. Annotated functions are pinned
	// to their declared set so callers see the published contract;
	// inference still runs against the same env to populate the
	// downstream check below.
	for {
		changed := false
		for _, stmt := range prog.Statements {
			if stmt.Fun == nil {
				continue
			}
			if _, pinned := declared[stmt.Fun.Name]; pinned {
				continue
			}
			t, err := env.GetVar(stmt.Fun.Name)
			if err != nil {
				continue
			}
			ft, ok := t.(FuncType)
			if !ok {
				continue
			}
			newEffs := inferFunctionEffects(stmt.Fun, env)
			if newEffs != ft.Effects {
				ft.Effects = newEffs
				env.SetVar(stmt.Fun.Name, ft, false)
				changed = true
			}
		}
		if !changed {
			break
		}
	}

	// MEP-15 Stage 2b: T065 declared-exceeds-inferred. For each
	// annotated function, re-run the walker against the finalized env
	// and reject any inferred label that escapes the declared set.
	for _, stmt := range prog.Statements {
		if stmt.Fun == nil {
			continue
		}
		d, ok := declared[stmt.Fun.Name]
		if !ok {
			continue
		}
		inferred := inferFunctionEffects(stmt.Fun, env)
		if !inferred.IsSubset(d) {
			errs = append(errs, errEffectsExceedDeclared(stmt.Fun.Pos, stmt.Fun.Name, d, inferred))
		}
	}

	// Second pass: process type declarations now that all functions are known.
	for _, stmt := range prog.Statements {
		if stmt.Type != nil {
			if err := checkStmt(stmt, env, UnitType{}, false); err != nil {
				errs = append(errs, err)
			}
		}
	}

	// Final pass: check remaining statements, including function bodies.
	for _, stmt := range prog.Statements {
		if stmt.Type == nil {
			if err := checkStmt(stmt, env, UnitType{}, false); err != nil {
				errs = append(errs, err)
			}
		}
	}
	// Flush diagnostics raised in resolveTypeRef and other contexts
	// that cannot return errors directly to a caller.
	errs = append(errs, env.TakeDiagnostics()...)
	return errs
}

// --- Helpers ---

func buildStreamFields(fields []*parser.StreamField, env *Env) []StructField {
	out := make([]StructField, 0, len(fields))
	for _, f := range fields {
		if f == nil {
			continue
		}
		out = append(out, StructField{Name: f.Name, Type: resolveTypeRef(f.Type, env)})
	}
	return out
}

func checkStmt(s *parser.Statement, env *Env, expectedReturn Type, inLoop bool) error {
	switch {
	case s.Stream != nil:
		fields := buildStreamFields(s.Stream.Fields, env)
		st := StructType{Name: s.Stream.Name, Fields: fields}
		env.SetStream(s.Stream.Name, st)
		env.SetStruct(s.Stream.Name, st)
		env.types[s.Stream.Name] = st
		return nil

	case s.Agent != nil:
		var fields []StructField
		methods := map[string]Method{}
		for _, blk := range s.Agent.Body {
			switch {
			case blk.Let != nil:
				var t Type = AnyType{}
				if blk.Let.Type != nil {
					t = resolveTypeRef(blk.Let.Type, env)
				}
				fields = append(fields, StructField{Name: blk.Let.Name, Type: t})
			case blk.Var != nil:
				var t Type = AnyType{}
				if blk.Var.Type != nil {
					t = resolveTypeRef(blk.Var.Type, env)
				}
				fields = append(fields, StructField{Name: blk.Var.Name, Type: t})
			case blk.Intent != nil:
				params := make([]Type, len(blk.Intent.Params))
				for i, p := range blk.Intent.Params {
					if p.Type != nil {
						params[i] = resolveTypeRef(p.Type, env)
					} else {
						params[i] = AnyType{}
					}
				}
				var ret Type = AnyType{}
				if blk.Intent.Return != nil {
					ret = resolveTypeRef(blk.Intent.Return, env)
				}
				intentFn := &parser.FunStmt{Params: blk.Intent.Params, Return: blk.Intent.Return, Body: blk.Intent.Body}
				methods[blk.Intent.Name] = Method{Decl: intentFn, Type: FuncType{Params: params, Return: ret, Effects: inferFunctionEffects(intentFn, env)}}
			}
		}
		st := StructType{Name: s.Agent.Name, Fields: fields, Methods: methods}
		env.SetStruct(s.Agent.Name, st)
		env.SetAgent(s.Agent.Name, s.Agent)
		env.types[s.Agent.Name] = st
		return nil

	case s.On != nil:
		st, ok := env.GetStream(s.On.Stream)
		if !ok {
			return errUnknownStream(s.On.Pos, s.On.Stream)
		}
		child := NewEnv(env)
		child.SetVar(s.On.Alias, st, true)
		for _, stmt := range s.On.Body {
			if err := checkStmt(stmt, child, expectedReturn, false); err != nil {
				return err
			}
		}
		return nil

	case s.Emit != nil:
		st, ok := env.GetStream(s.Emit.Stream)
		if !ok {
			return errUnknownStream(s.Emit.Pos, s.Emit.Stream)
		}
		for _, f := range s.Emit.Fields {
			ft, ok := st.FieldType(f.Name)
			if !ok {
				return errUnknownField(f.Pos, f.Name, st)
			}
			if _, err := checkExprWithExpected(f.Value, env, ft); err != nil {
				return err
			}
		}
		return nil

	case s.Let != nil:
		name := s.Let.Name
		var typ Type
		if s.Let.Type != nil {
			typ = resolveTypeRef(s.Let.Type, env)
			if s.Let.Value != nil {
				exprType, err := checkExprWithExpected(s.Let.Value, env, typ)
				if err != nil {
					return err
				}
				// MEP-11.2: route the let-binding check through
				// Assignable rather than unify. Direction matters:
				// the value flows into the declared slot, so we ask
				// `Assignable(rhs, declared)` not the symmetric form.
				if !Assignable(exprType, typ) {
					return errTypeMismatch(s.Let.Pos, typ, exprType)
				}
			}
		} else if s.Let.Value != nil {
			var err error
			typ, err = checkExprWithExpected(s.Let.Value, env, nil)
			if err != nil {
				return err
			}
		} else {
			return errLetMissingTypeOrValue(s.Let.Pos)
		}
		env.SetVar(name, typ, false)
		return nil

	case s.Var != nil:
		name := s.Var.Name
		// MEP-10 A3: reject aliasing an immutable aggregate into a
		// mutable binding. `let xs: list<int>; var ys = xs` would share
		// storage; a later `ys[0] = ...` would mutate xs too. Clone
		// explicitly or declare the source as `var`.
		if src := bareIdentName(s.Var.Value); src != "" {
			if mut, err := env.IsMutable(src); err == nil && !mut {
				if srcT, err := env.GetVar(src); err == nil && isAliasableAggregate(srcT) {
					return errAliasImmutableAggregate(s.Var.Pos, src)
				}
			}
		}
		var typ Type
		if s.Var.Type != nil {
			typ = resolveTypeRef(s.Var.Type, env)
			if s.Var.Value != nil {
				exprType, err := checkExprWithExpected(s.Var.Value, env, typ)
				if err != nil {
					return err
				}
				// MEP-11.2: route the var-binding check through
				// Assignable; see the matching change on Let above.
				if !Assignable(exprType, typ) {
					return errTypeMismatch(s.Var.Pos, typ, exprType)
				}
				// MEP-10 B3 / B3c: when the source expression names
				// live aggregate storage (bare identifier, an index
				// chain like `rows[0]`, or a field chain like
				// `obj.f`), the new binding shares storage with the
				// source. A widened element type lets a later
				// `ys[i] = ...` deposit a value the source's static
				// type rejects, corrupting reads through the source.
				// Aliasing requires structural equality on aggregate
				// element, key, and value types.
				if src := aliasSourceLabel(s.Var.Value); src != "" {
					if isAliasableAggregate(exprType) && isAliasableAggregate(typ) && !equalKinds(exprType, typ) {
						return errAliasWidensElement(s.Var.Pos, src, exprType, typ)
					}
				}
				// MEP-10 B3e: when the value is a list or map
				// literal targeting a structural aggregate slot,
				// reject element expressions that name live
				// aggregate storage of a narrower element type.
				if err := checkLiteralAliasElements(s.Var.Value, env, typ); err != nil {
					return err
				}
			}
		} else if s.Var.Value != nil {
			var err error
			typ, err = checkExprWithExpected(s.Var.Value, env, nil)
			if err != nil {
				return err
			}
		} else {
			return errLetMissingTypeOrValue(s.Var.Pos)
		}
		env.SetVar(name, typ, true)
		return nil

	case s.Import != nil:
		alias := s.Import.As
		if alias == "" {
			alias = parser.AliasFromPath(s.Import.Path)
		}
		env.SetVar(alias, AnyType{}, false)
		return nil

	case s.ExternVar != nil:
		var typ Type = AnyType{}
		if s.ExternVar.Type != nil {
			typ = resolveTypeRef(s.ExternVar.Type, env)
		}
		env.SetVar(s.ExternVar.Name(), typ, false)
		return nil

	case s.ExternFun != nil:
		params := make([]Type, len(s.ExternFun.Params))
		for i, p := range s.ExternFun.Params {
			if p.Type != nil {
				params[i] = resolveTypeRef(p.Type, env)
			} else {
				params[i] = AnyType{}
			}
		}
		var ret Type = AnyType{}
		if s.ExternFun.Return != nil {
			ret = resolveTypeRef(s.ExternFun.Return, env)
		}
		env.SetVar(s.ExternFun.Name(), FuncType{Params: params, Return: ret}, false)
		return nil

	case s.Fact != nil:
		return nil

	case s.Rule != nil:
		return nil

	case s.Assign != nil:
		// MEP-16 N4: when the LHS binding is narrowed (e.g., inside
		// `if x != none { ... }`), the assignment must reference the
		// declared type, not the narrowed shadow. Otherwise rebinding to
		// the wider option type would be rejected.
		lhsType, err := env.DeclaredVarType(s.Assign.Name)
		if err != nil {
			return errAssignUndeclared(s.Assign.Pos, s.Assign.Name)
		}
		mutable, err := env.IsMutable(s.Assign.Name)
		if err != nil {
			return errAssignUndeclared(s.Assign.Pos, s.Assign.Name)
		}
		if !mutable {
			return errAssignImmutableVar(s.Assign.Pos, s.Assign.Name)
		}
		if len(s.Assign.Index) > 0 {
			for _, idx := range s.Assign.Index {
				switch lt := lhsType.(type) {
				case MapType:
					if idx.Colon != nil {
						return errInvalidMapSlice(idx.Pos)
					}
					keyType, err := checkExpr(idx.Start, env)
					if err != nil {
						return err
					}
					if !unify(keyType, lt.Key, nil) {
						return errIndexTypeMismatch(idx.Pos, lt.Key, keyType)
					}
					lhsType = lt.Value
				case ListType:
					if idx.Colon != nil {
						if idx.Start != nil {
							t, err := checkExpr(idx.Start, env)
							if err != nil {
								return err
							}
							if !(unify(t, IntType{}, nil) || unify(t, Int64Type{}, nil)) {
								return errIndexNotInteger(idx.Pos)
							}
						}
						if idx.End != nil {
							t, err := checkExpr(idx.End, env)
							if err != nil {
								return err
							}
							if !(unify(t, IntType{}, nil) || unify(t, Int64Type{}, nil)) {
								return errIndexNotInteger(idx.Pos)
							}
						}
						lhsType = lt
					} else {
						idxType, err := checkExpr(idx.Start, env)
						if err != nil {
							return err
						}
						if _, ok := idxType.(IntType); !ok {
							if _, ok := idxType.(Int64Type); !ok {
								return errIndexNotInteger(idx.Pos)
							}
						}
						lhsType = lt.Elem
					}
				case StringType:
					if idx.Start == nil && idx.Colon == nil {
						return errMissingIndex(idx.Pos)
					}
					if idx.Start != nil {
						t, err := checkExpr(idx.Start, env)
						if err != nil {
							return err
						}
						if !(unify(t, IntType{}, nil) || unify(t, Int64Type{}, nil)) {
							return errIndexNotInteger(idx.Pos)
						}
					}
					if idx.End != nil {
						t, err := checkExpr(idx.End, env)
						if err != nil {
							return err
						}
						if !(unify(t, IntType{}, nil) || unify(t, Int64Type{}, nil)) {
							return errIndexNotInteger(idx.Pos)
						}
					}
					if idx.Colon != nil {
						lhsType = StringType{}
					} else {
						lhsType = StringType{}
					}
				case AnyType:
					lhsType = AnyType{}
				default:
					if IsAnyType(lhsType) {
						// Allow indexing on dynamic values in
						// assignments, propagating `any`.
						lhsType = AnyType{}
					} else {
						return errNotIndexable(s.Assign.Pos, lhsType)
					}
				}
			}
		}
		if len(s.Assign.Field) > 0 {
			for _, fop := range s.Assign.Field {
				field := fop.Name
				switch lt := lhsType.(type) {
				case StructType:
					ft, ok := lt.FieldType(field)
					if !ok {
						return errUnknownField(fop.Pos, field, lt)
					}
					lhsType = ft
				case MapType:
					if unify(lt.Key, StringType{}, nil) {
						lhsType = lt.Value
					} else {
						return errNotStruct(fop.Pos, lt)
					}
				case AnyType:
					lhsType = AnyType{}
				default:
					return errNotStruct(fop.Pos, lt)
				}
			}
		}
		rhsType, err := checkExprWithExpected(s.Assign.Value, env, lhsType)
		if err != nil {
			return err
		}
		// MEP-10 R1 also covers assignment: a non-option `int` flows
		// into an `int?` slot via auto-wrap. The let-binding path
		// already uses Assignable; mirror it here.
		if !unify(lhsType, rhsType, nil) && !Assignable(rhsType, lhsType) {
			return errCannotAssign(s.Assign.Pos, rhsType, s.Assign.Name, lhsType)
		}
		// MEP-10 B3d: when the LHS targets a slot reached through an
		// index or field (`bag[i]`, `r.items`) and the RHS names live
		// aggregate storage, the assignment aliases the RHS into a slot
		// with a widened element type. A later write through
		// `bag[i][j]` would deposit a value the RHS's static type
		// rejects, corrupting reads through the RHS name. Require
		// structural equality at the slot type. Fresh-value RHS
		// (literals, calls, computed values) keep working since
		// aliasSourceLabel only fires on live-storage paths.
		if len(s.Assign.Index) > 0 || len(s.Assign.Field) > 0 {
			if src := aliasSourceLabel(s.Assign.Value); src != "" {
				if isAliasableAggregate(rhsType) && isAliasableAggregate(lhsType) && !equalKinds(rhsType, lhsType) {
					return errAliasWidensElement(s.Assign.Pos, src, rhsType, lhsType)
				}
			}
			// MEP-10 B3e at index/field LHS: literal RHS may still
			// contain alias source elements (`bag[0] = [xs]`).
			if err := checkLiteralAliasElements(s.Assign.Value, env, lhsType); err != nil {
				return err
			}
		}
		if len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
			if ContainsAny(rhsType) {
				if _, ok := lhsType.(AnyType); ok {
					env.SetVar(s.Assign.Name, rhsType, true)
				}
			} else {
				env.SetVar(s.Assign.Name, rhsType, true)
			}
		}
		return nil

	case s.Fetch != nil:
		// type of the fetched value is unknown (any)
		if _, err := checkExprWithExpected(s.Fetch.URL, env, StringType{}); err != nil {
			return err
		}
		if s.Fetch.With != nil {
			if _, err := checkExpr(s.Fetch.With, env); err != nil {
				return err
			}
		}
		env.SetVar(s.Fetch.Target, AnyType{}, false)
		return nil

	case s.Update != nil:
		listType, err := env.GetVar(s.Update.Target)
		if err != nil {
			return errAssignUndeclared(s.Update.Pos, s.Update.Target)
		}
		lt, ok := listType.(ListType)
		if !ok {
			return errQuerySourceList(s.Update.Pos)
		}
		st, ok := lt.Elem.(StructType)
		if !ok {
			return fmt.Errorf("update element is not struct")
		}
		child := NewEnv(env)
		for _, f := range st.Fields {
			child.SetVar(f.Name, f.Type, true)
		}
		for _, item := range s.Update.Set.Items {
			if key, ok := stringKey(item.Key); ok {
				ft, ok2 := st.FieldType(key)
				if !ok2 {
					return errUnknownField(item.Pos, key, st)
				}
				vt, err := checkExpr(item.Value, child)
				if err != nil {
					return err
				}
				if !unify(ft, vt, nil) {
					return errTypeMismatch(item.Value.Pos, ft, vt)
				}
			}
		}
		if s.Update.Where != nil {
			wt, err := checkExprWithExpected(s.Update.Where, child, BoolType{})
			if err != nil {
				return err
			}
			if !unify(wt, BoolType{}, nil) {
				return errWhereBoolean(s.Update.Where.Pos)
			}
		}
		return nil

	case s.For != nil:
		// Check the loop expression (either a collection or a range start)
		sourceType, err := checkExprWithExpected(s.For.Source, env, nil)
		if err != nil {
			return err
		}

		var elemType Type

		if s.For.RangeEnd != nil {
			// It's a range loop: `for i in start..end`
			endType, err := checkExprWithExpected(s.For.RangeEnd, env, nil)
			if err != nil {
				return err
			}
			if !(unify(sourceType, IntType{}, nil) || unify(sourceType, Int64Type{}, nil)) ||
				!(unify(endType, IntType{}, nil) || unify(endType, Int64Type{}, nil)) {
				return errRangeRequiresInts(s.For.Pos)
			}
			// Range loop yields integers matching the input type.
			if _, ok := sourceType.(Int64Type); ok {
				elemType = Int64Type{}
			} else {
				elemType = IntType{}
			}
		} else {
			// MEP-5 §Inference for control flow [T-ForList, T-ForMap, T-ForStr]:
			// any other shape (including bare `any`) is T022.
			switch t := sourceType.(type) {
			case ListType:
				elemType = t.Elem
			case MapType:
				elemType = t.Key // loop iterates over keys
			case StringType:
				elemType = StringType{}
			default:
				return errCannotIterate(s.For.Pos, sourceType)
			}
		}

		// Create new scope for the loop variable
		child := NewEnv(env)
		child.SetVar(s.For.Name, elemType, true)

		// Check loop body
		for _, stmt := range s.For.Body {
			if err := checkStmt(stmt, child, expectedReturn, true); err != nil {
				return err
			}
		}
		return nil

	case s.Type != nil:
		if s.Type.Alias != nil {
			t := resolveTypeRef(s.Type.Alias, env)
			env.types[s.Type.Name] = t
			return nil
		}
		if len(s.Type.Members) > 0 {
			var fields []StructField
			methods := map[string]Method{}
			st := StructType{Name: s.Type.Name, Fields: fields, Methods: methods}
			env.SetStruct(s.Type.Name, st)
			env.types[s.Type.Name] = st
			// First pass: collect fields
			for _, m := range s.Type.Members {
				if m.Field != nil {
					fields = append(fields, StructField{Name: m.Field.Name, Type: resolveTypeRef(m.Field.Type, env)})
				}
			}
			// Precompute method types so they can reference each other.
			for _, m := range s.Type.Members {
				if m.Method != nil {
					params := make([]Type, len(m.Method.Params))
					for i, p := range m.Method.Params {
						if p.Type == nil {
							return errParamMissingType(m.Method.Pos, p.Name)
						}
						params[i] = resolveTypeRef(p.Type, env)
					}
					var ret Type = UnitType{}
					if m.Method.Return != nil {
						ret = resolveTypeRef(m.Method.Return, env)
					}
					methods[m.Method.Name] = Method{Decl: m.Method, Type: FuncType{Params: params, Return: ret}}
				}
			}
			// Second pass: check methods using the computed types.
			for _, m := range s.Type.Members {
				if m.Method != nil {
					meth := methods[m.Method.Name]
					params := meth.Type.Params
					ret := meth.Type.Return

					methodEnv := NewEnv(env)
					for _, f := range fields {
						methodEnv.SetVar(f.Name, f.Type, true)
					}
					for name, mt := range methods {
						methodEnv.SetVar(name, mt.Type, true)
					}
					for i, p := range m.Method.Params {
						methodEnv.SetVar(p.Name, params[i], true)
					}
					for _, stmt := range m.Method.Body {
						if err := checkStmt(stmt, methodEnv, ret, false); err != nil {
							return err
						}
					}
					methodFn := &parser.FunStmt{Params: m.Method.Params, Return: m.Method.Return, Body: m.Method.Body}
					methods[m.Method.Name] = Method{Decl: m.Method, Type: FuncType{Params: params, Return: ret, Effects: inferFunctionEffects(methodFn, methodEnv)}}
				}
			}
			st.Fields = fields
			st.Methods = methods
			env.SetStruct(s.Type.Name, st)
			env.types[s.Type.Name] = st
			return nil
		}
		if len(s.Type.Variants) > 0 {
			// Build the union with a shared variants map so recursive
			// references resolve correctly as variants are populated.
			variants := map[string]StructType{}
			variantOrder := make([]string, 0, len(s.Type.Variants))
			ut := UnionType{Name: s.Type.Name, Variants: variants, Order: variantOrder}
			env.SetUnion(s.Type.Name, ut)
			env.types[s.Type.Name] = ut

			for _, v := range s.Type.Variants {
				var vf []StructField
				for _, f := range v.Fields {
					vf = append(vf, StructField{Name: f.Name, Type: resolveTypeRef(f.Type, env)})
				}
				st := StructType{Name: v.Name, Fields: vf}
				variants[v.Name] = st
				variantOrder = append(variantOrder, v.Name)
				env.SetStruct(v.Name, st)
				params := make([]Type, 0, len(v.Fields))
				for _, f := range v.Fields {
					params = append(params, resolveTypeRef(f.Type, env))
				}
				env.SetFuncType(v.Name, FuncType{Params: params, Return: UnionType{Name: s.Type.Name, Variants: nil}})
				if len(params) == 0 {
					env.SetVar(v.Name, UnionType{Name: s.Type.Name, Variants: nil}, false)
				}
			}
			ut.Order = variantOrder
			env.SetUnion(s.Type.Name, ut)
			env.types[s.Type.Name] = ut
			return nil
		}
		return nil

	case s.Model != nil:
		for _, f := range s.Model.Fields {
			if _, err := checkExpr(f.Value, env); err != nil {
				return err
			}
		}
		return nil

	case s.Fun != nil:
		name := s.Fun.Name
		sigEnv := env
		if len(s.Fun.TypeParams) > 0 {
			sigEnv = NewEnv(env)
			for _, tp := range s.Fun.TypeParams {
				sigEnv.SetTypeParam(tp, &TypeVar{Name: tp})
			}
		}
		params := []Type{}
		for _, p := range s.Fun.Params {
			if p.Type == nil {
				// Permit functions without explicit parameter
				// annotations by defaulting them to `any`.
				params = append(params, AnyType{})
			} else {
				params = append(params, resolveTypeRef(p.Type, sigEnv))
			}
		}
		var ret Type = AnyType{}
		if s.Fun.Return != nil {
			ret = resolveTypeRef(s.Fun.Return, sigEnv)
		}
		// Top-level FunStmts already had their FuncType registered by
		// the Check pre-pass, where T064 and T065 diagnostics fired.
		// Nested FunStmts run the inference walker here and inherit
		// the declared set (if any) silently; their effects propagate
		// to outer callers through the next inference pass.
		effects := inferFunctionEffects(s.Fun, env)
		if len(s.Fun.Effects) > 0 {
			declared, _ := parseDeclaredEffects(s.Fun)
			effects = declared
		}
		env.SetVar(name, FuncType{
			Params:     params,
			Return:     ret,
			Effects:    effects,
			TypeParams: append([]string(nil), s.Fun.TypeParams...),
		}, false)
		env.SetFunc(name, s.Fun)

		child := NewEnv(sigEnv)
		for i, p := range s.Fun.Params {
			child.SetVar(p.Name, params[i], true)
		}
		for _, stmt := range s.Fun.Body {
			if err := checkStmt(stmt, child, ret, false); err != nil {
				return err
			}
		}
		return nil

	case s.Expr != nil:
		_, err := checkExprWithExpected(s.Expr.Expr, env, nil)
		return err

	case s.Return != nil:
		if s.Return.Value == nil {
			if !unify(UnitType{}, expectedReturn, nil) {
				return errReturnMismatch(s.Return.Pos, expectedReturn, UnitType{})
			}
			return nil
		}
		actual, err := checkExprWithExpected(s.Return.Value, env, expectedReturn)
		if err != nil {
			return err
		}
		if !unify(actual, expectedReturn, nil) {
			return errReturnMismatch(s.Return.Pos, expectedReturn, actual)
		}
		return nil

	case s.Test != nil:
		child := NewEnv(env)
		for _, stmt := range s.Test.Body {
			if err := checkStmt(stmt, child, expectedReturn, false); err != nil {
				return err
			}
		}
		return nil

	case s.Bench != nil:
		child := NewEnv(env)
		for _, stmt := range s.Bench.Body {
			if err := checkStmt(stmt, child, expectedReturn, false); err != nil {
				return err
			}
		}
		return nil

	case s.Expect != nil:
		t, err := checkExprWithExpected(s.Expect.Value, env, BoolType{})
		if err != nil {
			return err
		}
		if !unify(t, BoolType{}, nil) {
			return errExpectBoolean(s.Expect.Pos)
		}
		return nil

	case s.If != nil:
		return checkIfStmt(s.If, env, expectedReturn, inLoop)

	case s.While != nil:
		condT, err := checkExprWithExpected(s.While.Cond, env, nil)
		if err != nil {
			return err
		}
		if !unify(condT, BoolType{}, nil) {
			return errIfCondBoolean(s.While.Cond.Pos)
		}
		child := NewEnv(env)
		for _, stmt := range s.While.Body {
			if err := checkStmt(stmt, child, expectedReturn, true); err != nil {
				return err
			}
		}
		return nil

	case s.Break != nil:
		if !inLoop {
			return errBreakContinueOutsideLoop(s.Break.Pos, "break")
		}
		return nil

	case s.Continue != nil:
		if !inLoop {
			return errBreakContinueOutsideLoop(s.Continue.Pos, "continue")
		}
		return nil
	}
	return nil
}

func checkIfStmt(stmt *parser.IfStmt, env *Env, expectedReturn Type, inLoop bool) error {
	condT, err := checkExprWithExpected(stmt.Cond, env, nil)
	if err != nil {
		return err
	}
	if !unify(condT, BoolType{}, nil) {
		return errIfCondBoolean(stmt.Cond.Pos)
	}
	// MEP-16 N1: same narrowing the if-expression path applies. The
	// then-block sees the truthy narrowing and the else-block sees the
	// falsy narrowing of bare option-typed bindings compared to `none`.
	truthy, falsy := optionNarrowing(stmt.Cond, env)
	child := NewEnv(narrowedEnv(env, truthy))
	for _, s := range stmt.Then {
		if err := checkStmt(s, child, expectedReturn, inLoop); err != nil {
			return err
		}
		// MEP-16 N5: after a statement whose evaluation could call a
		// non-pure function, drop every `var` narrowing visible from
		// child. `let` bindings keep narrowing because the target
		// cannot be reassigned.
		if statementHasImpureCall(s, child) {
			dropMutableNarrowings(child)
		}
	}
	if stmt.ElseIf != nil {
		return checkIfStmt(stmt.ElseIf, narrowedEnv(env, falsy), expectedReturn, inLoop)
	}
	elseChild := NewEnv(narrowedEnv(env, falsy))
	for _, s := range stmt.Else {
		if err := checkStmt(s, elseChild, expectedReturn, inLoop); err != nil {
			return err
		}
		if statementHasImpureCall(s, elseChild) {
			dropMutableNarrowings(elseChild)
		}
	}
	return nil
}

