package lower

import (
	"fmt"
	"strconv"
	"strings"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/go/gotree"
)

// lowerExpr maps an aotir.Expr to a gotree.Expr.
func (l *lowerer) lowerExpr(e aotir.Expr) (gotree.Expr, error) {
	switch e := e.(type) {
	case *aotir.StringLit:
		return &gotree.BasicLit{Kind: gotree.StringLit, Value: e.Value}, nil
	case *aotir.IntLit:
		// Mochi int -> Go int64. Wrap the literal in
		// int64(N) so call sites that take int64 parameters
		// type-check without context-dependent inference.
		return &gotree.CallExpr{
			Fun:  &gotree.Ident{Name: "int64"},
			Args: []gotree.Expr{&gotree.BasicLit{Kind: gotree.IntLit, Value: strconv.FormatInt(e.Value, 10)}},
		}, nil
	case *aotir.FloatLit:
		return l.lowerFloatLit(e.Value), nil
	case *aotir.BoolLit:
		name := "false"
		if e.Value {
			name = "true"
		}
		return &gotree.Ident{Name: name}, nil
	case *aotir.VarRef:
		return varRefExpr(e.Name), nil
	case *aotir.BinaryExpr:
		return l.lowerBinary(e)
	case *aotir.UnaryExpr:
		return l.lowerUnary(e)
	case *aotir.ListLit:
		return l.lowerListLit(e)
	case *aotir.IndexExpr:
		return l.lowerIndexExpr(e)
	case *aotir.LenExpr:
		return l.lowerLenExpr(e)
	case *aotir.AppendExpr:
		return l.lowerAppendExpr(e)
	case *aotir.MapLit:
		return l.lowerMapLit(e)
	case *aotir.MapGetExpr:
		return l.lowerMapGetExpr(e)
	case *aotir.MapHasExpr:
		return l.lowerMapHasExpr(e)
	case *aotir.MapLenExpr:
		return l.lowerMapLenExpr(e)
	case *aotir.MapKeysExpr:
		return l.lowerMapKeysExpr(e)
	case *aotir.MapValuesExpr:
		return l.lowerMapValuesExpr(e)
	case *aotir.SetLiteralExpr:
		return l.lowerSetLit(e)
	case *aotir.SetAddExpr:
		return l.lowerSetAdd(e)
	case *aotir.SetHasExpr:
		return l.lowerSetHas(e)
	case *aotir.SetLenExpr:
		return l.lowerSetLen(e)
	case *aotir.SetToListExpr:
		return l.lowerSetToList(e)
	case *aotir.RecordLit:
		return l.lowerRecordLit(e)
	case *aotir.FieldAccess:
		return l.lowerFieldAccess(e)
	case *aotir.VariantLit:
		return l.lowerVariantLit(e)
	case *aotir.UnionVarRef:
		return varRefExpr(e.Name), nil
	case *aotir.VariantFieldAccess:
		recv, err := l.lowerExpr(e.Receiver)
		if err != nil {
			return nil, fmt.Errorf("variant field access receiver: %w", err)
		}
		return &gotree.SelectorExpr{X: recv, Sel: variantFieldName(e.VariantName, e.FieldName)}, nil
	case *aotir.CallExpr:
		return l.lowerCallExpr(e)
	case *aotir.FunLit:
		return l.lowerFunLit(e)
	case *aotir.FunCallExpr:
		return l.lowerFunCallExpr(e)
	case *aotir.StrLenExpr:
		return l.lowerStrLenExpr(e)
	case *aotir.StrContainsExpr:
		return l.lowerStrContainsExpr(e)
	case *aotir.StrConvertExpr:
		return l.lowerStrConvertExpr(e)
	case *aotir.ListSumExpr:
		return l.lowerListSumExpr(e)
	case *aotir.ListMinExpr:
		return l.lowerListMinExpr(e)
	case *aotir.ListMaxExpr:
		return l.lowerListMaxExpr(e)
	case *aotir.ListContainsExpr:
		return l.lowerListContainsExpr(e)
	case *aotir.NumCastExpr:
		return l.lowerNumCastExpr(e)
	case *aotir.StrUpperExpr:
		return l.lowerStrUpperLower(e.Receiver, "ToUpper")
	case *aotir.StrLowerExpr:
		return l.lowerStrUpperLower(e.Receiver, "ToLower")
	case *aotir.StrIndexExpr:
		return l.lowerStrIndexExpr(e)
	case *aotir.StrSubstringExpr:
		return l.lowerStrSubstringExpr(e)
	case *aotir.StrReverseExpr:
		return l.lowerStrReverseExpr(e)
	case *aotir.StrSplitExpr:
		return l.lowerStrSplitExpr(e)
	case *aotir.StrJoinExpr:
		return l.lowerStrJoinExpr(e)
	case *aotir.MathCallExpr:
		return l.lowerMathCallExpr(e)
	case *aotir.ReadFileExpr:
		return l.lowerReadFileExpr(e)
	case *aotir.LinesExpr:
		return l.lowerLinesExpr(e)
	case *aotir.LoadCSVExpr:
		return l.lowerLoadCSVExpr(e)
	case *aotir.OMapLiteralExpr:
		return l.lowerOMapLiteralExpr(e)
	case *aotir.OMapGetExpr:
		return l.lowerOMapGetExpr(e)
	case *aotir.OMapSetExpr:
		return l.lowerOMapSetExpr(e)
	case *aotir.OMapHasExpr:
		return l.lowerOMapHasExpr(e)
	case *aotir.OMapLenExpr:
		return l.lowerOMapLenExpr(e)
	case *aotir.ListMapExpr:
		return l.lowerListMapExpr(e)
	case *aotir.ListFilterExpr:
		return l.lowerListFilterExpr(e)
	case *aotir.ListFoldlExpr:
		return l.lowerListFoldlExpr(e)
	case *aotir.DatalogQueryExpr:
		return l.lowerDatalogQueryExpr(e)
	default:
		return nil, fmt.Errorf("transpiler3/go/lower: does not handle expr %T", e)
	}
}

// lowerStrUpperLower emits `strings.ToUpper(s)` / `strings.ToLower(s)`
// for the matching Mochi `upper(s)` / `lower(s)` builtins. The
// stdlib helpers apply Unicode case folding, matching the C runtime's
// per-rune transformation.
func (l *lowerer) lowerStrUpperLower(recv aotir.Expr, fn string) (gotree.Expr, error) {
	r, err := l.lowerExpr(recv)
	if err != nil {
		return nil, err
	}
	l.addImport("strings")
	return &gotree.CallExpr{
		Fun:  &gotree.SelectorExpr{X: &gotree.Ident{Name: "strings"}, Sel: fn},
		Args: []gotree.Expr{r},
	}, nil
}

// lowerStrIndexExpr emits `mochiStrIndex(s, i)`. The helper returns
// the single-character string at rune index i (Mochi indexing is
// rune-based, matching the C runtime). int64 -> int conversion lives
// inside the helper so the call site stays compact.
func (l *lowerer) lowerStrIndexExpr(e *aotir.StrIndexExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	idx, err := l.lowerExpr(e.Index)
	if err != nil {
		return nil, err
	}
	l.addHelper("mochiStrIndex")
	return &gotree.CallExpr{Fun: &gotree.Ident{Name: "mochiStrIndex"}, Args: []gotree.Expr{recv, idx}}, nil
}

// lowerStrSubstringExpr emits `mochiStrSubstring(s, start, end)`. The
// helper takes a rune-based half-open slice, matching the C runtime's
// mochi_str_substring. Both bounds are clamped to the string length.
func (l *lowerer) lowerStrSubstringExpr(e *aotir.StrSubstringExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	start, err := l.lowerExpr(e.Start)
	if err != nil {
		return nil, err
	}
	end, err := l.lowerExpr(e.End)
	if err != nil {
		return nil, err
	}
	l.addHelper("mochiStrSubstring")
	return &gotree.CallExpr{Fun: &gotree.Ident{Name: "mochiStrSubstring"}, Args: []gotree.Expr{recv, start, end}}, nil
}

// lowerStrReverseExpr emits `mochiStrReverse(s)`, which reverses by
// runes so multibyte characters survive the round trip.
func (l *lowerer) lowerStrReverseExpr(e *aotir.StrReverseExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	l.addHelper("mochiStrReverse")
	return &gotree.CallExpr{Fun: &gotree.Ident{Name: "mochiStrReverse"}, Args: []gotree.Expr{recv}}, nil
}

// lowerStrSplitExpr emits `strings.Split(s, sep)` for the Mochi
// `split(s, sep)` builtin.
func (l *lowerer) lowerStrSplitExpr(e *aotir.StrSplitExpr) (gotree.Expr, error) {
	s, err := l.lowerExpr(e.Str)
	if err != nil {
		return nil, err
	}
	sep, err := l.lowerExpr(e.Sep)
	if err != nil {
		return nil, err
	}
	l.addImport("strings")
	return &gotree.CallExpr{
		Fun:  &gotree.SelectorExpr{X: &gotree.Ident{Name: "strings"}, Sel: "Split"},
		Args: []gotree.Expr{s, sep},
	}, nil
}

// lowerStrJoinExpr emits `strings.Join(xs, sep)` for the Mochi
// `join(xs, sep)` builtin (xs must be list<string>).
func (l *lowerer) lowerStrJoinExpr(e *aotir.StrJoinExpr) (gotree.Expr, error) {
	xs, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, err
	}
	sep, err := l.lowerExpr(e.Sep)
	if err != nil {
		return nil, err
	}
	l.addImport("strings")
	return &gotree.CallExpr{
		Fun:  &gotree.SelectorExpr{X: &gotree.Ident{Name: "strings"}, Sel: "Join"},
		Args: []gotree.Expr{xs, sep},
	}, nil
}

// varRefExpr maps an aotir.VarRef.Name to a gotree expression. The
// shared C lowerer rewrites captured-variable references inside lifted
// closure bodies to `__e->FieldName` (C member-access syntax). For Go
// we translate that back to `__mochi_env.FieldName` (with FieldName
// capitalised so it matches the env struct's exported field).
func varRefExpr(name string) gotree.Expr {
	if rest, ok := strings.CutPrefix(name, "__e->"); ok {
		return &gotree.SelectorExpr{
			X:   &gotree.Ident{Name: envParamName},
			Sel: exportIdent(rest),
		}
	}
	return &gotree.Ident{Name: mangleIdent(name)}
}

// lowerFunLit emits a Go value of the function type carried by e.Sig.
//
// Non-capturing (no Captures): the lifted function has no env
// parameter, so the bare function name is itself a Go value of the
// right type.
//
// Capturing (len(Captures) > 0): the lifted function takes an env
// pointer as its first parameter. We emit an IIFE that materialises
// the env struct once from the captured outer-scope variables and
// returns a closure wrapper that threads the pointer on each call:
//
//	func() func(p1 T1) R {
//	    __env := &__anon_N_env_t{Field1: src1, Field2: src2}
//	    return func(p1 T1) R { return __anon_N(__env, p1) }
//	}()
//
// The IIFE form keeps the env allocation co-located with the FunLit
// regardless of whether the upstream C lowerer paired the literal with
// a ClosureEnvStmt (the `return fun(x) => ...` path does not).
func (l *lowerer) lowerFunLit(e *aotir.FunLit) (gotree.Expr, error) {
	if len(e.Captures) == 0 {
		return &gotree.Ident{Name: e.FuncName}, nil
	}
	if e.Sig == nil {
		return nil, fmt.Errorf("FunLit %s: missing Sig", e.FuncName)
	}
	if e.EnvTypeName == "" {
		return nil, fmt.Errorf("FunLit %s: capturing closure missing EnvTypeName", e.FuncName)
	}
	envLocal := "__env"
	elts := make([]gotree.Expr, 0, len(e.Captures))
	for _, c := range e.Captures {
		elts = append(elts, &gotree.KeyValueExpr{
			Key:   &gotree.Ident{Name: exportIdent(c.FieldName)},
			Value: varRefExpr(c.SrcName),
		})
	}
	envAlloc := &gotree.AssignStmt{
		Lhs: []gotree.Expr{&gotree.Ident{Name: envLocal}},
		Tok: ":=",
		Rhs: []gotree.Expr{&gotree.UnaryExpr{
			Op: "&",
			X: &gotree.CompositeLit{
				Type: &gotree.Ident{Name: e.EnvTypeName},
				Elts: elts,
			},
		}},
	}

	wrapper, err := l.funLitWrapper(e, envLocal)
	if err != nil {
		return nil, err
	}
	resultText, err := l.lowerFunType(e.Sig)
	if err != nil {
		return nil, fmt.Errorf("FunLit %s result type: %w", e.FuncName, err)
	}

	outerFn := &gotree.FuncLit{
		Type: &gotree.FuncType{Results: []gotree.Field{{Type: &gotree.Ident{Name: resultText}}}},
		Body: &gotree.BlockStmt{List: []gotree.Stmt{
			envAlloc,
			&gotree.ReturnStmt{Results: []gotree.Expr{wrapper}},
		}},
	}
	return &gotree.CallExpr{Fun: outerFn}, nil
}

// funLitWrapper builds the inner closure that forwards each call into
// the lifted function with envExpr threaded as the first argument.
func (l *lowerer) funLitWrapper(e *aotir.FunLit, envIdent string) (*gotree.FuncLit, error) {
	params := make([]gotree.Field, 0, len(e.Sig.ParamTypes))
	args := []gotree.Expr{&gotree.Ident{Name: envIdent}}
	for i, pt := range e.Sig.ParamTypes {
		pname := fmt.Sprintf("__p%d", i)
		ptText, err := l.lowerType(pt)
		if err != nil {
			return nil, fmt.Errorf("FunLit %s param %d: %w", e.FuncName, i, err)
		}
		if ptText == "" {
			return nil, fmt.Errorf("FunLit %s param %d: unit not allowed", e.FuncName, i)
		}
		params = append(params, gotree.Field{
			Names: []string{pname},
			Type:  &gotree.Ident{Name: ptText},
		})
		args = append(args, &gotree.Ident{Name: pname})
	}
	ft := &gotree.FuncType{Params: params}
	call := &gotree.CallExpr{Fun: &gotree.Ident{Name: e.FuncName}, Args: args}
	var bodyStmt gotree.Stmt
	if e.Sig.ReturnType == aotir.TypeUnit {
		bodyStmt = &gotree.ExprStmt{X: call}
	} else {
		rt, err := l.lowerType(e.Sig.ReturnType)
		if err != nil {
			return nil, fmt.Errorf("FunLit %s return: %w", e.FuncName, err)
		}
		ft.Results = []gotree.Field{{Type: &gotree.Ident{Name: rt}}}
		bodyStmt = &gotree.ReturnStmt{Results: []gotree.Expr{call}}
	}
	return &gotree.FuncLit{
		Type: ft,
		Body: &gotree.BlockStmt{List: []gotree.Stmt{bodyStmt}},
	}, nil
}

// lowerFunCallExpr emits `callee(arg0, arg1, ...)` for a call through
// a function-typed value. The callee is itself any expression that
// lowers to a Go func value (a VarRef bound to TypeFun, a FunLit, or a
// CallExpr returning TypeFun).
func (l *lowerer) lowerFunCallExpr(e *aotir.FunCallExpr) (gotree.Expr, error) {
	callee, err := l.lowerExpr(e.Callee)
	if err != nil {
		return nil, fmt.Errorf("FunCallExpr callee: %w", err)
	}
	args := make([]gotree.Expr, 0, len(e.Args))
	for i, a := range e.Args {
		v, err := l.lowerExpr(a)
		if err != nil {
			return nil, fmt.Errorf("FunCallExpr arg %d: %w", i, err)
		}
		args = append(args, v)
	}
	return &gotree.CallExpr{Fun: callee, Args: args}, nil
}

// lowerCallExpr emits a Go call expression for a user-defined function.
// Builtins like the print family are surfaced as CallStmts, so any
// CallExpr that reaches the lowerer is a user-defined Function whose
// mangled name is already stored verbatim in Func.
func (l *lowerer) lowerCallExpr(e *aotir.CallExpr) (gotree.Expr, error) {
	args := make([]gotree.Expr, 0, len(e.Args))
	for i, a := range e.Args {
		v, err := l.lowerExpr(a)
		if err != nil {
			return nil, fmt.Errorf("call %s arg %d: %w", e.Func, i, err)
		}
		args = append(args, v)
	}
	return &gotree.CallExpr{
		Fun:  &gotree.Ident{Name: e.Func},
		Args: args,
	}, nil
}

// lowerRecordLit emits a struct composite literal with explicit
// field-name initialisers, e.g. `Point{X: 1, Y: 2}`. Field order
// follows RecordLit.Fields (already reordered into source-declared
// order by the aotir lowerer).
func (l *lowerer) lowerRecordLit(e *aotir.RecordLit) (gotree.Expr, error) {
	elts := make([]gotree.Expr, 0, len(e.Fields))
	for _, f := range e.Fields {
		v, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, fmt.Errorf("record %s field %s: %w", e.TypeName, f.Name, err)
		}
		elts = append(elts, &gotree.KeyValueExpr{
			Key:   &gotree.Ident{Name: exportIdent(f.Name)},
			Value: v,
		})
	}
	return &gotree.CompositeLit{
		Type: &gotree.Ident{Name: e.TypeName},
		Elts: elts,
	}, nil
}

// lowerFieldAccess emits `recv.Field` (with Field capitalised so the
// underlying Go struct field is exported).
func (l *lowerer) lowerFieldAccess(e *aotir.FieldAccess) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("field access receiver: %w", err)
	}
	return &gotree.SelectorExpr{X: recv, Sel: exportIdent(e.FieldName)}, nil
}

// lowerVariantLit emits a call to the variant's constructor.
func (l *lowerer) lowerVariantLit(e *aotir.VariantLit) (gotree.Expr, error) {
	args := make([]gotree.Expr, 0, len(e.Fields))
	for _, f := range e.Fields {
		v, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, fmt.Errorf("variant %s field %s: %w", e.VariantName, f.Name, err)
		}
		args = append(args, v)
	}
	return &gotree.CallExpr{
		Fun:  &gotree.Ident{Name: variantCtorName(e.UnionName, e.VariantName)},
		Args: args,
	}, nil
}

// lowerSetLit emits an IIFE that builds the set with sequential
// assignments. The IIFE form, instead of a composite literal,
// handles duplicate elements (which Mochi allows in `set{...}`
// source but Go rejects as duplicate map keys).
func (l *lowerer) lowerSetLit(e *aotir.SetLiteralExpr) (gotree.Expr, error) {
	setType, err := l.lowerSetType(e.ElemType)
	if err != nil {
		return nil, fmt.Errorf("set literal: %w", err)
	}
	stmts := []gotree.Stmt{
		&gotree.AssignStmt{
			Lhs: []gotree.Expr{&gotree.Ident{Name: "s"}},
			Tok: ":=",
			Rhs: []gotree.Expr{&gotree.CompositeLit{Type: &gotree.RawExpr{Src: setType}}},
		},
	}
	for i, x := range e.Elems {
		ge, err := l.lowerExpr(x)
		if err != nil {
			return nil, fmt.Errorf("set literal elem %d: %w", i, err)
		}
		stmts = append(stmts, &gotree.AssignStmt{
			Lhs: []gotree.Expr{&gotree.IndexExpr{X: &gotree.Ident{Name: "s"}, Index: ge}},
			Tok: "=",
			Rhs: []gotree.Expr{&gotree.RawExpr{Src: "struct{}{}"}},
		})
	}
	stmts = append(stmts, &gotree.ReturnStmt{Results: []gotree.Expr{&gotree.Ident{Name: "s"}}})
	return &gotree.CallExpr{
		Fun: &gotree.FuncLit{
			Type: &gotree.FuncType{Results: []gotree.Field{{Type: &gotree.RawExpr{Src: setType}}}},
			Body: &gotree.BlockStmt{List: stmts},
		},
	}, nil
}

// lowerSetAdd emits an IIFE that clones the receiver and inserts
// the element, matching Mochi's pure-set semantics.
func (l *lowerer) lowerSetAdd(e *aotir.SetAddExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	elem, err := l.lowerExpr(e.Elem)
	if err != nil {
		return nil, err
	}
	setType, err := l.lowerSetType(e.ElemType)
	if err != nil {
		return nil, err
	}
	l.addImport("maps")
	body := &gotree.BlockStmt{List: []gotree.Stmt{
		&gotree.AssignStmt{
			Lhs: []gotree.Expr{&gotree.Ident{Name: "out"}},
			Tok: ":=",
			Rhs: []gotree.Expr{&gotree.CallExpr{
				Fun:  &gotree.SelectorExpr{X: &gotree.Ident{Name: "maps"}, Sel: "Clone"},
				Args: []gotree.Expr{recv},
			}},
		},
		&gotree.AssignStmt{
			Lhs: []gotree.Expr{&gotree.IndexExpr{X: &gotree.Ident{Name: "out"}, Index: elem}},
			Tok: "=",
			Rhs: []gotree.Expr{&gotree.RawExpr{Src: "struct{}{}"}},
		},
		&gotree.ReturnStmt{Results: []gotree.Expr{&gotree.Ident{Name: "out"}}},
	}}
	return &gotree.CallExpr{
		Fun: &gotree.FuncLit{
			Type: &gotree.FuncType{Results: []gotree.Field{{Type: &gotree.RawExpr{Src: setType}}}},
			Body: body,
		},
	}, nil
}

// lowerSetHas emits the same IIFE pattern as MapHasExpr.
func (l *lowerer) lowerSetHas(e *aotir.SetHasExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	elem, err := l.lowerExpr(e.Elem)
	if err != nil {
		return nil, err
	}
	return &gotree.CallExpr{
		Fun: &gotree.FuncLit{
			Type: &gotree.FuncType{Results: []gotree.Field{{Type: &gotree.Ident{Name: "bool"}}}},
			Body: &gotree.BlockStmt{List: []gotree.Stmt{
				&gotree.AssignStmt{
					Lhs: []gotree.Expr{&gotree.Ident{Name: "_"}, &gotree.Ident{Name: "ok"}},
					Tok: ":=",
					Rhs: []gotree.Expr{&gotree.IndexExpr{X: recv, Index: elem}},
				},
				&gotree.ReturnStmt{Results: []gotree.Expr{&gotree.Ident{Name: "ok"}}},
			}},
		},
	}, nil
}

// lowerSetLen emits `int64(len(s))`.
func (l *lowerer) lowerSetLen(e *aotir.SetLenExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &gotree.CallExpr{
		Fun: &gotree.Ident{Name: "int64"},
		Args: []gotree.Expr{&gotree.CallExpr{
			Fun:  &gotree.Ident{Name: "len"},
			Args: []gotree.Expr{recv},
		}},
	}, nil
}

// lowerSetToList emits `slices.Sorted(maps.Keys(s))` so iteration
// order matches Mochi's sorted-on-iteration semantics. Bool-element
// sets fall back to a manual sorted enumeration because cmp.Ordered
// excludes bool.
func (l *lowerer) lowerSetToList(e *aotir.SetToListExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	if e.ElemType == aotir.TypeBool {
		return l.boolSetToList(recv), nil
	}
	l.addImport("maps")
	l.addImport("slices")
	return &gotree.CallExpr{
		Fun: &gotree.SelectorExpr{X: &gotree.Ident{Name: "slices"}, Sel: "Sorted"},
		Args: []gotree.Expr{&gotree.CallExpr{
			Fun:  &gotree.SelectorExpr{X: &gotree.Ident{Name: "maps"}, Sel: "Keys"},
			Args: []gotree.Expr{recv},
		}},
	}, nil
}

// boolSetToList returns a slice in canonical false-then-true order
// for a set<bool>, since Go's cmp.Ordered does not include bool.
// The IIFE binds the receiver to `s` once so re-emitting the
// receiver subtree across multiple statements is safe.
func (l *lowerer) boolSetToList(recv gotree.Expr) gotree.Expr {
	body := &gotree.BlockStmt{List: []gotree.Stmt{
		&gotree.AssignStmt{
			Lhs: []gotree.Expr{&gotree.Ident{Name: "s"}},
			Tok: ":=",
			Rhs: []gotree.Expr{recv},
		},
		&gotree.AssignStmt{
			Lhs: []gotree.Expr{&gotree.Ident{Name: "out"}},
			Tok: ":=",
			Rhs: []gotree.Expr{&gotree.CallExpr{
				Fun: &gotree.Ident{Name: "make"},
				Args: []gotree.Expr{
					&gotree.RawExpr{Src: "[]bool"},
					&gotree.BasicLit{Kind: gotree.IntLit, Value: "0"},
					&gotree.CallExpr{Fun: &gotree.Ident{Name: "len"}, Args: []gotree.Expr{&gotree.Ident{Name: "s"}}},
				},
			}},
		},
		boolSetAppendIf("false"),
		boolSetAppendIf("true"),
		&gotree.ReturnStmt{Results: []gotree.Expr{&gotree.Ident{Name: "out"}}},
	}}
	return &gotree.CallExpr{
		Fun: &gotree.FuncLit{
			Type: &gotree.FuncType{Results: []gotree.Field{{Type: &gotree.RawExpr{Src: "[]bool"}}}},
			Body: body,
		},
	}
}

// boolSetAppendIf builds `if _, ok := s[BOOL]; ok { out = append(out, BOOL) }`.
func boolSetAppendIf(bool_ string) gotree.Stmt {
	return &gotree.IfStmt{
		Init: &gotree.AssignStmt{
			Lhs: []gotree.Expr{&gotree.Ident{Name: "_"}, &gotree.Ident{Name: "ok"}},
			Tok: ":=",
			Rhs: []gotree.Expr{&gotree.IndexExpr{X: &gotree.Ident{Name: "s"}, Index: &gotree.Ident{Name: bool_}}},
		},
		Cond: &gotree.Ident{Name: "ok"},
		Body: &gotree.BlockStmt{List: []gotree.Stmt{
			&gotree.AssignStmt{
				Lhs: []gotree.Expr{&gotree.Ident{Name: "out"}},
				Tok: "=",
				Rhs: []gotree.Expr{&gotree.CallExpr{
					Fun:  &gotree.Ident{Name: "append"},
					Args: []gotree.Expr{&gotree.Ident{Name: "out"}, &gotree.Ident{Name: bool_}},
				}},
			},
		}},
	}
}

// lowerMapLit emits `map[K]V{k0: v0, k1: v1, ...}`.
func (l *lowerer) lowerMapLit(e *aotir.MapLit) (gotree.Expr, error) {
	mapType, err := l.lowerMapTypeWithList(e.KeyType, e.ValueType, e.ListValueElemType)
	if err != nil {
		return nil, fmt.Errorf("map literal: %w", err)
	}
	if len(e.Keys) != len(e.Values) {
		return nil, fmt.Errorf("map literal: %d keys vs %d values", len(e.Keys), len(e.Values))
	}
	elts := make([]gotree.Expr, 0, len(e.Keys))
	for i := range e.Keys {
		k, err := l.lowerExpr(e.Keys[i])
		if err != nil {
			return nil, fmt.Errorf("map literal key %d: %w", i, err)
		}
		v, err := l.lowerExpr(e.Values[i])
		if err != nil {
			return nil, fmt.Errorf("map literal value %d: %w", i, err)
		}
		elts = append(elts, &gotree.KeyValueExpr{Key: k, Value: v})
	}
	return &gotree.CompositeLit{
		Type: &gotree.RawExpr{Src: mapType},
		Elts: elts,
	}, nil
}

// lowerMapGetExpr emits `m[k]`. Mochi requires the key to be
// present; the verifier ensures this via prior MapHasExpr guards.
func (l *lowerer) lowerMapGetExpr(e *aotir.MapGetExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	key, err := l.lowerExpr(e.Key)
	if err != nil {
		return nil, err
	}
	return &gotree.IndexExpr{X: recv, Index: key}, nil
}

// lowerMapHasExpr emits a `func() bool { _, ok := m[k]; return ok }()`
// IIFE so the result composes inside larger expressions.
func (l *lowerer) lowerMapHasExpr(e *aotir.MapHasExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	key, err := l.lowerExpr(e.Key)
	if err != nil {
		return nil, err
	}
	return &gotree.CallExpr{
		Fun: &gotree.FuncLit{
			Type: &gotree.FuncType{Results: []gotree.Field{{Type: &gotree.Ident{Name: "bool"}}}},
			Body: &gotree.BlockStmt{List: []gotree.Stmt{
				&gotree.AssignStmt{
					Lhs: []gotree.Expr{&gotree.Ident{Name: "_"}, &gotree.Ident{Name: "ok"}},
					Tok: ":=",
					Rhs: []gotree.Expr{&gotree.IndexExpr{X: recv, Index: key}},
				},
				&gotree.ReturnStmt{Results: []gotree.Expr{&gotree.Ident{Name: "ok"}}},
			}},
		},
	}, nil
}

// lowerMapLenExpr emits `int64(len(m))`.
func (l *lowerer) lowerMapLenExpr(e *aotir.MapLenExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &gotree.CallExpr{
		Fun: &gotree.Ident{Name: "int64"},
		Args: []gotree.Expr{&gotree.CallExpr{
			Fun:  &gotree.Ident{Name: "len"},
			Args: []gotree.Expr{recv},
		}},
	}, nil
}

// lowerMapKeysExpr emits `slices.Sorted(maps.Keys(m))`. Mochi's
// keys()/values() return sorted lists so byte-equal stdout matches
// the vm; Go 1.23+ gives us slices.Sorted + maps.Keys natively.
func (l *lowerer) lowerMapKeysExpr(e *aotir.MapKeysExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	l.addImport("maps")
	l.addImport("slices")
	return &gotree.CallExpr{
		Fun: &gotree.SelectorExpr{X: &gotree.Ident{Name: "slices"}, Sel: "Sorted"},
		Args: []gotree.Expr{&gotree.CallExpr{
			Fun:  &gotree.SelectorExpr{X: &gotree.Ident{Name: "maps"}, Sel: "Keys"},
			Args: []gotree.Expr{recv},
		}},
	}, nil
}

// lowerMapValuesExpr emits an IIFE that iterates the key-sorted
// keys and looks each value up so the output order matches
// MapKeysExpr.
func (l *lowerer) lowerMapValuesExpr(e *aotir.MapValuesExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	kt, err := l.lowerType(e.KeyType)
	if err != nil {
		return nil, fmt.Errorf("map values key type: %w", err)
	}
	vt, err := l.lowerType(e.ValueType)
	if err != nil {
		return nil, fmt.Errorf("map values value type: %w", err)
	}
	l.addImport("maps")
	l.addImport("slices")
	// func() []V { ks := slices.Sorted(maps.Keys(m)); vs := make([]V, len(ks)); for i, k := range ks { vs[i] = m[k] }; return vs }()
	body := &gotree.BlockStmt{List: []gotree.Stmt{
		&gotree.AssignStmt{
			Lhs: []gotree.Expr{&gotree.Ident{Name: "ks"}},
			Tok: ":=",
			Rhs: []gotree.Expr{&gotree.CallExpr{
				Fun: &gotree.SelectorExpr{X: &gotree.Ident{Name: "slices"}, Sel: "Sorted"},
				Args: []gotree.Expr{&gotree.CallExpr{
					Fun:  &gotree.SelectorExpr{X: &gotree.Ident{Name: "maps"}, Sel: "Keys"},
					Args: []gotree.Expr{recv},
				}},
			}},
		},
		&gotree.AssignStmt{
			Lhs: []gotree.Expr{&gotree.Ident{Name: "vs"}},
			Tok: ":=",
			Rhs: []gotree.Expr{&gotree.CallExpr{
				Fun: &gotree.Ident{Name: "make"},
				Args: []gotree.Expr{
					&gotree.RawExpr{Src: "[]" + vt},
					&gotree.CallExpr{Fun: &gotree.Ident{Name: "len"}, Args: []gotree.Expr{&gotree.Ident{Name: "ks"}}},
				},
			}},
		},
		&gotree.RangeStmt{
			Key:   &gotree.Ident{Name: "i"},
			Value: &gotree.Ident{Name: "k"},
			Tok:   ":=",
			X:     &gotree.Ident{Name: "ks"},
			Body: &gotree.BlockStmt{List: []gotree.Stmt{
				&gotree.AssignStmt{
					Lhs: []gotree.Expr{&gotree.IndexExpr{X: &gotree.Ident{Name: "vs"}, Index: &gotree.Ident{Name: "i"}}},
					Tok: "=",
					Rhs: []gotree.Expr{&gotree.IndexExpr{X: recv, Index: &gotree.Ident{Name: "k"}}},
				},
			}},
		},
		&gotree.ReturnStmt{Results: []gotree.Expr{&gotree.Ident{Name: "vs"}}},
	}}
	_ = kt
	return &gotree.CallExpr{
		Fun: &gotree.FuncLit{
			Type: &gotree.FuncType{Results: []gotree.Field{{Type: &gotree.RawExpr{Src: "[]" + vt}}}},
			Body: body,
		},
	}, nil
}

// lowerListLit emits `[]T{e0, e1, ...}`. Phase 3.1 handles scalar
// element types; Phase 3.4 widens to record element types (where
// the element type is the record's Go struct name).
func (l *lowerer) lowerListLit(e *aotir.ListLit) (gotree.Expr, error) {
	var elemType string
	switch e.ElemType {
	case aotir.TypeRecord:
		if e.ElemRecordName == "" {
			return nil, fmt.Errorf("list literal of records missing ElemRecordName")
		}
		elemType = e.ElemRecordName
	case aotir.TypeList:
		inner, err := l.lowerListType(e.InnerElemType)
		if err != nil {
			return nil, fmt.Errorf("list<list> literal: %w", err)
		}
		elemType = inner
	default:
		t, err := l.lowerType(e.ElemType)
		if err != nil {
			return nil, fmt.Errorf("list literal: %w", err)
		}
		elemType = t
	}
	elts := make([]gotree.Expr, 0, len(e.Elems))
	for i, x := range e.Elems {
		ge, err := l.lowerExpr(x)
		if err != nil {
			return nil, fmt.Errorf("list literal elem %d: %w", i, err)
		}
		elts = append(elts, ge)
	}
	return &gotree.CompositeLit{
		Type: &gotree.RawExpr{Src: "[]" + elemType},
		Elts: elts,
	}, nil
}

// lowerIndexExpr emits `recv[int(idx)]`. Mochi list indices are
// int64 but Go's slice indexing requires int, so a narrowing
// conversion is wrapped around any non-literal index. Literal
// indices are emitted as bare int literals to keep gofmt output
// compact.
func (l *lowerer) lowerIndexExpr(e *aotir.IndexExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	idx, err := l.lowerExpr(e.Index)
	if err != nil {
		return nil, err
	}
	return &gotree.IndexExpr{X: recv, Index: narrowToInt(idx)}, nil
}

// lowerLenExpr emits `int64(len(xs))` so the result keeps the
// Mochi int pin.
func (l *lowerer) lowerLenExpr(e *aotir.LenExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &gotree.CallExpr{
		Fun: &gotree.Ident{Name: "int64"},
		Args: []gotree.Expr{&gotree.CallExpr{
			Fun:  &gotree.Ident{Name: "len"},
			Args: []gotree.Expr{recv},
		}},
	}, nil
}

// lowerAppendExpr emits `append(xs, v)`. Go's append is variadic
// and accepts the element type directly, no wrapping needed.
func (l *lowerer) lowerAppendExpr(e *aotir.AppendExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(e.Value)
	if err != nil {
		return nil, err
	}
	return &gotree.CallExpr{
		Fun:  &gotree.Ident{Name: "append"},
		Args: []gotree.Expr{recv, val},
	}, nil
}

// narrowToInt wraps an int64-typed expression in int(...) so it
// can be used as a Go slice index. An int64 literal already
// produced by lowerExpr looks like `int64(N)`; unwrap that to the
// raw N rather than emitting `int(int64(N))`.
func narrowToInt(x gotree.Expr) gotree.Expr {
	if call, ok := x.(*gotree.CallExpr); ok {
		if id, ok := call.Fun.(*gotree.Ident); ok && id.Name == "int64" && len(call.Args) == 1 {
			if lit, ok := call.Args[0].(*gotree.BasicLit); ok && lit.Kind == gotree.IntLit {
				return lit
			}
		}
	}
	return &gotree.CallExpr{Fun: &gotree.Ident{Name: "int"}, Args: []gotree.Expr{x}}
}

// lowerFloatLit emits a `float64(N)` wrapper around the lexical
// representation of v. NaN and Inf flow through math.NaN(),
// math.Inf(+1), and math.Inf(-1) because Go syntax has no
// literal form for them.
func (l *lowerer) lowerFloatLit(v float64) gotree.Expr {
	switch {
	case v != v: // NaN
		l.addImport("math")
		return &gotree.CallExpr{Fun: &gotree.SelectorExpr{X: &gotree.Ident{Name: "math"}, Sel: "NaN"}}
	case v > 0 && v*2 == v: // +Inf
		l.addImport("math")
		return &gotree.CallExpr{
			Fun:  &gotree.SelectorExpr{X: &gotree.Ident{Name: "math"}, Sel: "Inf"},
			Args: []gotree.Expr{&gotree.BasicLit{Kind: gotree.IntLit, Value: "1"}},
		}
	case v < 0 && v*2 == v: // -Inf
		l.addImport("math")
		return &gotree.CallExpr{
			Fun:  &gotree.SelectorExpr{X: &gotree.Ident{Name: "math"}, Sel: "Inf"},
			Args: []gotree.Expr{&gotree.BasicLit{Kind: gotree.IntLit, Value: "-1"}},
		}
	}
	return &gotree.CallExpr{
		Fun:  &gotree.Ident{Name: "float64"},
		Args: []gotree.Expr{&gotree.BasicLit{Kind: gotree.FloatLit, Value: strconv.FormatFloat(v, 'g', -1, 64)}},
	}
}

// binOpText returns the Go infix operator string for op, plus
// a boolean noting whether the operator is a function call
// instead (e.g. string concat lowers to a + b, but record
// equality lowers to a function call). Phase 2 returns only
// infix forms.
func (l *lowerer) lowerBinary(b *aotir.BinaryExpr) (gotree.Expr, error) {
	left, err := l.lowerExpr(b.Left)
	if err != nil {
		return nil, err
	}
	right, err := l.lowerExpr(b.Right)
	if err != nil {
		return nil, err
	}
	op, ok := binOpInfix(b.Op)
	if ok {
		return &gotree.BinaryExpr{X: left, Op: op, Y: right}, nil
	}
	return nil, fmt.Errorf("transpiler3/go/lower: Phase 2 does not handle BinOp %v", b.Op)
}

func binOpInfix(op aotir.BinOp) (string, bool) {
	switch op {
	case aotir.BinAddI64, aotir.BinAddF64, aotir.BinStrCat:
		return "+", true
	case aotir.BinSubI64, aotir.BinSubF64:
		return "-", true
	case aotir.BinMulI64, aotir.BinMulF64:
		return "*", true
	case aotir.BinDivI64, aotir.BinDivF64:
		return "/", true
	case aotir.BinModI64:
		return "%", true
	case aotir.BinEqI64, aotir.BinEqF64, aotir.BinEqBool, aotir.BinEqStr, aotir.BinEqRec:
		return "==", true
	case aotir.BinNeI64, aotir.BinNeF64, aotir.BinNeBool, aotir.BinNeStr, aotir.BinNeRec:
		return "!=", true
	case aotir.BinLtI64, aotir.BinLtF64:
		return "<", true
	case aotir.BinLeI64, aotir.BinLeF64:
		return "<=", true
	case aotir.BinGtI64, aotir.BinGtF64:
		return ">", true
	case aotir.BinGeI64, aotir.BinGeF64:
		return ">=", true
	case aotir.BinAndBool:
		return "&&", true
	case aotir.BinOrBool:
		return "||", true
	}
	return "", false
}

func (l *lowerer) lowerUnary(u *aotir.UnaryExpr) (gotree.Expr, error) {
	operand, err := l.lowerExpr(u.Operand)
	if err != nil {
		return nil, err
	}
	switch u.Op {
	case aotir.UnNegI64, aotir.UnNegF64:
		return &gotree.UnaryExpr{Op: "-", X: operand}, nil
	case aotir.UnNotBool:
		return &gotree.UnaryExpr{Op: "!", X: operand}, nil
	}
	return nil, fmt.Errorf("transpiler3/go/lower: Phase 2 does not handle UnOp %v", u.Op)
}

// lowerStrLenExpr emits `int64(len([]rune(s)))` so the length follows
// the Mochi semantics of counting Unicode codepoints rather than UTF-8
// bytes. A pure-ASCII input still gets the right answer; multibyte
// runes count as one each. The int64 wrapper keeps the Mochi int pin.
func (l *lowerer) lowerStrLenExpr(e *aotir.StrLenExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	runes := &gotree.CallExpr{
		Fun:  &gotree.Ident{Name: "[]rune"},
		Args: []gotree.Expr{recv},
	}
	return &gotree.CallExpr{
		Fun: &gotree.Ident{Name: "int64"},
		Args: []gotree.Expr{&gotree.CallExpr{
			Fun:  &gotree.Ident{Name: "len"},
			Args: []gotree.Expr{runes},
		}},
	}, nil
}

// lowerStrConvertExpr emits the Mochi `str(x)` builtin. The result
// must match the C runtime's mochi_str_from_<T> formatting:
//   - int: base-10, no leading zero
//   - float: shortest round-trip via strconv.FormatFloat 'g'
//   - bool: "true" / "false"
//   - string: identity
func (l *lowerer) lowerStrConvertExpr(e *aotir.StrConvertExpr) (gotree.Expr, error) {
	operand, err := l.lowerExpr(e.Operand)
	if err != nil {
		return nil, err
	}
	switch e.Operand.Type() {
	case aotir.TypeString:
		return operand, nil
	case aotir.TypeInt:
		l.addImport("strconv")
		return &gotree.CallExpr{
			Fun: &gotree.SelectorExpr{X: &gotree.Ident{Name: "strconv"}, Sel: "FormatInt"},
			Args: []gotree.Expr{operand, &gotree.BasicLit{Kind: gotree.IntLit, Value: "10"}},
		}, nil
	case aotir.TypeFloat:
		l.addImport("strconv")
		return &gotree.CallExpr{
			Fun: &gotree.SelectorExpr{X: &gotree.Ident{Name: "strconv"}, Sel: "FormatFloat"},
			Args: []gotree.Expr{
				operand,
				&gotree.BasicLit{Kind: gotree.CharLit, Value: "'g'"},
				&gotree.UnaryExpr{Op: "-", X: &gotree.BasicLit{Kind: gotree.IntLit, Value: "1"}},
				&gotree.BasicLit{Kind: gotree.IntLit, Value: "64"},
			},
		}, nil
	case aotir.TypeBool:
		l.addImport("strconv")
		return &gotree.CallExpr{
			Fun:  &gotree.SelectorExpr{X: &gotree.Ident{Name: "strconv"}, Sel: "FormatBool"},
			Args: []gotree.Expr{operand},
		}, nil
	}
	return nil, fmt.Errorf("transpiler3/go/lower: StrConvertExpr does not handle operand type %s", e.Operand.Type())
}

// lowerStrContainsExpr emits `strings.Contains(s, sub)`, matching the
// C runtime's mochi_str_contains semantics (substring search returns
// true for empty sub).
func (l *lowerer) lowerStrContainsExpr(e *aotir.StrContainsExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	sub, err := l.lowerExpr(e.Sub)
	if err != nil {
		return nil, err
	}
	l.addImport("strings")
	return &gotree.CallExpr{
		Fun:  &gotree.SelectorExpr{X: &gotree.Ident{Name: "strings"}, Sel: "Contains"},
		Args: []gotree.Expr{recv, sub},
	}, nil
}

// lowerListSumExpr emits `mochiListSumI64(xs)` for list<int> and
// `mochiListSumF64(xs)` for list<float>. The helper does the
// straightforward accumulator loop so the sum stays type-stable
// at the Mochi int / float pin.
func (l *lowerer) lowerListSumExpr(e *aotir.ListSumExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	switch e.ElemType {
	case aotir.TypeInt:
		l.addHelper("mochiListSumI64")
		return &gotree.CallExpr{Fun: &gotree.Ident{Name: "mochiListSumI64"}, Args: []gotree.Expr{recv}}, nil
	case aotir.TypeFloat:
		l.addHelper("mochiListSumF64")
		return &gotree.CallExpr{Fun: &gotree.Ident{Name: "mochiListSumF64"}, Args: []gotree.Expr{recv}}, nil
	}
	return nil, fmt.Errorf("transpiler3/go/lower: sum() does not handle element type %s", e.ElemType)
}

// lowerListMinExpr emits `mochiListMin[T](xs)`. The helper panics
// when xs is empty (matching the C runtime's mochi_panic_empty).
func (l *lowerer) lowerListMinExpr(e *aotir.ListMinExpr) (gotree.Expr, error) {
	return l.lowerListMinMax(e.Receiver, e.ElemType, "Min")
}

func (l *lowerer) lowerListMaxExpr(e *aotir.ListMaxExpr) (gotree.Expr, error) {
	return l.lowerListMinMax(e.Receiver, e.ElemType, "Max")
}

func (l *lowerer) lowerListMinMax(recv aotir.Expr, elem aotir.Type, op string) (gotree.Expr, error) {
	r, err := l.lowerExpr(recv)
	if err != nil {
		return nil, err
	}
	var helper string
	switch elem {
	case aotir.TypeInt:
		helper = "mochiList" + op + "I64"
	case aotir.TypeFloat:
		helper = "mochiList" + op + "F64"
	case aotir.TypeString:
		helper = "mochiList" + op + "Str"
	default:
		return nil, fmt.Errorf("transpiler3/go/lower: %s() does not handle element type %s", op, elem)
	}
	l.addHelper(helper)
	return &gotree.CallExpr{Fun: &gotree.Ident{Name: helper}, Args: []gotree.Expr{r}}, nil
}

// lowerListContainsExpr emits `slices.Contains(xs, v)` for the
// `v in xs` predicate. The stdlib helper works for every scalar
// element type Mochi allows here (int/float/bool/string).
func (l *lowerer) lowerListContainsExpr(e *aotir.ListContainsExpr) (gotree.Expr, error) {
	xs, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, err
	}
	v, err := l.lowerExpr(e.Value)
	if err != nil {
		return nil, err
	}
	l.addImport("slices")
	return &gotree.CallExpr{
		Fun:  &gotree.SelectorExpr{X: &gotree.Ident{Name: "slices"}, Sel: "Contains"},
		Args: []gotree.Expr{xs, v},
	}, nil
}

// lowerNumCastExpr emits the int(x) cast for a float operand. The
// aotir node fixes Result==TypeInt and the verifier ensures the
// operand type is TypeFloat, so the lowering is a constant
// `int64(operand)` wrap that truncates toward zero (Go conversion
// semantics, matching the C runtime's `(int64_t)x` lowering).
func (l *lowerer) lowerNumCastExpr(e *aotir.NumCastExpr) (gotree.Expr, error) {
	operand, err := l.lowerExpr(e.Operand)
	if err != nil {
		return nil, err
	}
	return &gotree.CallExpr{Fun: &gotree.Ident{Name: "int64"}, Args: []gotree.Expr{operand}}, nil
}

// lowerMathCallExpr lowers MathCallExpr for the four supported
// builtins. abs_i64 uses a tiny generic helper because Go has no
// integer math.Abs; abs_f64 / floor / ceil delegate to math.* and
// register the import. The C runtime emits the same operations.
func (l *lowerer) lowerMathCallExpr(e *aotir.MathCallExpr) (gotree.Expr, error) {
	arg, err := l.lowerExpr(e.Arg)
	if err != nil {
		return nil, err
	}
	switch e.Func {
	case "abs_i64":
		l.addHelper("mochiAbsI64")
		return &gotree.CallExpr{Fun: &gotree.Ident{Name: "mochiAbsI64"}, Args: []gotree.Expr{arg}}, nil
	case "abs_f64":
		l.addImport("math")
		return &gotree.CallExpr{
			Fun:  &gotree.SelectorExpr{X: &gotree.Ident{Name: "math"}, Sel: "Abs"},
			Args: []gotree.Expr{arg},
		}, nil
	case "floor":
		l.addImport("math")
		return &gotree.CallExpr{
			Fun:  &gotree.SelectorExpr{X: &gotree.Ident{Name: "math"}, Sel: "Floor"},
			Args: []gotree.Expr{arg},
		}, nil
	case "ceil":
		l.addImport("math")
		return &gotree.CallExpr{
			Fun:  &gotree.SelectorExpr{X: &gotree.Ident{Name: "math"}, Sel: "Ceil"},
			Args: []gotree.Expr{arg},
		}, nil
	default:
		return nil, fmt.Errorf("transpiler3/go/lower: math builtin %q not supported", e.Func)
	}
}

// lowerReadFileExpr emits mochiReadFile(path) which wraps os.ReadFile,
// returns the contents as a string and discards the error (matching the
// C runtime, which returns "" on read failure).
func (l *lowerer) lowerReadFileExpr(e *aotir.ReadFileExpr) (gotree.Expr, error) {
	path, err := l.lowerExpr(e.Path)
	if err != nil {
		return nil, err
	}
	l.addImport("os")
	l.addHelper("mochiReadFile")
	return &gotree.CallExpr{Fun: &gotree.Ident{Name: "mochiReadFile"}, Args: []gotree.Expr{path}}, nil
}

// lowerLinesExpr emits mochiLines(path) which reads the file and splits
// on '\n', dropping a single trailing empty token to match the C runtime's
// behaviour for trailing-newline files.
func (l *lowerer) lowerLinesExpr(e *aotir.LinesExpr) (gotree.Expr, error) {
	path, err := l.lowerExpr(e.Path)
	if err != nil {
		return nil, err
	}
	l.addImport("os")
	l.addImport("strings")
	l.addHelper("mochiLines")
	return &gotree.CallExpr{Fun: &gotree.Ident{Name: "mochiLines"}, Args: []gotree.Expr{path}}, nil
}

// lowerOMapLiteralExpr emits an IIFE that allocates a fresh *mochiOMap[K, V]
// via mochiOMapNew and populates it with sequential mochiOMapSet calls.
// The IIFE form (instead of a composite literal) preserves insertion order
// across both first-write and overwrite, and gives the local a stable type
// so callers can chain the result into a let / param / return without an
// extra type annotation.
func (l *lowerer) lowerOMapLiteralExpr(e *aotir.OMapLiteralExpr) (gotree.Expr, error) {
	if len(e.Keys) != len(e.Values) {
		return nil, fmt.Errorf("omap literal: %d keys vs %d values", len(e.Keys), len(e.Values))
	}
	typeText, err := l.lowerOMapType(e.KeyType, e.ValueType)
	if err != nil {
		return nil, fmt.Errorf("omap literal: %w", err)
	}
	kt, err := l.lowerType(e.KeyType)
	if err != nil {
		return nil, fmt.Errorf("omap literal key: %w", err)
	}
	vt, err := l.lowerType(e.ValueType)
	if err != nil {
		return nil, fmt.Errorf("omap literal value: %w", err)
	}
	l.addHelper("mochiOMap")
	l.addHelper("mochiOMapNew")
	l.addHelper("mochiOMapSet")
	stmts := []gotree.Stmt{
		&gotree.AssignStmt{
			Lhs: []gotree.Expr{&gotree.Ident{Name: "o"}},
			Tok: ":=",
			Rhs: []gotree.Expr{&gotree.CallExpr{
				Fun: &gotree.Ident{Name: "mochiOMapNew[" + kt + ", " + vt + "]"},
			}},
		},
	}
	for i := range e.Keys {
		k, err := l.lowerExpr(e.Keys[i])
		if err != nil {
			return nil, fmt.Errorf("omap literal key %d: %w", i, err)
		}
		v, err := l.lowerExpr(e.Values[i])
		if err != nil {
			return nil, fmt.Errorf("omap literal value %d: %w", i, err)
		}
		stmts = append(stmts, &gotree.ExprStmt{X: &gotree.CallExpr{
			Fun:  &gotree.Ident{Name: "mochiOMapSet"},
			Args: []gotree.Expr{&gotree.Ident{Name: "o"}, k, v},
		}})
	}
	stmts = append(stmts, &gotree.ReturnStmt{Results: []gotree.Expr{&gotree.Ident{Name: "o"}}})
	return &gotree.CallExpr{
		Fun: &gotree.FuncLit{
			Type: &gotree.FuncType{Results: []gotree.Field{{Type: &gotree.RawExpr{Src: typeText}}}},
			Body: &gotree.BlockStmt{List: stmts},
		},
	}, nil
}

// lowerOMapGetExpr emits `mochiOMapGet(recv, key)`.
func (l *lowerer) lowerOMapGetExpr(e *aotir.OMapGetExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("omap get receiver: %w", err)
	}
	key, err := l.lowerExpr(e.Key)
	if err != nil {
		return nil, fmt.Errorf("omap get key: %w", err)
	}
	l.addHelper("mochiOMap")
	l.addHelper("mochiOMapGet")
	return &gotree.CallExpr{
		Fun:  &gotree.Ident{Name: "mochiOMapGet"},
		Args: []gotree.Expr{recv, key},
	}, nil
}

// lowerOMapSetExpr emits an IIFE that calls mochiOMapSet(recv, k, v) and
// returns the receiver. Functional set expressions are rare in practice
// (the C lowerer prefers OMapPutStmt) but the IIFE keeps the expression
// position viable when the source program uses the value-form.
func (l *lowerer) lowerOMapSetExpr(e *aotir.OMapSetExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("omap set receiver: %w", err)
	}
	key, err := l.lowerExpr(e.Key)
	if err != nil {
		return nil, fmt.Errorf("omap set key: %w", err)
	}
	val, err := l.lowerExpr(e.Value)
	if err != nil {
		return nil, fmt.Errorf("omap set value: %w", err)
	}
	typeText, err := l.lowerOMapType(e.KeyType, e.ValueType)
	if err != nil {
		return nil, fmt.Errorf("omap set type: %w", err)
	}
	l.addHelper("mochiOMap")
	l.addHelper("mochiOMapSet")
	body := &gotree.BlockStmt{List: []gotree.Stmt{
		&gotree.AssignStmt{
			Lhs: []gotree.Expr{&gotree.Ident{Name: "o"}},
			Tok: ":=",
			Rhs: []gotree.Expr{recv},
		},
		&gotree.ExprStmt{X: &gotree.CallExpr{
			Fun:  &gotree.Ident{Name: "mochiOMapSet"},
			Args: []gotree.Expr{&gotree.Ident{Name: "o"}, key, val},
		}},
		&gotree.ReturnStmt{Results: []gotree.Expr{&gotree.Ident{Name: "o"}}},
	}}
	return &gotree.CallExpr{
		Fun: &gotree.FuncLit{
			Type: &gotree.FuncType{Results: []gotree.Field{{Type: &gotree.RawExpr{Src: typeText}}}},
			Body: body,
		},
	}, nil
}

// lowerOMapHasExpr emits `mochiOMapHas(recv, key)`.
func (l *lowerer) lowerOMapHasExpr(e *aotir.OMapHasExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("omap has receiver: %w", err)
	}
	key, err := l.lowerExpr(e.Key)
	if err != nil {
		return nil, fmt.Errorf("omap has key: %w", err)
	}
	l.addHelper("mochiOMap")
	l.addHelper("mochiOMapHas")
	return &gotree.CallExpr{
		Fun:  &gotree.Ident{Name: "mochiOMapHas"},
		Args: []gotree.Expr{recv, key},
	}, nil
}

// lowerOMapLenExpr emits `mochiOMapLen(recv)`.
func (l *lowerer) lowerOMapLenExpr(e *aotir.OMapLenExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("omap len receiver: %w", err)
	}
	l.addHelper("mochiOMap")
	l.addHelper("mochiOMapLen")
	return &gotree.CallExpr{
		Fun:  &gotree.Ident{Name: "mochiOMapLen"},
		Args: []gotree.Expr{recv},
	}, nil
}

// lowerListMapExpr emits an IIFE that walks the input list and applies the
// function to each element. The result-element type comes from ListMapExpr.ElemType
// (B in `map(xs: list<A>, fn: fun(A): B): list<B>`). The IIFE wrapper keeps
// the result available as an expression so it composes inside larger forms.
func (l *lowerer) lowerListMapExpr(e *aotir.ListMapExpr) (gotree.Expr, error) {
	xs, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, fmt.Errorf("map list: %w", err)
	}
	fn, err := l.lowerExpr(e.Fn)
	if err != nil {
		return nil, fmt.Errorf("map fn: %w", err)
	}
	outType, err := l.lowerListType(e.ElemType)
	if err != nil {
		return nil, fmt.Errorf("map result type: %w", err)
	}
	body := &gotree.BlockStmt{List: []gotree.Stmt{
		&gotree.AssignStmt{
			Lhs: []gotree.Expr{&gotree.Ident{Name: "out"}},
			Tok: ":=",
			Rhs: []gotree.Expr{&gotree.CallExpr{
				Fun: &gotree.Ident{Name: "make"},
				Args: []gotree.Expr{
					&gotree.RawExpr{Src: outType},
					&gotree.BasicLit{Kind: gotree.IntLit, Value: "0"},
					&gotree.CallExpr{Fun: &gotree.Ident{Name: "len"}, Args: []gotree.Expr{xs}},
				},
			}},
		},
		&gotree.RangeStmt{
			Key:   &gotree.Ident{Name: "_"},
			Value: &gotree.Ident{Name: "x"},
			Tok:   ":=",
			X:     xs,
			Body: &gotree.BlockStmt{List: []gotree.Stmt{
				&gotree.AssignStmt{
					Lhs: []gotree.Expr{&gotree.Ident{Name: "out"}},
					Tok: "=",
					Rhs: []gotree.Expr{&gotree.CallExpr{
						Fun:  &gotree.Ident{Name: "append"},
						Args: []gotree.Expr{&gotree.Ident{Name: "out"}, &gotree.CallExpr{Fun: fn, Args: []gotree.Expr{&gotree.Ident{Name: "x"}}}},
					}},
				},
			}},
		},
		&gotree.ReturnStmt{Results: []gotree.Expr{&gotree.Ident{Name: "out"}}},
	}}
	return &gotree.CallExpr{
		Fun: &gotree.FuncLit{
			Type: &gotree.FuncType{Results: []gotree.Field{{Type: &gotree.RawExpr{Src: outType}}}},
			Body: body,
		},
	}, nil
}

// lowerListFilterExpr emits an IIFE that walks the input list and keeps
// elements for which fn returns true. The result element type is the
// preserved input element type (ListFilterExpr.ElemType).
func (l *lowerer) lowerListFilterExpr(e *aotir.ListFilterExpr) (gotree.Expr, error) {
	xs, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, fmt.Errorf("filter list: %w", err)
	}
	fn, err := l.lowerExpr(e.Fn)
	if err != nil {
		return nil, fmt.Errorf("filter fn: %w", err)
	}
	outType, err := l.lowerListType(e.ElemType)
	if err != nil {
		return nil, fmt.Errorf("filter result type: %w", err)
	}
	body := &gotree.BlockStmt{List: []gotree.Stmt{
		&gotree.AssignStmt{
			Lhs: []gotree.Expr{&gotree.Ident{Name: "out"}},
			Tok: ":=",
			Rhs: []gotree.Expr{&gotree.CallExpr{
				Fun: &gotree.Ident{Name: "make"},
				Args: []gotree.Expr{
					&gotree.RawExpr{Src: outType},
					&gotree.BasicLit{Kind: gotree.IntLit, Value: "0"},
					&gotree.CallExpr{Fun: &gotree.Ident{Name: "len"}, Args: []gotree.Expr{xs}},
				},
			}},
		},
		&gotree.RangeStmt{
			Key:   &gotree.Ident{Name: "_"},
			Value: &gotree.Ident{Name: "x"},
			Tok:   ":=",
			X:     xs,
			Body: &gotree.BlockStmt{List: []gotree.Stmt{
				&gotree.IfStmt{
					Cond: &gotree.CallExpr{Fun: fn, Args: []gotree.Expr{&gotree.Ident{Name: "x"}}},
					Body: &gotree.BlockStmt{List: []gotree.Stmt{
						&gotree.AssignStmt{
							Lhs: []gotree.Expr{&gotree.Ident{Name: "out"}},
							Tok: "=",
							Rhs: []gotree.Expr{&gotree.CallExpr{
								Fun:  &gotree.Ident{Name: "append"},
								Args: []gotree.Expr{&gotree.Ident{Name: "out"}, &gotree.Ident{Name: "x"}},
							}},
						},
					}},
				},
			}},
		},
		&gotree.ReturnStmt{Results: []gotree.Expr{&gotree.Ident{Name: "out"}}},
	}}
	return &gotree.CallExpr{
		Fun: &gotree.FuncLit{
			Type: &gotree.FuncType{Results: []gotree.Field{{Type: &gotree.RawExpr{Src: outType}}}},
			Body: body,
		},
	}, nil
}

// lowerListFoldlExpr emits an IIFE that walks the input list and folds
// each element into the accumulator via `acc = fn(acc, x)`. AccType is the
// result type carried on the node.
func (l *lowerer) lowerListFoldlExpr(e *aotir.ListFoldlExpr) (gotree.Expr, error) {
	xs, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, fmt.Errorf("reduce list: %w", err)
	}
	fn, err := l.lowerExpr(e.Fn)
	if err != nil {
		return nil, fmt.Errorf("reduce fn: %w", err)
	}
	init, err := l.lowerExpr(e.Init)
	if err != nil {
		return nil, fmt.Errorf("reduce init: %w", err)
	}
	accType, err := l.lowerType(e.AccType)
	if err != nil {
		return nil, fmt.Errorf("reduce acc type: %w", err)
	}
	body := &gotree.BlockStmt{List: []gotree.Stmt{
		&gotree.AssignStmt{
			Lhs: []gotree.Expr{&gotree.Ident{Name: "acc"}},
			Tok: ":=",
			Rhs: []gotree.Expr{init},
		},
		&gotree.RangeStmt{
			Key:   &gotree.Ident{Name: "_"},
			Value: &gotree.Ident{Name: "x"},
			Tok:   ":=",
			X:     xs,
			Body: &gotree.BlockStmt{List: []gotree.Stmt{
				&gotree.AssignStmt{
					Lhs: []gotree.Expr{&gotree.Ident{Name: "acc"}},
					Tok: "=",
					Rhs: []gotree.Expr{&gotree.CallExpr{
						Fun:  fn,
						Args: []gotree.Expr{&gotree.Ident{Name: "acc"}, &gotree.Ident{Name: "x"}},
					}},
				},
			}},
		},
		&gotree.ReturnStmt{Results: []gotree.Expr{&gotree.Ident{Name: "acc"}}},
	}}
	return &gotree.CallExpr{
		Fun: &gotree.FuncLit{
			Type: &gotree.FuncType{Results: []gotree.Field{{Type: &gotree.RawExpr{Src: accType}}}},
			Body: body,
		},
	}, nil
}

// lowerLoadCSVExpr emits mochiLoadCSV(path) which uses encoding/csv to
// parse the file as RFC 4180. Returns nil on error so the program can
// inspect len(rows)==0 rather than handling a missing-file panic
// (matches the C runtime's silent-failure behaviour).
func (l *lowerer) lowerLoadCSVExpr(e *aotir.LoadCSVExpr) (gotree.Expr, error) {
	path, err := l.lowerExpr(e.Path)
	if err != nil {
		return nil, err
	}
	l.addImport("os")
	l.addImport("encoding/csv")
	l.addHelper("mochiLoadCSV")
	return &gotree.CallExpr{Fun: &gotree.Ident{Name: "mochiLoadCSV"}, Args: []gotree.Expr{path}}, nil
}
