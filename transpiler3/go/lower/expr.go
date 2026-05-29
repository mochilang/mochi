package lower

import (
	"fmt"
	"strconv"

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
		return &gotree.Ident{Name: mangleIdent(e.Name)}, nil
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
	default:
		return nil, fmt.Errorf("transpiler3/go/lower: does not handle expr %T", e)
	}
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
	mapType, err := l.lowerMapType(e.KeyType, e.ValueType)
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
