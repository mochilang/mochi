package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// lowerExprs lowers a slice of aotir.Expr using the lowerer.
func (l *lowerer) lowerExprs(es []aotir.Expr) ([]javasrc.Expr, error) {
	out := make([]javasrc.Expr, len(es))
	for i, e := range es {
		j, err := l.lowerExpr(e)
		if err != nil {
			return nil, err
		}
		out[i] = j
	}
	return out, nil
}

// lowerExpr translates an aotir.Expr to a javasrc.Expr.
func (l *lowerer) lowerExpr(e aotir.Expr) (javasrc.Expr, error) {
	switch e := e.(type) {
	case *aotir.StringLit:
		return javasrc.StringLit(e.Value), nil
	case *aotir.IntLit:
		return javasrc.LongLit(e.Value), nil
	case *aotir.BoolLit:
		return javasrc.BoolLit(e.Value), nil
	case *aotir.FloatLit:
		return javasrc.DoubleLit(e.Value), nil

	case *aotir.VarRef:
		return &javasrc.NameExpr{Name: e.Name}, nil

	case *aotir.BinaryExpr:
		return l.lowerBinaryExpr(e)

	case *aotir.UnaryExpr:
		return l.lowerUnaryExpr(e)

	case *aotir.NumCastExpr:
		// int(x) where x is float: cast to long
		operand, err := l.lowerExpr(e.Operand)
		if err != nil {
			return nil, err
		}
		return &javasrc.CastExpr{Type: javasrc.TypeLong, X: operand}, nil

	case *aotir.CallExpr:
		return l.lowerCallExpr(e)

	case *aotir.JavaCallExpr:
		return l.lowerJavaCallExpr(e)

	// --- List expressions ---

	case *aotir.ListLit:
		return l.lowerListLit(e)

	case *aotir.IndexExpr:
		return l.lowerIndexExpr(e)

	case *aotir.LenExpr:
		return l.lowerLenExpr(e)

	case *aotir.AppendExpr:
		// append(xs, v) -- functional append: creates new ArrayList copying old + adding new elem.
		return l.lowerAppendExpr(e)

	// --- Map expressions ---

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

	// --- Set expressions ---

	case *aotir.SetLiteralExpr:
		return l.lowerSetLiteralExpr(e)

	case *aotir.SetAddExpr:
		return l.lowerSetAddExpr(e)

	case *aotir.SetHasExpr:
		return l.lowerSetHasExpr(e)

	case *aotir.SetLenExpr:
		return l.lowerSetLenExpr(e)

	// --- Record expressions ---

	case *aotir.RecordLit:
		return l.lowerRecordLit(e)

	case *aotir.FieldAccess:
		return l.lowerFieldAccess(e)

	// --- Sum type expressions (Phase 5) ---

	case *aotir.VariantLit:
		return l.lowerVariantLit(e)

	case *aotir.UnionVarRef:
		// A union-typed variable reference: just emit the variable name.
		return &javasrc.NameExpr{Name: e.Name}, nil

	case *aotir.VariantFieldAccess:
		return l.lowerVariantFieldAccess(e)

	// --- Closure / HOF expressions (Phase 6) ---

	case *aotir.FunLit:
		return l.lowerFunLit(e)

	case *aotir.FunCallExpr:
		return l.lowerFunCallExpr(e)

	case *aotir.ListMapExpr:
		return l.lowerListMapExpr(e)

	case *aotir.ListFilterExpr:
		return l.lowerListFilterExpr(e)

	case *aotir.ListFoldlExpr:
		return l.lowerListFoldlExpr(e)

	// --- Query DSL expressions (Phase 7) ---

	case *aotir.ListSortAscExpr:
		return l.lowerListSortAscExpr(e)

	case *aotir.ListSliceExpr:
		return l.lowerListSliceExpr(e)

	// --- Datalog expressions (Phase 8) ---

	case *aotir.DatalogQueryExpr:
		return l.lowerDatalogQueryExpr(e)

	// --- Agent expressions (Phase 9) ---

	case *aotir.AgentLit:
		return l.lowerAgentLitExpr(e)

	case *aotir.AgentSpawnExpr:
		return l.lowerAgentSpawnExpr(e)

	case *aotir.AgentIntentCallExpr:
		return l.lowerAgentIntentCallExpr(e)

	// --- Stream / channel expressions (Phase 10) ---

	case *aotir.ChanMakeExpr:
		return l.lowerChanMakeExpr(e)

	case *aotir.ChanRecvExpr:
		return l.lowerChanRecvExpr(e)

	case *aotir.StreamMakeExpr:
		return l.lowerStreamMakeExpr(e)

	case *aotir.SubMakeExpr:
		return l.lowerSubMakeExpr(e)

	case *aotir.SubMakeLimitExpr:
		return l.lowerSubMakeLimitExpr(e)

	case *aotir.SubRecvExpr:
		return l.lowerSubRecvExpr(e)

	case *aotir.AsyncExpr:
		return l.lowerAsyncExpr(e)

	case *aotir.AwaitExpr:
		return l.lowerAwaitExpr(e)

	default:
		return nil, fmt.Errorf("jvm/lower: unsupported expr %T", e)
	}
}

func (l *lowerer) lowerBinaryExpr(e *aotir.BinaryExpr) (javasrc.Expr, error) {
	left, err := l.lowerExpr(e.Left)
	if err != nil {
		return nil, err
	}
	right, err := l.lowerExpr(e.Right)
	if err != nil {
		return nil, err
	}

	switch e.Op {
	// Integer arithmetic
	case aotir.BinAddI64:
		return &javasrc.BinaryExpr{Left: left, Op: "+", Right: right}, nil
	case aotir.BinSubI64:
		return &javasrc.BinaryExpr{Left: left, Op: "-", Right: right}, nil
	case aotir.BinMulI64:
		return &javasrc.BinaryExpr{Left: left, Op: "*", Right: right}, nil
	case aotir.BinDivI64:
		// Use IntMath.div to get divide-by-zero panic semantics
		return &javasrc.StaticCallExpr{
			Class:  "dev.mochi.runtime.math.IntMath",
			Method: "div",
			Args:   []javasrc.Expr{left, right},
		}, nil
	case aotir.BinModI64:
		return &javasrc.StaticCallExpr{
			Class:  "dev.mochi.runtime.math.IntMath",
			Method: "mod",
			Args:   []javasrc.Expr{left, right},
		}, nil

	// Float arithmetic
	case aotir.BinAddF64:
		return &javasrc.BinaryExpr{Left: left, Op: "+", Right: right}, nil
	case aotir.BinSubF64:
		return &javasrc.BinaryExpr{Left: left, Op: "-", Right: right}, nil
	case aotir.BinMulF64:
		return &javasrc.BinaryExpr{Left: left, Op: "*", Right: right}, nil
	case aotir.BinDivF64:
		return &javasrc.BinaryExpr{Left: left, Op: "/", Right: right}, nil

	// Integer comparisons
	case aotir.BinEqI64:
		return &javasrc.BinaryExpr{Left: left, Op: "==", Right: right}, nil
	case aotir.BinNeI64:
		return &javasrc.BinaryExpr{Left: left, Op: "!=", Right: right}, nil
	case aotir.BinLtI64:
		return &javasrc.BinaryExpr{Left: left, Op: "<", Right: right}, nil
	case aotir.BinLeI64:
		return &javasrc.BinaryExpr{Left: left, Op: "<=", Right: right}, nil
	case aotir.BinGtI64:
		return &javasrc.BinaryExpr{Left: left, Op: ">", Right: right}, nil
	case aotir.BinGeI64:
		return &javasrc.BinaryExpr{Left: left, Op: ">=", Right: right}, nil

	// Float comparisons
	case aotir.BinEqF64:
		return &javasrc.BinaryExpr{Left: left, Op: "==", Right: right}, nil
	case aotir.BinNeF64:
		return &javasrc.BinaryExpr{Left: left, Op: "!=", Right: right}, nil
	case aotir.BinLtF64:
		return &javasrc.BinaryExpr{Left: left, Op: "<", Right: right}, nil
	case aotir.BinLeF64:
		return &javasrc.BinaryExpr{Left: left, Op: "<=", Right: right}, nil
	case aotir.BinGtF64:
		return &javasrc.BinaryExpr{Left: left, Op: ">", Right: right}, nil
	case aotir.BinGeF64:
		return &javasrc.BinaryExpr{Left: left, Op: ">=", Right: right}, nil

	// Bool comparisons
	case aotir.BinEqBool:
		return &javasrc.BinaryExpr{Left: left, Op: "==", Right: right}, nil
	case aotir.BinNeBool:
		return &javasrc.BinaryExpr{Left: left, Op: "!=", Right: right}, nil

	// String comparisons -- must use .equals(), not ==
	case aotir.BinEqStr:
		return &javasrc.CallExpr{
			Receiver: left,
			Method:   "equals",
			Args:     []javasrc.Expr{right},
		}, nil
	case aotir.BinNeStr:
		eq := &javasrc.CallExpr{
			Receiver: left,
			Method:   "equals",
			Args:     []javasrc.Expr{right},
		}
		return &javasrc.UnaryExpr{Op: "!", Operand: eq}, nil

	// String concatenation
	case aotir.BinStrCat:
		return &javasrc.BinaryExpr{Left: left, Op: "+", Right: right}, nil

	// Boolean short-circuit
	case aotir.BinAndBool:
		return &javasrc.BinaryExpr{Left: left, Op: "&&", Right: right}, nil
	case aotir.BinOrBool:
		return &javasrc.BinaryExpr{Left: left, Op: "||", Right: right}, nil

	// Record equality: Java records auto-generate equals(); use Objects.equals for null safety.
	case aotir.BinEqRec:
		return &javasrc.StaticCallExpr{
			Class:  "Objects",
			Method: "equals",
			Args:   []javasrc.Expr{left, right},
		}, nil
	case aotir.BinNeRec:
		eq := &javasrc.StaticCallExpr{
			Class:  "Objects",
			Method: "equals",
			Args:   []javasrc.Expr{left, right},
		}
		return &javasrc.UnaryExpr{Op: "!", Operand: eq}, nil

	default:
		return nil, fmt.Errorf("jvm/lower: unsupported binary op %v", e.Op)
	}
}

func (l *lowerer) lowerUnaryExpr(e *aotir.UnaryExpr) (javasrc.Expr, error) {
	operand, err := l.lowerExpr(e.Operand)
	if err != nil {
		return nil, err
	}
	switch e.Op {
	case aotir.UnNegI64:
		return &javasrc.UnaryExpr{Op: "-", Operand: operand}, nil
	case aotir.UnNegF64:
		return &javasrc.UnaryExpr{Op: "-", Operand: operand}, nil
	case aotir.UnNotBool:
		return &javasrc.UnaryExpr{Op: "!", Operand: operand}, nil
	default:
		return nil, fmt.Errorf("jvm/lower: unsupported unary op %v", e.Op)
	}
}

// lowerCallExpr handles value-producing calls to user-defined functions.
// Phase 12.0: if the callee is a Java FFI function, emit the direct Java call.
func (l *lowerer) lowerCallExpr(e *aotir.CallExpr) (javasrc.Expr, error) {
	// Phase 12.0: Java FFI call.
	if decl, ok := l.javaFuncs[e.Func]; ok {
		args, err := l.lowerExprs(e.Args)
		if err != nil {
			return nil, err
		}
		if decl.IsStatic {
			return &javasrc.StaticCallExpr{
				Class:  decl.ClassName,
				Method: decl.MethodName,
				Args:   args,
			}, nil
		}
		if len(args) == 0 {
			return nil, fmt.Errorf("jvm/lower: instance Java call %s.%s needs at least one arg",
				decl.ClassName, decl.MethodName)
		}
		return &javasrc.CallExpr{
			Receiver: args[0],
			Method:   decl.MethodName,
			Args:     args[1:],
		}, nil
	}
	args, err := l.lowerExprs(e.Args)
	if err != nil {
		return nil, err
	}
	return &javasrc.StaticCallExpr{
		Class:  l.className,
		Method: e.Func,
		Args:   args,
	}, nil
}

// --- List lowering ---

// lowerListLit lowers a ListLit expression.
// For immutable (let) lists: java.util.List.of(...)
// For mutable (var) lists: new java.util.ArrayList<>(java.util.List.of(...))
// Note: the mutability decision is made at the LetStmt level; here we always
// produce an ArrayList so it can be used in either context. The LetStmt
// lowering decides the declared type.
func (l *lowerer) lowerListLit(e *aotir.ListLit) (javasrc.Expr, error) {
	elems, err := l.lowerExprs(e.Elems)
	if err != nil {
		return nil, err
	}
	if len(elems) == 0 {
		// new java.util.ArrayList<>()
		return &javasrc.NewExpr{
			Type: javasrc.TypeRef{Name: "java.util.ArrayList"},
			Args: nil,
		}, nil
	}
	// new java.util.ArrayList<>(java.util.List.of(e1, e2, ...))
	listOf := &javasrc.StaticCallExpr{
		Class:  "java.util.List",
		Method: "of",
		Args:   elems,
	}
	return &javasrc.NewExpr{
		Type: javasrc.TypeRef{Name: "java.util.ArrayList"},
		Args: []javasrc.Expr{listOf},
	}, nil
}

// lowerIndexExpr lowers xs[i] to (T) xs.get((int) i).
func (l *lowerer) lowerIndexExpr(e *aotir.IndexExpr) (javasrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	idx, err := l.lowerExpr(e.Index)
	if err != nil {
		return nil, err
	}
	// Java List.get(int) requires int, not long
	intIdx := &javasrc.CastExpr{Type: javasrc.TypeRef{Name: "int"}, X: idx}
	get := &javasrc.CallExpr{
		Receiver: recv,
		Method:   "get",
		Args:     []javasrc.Expr{intIdx},
	}
	// For record elements, an explicit cast is needed because the list is List<Object>.
	if e.ElemType == aotir.TypeRecord && e.ElemRecordName != "" {
		return &javasrc.CastExpr{
			Type: javasrc.TypeRef{Name: e.ElemRecordName},
			X:    get,
		}, nil
	}
	// The result of List.get() is Object; cast to the unboxed primitive where needed.
	// Java auto-unboxes when assigning to a primitive variable, so we don't need
	// an explicit unboxing cast here -- the context will handle it.
	return get, nil
}

// lowerLenExpr lowers len(xs) for a list to (long) xs.size().
func (l *lowerer) lowerLenExpr(e *aotir.LenExpr) (javasrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	sizeCall := &javasrc.CallExpr{
		Receiver: recv,
		Method:   "size",
		Args:     nil,
	}
	return &javasrc.CastExpr{Type: javasrc.TypeLong, X: sizeCall}, nil
}

// lowerAppendExpr lowers append(xs, v) to a new ArrayList copying xs then adding v.
// Since in practice this appears as AssignStmt{xs = append(xs, v)}, we emit
// a new ArrayList to preserve functional semantics.
func (l *lowerer) lowerAppendExpr(e *aotir.AppendExpr) (javasrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(e.Value)
	if err != nil {
		return nil, err
	}
	// Build: dev.mochi.runtime.coll.ListUtil.append(xs, v)
	return &javasrc.StaticCallExpr{
		Class:  "dev.mochi.runtime.coll.ListUtil",
		Method: "append",
		Args:   []javasrc.Expr{recv, val},
	}, nil
}

// --- Map lowering ---

// lowerMapLit lowers a MapLit to a MapUtil.of(k1,v1,k2,v2,...) or MapUtil.empty().
func (l *lowerer) lowerMapLit(e *aotir.MapLit) (javasrc.Expr, error) {
	if len(e.Keys) == 0 {
		return &javasrc.StaticCallExpr{
			Class:  "dev.mochi.runtime.coll.MapUtil",
			Method: "empty",
			Args:   nil,
		}, nil
	}
	// Interleave keys and values: MapUtil.of(k1, v1, k2, v2, ...)
	args := make([]javasrc.Expr, 0, len(e.Keys)*2)
	for i := range e.Keys {
		k, err := l.lowerExpr(e.Keys[i])
		if err != nil {
			return nil, err
		}
		v, err := l.lowerExpr(e.Values[i])
		if err != nil {
			return nil, err
		}
		args = append(args, k, v)
	}
	return &javasrc.StaticCallExpr{
		Class:  "dev.mochi.runtime.coll.MapUtil",
		Method: "of",
		Args:   args,
	}, nil
}

// lowerMapGetExpr lowers m[k] to m.get(k).
func (l *lowerer) lowerMapGetExpr(e *aotir.MapGetExpr) (javasrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	key, err := l.lowerExpr(e.Key)
	if err != nil {
		return nil, err
	}
	return &javasrc.CallExpr{
		Receiver: recv,
		Method:   "get",
		Args:     []javasrc.Expr{key},
	}, nil
}

// lowerMapHasExpr lowers m.has(k) to m.containsKey(k).
func (l *lowerer) lowerMapHasExpr(e *aotir.MapHasExpr) (javasrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	key, err := l.lowerExpr(e.Key)
	if err != nil {
		return nil, err
	}
	return &javasrc.CallExpr{
		Receiver: recv,
		Method:   "containsKey",
		Args:     []javasrc.Expr{key},
	}, nil
}

// lowerMapLenExpr lowers len(m) to (long) m.size().
func (l *lowerer) lowerMapLenExpr(e *aotir.MapLenExpr) (javasrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	sizeCall := &javasrc.CallExpr{
		Receiver: recv,
		Method:   "size",
		Args:     nil,
	}
	return &javasrc.CastExpr{Type: javasrc.TypeLong, X: sizeCall}, nil
}

// lowerMapKeysExpr lowers m.keys() to new java.util.ArrayList<>(m.keySet()).
func (l *lowerer) lowerMapKeysExpr(e *aotir.MapKeysExpr) (javasrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	keySet := &javasrc.CallExpr{
		Receiver: recv,
		Method:   "keySet",
		Args:     nil,
	}
	return &javasrc.NewExpr{
		Type: javasrc.TypeRef{Name: "java.util.ArrayList"},
		Args: []javasrc.Expr{keySet},
	}, nil
}

// --- Set lowering ---

// lowerSetLiteralExpr lowers set{e1,e2,...} to
// new java.util.LinkedHashSet<>(java.util.List.of(e1, e2, ...)).
func (l *lowerer) lowerSetLiteralExpr(e *aotir.SetLiteralExpr) (javasrc.Expr, error) {
	elems, err := l.lowerExprs(e.Elems)
	if err != nil {
		return nil, err
	}
	if len(elems) == 0 {
		return &javasrc.NewExpr{
			Type: javasrc.TypeRef{Name: "java.util.LinkedHashSet"},
			Args: nil,
		}, nil
	}
	listOf := &javasrc.StaticCallExpr{
		Class:  "java.util.List",
		Method: "of",
		Args:   elems,
	}
	return &javasrc.NewExpr{
		Type: javasrc.TypeRef{Name: "java.util.LinkedHashSet"},
		Args: []javasrc.Expr{listOf},
	}, nil
}

// lowerSetAddExpr lowers add(s, x).
// Since Java LinkedHashSet.add() mutates in place and returns boolean,
// we lower SetAddExpr in two ways:
// - As a standalone expression: emit a StaticCall to ListUtil.setAdd(s, x) which
//   does s.add(x) and returns s.
// - In AssignStmt context: handled separately in lowerStmt.
// Here we emit a static helper call that returns the (mutated) set reference.
func (l *lowerer) lowerSetAddExpr(e *aotir.SetAddExpr) (javasrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	elem, err := l.lowerExpr(e.Elem)
	if err != nil {
		return nil, err
	}
	// Emit receiver.add(elem) as a call expression that returns the receiver.
	// We use ListUtil.setAdd(set, elem) -> set to keep it as an expression.
	return &javasrc.StaticCallExpr{
		Class:  "dev.mochi.runtime.coll.ListUtil",
		Method: "setAdd",
		Args:   []javasrc.Expr{recv, elem},
	}, nil
}

// lowerSetHasExpr lowers has(s, x) to s.contains(x).
func (l *lowerer) lowerSetHasExpr(e *aotir.SetHasExpr) (javasrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	elem, err := l.lowerExpr(e.Elem)
	if err != nil {
		return nil, err
	}
	return &javasrc.CallExpr{
		Receiver: recv,
		Method:   "contains",
		Args:     []javasrc.Expr{elem},
	}, nil
}

// lowerSetLenExpr lowers len(s) to (long) s.size().
func (l *lowerer) lowerSetLenExpr(e *aotir.SetLenExpr) (javasrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	sizeCall := &javasrc.CallExpr{
		Receiver: recv,
		Method:   "size",
		Args:     nil,
	}
	return &javasrc.CastExpr{Type: javasrc.TypeLong, X: sizeCall}, nil
}

// --- Record lowering ---

// lowerRecordLit lowers a RecordLit to `new RecordName(arg1, arg2, ...)`.
// The fields in RecordLit are already in record-decl source order (the C lowerer
// enforces this), so we emit them in order to match the canonical constructor.
func (l *lowerer) lowerRecordLit(e *aotir.RecordLit) (javasrc.Expr, error) {
	args := make([]javasrc.Expr, len(e.Fields))
	for i, f := range e.Fields {
		v, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, fmt.Errorf("record %q field %q: %w", e.TypeName, f.Name, err)
		}
		args[i] = v
	}
	return &javasrc.NewExpr{
		Type: javasrc.TypeRef{Name: e.TypeName},
		Args: args,
	}, nil
}

// lowerFieldAccess lowers `p.fieldName` to `p.fieldName()`.
// Java records expose fields via accessor methods with the same name as the field,
// not as public fields. So `p.x` in Mochi becomes `p.x()` in Java.
func (l *lowerer) lowerFieldAccess(e *aotir.FieldAccess) (javasrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &javasrc.CallExpr{
		Receiver: recv,
		Method:   e.FieldName,
		Args:     nil,
	}, nil
}

// lowerVariantLit lowers variant construction `Circle(1.5)` to
// `new Shape.Circle(1.5)` (qualified with union name).
func (l *lowerer) lowerVariantLit(e *aotir.VariantLit) (javasrc.Expr, error) {
	args := make([]javasrc.Expr, len(e.Fields))
	for i, f := range e.Fields {
		v, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, fmt.Errorf("variant %q.%q field %q: %w", e.UnionName, e.VariantName, f.Name, err)
		}
		args[i] = v
	}
	return &javasrc.NewExpr{
		Type: javasrc.TypeRef{Name: e.UnionName + "." + e.VariantName},
		Args: args,
	}, nil
}

// lowerVariantFieldAccess lowers a field read from a variant-typed value.
// The aotir knows which variant we're in (VariantName), so the receiver is
// already typed. Java record accessor methods use the field name directly,
// same as for plain records.
func (l *lowerer) lowerVariantFieldAccess(e *aotir.VariantFieldAccess) (javasrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &javasrc.CallExpr{
		Receiver: recv,
		Method:   e.FieldName,
		Args:     nil,
	}, nil
}
