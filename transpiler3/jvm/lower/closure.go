package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// lowerFunLit translates an aotir.FunLit (closure literal) to a Java lambda
// expression. The FunLit refers to a lifted function by name; the lambda body
// calls that lifted static method, threading any captured variables from the env.
//
// For non-capturing closures (Captures empty), the lambda directly calls the
// lifted static method. For capturing closures, the lifted function expects the
// captured variables as additional leading parameters; the lambda captures them
// from the enclosing scope and forwards them.
//
// Example (non-capturing, (int)->int):
//   FunLit{FuncName: "__anon_1", Sig: {[TypeInt], TypeInt}, Captures: []}
//   -> x -> __anon_1(x)
//
// Example (capturing, (int)->int capturing n):
//   FunLit{FuncName: "__anon_2", Sig: {[TypeInt], TypeInt}, Captures: [{n, TypeInt, "n"}]}
//   -> x -> __anon_2(n, x)
func (l *lowerer) lowerFunLit(e *aotir.FunLit) (javasrc.Expr, error) {
	sig := e.Sig
	if sig == nil {
		return nil, fmt.Errorf("jvm/lower: FunLit %q has nil Sig", e.FuncName)
	}

	// Build lambda parameters (the Sig's ParamTypes, not the captures).
	params := make([]javasrc.Param, len(sig.ParamTypes))
	paramNames := make([]string, len(sig.ParamTypes))
	for i := range sig.ParamTypes {
		name := fmt.Sprintf("__p%d", i)
		paramNames[i] = name
		params[i] = javasrc.Param{Name: name} // type-inferred lambda param
	}

	// Build the call arguments: captures first, then the sig params.
	callArgs := make([]javasrc.Expr, 0, len(e.Captures)+len(paramNames))
	for _, cap := range e.Captures {
		callArgs = append(callArgs, &javasrc.NameExpr{Name: cap.SrcName})
	}
	for _, pn := range paramNames {
		callArgs = append(callArgs, &javasrc.NameExpr{Name: pn})
	}

	// Determine the body: static call to the lifted function.
	if sig.ReturnType == aotir.TypeUnit {
		// Return type is void: use a block lambda with ExprStmt.
		callStmt := &javasrc.ExprStmt{X: &javasrc.StaticCallExpr{
			Class:  l.className,
			Method: e.FuncName,
			Args:   callArgs,
		}}
		block := &javasrc.Block{Stmts: []javasrc.Stmt{callStmt}}
		return &javasrc.LambdaExpr{Params: params, Block: block}, nil
	}
	bodyExpr := &javasrc.StaticCallExpr{
		Class:  l.className,
		Method: e.FuncName,
		Args:   callArgs,
	}

	return &javasrc.LambdaExpr{Params: params, Body: bodyExpr}, nil
}

// lowerFunCallExpr translates an aotir.FunCallExpr (indirect call through a
// function-typed value) to a Java functional interface method invocation.
// The SAM method name depends on the functional interface, which depends on
// the callee's FunSig.
func (l *lowerer) lowerFunCallExpr(e *aotir.FunCallExpr) (javasrc.Expr, error) {
	callee, err := l.lowerExpr(e.Callee)
	if err != nil {
		return nil, err
	}
	args, err := l.lowerExprs(e.Args)
	if err != nil {
		return nil, err
	}

	// Determine the SAM name from the callee's FunSig.
	samName := samMethodName(e.Callee)

	return &javasrc.CallExpr{
		Receiver: callee,
		Method:   samName,
		Args:     args,
	}, nil
}

// samMethodName returns the SAM method name for invoking a function-typed value.
// It inspects the Expr to find its FunSig; falls back to "apply" (generic
// Function.apply) when the sig is unavailable.
func samMethodName(e aotir.Expr) string {
	var sig *aotir.FunSig
	switch e := e.(type) {
	case *aotir.VarRef:
		sig = e.FunSig
	case *aotir.FunLit:
		sig = e.Sig
	}
	if sig == nil {
		return "apply"
	}
	return samFromSig(sig)
}

// samFromSig selects the SAM method name for the functional interface that
// matches sig, following the preference table in Phase 6 spec.
func samFromSig(sig *aotir.FunSig) string {
	n := len(sig.ParamTypes)
	ret := sig.ReturnType

	switch n {
	case 0:
		switch ret {
		case aotir.TypeInt:
			return "getAsLong"
		case aotir.TypeFloat:
			return "getAsDouble"
		case aotir.TypeBool:
			return "getAsBoolean"
		case aotir.TypeUnit:
			return "run"
		default:
			return "get"
		}
	case 1:
		p0 := sig.ParamTypes[0]
		switch {
		case p0 == aotir.TypeInt && ret == aotir.TypeInt:
			return "applyAsLong"
		case p0 == aotir.TypeInt && ret == aotir.TypeBool:
			return "test"
		case p0 == aotir.TypeFloat && ret == aotir.TypeFloat:
			return "applyAsDouble"
		case ret == aotir.TypeBool:
			return "test"
		case ret == aotir.TypeUnit:
			return "accept"
		default:
			return "apply"
		}
	case 2:
		p0, p1 := sig.ParamTypes[0], sig.ParamTypes[1]
		switch {
		case p0 == aotir.TypeInt && p1 == aotir.TypeInt && ret == aotir.TypeInt:
			return "applyAsLong"
		case ret == aotir.TypeBool:
			return "test"
		case ret == aotir.TypeUnit:
			return "accept"
		default:
			return "apply"
		}
	default:
		return "apply"
	}
}

// lowerListMapExpr lowers ListMapExpr (map(xs, fn)) to a stream pipeline:
// xs.stream().map(fn).collect(java.util.stream.Collectors.toList())
func (l *lowerer) lowerListMapExpr(e *aotir.ListMapExpr) (javasrc.Expr, error) {
	list, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, err
	}
	fn, err := l.lowerExpr(e.Fn)
	if err != nil {
		return nil, err
	}

	// xs.stream().map(fn).collect(Collectors.toList())
	stream := &javasrc.CallExpr{Receiver: list, Method: "stream", Args: nil}
	mapCall := &javasrc.CallExpr{Receiver: stream, Method: "map", Args: []javasrc.Expr{fn}}
	collectors := &javasrc.StaticCallExpr{
		Class:  "java.util.stream.Collectors",
		Method: "toList",
		Args:   nil,
	}
	return &javasrc.CallExpr{Receiver: mapCall, Method: "collect", Args: []javasrc.Expr{collectors}}, nil
}

// lowerListFilterExpr lowers ListFilterExpr (filter(xs, fn)) to a stream pipeline:
// xs.stream().filter(fn).collect(java.util.stream.Collectors.toList())
func (l *lowerer) lowerListFilterExpr(e *aotir.ListFilterExpr) (javasrc.Expr, error) {
	list, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, err
	}
	fn, err := l.lowerExpr(e.Fn)
	if err != nil {
		return nil, err
	}

	stream := &javasrc.CallExpr{Receiver: list, Method: "stream", Args: nil}
	filterCall := &javasrc.CallExpr{Receiver: stream, Method: "filter", Args: []javasrc.Expr{fn}}
	collectors := &javasrc.StaticCallExpr{
		Class:  "java.util.stream.Collectors",
		Method: "toList",
		Args:   nil,
	}
	return &javasrc.CallExpr{Receiver: filterCall, Method: "collect", Args: []javasrc.Expr{collectors}}, nil
}

// lowerListFoldlExpr lowers ListFoldlExpr (reduce(xs, fn, init)) to a stream reduce:
// xs.stream().reduce(init, fn)
func (l *lowerer) lowerListFoldlExpr(e *aotir.ListFoldlExpr) (javasrc.Expr, error) {
	list, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, err
	}
	fn, err := l.lowerExpr(e.Fn)
	if err != nil {
		return nil, err
	}
	init, err := l.lowerExpr(e.Init)
	if err != nil {
		return nil, err
	}

	// xs.stream().reduce(init, fn)
	stream := &javasrc.CallExpr{Receiver: list, Method: "stream", Args: nil}
	return &javasrc.CallExpr{
		Receiver: stream,
		Method:   "reduce",
		Args:     []javasrc.Expr{init, fn},
	}, nil
}

// funcTypeRef returns the Java functional interface TypeRef for a FunSig.
// This is used for variable declarations of function type.
func funcTypeRef(sig *aotir.FunSig) javasrc.TypeRef {
	if sig == nil {
		return javasrc.TypeRef{Name: "java.util.function.Function", TypeArgs: []javasrc.TypeRef{javasrc.TypeObject, javasrc.TypeObject}}
	}
	n := len(sig.ParamTypes)
	ret := sig.ReturnType

	switch n {
	case 0:
		switch ret {
		case aotir.TypeInt:
			return javasrc.TypeRef{Name: "java.util.function.LongSupplier"}
		case aotir.TypeFloat:
			return javasrc.TypeRef{Name: "java.util.function.DoubleSupplier"}
		case aotir.TypeBool:
			return javasrc.TypeRef{Name: "java.util.function.BooleanSupplier"}
		case aotir.TypeUnit:
			return javasrc.TypeRef{Name: "java.lang.Runnable"}
		default:
			return javasrc.TypeRef{Name: "java.util.function.Supplier", TypeArgs: []javasrc.TypeRef{boxedType(ret)}}
		}
	case 1:
		p0 := sig.ParamTypes[0]
		switch {
		case p0 == aotir.TypeInt && ret == aotir.TypeInt:
			return javasrc.TypeRef{Name: "java.util.function.LongUnaryOperator"}
		case p0 == aotir.TypeInt && ret == aotir.TypeBool:
			return javasrc.TypeRef{Name: "java.util.function.LongPredicate"}
		case p0 == aotir.TypeFloat && ret == aotir.TypeFloat:
			return javasrc.TypeRef{Name: "java.util.function.DoubleUnaryOperator"}
		case ret == aotir.TypeBool:
			return javasrc.TypeRef{Name: "java.util.function.Predicate", TypeArgs: []javasrc.TypeRef{boxedType(p0)}}
		case ret == aotir.TypeUnit:
			return javasrc.TypeRef{Name: "java.util.function.Consumer", TypeArgs: []javasrc.TypeRef{boxedType(p0)}}
		default:
			return javasrc.TypeRef{Name: "java.util.function.Function", TypeArgs: []javasrc.TypeRef{boxedType(p0), boxedType(ret)}}
		}
	case 2:
		p0, p1 := sig.ParamTypes[0], sig.ParamTypes[1]
		switch {
		case p0 == aotir.TypeInt && p1 == aotir.TypeInt && ret == aotir.TypeInt:
			return javasrc.TypeRef{Name: "java.util.function.LongBinaryOperator"}
		case ret == aotir.TypeBool:
			return javasrc.TypeRef{Name: "java.util.function.BiPredicate", TypeArgs: []javasrc.TypeRef{boxedType(p0), boxedType(p1)}}
		case ret == aotir.TypeUnit:
			return javasrc.TypeRef{Name: "java.util.function.BiConsumer", TypeArgs: []javasrc.TypeRef{boxedType(p0), boxedType(p1)}}
		default:
			return javasrc.TypeRef{Name: "java.util.function.BiFunction", TypeArgs: []javasrc.TypeRef{boxedType(p0), boxedType(p1), boxedType(ret)}}
		}
	default:
		return javasrc.TypeRef{Name: "java.util.function.Function", TypeArgs: []javasrc.TypeRef{javasrc.TypeObject, javasrc.TypeObject}}
	}
}
