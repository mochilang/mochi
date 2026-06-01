// Phase 8: string/list transform and math builtin lowering.
//
// Lowering table:
//
//	ListMapExpr    → xs.map(fn)
//	ListFilterExpr → xs.filter(fn)
//	ListFoldlExpr  → xs.reduce(fn, init)
//	StrSplitExpr   → s.split(sep)
//	StrJoinExpr    → xs.join(sep)
//	StrUpperExpr   → s.toUpperCase()
//	StrLowerExpr   → s.toLowerCase()
//	StrReverseExpr → mochi_str_reverse(s) — runtime helper
//	NumCastExpr    → Math.trunc(v) for float→int; identity for int→float
//	MathCallExpr   → Math.sqrt / Math.abs / Math.floor / Math.ceil / etc.
package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

func (l *lowerer) lowerListMapExpr(e *aotir.ListMapExpr) (tstree.Expr, error) {
	xs, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, fmt.Errorf("ts lower: list map: %w", err)
	}
	fn, err := l.lowerExpr(e.Fn)
	if err != nil {
		return nil, fmt.Errorf("ts lower: list map fn: %w", err)
	}
	return &tstree.MemberCallExpr{
		Receiver: xs,
		Method:   "map",
		Args:     []tstree.Expr{fn},
	}, nil
}

func (l *lowerer) lowerListFilterExpr(e *aotir.ListFilterExpr) (tstree.Expr, error) {
	xs, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, fmt.Errorf("ts lower: list filter: %w", err)
	}
	fn, err := l.lowerExpr(e.Fn)
	if err != nil {
		return nil, fmt.Errorf("ts lower: list filter fn: %w", err)
	}
	return &tstree.MemberCallExpr{
		Receiver: xs,
		Method:   "filter",
		Args:     []tstree.Expr{fn},
	}, nil
}

func (l *lowerer) lowerListFoldlExpr(e *aotir.ListFoldlExpr) (tstree.Expr, error) {
	xs, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, fmt.Errorf("ts lower: list foldl: %w", err)
	}
	fn, err := l.lowerExpr(e.Fn)
	if err != nil {
		return nil, fmt.Errorf("ts lower: list foldl fn: %w", err)
	}
	init, err := l.lowerExpr(e.Init)
	if err != nil {
		return nil, fmt.Errorf("ts lower: list foldl init: %w", err)
	}
	return &tstree.MemberCallExpr{
		Receiver: xs,
		Method:   "reduce",
		Args:     []tstree.Expr{fn, init},
	}, nil
}

func (l *lowerer) lowerStrSplitExpr(e *aotir.StrSplitExpr) (tstree.Expr, error) {
	s, err := l.lowerExpr(e.Str)
	if err != nil {
		return nil, fmt.Errorf("ts lower: str split: %w", err)
	}
	sep, err := l.lowerExpr(e.Sep)
	if err != nil {
		return nil, fmt.Errorf("ts lower: str split sep: %w", err)
	}
	return &tstree.MemberCallExpr{
		Receiver: s,
		Method:   "split",
		Args:     []tstree.Expr{sep},
	}, nil
}

func (l *lowerer) lowerStrJoinExpr(e *aotir.StrJoinExpr) (tstree.Expr, error) {
	xs, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, fmt.Errorf("ts lower: str join: %w", err)
	}
	sep, err := l.lowerExpr(e.Sep)
	if err != nil {
		return nil, fmt.Errorf("ts lower: str join sep: %w", err)
	}
	return &tstree.MemberCallExpr{
		Receiver: xs,
		Method:   "join",
		Args:     []tstree.Expr{sep},
	}, nil
}

func (l *lowerer) lowerStrUpperExpr(e *aotir.StrUpperExpr) (tstree.Expr, error) {
	s, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: str upper: %w", err)
	}
	return &tstree.MemberCallExpr{Receiver: s, Method: "toUpperCase"}, nil
}

func (l *lowerer) lowerStrLowerExpr(e *aotir.StrLowerExpr) (tstree.Expr, error) {
	s, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: str lower: %w", err)
	}
	return &tstree.MemberCallExpr{Receiver: s, Method: "toLowerCase"}, nil
}

func (l *lowerer) lowerStrReverseExpr(e *aotir.StrReverseExpr) (tstree.Expr, error) {
	l.runtime.strReverse = true
	s, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: str reverse: %w", err)
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "mochi_str_reverse"},
		Args:   []tstree.Expr{s},
	}, nil
}

func (l *lowerer) lowerNumCastExpr(e *aotir.NumCastExpr) (tstree.Expr, error) {
	inner, err := l.lowerExpr(e.Operand)
	if err != nil {
		return nil, fmt.Errorf("ts lower: num cast: %w", err)
	}
	// float→int: Math.trunc truncates toward zero, matching Mochi semantics.
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "Math.trunc"},
		Args:   []tstree.Expr{inner},
	}, nil
}

func (l *lowerer) lowerMathCallExpr(e *aotir.MathCallExpr) (tstree.Expr, error) {
	arg, err := l.lowerExpr(e.Arg)
	if err != nil {
		return nil, fmt.Errorf("ts lower: math call %q: %w", e.Func, err)
	}
	fn := mathTSFunc(e.Func)
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: fn},
		Args:   []tstree.Expr{arg},
	}, nil
}

// strReverseDecls emits the mochi_str_reverse helper when needed.
func (l *lowerer) strReverseDecls() []tstree.Decl {
	if !l.runtime.strReverse {
		return nil
	}
	return []tstree.Decl{&tstree.FuncDecl{
		Name:       "mochi_str_reverse",
		Params:     []tstree.FuncParam{{Name: "s", Type: "string"}},
		ReturnType: "string",
		Body: []tstree.Stmt{
			&tstree.RawStmt{Text: "return [...s].reverse().join(\"\");"},
		},
	}}
}

// mathTSFunc maps aotir MathCallExpr.Func names to TypeScript Math method paths.
func mathTSFunc(name string) string {
	switch name {
	case "abs_i64", "abs_f64":
		return "Math.abs"
	case "floor":
		return "Math.floor"
	case "ceil":
		return "Math.ceil"
	case "sqrt":
		return "Math.sqrt"
	case "pow":
		return "Math.pow"
	case "log":
		return "Math.log"
	case "log2":
		return "Math.log2"
	case "exp":
		return "Math.exp"
	case "sin":
		return "Math.sin"
	case "cos":
		return "Math.cos"
	case "tan":
		return "Math.tan"
	case "atan2":
		return "Math.atan2"
	default:
		return "Math.abs"
	}
}
