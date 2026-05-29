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
	default:
		return nil, fmt.Errorf("transpiler3/go/lower: does not handle expr %T", e)
	}
}

// lowerListLit emits `[]T{e0, e1, ...}` for a Phase 3.1 list of
// scalar elements.
func (l *lowerer) lowerListLit(e *aotir.ListLit) (gotree.Expr, error) {
	elemType, err := l.lowerType(e.ElemType)
	if err != nil {
		return nil, fmt.Errorf("list literal: %w", err)
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
	case aotir.BinEqI64, aotir.BinEqF64, aotir.BinEqBool, aotir.BinEqStr:
		return "==", true
	case aotir.BinNeI64, aotir.BinNeF64, aotir.BinNeBool, aotir.BinNeStr:
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
