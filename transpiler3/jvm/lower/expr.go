package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// lowerExpr translates an aotir.Expr to a javasrc.Expr.
func lowerExpr(e aotir.Expr) (javasrc.Expr, error) {
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
		return lowerBinaryExpr(e)

	case *aotir.UnaryExpr:
		return lowerUnaryExpr(e)

	case *aotir.NumCastExpr:
		// int(x) where x is float: cast to long
		operand, err := lowerExpr(e.Operand)
		if err != nil {
			return nil, err
		}
		return &javasrc.CastExpr{Type: javasrc.TypeLong, X: operand}, nil

	default:
		return nil, fmt.Errorf("jvm/lower: unsupported expr %T", e)
	}
}

func lowerBinaryExpr(e *aotir.BinaryExpr) (javasrc.Expr, error) {
	left, err := lowerExpr(e.Left)
	if err != nil {
		return nil, err
	}
	right, err := lowerExpr(e.Right)
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

	default:
		return nil, fmt.Errorf("jvm/lower: unsupported binary op %v", e.Op)
	}
}

func lowerUnaryExpr(e *aotir.UnaryExpr) (javasrc.Expr, error) {
	operand, err := lowerExpr(e.Operand)
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
