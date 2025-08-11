package interpreter

import (
	"mochi/parser"
	"mochi/types"
)

// FoldPureCalls traverses prog and replaces pure function calls with their
// evaluated literal result. This runs a simple form of ahead-of-time
// constant folding for the interpreter.
func FoldPureCalls(prog *parser.Program, env *types.Env) {
	if prog == nil || env == nil {
		return
	}
	// Register top-level functions so EvalPureCall can resolve them.
	for _, s := range prog.Statements {
		if s.Fun != nil {
			env.SetFunc(s.Fun.Name, s.Fun)
		}
	}
	for _, s := range prog.Statements {
		foldStmt(s, env)
	}
}

func foldStmt(s *parser.Statement, env *types.Env) {
	switch {
	case s.Let != nil && s.Let.Value != nil:
		foldExpr(s.Let.Value, env)
		if lit, ok := evalConstExpr(s.Let.Value, env); ok {
			env.SetValue(s.Let.Name, literalValue(lit), false)
		}
	case s.Var != nil && s.Var.Value != nil:
		foldExpr(s.Var.Value, env)
	case s.Assign != nil:
		foldExpr(s.Assign.Value, env)
	case s.Expr != nil:
		foldExpr(s.Expr.Expr, env)
	case s.Return != nil:
		foldExpr(s.Return.Value, env)
	case s.If != nil:
		foldExpr(s.If.Cond, env)
		for _, stmt := range s.If.Then {
			foldStmt(stmt, env)
		}
		for _, stmt := range s.If.Else {
			foldStmt(stmt, env)
		}
	case s.While != nil:
		foldExpr(s.While.Cond, env)
		for _, stmt := range s.While.Body {
			foldStmt(stmt, env)
		}
	case s.For != nil:
		foldExpr(s.For.Source, env)
		if s.For.RangeEnd != nil {
			foldExpr(s.For.RangeEnd, env)
		}
		for _, stmt := range s.For.Body {
			foldStmt(stmt, env)
		}
	case s.Fun != nil:
		child := types.NewEnv(env)
		for _, stmt := range s.Fun.Body {
			foldStmt(stmt, child)
		}
	}
}

func foldExpr(e *parser.Expr, env *types.Env) {
	if e == nil {
		return
	}
	foldUnary(e.Binary.Left, env)
	for _, op := range e.Binary.Right {
		// BinaryOp.Right now holds a *parser.Unary. Fold the unary
		// expression rather than assuming a postfix expression.
		foldUnary(op.Right, env)
	}
	if call, ok := callPattern(e); ok {
		if lit, ok := foldCall(call, env); ok {
			e.Binary.Left = &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Lit: lit}}}
			e.Binary.Right = nil
		}
	}
}

func foldUnary(u *parser.Unary, env *types.Env) {
	if u == nil {
		return
	}
	foldPostfixExpr(u.Value, env)
}

func foldPostfixExpr(p *parser.PostfixExpr, env *types.Env) {
	if p == nil {
		return
	}
	foldPrimary(p.Target, env)
	for _, op := range p.Ops {
		if call := op.Call; call != nil {
			for _, a := range call.Args {
				foldExpr(a, env)
			}
			// Postfix call cannot be folded without a function name
		} else if idx := op.Index; idx != nil {
			if idx.Start != nil {
				foldExpr(idx.Start, env)
			}
			if idx.End != nil {
				foldExpr(idx.End, env)
			}
		}
	}
}

func foldPrimary(p *parser.Primary, env *types.Env) {
	if p == nil {
		return
	}
	if p.Call != nil {
		for _, a := range p.Call.Args {
			foldExpr(a, env)
		}
		if lit, ok := EvalPureCall(p.Call, env); ok {
			p.Lit = lit
			p.Call = nil
		}
	} else if p.Group != nil {
		foldExpr(p.Group, env)
		if lit := extractLiteral(p.Group); lit != nil {
			p.Lit = lit
			p.Group = nil
		}
	} else if p.FunExpr != nil {
		child := types.NewEnv(env)
		if p.FunExpr.ExprBody != nil {
			foldExpr(p.FunExpr.ExprBody, child)
		} else {
			for _, s := range p.FunExpr.BlockBody {
				foldStmt(s, child)
			}
		}
	}
}

func extractLiteral(e *parser.Expr) *parser.Literal {
	if e == nil {
		return nil
	}
	if len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return nil
	}
	if p.Target != nil {
		return p.Target.Lit
	}
	return nil
}

func evalConstExpr(e *parser.Expr, env *types.Env) (*parser.Literal, bool) {
	if lit := extractLiteral(e); lit != nil {
		return lit, true
	}
	if call, ok := callPattern(e); ok {
		return foldCall(call, env)
	}
	return EvalConstExpr(e, env)
}

func literalValue(l *parser.Literal) any {
	switch {
	case l.Int != nil:
		return *l.Int
	case l.Float != nil:
		return *l.Float
	case l.Str != nil:
		return *l.Str
	case l.Bool != nil:
		return *l.Bool
	default:
		return nil
	}
}

func foldCall(call *parser.CallExpr, env *types.Env) (*parser.Literal, bool) {
	args := make([]*parser.Expr, len(call.Args))
	for i, a := range call.Args {
		if lit := extractLiteral(a); lit != nil {
			args[i] = &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Lit: lit}}}}}
			continue
		}
		if name, ok := identName(a); ok {
			if val, err := env.GetValue(name); err == nil {
				if l := types.AnyToLiteral(val); l != nil {
					args[i] = &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Lit: l}}}}}
					continue
				}
			}
		}
		return nil, false
	}
	return EvalPureCall(&parser.CallExpr{Func: call.Func, Args: args}, env)
}
