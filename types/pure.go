package types

import (
	"mochi/parser"

	"github.com/alecthomas/participle/v2/lexer"
)

// isLiteralExpr returns true if e is a literal expression.
func IsLiteralExpr(e *parser.Expr) bool {
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
	return p.Target != nil && p.Target.Lit != nil
}

// anyToLiteral converts basic Go values to a Mochi literal.
func AnyToLiteral(v any) *parser.Literal {
	switch t := v.(type) {
	case int:
		il := parser.IntLit(t)
		return &parser.Literal{Int: &il}
	case float64:
		return &parser.Literal{Float: &t}
	case string:
		return &parser.Literal{Str: &t}
	default:
		return nil
	}
}

// isPureCall returns true if call invokes a pure function.
func isPureCall(call *parser.CallExpr, env *Env) bool {
	t, err := env.GetVar(call.Func)
	if err != nil {
		return false
	}
	ft, ok := t.(FuncType)
	if !ok || !ft.Pure {
		return false
	}
	for _, arg := range call.Args {
		if !IsLiteralExpr(arg) {
			return false
		}
	}
	return true
}

// isPureStmt checks if a statement has no side effects.
func isPureStmt(s *parser.Statement, env *Env) bool {
	switch {
	case s.Let != nil:
		if s.Let.Value != nil && !isPureExpr(s.Let.Value, env) {
			return false
		}
		env.SetVar(s.Let.Name, AnyType{}, false)
		return true
	case s.Return != nil:
		return isPureExpr(s.Return.Value, env)
	case s.Expr != nil:
		return isPureExpr(s.Expr.Expr, env)
	case s.Fun != nil:
		return isPureFunction(s.Fun, env)
	}
	return false
}

// isPureExpr recursively checks if expression e has no side effects.
func isPureExpr(e *parser.Expr, env *Env) bool {
	if e == nil {
		return true
	}
	if call, ok := callPattern(e); ok {
		return isPureCall(call, env)
	}
	if !isPureUnary(e.Binary.Left, env) {
		return false
	}
	for _, op := range e.Binary.Right {
		if !isPurePostfix(op.Right, env) {
			return false
		}
	}
	return true
}

func isPureUnary(u *parser.Unary, env *Env) bool {
	if !isPurePostfix(u.Value, env) {
		return false
	}
	return true
}

func isPurePostfix(p *parser.PostfixExpr, env *Env) bool {
	if !isPurePrimary(p.Target, env) {
		return false
	}
	for _, op := range p.Ops {
		if op.Index != nil || op.Cast != nil || op.Call != nil {
			return false
		}
	}
	return true
}

func isPurePrimary(p *parser.Primary, env *Env) bool {
	switch {
	case p.Lit != nil:
		return true
	case p.Call != nil:
		return isPureCall(p.Call, env)
	case p.Group != nil:
		return isPureExpr(p.Group, env)
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			mutable, err := env.IsMutable(p.Selector.Root)
			if err == nil && !mutable {
				return true
			}
		}
		return false
	case p.FunExpr != nil:
		return false
	default:
		return false
	}
}

// isPureFunction analyses a function and determines if it is pure.
func isPureFunction(fn *parser.FunStmt, env *Env) bool {
	child := NewEnv(env)
	for _, p := range fn.Params {
		if p.Type != nil {
			child.SetVar(p.Name, resolveTypeRef(p.Type, env), false)
		} else {
			child.SetVar(p.Name, AnyType{}, false)
		}
	}
	for _, stmt := range fn.Body {
		if !isPureStmt(stmt, child) {
			return false
		}
	}
	return true
}

// firstImpureCall walks e and returns the name and position of the first
// call to a function whose Pure flag is false. Returns found=false if all
// calls in the expression are to pure functions.
func firstImpureCall(e *parser.Expr, env *Env) (name string, pos lexer.Position, found bool) {
	if e == nil {
		return
	}
	return firstImpureUnary(e.Binary.Left, env)
}

func firstImpureUnary(u *parser.Unary, env *Env) (string, lexer.Position, bool) {
	if n, p, ok := firstImpurePostfix(u.Value, env); ok {
		return n, p, true
	}
	return "", lexer.Position{}, false
}

func firstImpurePostfix(p *parser.PostfixExpr, env *Env) (string, lexer.Position, bool) {
	if n, pos, ok := firstImpurePrimary(p.Target, env); ok {
		return n, pos, true
	}
	for _, op := range p.Ops {
		if op.Call != nil {
			if p.Target != nil && p.Target.Selector != nil {
				name := p.Target.Selector.Root
				t, err := env.GetVar(name)
				if err == nil {
					if ft, ok := t.(FuncType); ok && !ft.Pure {
						return name, p.Target.Pos, true
					}
				}
			}
		}
	}
	return "", lexer.Position{}, false
}

func firstImpurePrimary(p *parser.Primary, env *Env) (string, lexer.Position, bool) {
	if p.Call != nil {
		t, err := env.GetVar(p.Call.Func)
		if err == nil {
			if ft, ok := t.(FuncType); ok && !ft.Pure {
				return p.Call.Func, p.Pos, true
			}
		}
		for _, arg := range p.Call.Args {
			if n, pos, ok := firstImpureCall(arg, env); ok {
				return n, pos, true
			}
		}
	}
	if p.Group != nil {
		return firstImpureCall(p.Group, env)
	}
	return "", lexer.Position{}, false
}
