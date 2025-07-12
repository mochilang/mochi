//go:build slow

package pycode

import (
	"mochi/parser"
	"mochi/types"
)

// inferExprType delegates to types.ExprType.
func (c *Compiler) inferExprType(e *parser.Expr) types.Type {
	return types.ExprType(e, c.env)
}

// inferExprTypeHint delegates to types.ExprTypeHint.
func (c *Compiler) inferExprTypeHint(e *parser.Expr, hint types.Type) types.Type {
	return types.ExprTypeHint(e, hint, c.env)
}

func (c *Compiler) inferUnaryType(u *parser.Unary) types.Type {
	if u == nil {
		return types.AnyType{}
	}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: u}}
	return types.ExprType(expr, c.env)
}

func (c *Compiler) inferPostfixType(p *parser.PostfixExpr) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	unary := &parser.Unary{Value: p}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: unary}}
	return types.ExprType(expr, c.env)
}

func (c *Compiler) inferPrimaryType(p *parser.Primary) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	postfix := &parser.PostfixExpr{Target: p}
	unary := &parser.Unary{Value: postfix}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: unary}}
	return types.ExprType(expr, c.env)
}

func resultType(op string, left, right types.Type) types.Type {
        return types.ResultType(op, left, right)
}

// inferFunReturnType returns the unified type of all return statements in body.
// If return statements have differing types it falls back to AnyType. When no
// explicit return is present it yields VoidType.
func (c *Compiler) inferFunReturnType(body []*parser.Statement) types.Type {
        var ret types.Type
        for _, st := range body {
                if st.Return != nil {
                        t := c.inferExprType(st.Return.Value)
                        if ret == nil {
                                ret = t
                        } else if !equalTypes(ret, t) {
                                return types.AnyType{}
                        }
                }
        }
        if ret == nil {
                return types.VoidType{}
        }
        return ret
}
