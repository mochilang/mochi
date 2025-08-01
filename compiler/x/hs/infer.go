//go:build slow

package hscode

import (
	"mochi/parser"
	"mochi/types"
)

// inferExprType delegates to types.ExprType.
func (c *Compiler) inferExprType(e *parser.Expr) types.Type {
	if c.env == nil {
		return types.ExprType(e, nil)
	}
	return types.CheckExprType(e, c.env)
}

func (c *Compiler) inferUnaryType(u *parser.Unary) types.Type {
	if u == nil {
		return types.AnyType{}
	}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: u}}
	if c.env == nil {
		return types.ExprType(expr, nil)
	}
	return types.CheckExprType(expr, c.env)
}

func (c *Compiler) inferPostfixType(p *parser.PostfixExpr) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	unary := &parser.Unary{Value: p}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: unary}}
	if c.env == nil {
		return types.ExprType(expr, nil)
	}
	return types.CheckExprType(expr, c.env)
}

func (c *Compiler) inferPrimaryType(p *parser.Primary) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	postfix := &parser.PostfixExpr{Target: p}
	unary := &parser.Unary{Value: postfix}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: unary}}
	if c.env == nil {
		return types.ExprType(expr, nil)
	}
	return types.CheckExprType(expr, c.env)
}

// inferExprTypeHint delegates to types.ExprTypeHint.
func (c *Compiler) inferExprTypeHint(e *parser.Expr, hint types.Type) types.Type {
	if e != nil && e.Binary != nil && len(e.Binary.Right) == 0 {
		if ll := e.Binary.Left.Value.Target.List; ll != nil {
			if lt, ok := hint.(types.ListType); ok {
				if st, ok2 := lt.Elem.(types.StructType); ok2 {
					if inf, ok3 := types.InferStructFromList(ll, c.env); ok3 && types.StructMatches(st, inf.Fields, inf.Order) {
						return lt
					}
				}
			}
		}
		if ml := e.Binary.Left.Value.Target.Map; ml != nil {
			if st, ok := hint.(types.StructType); ok {
				if inf, ok3 := types.InferStructFromMapEnv(ml, c.env); ok3 && types.StructMatches(st, inf.Fields, inf.Order) {
					return st
				}
			}
		}
	}
	return types.ExprTypeHint(e, hint, c.env)
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	return types.ResolveTypeRef(t, c.env)
}

// helper predicates following Go naming conventions
func (c *Compiler) isStringExpr(e *parser.Expr) bool {
	_, ok := types.ExprType(e, c.env).(types.StringType)
	return ok
}

func (c *Compiler) isIntExpr(e *parser.Expr) bool {
	return isInt(types.ExprType(e, c.env))
}

func (c *Compiler) isIntUnary(u *parser.Unary) bool {
	return isInt(c.inferUnaryType(u))
}

func (c *Compiler) isIntPostfix(p *parser.PostfixExpr) bool {
	return isInt(c.inferPostfixType(p))
}

func (c *Compiler) isIntPrimary(p *parser.Primary) bool {
	return isInt(c.inferPrimaryType(p))
}

func (c *Compiler) isMapPrimary(p *parser.Primary) bool {
	_, ok := c.inferPrimaryType(p).(types.MapType)
	return ok
}

func (c *Compiler) isStringPrimary(p *parser.Primary) bool {
	_, ok := c.inferPrimaryType(p).(types.StringType)
	return ok
}

func (c *Compiler) isListExpr(e *parser.Expr) bool {
	_, ok := types.ExprType(e, c.env).(types.ListType)
	return ok
}

func (c *Compiler) isMapPostfix(p *parser.PostfixExpr) bool {
	_, ok := c.inferPostfixType(p).(types.MapType)
	return ok
}

func (c *Compiler) isStringUnary(u *parser.Unary) bool {
	_, ok := c.inferUnaryType(u).(types.StringType)
	return ok
}

func (c *Compiler) isStringPostfix(p *parser.PostfixExpr) bool {
	_, ok := c.inferPostfixType(p).(types.StringType)
	return ok
}

// containsAny reports whether the type t or any nested component is AnyType.
func containsAny(t types.Type) bool {
	return types.ContainsAny(t)
}

// local helpers
func isInt(t types.Type) bool {
	switch t.(type) {
	case types.IntType, types.Int64Type:
		return true
	default:
		return false
	}
}

func isFloat(t types.Type) bool  { _, ok := t.(types.FloatType); return ok }
func isString(t types.Type) bool { _, ok := t.(types.StringType); return ok }
func isAny(t types.Type) bool    { _, ok := t.(types.AnyType); return ok }
