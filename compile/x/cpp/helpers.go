package cppcode

import (
	"mochi/parser"
	"mochi/types"
)

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil || len(e.Binary.Right) != 0 {
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
	return p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = c.resolveTypeRef(p)
		}
		var ret types.Type = types.VoidType{}
		if t.Fun.Return != nil {
			ret = c.resolveTypeRef(t.Fun.Return)
		}
		return types.FuncType{Params: params, Return: ret}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return types.ListType{Elem: c.resolveTypeRef(t.Generic.Args[0])}
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			return types.MapType{Key: c.resolveTypeRef(t.Generic.Args[0]), Value: c.resolveTypeRef(t.Generic.Args[1])}
		}
		return types.AnyType{}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "float":
			return types.FloatType{}
		case "bool":
			return types.BoolType{}
		case "string":
			return types.StringType{}
		default:
			if c.env != nil {
				if st, ok := c.env.GetStruct(*t.Simple); ok {
					return st
				}
				if ut, ok := c.env.GetUnion(*t.Simple); ok {
					return ut
				}
			}
			return types.AnyType{}
		}
	}
	return types.AnyType{}
}

func collectExprVars(e *parser.Expr, vars map[string]struct{}) {
	if e == nil {
		return
	}
	var scanUnary func(u *parser.Unary)
	var scanPostfix func(p *parser.PostfixExpr)
	var scanPrimary func(p *parser.Primary)

	scanUnary = func(u *parser.Unary) {
		if u == nil {
			return
		}
		scanPostfix(u.Value)
	}
	scanPostfix = func(p *parser.PostfixExpr) {
		if p == nil {
			return
		}
		scanPrimary(p.Target)
		for _, op := range p.Ops {
			if op.Index != nil {
				collectExprVars(op.Index.Start, vars)
				collectExprVars(op.Index.End, vars)
			}
			if op.Call != nil {
				for _, a := range op.Call.Args {
					collectExprVars(a, vars)
				}
			}
		}
	}
	scanPrimary = func(p *parser.Primary) {
		if p == nil {
			return
		}
		if p.Selector != nil {
			vars[p.Selector.Root] = struct{}{}
		}
		if p.Group != nil {
			collectExprVars(p.Group, vars)
		}
		if p.FunExpr != nil {
			collectExprVars(p.FunExpr.ExprBody, vars)
			for _, st := range p.FunExpr.BlockBody {
				// expressions inside statements
				if st.Expr != nil {
					collectExprVars(st.Expr.Expr, vars)
				}
			}
		}
		if p.List != nil {
			for _, e := range p.List.Elems {
				collectExprVars(e, vars)
			}
		}
		if p.Map != nil {
			for _, it := range p.Map.Items {
				collectExprVars(it.Key, vars)
				collectExprVars(it.Value, vars)
			}
		}
		if p.Call != nil {
			for _, a := range p.Call.Args {
				collectExprVars(a, vars)
			}
		}
		if p.Query != nil {
			collectExprVars(p.Query.Source, vars)
			for _, f := range p.Query.Froms {
				collectExprVars(f.Src, vars)
			}
			for _, j := range p.Query.Joins {
				collectExprVars(j.Src, vars)
				collectExprVars(j.On, vars)
			}
			if p.Query.Group != nil {
				collectExprVars(p.Query.Group.Expr, vars)
			}
			collectExprVars(p.Query.Select, vars)
			collectExprVars(p.Query.Where, vars)
			collectExprVars(p.Query.Sort, vars)
			collectExprVars(p.Query.Skip, vars)
			collectExprVars(p.Query.Take, vars)
		}
	}

	scanUnary(e.Binary.Left)
	for _, op := range e.Binary.Right {
		scanPostfix(op.Right)
	}
}
