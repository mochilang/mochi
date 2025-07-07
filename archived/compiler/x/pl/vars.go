//go:build archived

package plcode

import "mochi/parser"

func exprVars(e *parser.Expr) map[string]struct{} {
	vars := map[string]struct{}{}
	scanExprVars(e, vars)
	return vars
}

func scanExprVars(e *parser.Expr, vars map[string]struct{}) {
	if e == nil {
		return
	}
	scanUnaryVars(e.Binary.Left, vars)
	for _, op := range e.Binary.Right {
		scanPostfixVars(op.Right, vars)
	}
}

func scanUnaryVars(u *parser.Unary, vars map[string]struct{}) {
	if u == nil {
		return
	}
	scanPostfixVars(u.Value, vars)
}

func scanPostfixVars(p *parser.PostfixExpr, vars map[string]struct{}) {
	if p == nil {
		return
	}
	scanPrimaryVars(p.Target, vars)
	for _, op := range p.Ops {
		if op.Index != nil {
			scanExprVars(op.Index.Start, vars)
			scanExprVars(op.Index.End, vars)
			scanExprVars(op.Index.Step, vars)
		}
		if op.Call != nil {
			for _, a := range op.Call.Args {
				scanExprVars(a, vars)
			}
		}
	}
}

func scanPrimaryVars(p *parser.Primary, vars map[string]struct{}) {
	if p == nil {
		return
	}
	if p.Selector != nil {
		vars[p.Selector.Root] = struct{}{}
	}
	if p.Group != nil {
		scanExprVars(p.Group, vars)
	}
	if p.FunExpr != nil {
		scanExprVars(p.FunExpr.ExprBody, vars)
		for _, st := range p.FunExpr.BlockBody {
			scanStmtVars(st, vars)
		}
	}
	if p.List != nil {
		for _, e := range p.List.Elems {
			scanExprVars(e, vars)
		}
	}
	if p.Map != nil {
		for _, it := range p.Map.Items {
			scanExprVars(it.Key, vars)
			scanExprVars(it.Value, vars)
		}
	}
	if p.Call != nil {
		for _, a := range p.Call.Args {
			scanExprVars(a, vars)
		}
	}
	if p.Query != nil {
		scanExprVars(p.Query.Source, vars)
		for _, f := range p.Query.Froms {
			scanExprVars(f.Src, vars)
		}
		for _, j := range p.Query.Joins {
			scanExprVars(j.Src, vars)
			scanExprVars(j.On, vars)
		}
		if p.Query.Group != nil {
			scanExprVars(p.Query.Group.Exprs[0], vars)
		}
		scanExprVars(p.Query.Select, vars)
		scanExprVars(p.Query.Where, vars)
		scanExprVars(p.Query.Sort, vars)
		scanExprVars(p.Query.Skip, vars)
		scanExprVars(p.Query.Take, vars)
		vars[p.Query.Var] = struct{}{}
		for _, f := range p.Query.Froms {
			vars[f.Var] = struct{}{}
		}
		if p.Query.Group != nil {
			vars[p.Query.Group.Name] = struct{}{}
		}
	}
}

func scanStmtVars(s *parser.Statement, vars map[string]struct{}) {
	if s == nil {
		return
	}
	switch {
	case s.Let != nil:
		scanExprVars(s.Let.Value, vars)
	case s.Var != nil:
		scanExprVars(s.Var.Value, vars)
	case s.Assign != nil:
		scanExprVars(s.Assign.Value, vars)
	case s.Return != nil:
		scanExprVars(s.Return.Value, vars)
	case s.Expr != nil:
		scanExprVars(s.Expr.Expr, vars)
	case s.For != nil:
		scanExprVars(s.For.Source, vars)
		scanExprVars(s.For.RangeEnd, vars)
		for _, st := range s.For.Body {
			scanStmtVars(st, vars)
		}
	case s.While != nil:
		scanExprVars(s.While.Cond, vars)
		for _, st := range s.While.Body {
			scanStmtVars(st, vars)
		}
	case s.If != nil:
		scanExprVars(s.If.Cond, vars)
		for _, st := range s.If.Then {
			scanStmtVars(st, vars)
		}
		if s.If.ElseIf != nil {
			scanStmtVars(&parser.Statement{If: s.If.ElseIf}, vars)
		}
		for _, st := range s.If.Else {
			scanStmtVars(st, vars)
		}
	}
}
