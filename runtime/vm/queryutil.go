package vm

import (
	"github.com/alecthomas/participle/v2/lexer"
	"mochi/parser"
)

var aggNeedsItems = map[string]struct{}{
	"sum":    {},
	"avg":    {},
	"min":    {},
	"max":    {},
	"values": {},
}

// aggregateCall returns the aggregate opcode and argument if e is a simple
// aggregate function call like `sum(x)`.
func aggregateCall(e *parser.Expr) (Op, *parser.Expr, lexer.Position, bool) {
	if e == nil || e.Binary == nil {
		return 0, nil, lexer.Position{}, false
	}
	if len(e.Binary.Right) != 0 {
		return 0, nil, lexer.Position{}, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return 0, nil, lexer.Position{}, false
	}
	p := u.Value
	if p == nil || len(p.Ops) != 0 || p.Target == nil || p.Target.Call == nil {
		return 0, nil, lexer.Position{}, false
	}
	call := p.Target.Call
	if len(call.Args) != 1 {
		return 0, nil, lexer.Position{}, false
	}
	switch call.Func {
	case "sum":
		return OpSum, call.Args[0], call.Pos, true
	case "avg":
		return OpAvg, call.Args[0], call.Pos, true
	case "min":
		return OpMin, call.Args[0], call.Pos, true
	case "max":
		return OpMax, call.Args[0], call.Pos, true
	case "count":
		return OpCount, call.Args[0], call.Pos, true
	default:
		return 0, nil, lexer.Position{}, false
	}
}

// exprVars collects variable names referenced in expression e.
func exprVars(e *parser.Expr, vars map[string]struct{}) {
	if e == nil || e.Binary == nil {
		return
	}
	var scanUnary func(*parser.Unary)
	var scanPostfix func(*parser.PostfixExpr)
	var scanPrimary func(*parser.Primary)

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
				exprVars(op.Index.Start, vars)
				exprVars(op.Index.End, vars)
			}
			if op.Call != nil {
				for _, a := range op.Call.Args {
					exprVars(a, vars)
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
			exprVars(p.Group, vars)
		}
		if p.FunExpr != nil {
			exprVars(p.FunExpr.ExprBody, vars)
			for _, st := range p.FunExpr.BlockBody {
				scanStmtVars(st, vars)
			}
		}
		if p.List != nil {
			for _, el := range p.List.Elems {
				exprVars(el, vars)
			}
		}
		if p.Map != nil {
			for _, it := range p.Map.Items {
				exprVars(it.Key, vars)
				exprVars(it.Value, vars)
			}
		}
		if p.Call != nil {
			for _, a := range p.Call.Args {
				exprVars(a, vars)
			}
		}
	}

	scanUnary(e.Binary.Left)
	for _, op := range e.Binary.Right {
		scanPostfix(op.Right)
	}
}

func scanStmtVars(s *parser.Statement, vars map[string]struct{}) {
	if s == nil {
		return
	}
	switch {
	case s.Let != nil:
		exprVars(s.Let.Value, vars)
	case s.Var != nil:
		exprVars(s.Var.Value, vars)
	case s.Assign != nil:
		exprVars(s.Assign.Value, vars)
	case s.Return != nil:
		exprVars(s.Return.Value, vars)
	case s.Expr != nil:
		exprVars(s.Expr.Expr, vars)
	case s.For != nil:
		exprVars(s.For.Source, vars)
		exprVars(s.For.RangeEnd, vars)
		for _, st := range s.For.Body {
			scanStmtVars(st, vars)
		}
	case s.While != nil:
		exprVars(s.While.Cond, vars)
		for _, st := range s.While.Body {
			scanStmtVars(st, vars)
		}
	case s.If != nil:
		exprVars(s.If.Cond, vars)
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

// whereEvalLevel returns the earliest FROM clause index where the query's WHERE
// predicate can be evaluated. Index 0 refers to the initial source variable.
func whereEvalLevel(q *parser.QueryExpr) int {
	if q.Where == nil {
		return len(q.Froms)
	}
	vars := map[string]struct{}{}
	exprVars(q.Where, vars)
	positions := map[string]int{q.Var: 0}
	for i, f := range q.Froms {
		positions[f.Var] = i + 1
	}
	level := 0
	for v := range vars {
		if idx, ok := positions[v]; ok && idx > level {
			level = idx
		}
	}
	if level > len(q.Froms) {
		level = len(q.Froms)
	}
	return level
}

// exprUsesOnlyAlias reports whether expression e references only the given alias.
func exprUsesOnlyAlias(e *parser.Expr, alias string) bool {
	vars := map[string]struct{}{}
	exprVars(e, vars)
	if len(vars) != 1 {
		return false
	}
	_, ok := vars[alias]
	return ok
}

func unaryToExpr(u *parser.Unary) *parser.Expr {
	if u == nil {
		return nil
	}
	return &parser.Expr{Pos: u.Pos, Binary: &parser.BinaryExpr{Left: u}}
}

func postfixToExpr(p *parser.PostfixExpr, pos lexer.Position) *parser.Expr {
	if p == nil {
		return nil
	}
	return &parser.Expr{Pos: pos, Binary: &parser.BinaryExpr{Left: &parser.Unary{Pos: pos, Value: p}}}
}

// eqJoinKeys checks if ON clause represents equality between expressions
// that exclusively reference leftAlias and rightAlias. It returns the
// left and right expressions when recognized.
func eqJoinKeys(on *parser.Expr, leftAlias, rightAlias string) (*parser.Expr, *parser.Expr, bool) {
	if on == nil || on.Binary == nil || len(on.Binary.Right) != 1 {
		return nil, nil, false
	}
	bop := on.Binary.Right[0]
	if bop.Op != "==" {
		return nil, nil, false
	}
	leftExpr := unaryToExpr(on.Binary.Left)
	rightExpr := postfixToExpr(bop.Right, bop.Pos)
	if exprUsesOnlyAlias(leftExpr, leftAlias) && exprUsesOnlyAlias(rightExpr, rightAlias) {
		return leftExpr, rightExpr, true
	}
	if exprUsesOnlyAlias(leftExpr, rightAlias) && exprUsesOnlyAlias(rightExpr, leftAlias) {
		return rightExpr, leftExpr, true
	}
	return nil, nil, false
}

// whereAlias returns the alias referenced by the WHERE clause if exactly one
// alias is present. The bool result is false when the predicate references
// multiple aliases or none at all.
func whereAlias(where *parser.Expr) (string, bool) {
	if where == nil {
		return "", false
	}
	vars := map[string]struct{}{}
	exprVars(where, vars)
	if len(vars) != 1 {
		return "", false
	}
	for v := range vars {
		return v, true
	}
	return "", false
}

// exprHasAggCall reports whether expression e contains a function call that
// requires group items such as sum(), avg(), min(), max() or values().
func exprHasAggCall(e *parser.Expr) bool {
	found := false
	var scanExpr func(*parser.Expr)
	var scanUnary func(*parser.Unary)
	var scanPostfix func(*parser.PostfixExpr)
	var scanPrimary func(*parser.Primary)

	scanExpr = func(e *parser.Expr) {
		if found || e == nil || e.Binary == nil {
			return
		}
		scanUnary(e.Binary.Left)
		for _, op := range e.Binary.Right {
			scanPostfix(op.Right)
		}
	}
	scanUnary = func(u *parser.Unary) {
		if found || u == nil {
			return
		}
		scanPostfix(u.Value)
	}
	scanPostfix = func(p *parser.PostfixExpr) {
		if found || p == nil {
			return
		}
		scanPrimary(p.Target)
		for _, op := range p.Ops {
			if op.Call != nil {
				for _, a := range op.Call.Args {
					scanExpr(a)
				}
			}
			if op.Index != nil {
				scanExpr(op.Index.Start)
				scanExpr(op.Index.End)
				scanExpr(op.Index.Step)
			}
			if op.Field != nil {
				// no-op
			}
		}
	}
	scanPrimary = func(p *parser.Primary) {
		if found || p == nil {
			return
		}
		if p.Call != nil {
			if _, ok := aggNeedsItems[p.Call.Func]; ok {
				found = true
				return
			}
			for _, a := range p.Call.Args {
				scanExpr(a)
			}
		}
		if p.Struct != nil {
			for _, f := range p.Struct.Fields {
				scanExpr(f.Value)
			}
		}
		if p.List != nil {
			for _, el := range p.List.Elems {
				scanExpr(el)
			}
		}
		if p.Map != nil {
			for _, it := range p.Map.Items {
				scanExpr(it.Key)
				scanExpr(it.Value)
			}
		}
		if p.Group != nil {
			scanExpr(p.Group)
		}
		if p.If != nil {
			scanExpr(p.If.Cond)
			scanExpr(p.If.Then)
			scanExpr(p.If.Else)
			if p.If.ElseIf != nil {
				scanExpr(p.If.ElseIf.Cond)
				scanExpr(p.If.ElseIf.Then)
				scanExpr(p.If.ElseIf.Else)
			}
		}
		if p.Query != nil {
			// ignore nested query expressions
		}
		if p.FunExpr != nil {
			scanExpr(p.FunExpr.ExprBody)
			for _, st := range p.FunExpr.BlockBody {
				// Only expressions from statements are scanned
				if st.Return != nil {
					scanExpr(st.Return.Value)
				}
				if st.Expr != nil {
					scanExpr(st.Expr.Expr)
				}
			}
		}
		if p.Match != nil {
			scanExpr(p.Match.Target)
			for _, c := range p.Match.Cases {
				scanExpr(c.Pattern)
				scanExpr(c.Result)
			}
		}
		if p.Generate != nil {
			for _, f := range p.Generate.Fields {
				scanExpr(f.Value)
			}
		}
		if p.Fetch != nil {
			scanExpr(p.Fetch.URL)
			scanExpr(p.Fetch.With)
		}
		if p.Load != nil {
			scanExpr(p.Load.With)
		}
		if p.Save != nil {
			scanExpr(p.Save.Src)
			scanExpr(p.Save.With)
		}
	}

	scanExpr(e)
	return found
}

// exprUsesField reports whether expression e references alias.field.
func exprUsesField(e *parser.Expr, alias, field string) bool {
	found := false
	var scanExpr func(*parser.Expr)
	var scanUnary func(*parser.Unary)
	var scanPostfix func(*parser.PostfixExpr)
	var scanPrimary func(*parser.Primary)

	scanExpr = func(e *parser.Expr) {
		if found || e == nil || e.Binary == nil {
			return
		}
		scanUnary(e.Binary.Left)
		for _, op := range e.Binary.Right {
			scanPostfix(op.Right)
		}
	}
	scanUnary = func(u *parser.Unary) {
		if found || u == nil {
			return
		}
		scanPostfix(u.Value)
	}
	scanPostfix = func(p *parser.PostfixExpr) {
		if found || p == nil {
			return
		}
		scanPrimary(p.Target)
		for _, op := range p.Ops {
			if op.Field != nil {
				if p.Target != nil && p.Target.Selector != nil {
					if p.Target.Selector.Root == alias {
						for _, t := range append([]string{op.Field.Name}, op.Field.Name) {
							if t == field {
								found = true
								return
							}
						}
					}
				}
			}
			if op.Call != nil {
				for _, a := range op.Call.Args {
					scanExpr(a)
				}
			}
			if op.Index != nil {
				scanExpr(op.Index.Start)
				scanExpr(op.Index.End)
				scanExpr(op.Index.Step)
			}
		}
	}
	scanPrimary = func(p *parser.Primary) {
		if found || p == nil {
			return
		}
		if p.Selector != nil {
			if p.Selector.Root == alias {
				for _, n := range p.Selector.Tail {
					if n == field {
						found = true
						return
					}
				}
			}
		}
		if p.Call != nil {
			for _, a := range p.Call.Args {
				scanExpr(a)
			}
		}
		if p.Struct != nil {
			for _, f := range p.Struct.Fields {
				scanExpr(f.Value)
			}
		}
		if p.List != nil {
			for _, el := range p.List.Elems {
				scanExpr(el)
			}
		}
		if p.Map != nil {
			for _, it := range p.Map.Items {
				scanExpr(it.Key)
				scanExpr(it.Value)
			}
		}
		if p.Group != nil {
			scanExpr(p.Group)
		}
	}

	scanExpr(e)
	return found
}

// groupNeedsItems determines whether GROUP BY query q requires storing
// group items based on its SELECT, HAVING or SORT expressions.
func groupNeedsItems(q *parser.QueryExpr) bool {
	if q == nil || q.Group == nil {
		return false
	}
	if exprHasAggCall(q.Select) || exprHasAggCall(q.Group.Having) || exprHasAggCall(q.Sort) {
		return true
	}
	name := q.Group.Name
	if exprUsesField(q.Select, name, "items") || exprUsesField(q.Group.Having, name, "items") || exprUsesField(q.Sort, name, "items") {
		return true
	}
	return false
}
