package vm

import (
	"github.com/alecthomas/participle/v2/lexer"
	"mochi/parser"
)

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

// groupAggInfo describes an aggregate like `sum(from x in g select e)` or
// `min(from x in g select e)` discovered within a GROUP BY query.
type groupAggInfo struct {
	Call     *parser.CallExpr
	FuncName string
	VarName  string
	Expr     *parser.Expr
	Field    string
}

// matchGroupAgg checks if the call expression is an aggregate over a simple
// subquery sourced from the group variable g.
func matchGroupAgg(call *parser.CallExpr, g string) (string, *parser.Expr, bool) {
	if len(call.Args) != 1 {
		return "", nil, false
	}
	arg := call.Args[0]
	if arg == nil || arg.Binary == nil || len(arg.Binary.Right) != 0 {
		return "", nil, false
	}
	u := arg.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return "", nil, false
	}
	if len(u.Value.Ops) != 0 || u.Value.Target == nil || u.Value.Target.Query == nil {
		return "", nil, false
	}
	q := u.Value.Target.Query
	if name, ok := identName(q.Source); !ok || name != g {
		return "", nil, false
	}
	if len(q.Froms) != 0 || len(q.Joins) != 0 || q.Where != nil || q.Group != nil ||
		q.Sort != nil || q.Skip != nil || q.Take != nil {
		return "", nil, false
	}
	return q.Var, q.Select, true
}

// collectGroupAggs walks expression e and records aggregate calls that operate
// on the group variable g. Results are stored in aggs keyed by the CallExpr.
func collectGroupAggs(e *parser.Expr, g string, aggs map[*parser.CallExpr]*groupAggInfo) {
	if e == nil {
		return
	}
	var walkExpr func(*parser.Expr)
	var walkUnary func(*parser.Unary)
	var walkPostfix func(*parser.PostfixExpr)
	var walkPrimary func(*parser.Primary)

	walkExpr = func(e *parser.Expr) {
		if e == nil {
			return
		}
		walkUnary(e.Binary.Left)
		for _, op := range e.Binary.Right {
			walkPostfix(op.Right)
		}
	}
	walkUnary = func(u *parser.Unary) {
		if u == nil {
			return
		}
		walkPostfix(u.Value)
	}
	walkPostfix = func(pf *parser.PostfixExpr) {
		if pf == nil {
			return
		}
		walkPrimary(pf.Target)
		for _, op := range pf.Ops {
			if op.Call != nil {
				for _, a := range op.Call.Args {
					walkExpr(a)
				}
			}
			if op.Index != nil {
				walkExpr(op.Index.Start)
				walkExpr(op.Index.End)
				walkExpr(op.Index.Step)
			}
		}
	}
	walkPrimary = func(p *parser.Primary) {
		if p == nil {
			return
		}
		switch {
		case p.Call != nil:
			fn := p.Call.Func
			if (fn == "sum" || fn == "avg" || fn == "min" || fn == "max" || fn == "count") && g != "" {
				if varName, expr, ok := matchGroupAgg(p.Call, g); ok {
					aggs[p.Call] = &groupAggInfo{Call: p.Call, FuncName: fn, VarName: varName, Expr: expr}
					return
				}
			}
			for _, a := range p.Call.Args {
				walkExpr(a)
			}
		case p.Selector != nil:
			// no-op
		case p.Query != nil:
			walkExpr(p.Query.Source)
			for _, f := range p.Query.Froms {
				walkExpr(f.Src)
			}
			for _, j := range p.Query.Joins {
				walkExpr(j.Src)
				walkExpr(j.On)
			}
			walkExpr(p.Query.Where)
			if p.Query.Group != nil {
				for _, g := range p.Query.Group.Exprs {
					walkExpr(g)
				}
			}
			walkExpr(p.Query.Sort)
			walkExpr(p.Query.Skip)
			walkExpr(p.Query.Take)
			walkExpr(p.Query.Select)
		case p.List != nil:
			for _, el := range p.List.Elems {
				walkExpr(el)
			}
		case p.Map != nil:
			for _, it := range p.Map.Items {
				walkExpr(it.Key)
				walkExpr(it.Value)
			}
		case p.FunExpr != nil:
			walkExpr(p.FunExpr.ExprBody)
		}
		if p.Group != nil {
			walkExpr(p.Group)
		}
	}

	walkExpr(e)
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
