package vm

import (
	"fmt"

	"github.com/alecthomas/participle/v2/lexer"
	"mochi/parser"
)

// aggregateCall returns the aggregate opcode and argument if e is a simple
// aggregate function call like `sum(x)`.
func aggregateCall(e *parser.Expr) (Op, *parser.Expr, lexer.Position, bool) {
	for e != nil {
		if e.Binary == nil || len(e.Binary.Right) != 0 {
			return 0, nil, lexer.Position{}, false
		}
		u := e.Binary.Left
		if len(u.Ops) != 0 {
			return 0, nil, lexer.Position{}, false
		}
		p := u.Value
		if p == nil || len(p.Ops) != 0 {
			return 0, nil, lexer.Position{}, false
		}
		if p.Target != nil && p.Target.Call != nil {
			call := p.Target.Call
			if len(call.Args) != 1 {
				return 0, nil, lexer.Position{}, false
			}
			// Avoid treating calls like count(<query>) as aggregates
			// since they may depend on the outer query row.
			if isQueryExpr(call.Args[0]) {
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
		if p.Target != nil && p.Target.Group != nil {
			e = p.Target.Group
			continue
		}
		return 0, nil, lexer.Position{}, false
	}
	return 0, nil, lexer.Position{}, false
}

func isQueryExpr(e *parser.Expr) bool {
	for e != nil {
		if e.Binary == nil || len(e.Binary.Right) != 0 {
			return false
		}
		u := e.Binary.Left
		if len(u.Ops) != 0 {
			return false
		}
		p := u.Value
		if p == nil || len(p.Ops) != 0 {
			return false
		}
		prim := p.Target
		if prim == nil {
			return false
		}
		if prim.Query != nil || prim.LogicQuery != nil {
			return true
		}
		if prim.Group != nil {
			e = prim.Group
			continue
		}
		return false
	}
	return false
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
		if s.Return.Value != nil {
			exprVars(s.Return.Value, vars)
		}
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

func constStringExpr(pos lexer.Position, s string) *parser.Expr {
	return &parser.Expr{
		Pos:    pos,
		Binary: &parser.BinaryExpr{Left: &parser.Unary{Pos: pos, Value: &parser.PostfixExpr{Target: &parser.Primary{Lit: &parser.Literal{Str: &s}}}}},
	}
}

// eqJoinKeys checks if ON clause represents equality between expressions
// that exclusively reference leftAlias and rightAlias. It returns the
// left and right expressions when recognized.
func eqJoinKeys(on *parser.Expr, leftAlias, rightAlias string) (*parser.Expr, *parser.Expr, bool) {
	type pair struct{ left, right *parser.Expr }

	var pairs []pair

	var collect func(e *parser.Expr) bool
	collect = func(e *parser.Expr) bool {
		if e == nil || e.Binary == nil {
			return false
		}
		if len(e.Binary.Right) == 1 && e.Binary.Right[0].Op == "&&" {
			left := &parser.Expr{Pos: e.Pos, Binary: &parser.BinaryExpr{Left: e.Binary.Left}}
			if !collect(left) {
				return false
			}
			rightExpr := postfixToExpr(e.Binary.Right[0].Right, e.Binary.Right[0].Pos)
			return collect(rightExpr)
		}
		if len(e.Binary.Right) != 1 || e.Binary.Right[0].Op != "==" {
			return false
		}
		l := unaryToExpr(e.Binary.Left)
		r := postfixToExpr(e.Binary.Right[0].Right, e.Binary.Right[0].Pos)
		if exprUsesOnlyAlias(l, leftAlias) && exprUsesOnlyAlias(r, rightAlias) {
			pairs = append(pairs, pair{l, r})
			return true
		}
		if exprUsesOnlyAlias(l, rightAlias) && exprUsesOnlyAlias(r, leftAlias) {
			pairs = append(pairs, pair{r, l})
			return true
		}
		return false
	}

	if !collect(on) || len(pairs) == 0 {
		return nil, nil, false
	}

	if len(pairs) == 1 {
		return pairs[0].left, pairs[0].right, true
	}

	makeMap := func(ps []pair, pickLeft bool) *parser.Expr {
		items := make([]*parser.MapEntry, len(ps))
		for i, p := range ps {
			keyStr := fmt.Sprintf("k%d", i)
			keyExpr := constStringExpr(on.Pos, keyStr)
			val := p.right
			if pickLeft {
				val = p.left
			}
			items[i] = &parser.MapEntry{Key: keyExpr, Value: val}
		}
		mapPrim := &parser.Primary{Map: &parser.MapLiteral{Items: items}}
		return postfixToExpr(&parser.PostfixExpr{Target: mapPrim}, on.Pos)
	}

	leftMap := makeMap(pairs, true)
	rightMap := makeMap(pairs, false)
	return leftMap, rightMap, true
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
