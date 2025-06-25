package data

import (
	"fmt"
	"mochi/ast"
	"mochi/parser"
)

// BuildPlan converts a parsed QueryExpr into a logical plan tree.
func BuildPlan(q *parser.QueryExpr) (Plan, error) {
	if q == nil {
		return nil, fmt.Errorf("nil query expression")
	}

	cond := q.Where
	pushAlias := ""
	if cond != nil {
		if aliases := usedAliases(cond); len(aliases) == 1 {
			for a := range aliases {
				pushAlias = a
			}
		}
	}

	var root Plan = &scanPlan{Src: q.Source, Alias: q.Var}
	if pushAlias == q.Var {
		root = &wherePlan{Cond: cond, Input: root}
		cond = nil
	}

	// additional FROM clauses become cross joins
	for _, f := range q.Froms {
		var rhs Plan = &scanPlan{Src: f.Src, Alias: f.Var}
		if pushAlias == f.Var {
			rhs = &wherePlan{Cond: cond, Input: rhs}
			cond = nil
			pushAlias = ""
		}
		root = &joinPlan{Left: root, Right: rhs, JoinType: "inner"}
	}

	// explicit JOIN clauses
	for _, j := range q.Joins {
		joinType := "inner"
		if j.Side != nil {
			joinType = *j.Side
		}
		var rhs Plan = &scanPlan{Src: j.Src, Alias: j.Var}
		if joinType == "inner" && pushAlias == j.Var {
			rhs = &wherePlan{Cond: cond, Input: rhs}
			cond = nil
			pushAlias = ""
		}
		root = &joinPlan{Left: root, Right: rhs, On: j.On, JoinType: joinType}
	}

	if cond != nil {
		root = &wherePlan{Cond: cond, Input: root}
	}

	if q.Group != nil {
		exprs := q.Group.ExprsList()
		names := make([]string, len(exprs))
		for i, e := range exprs {
			names[i] = deriveName(e)
		}
		root = &groupPlan{By: exprs, Names: names, Name: q.Group.Name, Input: root}
	}

	if q.Sort != nil {
		root = &sortPlan{Key: q.Sort, Input: root}
	}

	if q.Skip != nil || q.Take != nil {
		root = &limitPlan{Skip: q.Skip, Take: q.Take, Input: root}
	}

	// final projection
	if q.Select != nil {
		root = &selectPlan{Expr: q.Select, Input: root}
	}

	return root, nil
}

// usedAliases returns the set of selector roots referenced in e.
func usedAliases(e *parser.Expr) map[string]struct{} {
	aliases := map[string]struct{}{}
	if e == nil {
		return aliases
	}
	node := ast.FromExpr(e)
	var walk func(n *ast.Node)
	walk = func(n *ast.Node) {
		if n.Kind == "selector" {
			base := n
			for len(base.Children) == 1 && base.Children[0].Kind == "selector" {
				base = base.Children[0]
			}
			if s, ok := base.Value.(string); ok {
				aliases[s] = struct{}{}
			}
		}
		for _, c := range n.Children {
			walk(c)
		}
	}
	walk(node)
	return aliases
}

// deriveName attempts to infer a field name from expr for multi-key grouping.
func deriveName(e *parser.Expr) string {
	if e == nil {
		return "key"
	}
	n := ast.FromExpr(e)
	if n.Kind == "selector" {
		if s, ok := n.Value.(string); ok {
			return s
		}
	}
	return "key"
}
