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

	var root Plan = &ScanPlan{Src: q.Source, Alias: q.Var}
	if pushAlias == q.Var {
		root = &WherePlan{Cond: cond, Input: root}
		cond = nil
	}

	// additional FROM clauses become cross joins
	for _, f := range q.Froms {
		var rhs Plan = &ScanPlan{Src: f.Src, Alias: f.Var}
		if pushAlias == f.Var {
			rhs = &WherePlan{Cond: cond, Input: rhs}
			cond = nil
			pushAlias = ""
		}
		root = &JoinPlan{Left: root, Right: rhs, JoinType: "inner"}
	}

	// explicit JOIN clauses
	for _, j := range q.Joins {
		joinType := "inner"
		if j.Side != nil {
			joinType = *j.Side
		}
		var rhs Plan = &ScanPlan{Src: j.Src, Alias: j.Var}
		if joinType == "inner" && pushAlias == j.Var {
			rhs = &WherePlan{Cond: cond, Input: rhs}
			cond = nil
			pushAlias = ""
		}
		root = &JoinPlan{Left: root, Right: rhs, On: j.On, JoinType: joinType}
	}

	if cond != nil {
		root = &WherePlan{Cond: cond, Input: root}
	}

	if q.Group != nil {
		root = &GroupPlan{By: q.Group.Exprs, Name: q.Group.Name, Input: root}
	}

	if q.Sort != nil {
		root = &SortPlan{Key: q.Sort, Input: root}
	}

	if q.Skip != nil || q.Take != nil {
		root = &LimitPlan{Skip: q.Skip, Take: q.Take, Input: root}
	}

	// final projection
	if q.Select != nil {
		root = &SelectPlan{Expr: q.Select, Input: root}
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
