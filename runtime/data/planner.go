package data

import (
	"fmt"
	"mochi/parser"
)

// BuildPlan converts a parsed QueryExpr into a logical plan tree.
func BuildPlan(q *parser.QueryExpr) (Plan, error) {
	if q == nil {
		return nil, fmt.Errorf("nil query expression")
	}

	var root Plan = &scanPlan{Src: q.Source, Alias: q.Var}

	// additional FROM clauses become cross joins
	for _, f := range q.Froms {
		rhs := &scanPlan{Src: f.Src, Alias: f.Var}
		root = &joinPlan{Left: root, Right: rhs, JoinType: "inner"}
	}

	// explicit JOIN clauses
	for _, j := range q.Joins {
		joinType := "inner"
		if j.Side != nil {
			joinType = *j.Side
		}
		rhs := &scanPlan{Src: j.Src, Alias: j.Var}
		root = &joinPlan{Left: root, Right: rhs, On: j.On, JoinType: joinType}
	}

	if q.Where != nil {
		root = &wherePlan{Cond: q.Where, Input: root}
	}

	if q.Group != nil {
		root = &groupPlan{By: q.Group.Expr, Name: q.Group.Name, Input: root}
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
