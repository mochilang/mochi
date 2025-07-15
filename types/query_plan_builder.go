package types

import (
	"fmt"
	"mochi/ast"
	"mochi/parser"
	"mochi/types/plan"
)

// BuildQueryPlan converts a parsed QueryExpr into a typed logical plan.
func BuildQueryPlan(q *parser.QueryExpr, env *Env) (plan.Node, error) {
	if q == nil {
		return nil, fmt.Errorf("nil query expression")
	}

	srcT, err := checkExpr(q.Source, env)
	if err != nil {
		return nil, err
	}
	var elemT Type
	switch t := srcT.(type) {
	case ListType:
		elemT = t.Elem
	case GroupType:
		elemT = t.Elem
	default:
		return nil, errQuerySourceList(q.Pos)
	}
	child := NewEnv(env)
	child.SetVar(q.Var, elemT, true)

	cond := q.Where
	pushAlias := ""
	if cond != nil {
		if aliases := usedAliases(cond); len(aliases) == 1 {
			for a := range aliases {
				pushAlias = a
			}
		}
	}

	var root plan.Node = &plan.Scan{Src: q.Source, Alias: q.Var, Elem: elemT}
	if pushAlias == q.Var {
		if _, err := checkExprWithExpected(cond, child, BoolType{}); err != nil {
			return nil, err
		}
		root = &plan.Where{Cond: cond, Input: root}
		cond = nil
	}

	for _, f := range q.Froms {
		ft, err := checkExpr(f.Src, child)
		if err != nil {
			return nil, err
		}
		var fe Type
		switch t := ft.(type) {
		case ListType:
			fe = t.Elem
		case GroupType:
			fe = t.Elem
		default:
			return nil, errJoinSourceList(f.Pos)
		}
		child.SetVar(f.Var, fe, true)
		var rhs plan.Node = &plan.Scan{Src: f.Src, Alias: f.Var, Elem: fe}
		if pushAlias == f.Var {
			if _, err := checkExprWithExpected(cond, child, BoolType{}); err != nil {
				return nil, err
			}
			rhs = &plan.Where{Cond: cond, Input: rhs}
			cond = nil
			pushAlias = ""
		}
		root = &plan.Join{Left: root, Right: rhs, JoinType: "inner"}
	}

	for _, j := range q.Joins {
		jt, err := checkExpr(j.Src, child)
		if err != nil {
			return nil, err
		}
		var je Type
		switch t := jt.(type) {
		case ListType:
			je = t.Elem
		case GroupType:
			je = t.Elem
		default:
			return nil, errJoinSourceList(j.Pos)
		}
		child.SetVar(j.Var, je, true)
		if _, err := checkExprWithExpected(j.On, child, BoolType{}); err != nil {
			return nil, err
		}
		joinType := "inner"
		if j.Side != nil {
			joinType = *j.Side
		}
		var rhs plan.Node = &plan.Scan{Src: j.Src, Alias: j.Var, Elem: je}
		if joinType == "inner" && pushAlias == j.Var {
			if _, err := checkExprWithExpected(cond, child, BoolType{}); err != nil {
				return nil, err
			}
			rhs = &plan.Where{Cond: cond, Input: rhs}
			cond = nil
			pushAlias = ""
		}
		root = &plan.Join{Left: root, Right: rhs, On: j.On, JoinType: joinType}
	}

	if cond != nil {
		if _, err := checkExprWithExpected(cond, child, BoolType{}); err != nil {
			return nil, err
		}
		root = &plan.Where{Cond: cond, Input: root}
	}

	selEnv := child
	if q.Group != nil {
		for _, e := range q.Group.Exprs {
			if _, err := checkExpr(e, child); err != nil {
				return nil, err
			}
		}
		genv := NewEnv(child)
		genv.SetVar(q.Group.Name, GroupType{Key: AnyType{}, Elem: elemT}, true)
		if q.Group.Having != nil {
			if _, err := checkExprWithExpected(q.Group.Having, genv, BoolType{}); err != nil {
				return nil, err
			}
		}
		root = &plan.Group{By: q.Group.Exprs, Name: q.Group.Name, Input: root}
		selEnv = genv
	}

	if q.Sort != nil {
		if _, err := checkExpr(q.Sort, child); err != nil {
			return nil, err
		}
		root = &plan.Sort{Key: q.Sort, Input: root}
	}

	if q.Skip != nil || q.Take != nil {
		if q.Skip != nil {
			if _, err := checkExprWithExpected(q.Skip, child, IntType{}); err != nil {
				return nil, err
			}
		}
		if q.Take != nil {
			if _, err := checkExprWithExpected(q.Take, child, IntType{}); err != nil {
				return nil, err
			}
		}
		root = &plan.Limit{Skip: q.Skip, Take: q.Take, Input: root}
	}

	if q.Select != nil {
		if _, err := checkExpr(q.Select, selEnv); err != nil {
			return nil, err
		}
		root = &plan.Select{Expr: q.Select, Input: root}
	}

	return root, nil
}

// PlanString pretty prints a plan.Node as a Lisp-like tree.
func PlanString(pl plan.Node) string {
	return plan.String(pl)
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
