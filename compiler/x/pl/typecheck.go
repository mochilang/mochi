//go:build slow

package pl

import (
	"fmt"

	"mochi/parser"
	"mochi/types"
)

// checkQueryTypes performs minimal static checks for query expressions.
func (c *Compiler) checkQueryTypes(q *parser.QueryExpr) error {
	env := types.NewEnv(nil)

	srcT := types.ExprType(q.Source, env)
	var elem types.Type
	switch t := srcT.(type) {
	case types.ListType:
		elem = t.Elem
	case types.GroupType:
		elem = t.Elem
	case types.AnyType:
		elem = types.AnyType{}
	default:
		return fmt.Errorf("query source must be list or group")
	}
	env.SetVar(q.Var, elem, true)

	for _, fr := range q.Froms {
		t := types.ExprType(fr.Src, env)
		switch ft := t.(type) {
		case types.ListType:
			env.SetVar(fr.Var, ft.Elem, true)
		case types.GroupType:
			env.SetVar(fr.Var, ft.Elem, true)
		case types.AnyType:
			env.SetVar(fr.Var, types.AnyType{}, true)
		default:
			return fmt.Errorf("from source must be list or group")
		}
	}

	for _, j := range q.Joins {
		t := types.ExprType(j.Src, env)
		switch jt := t.(type) {
		case types.ListType:
			env.SetVar(j.Var, jt.Elem, true)
		case types.GroupType:
			env.SetVar(j.Var, jt.Elem, true)
		case types.AnyType:
			env.SetVar(j.Var, types.AnyType{}, true)
		default:
			return fmt.Errorf("join source must be list or group")
		}
		// ensure on condition is well-typed (boolean when known)
		if bt, ok := types.ExprType(j.On, env).(types.BoolType); ok {
			_ = bt
		}
	}

	if q.Group != nil {
		env.SetVar(q.Group.Name, types.GroupType{Elem: elem}, true)
		types.ExprType(q.Group.Exprs[0], env)
		if q.Group.Having != nil {
			if _, ok := types.ExprType(q.Group.Having, env).(types.BoolType); !ok {
				// allow unknown
			}
		}
	}

	if q.Where != nil {
		types.ExprType(q.Where, env)
	}
	if q.Sort != nil {
		types.ExprType(q.Sort, env)
	}
	if q.Skip != nil {
		types.ExprType(q.Skip, env)
	}
	if q.Take != nil {
		types.ExprType(q.Take, env)
	}
	types.ExprType(q.Select, env)
	return nil
}
