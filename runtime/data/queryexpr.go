package data

import (
	"fmt"

	"mochi/parser"
	"mochi/types"
)

// EvalQuery interprets a parsed QueryExpr over the given source dataset.
// The eval function is used to evaluate nested expressions with the provided environment.
func EvalQuery(src any, q *parser.QueryExpr, env *types.Env, eval func(*parser.Expr) (any, error)) ([]any, error) {
	var list []any
	switch v := src.(type) {
	case []any:
		list = v
	case *Group:
		list = v.Items
	default:
		return nil, fmt.Errorf("query source must be list, got %T", src)
	}

	child := env
	opts := QueryOptions{}

	setEnv := func(item any) {
		if m, ok := item.(map[string]any); ok && m["__join__"] == true {
			for k, v := range m {
				if k == "__join__" {
					continue
				}
				child.SetValue(k, v, true)
			}
		} else {
			child.SetValue(q.Var, item, true)
		}
	}

	for _, f := range q.Froms {
		srcVal, err := eval(f.Src)
		if err != nil {
			return nil, err
		}
		var joinList []any
		switch vv := srcVal.(type) {
		case []any:
			joinList = vv
		case *Group:
			joinList = vv.Items
		default:
			return nil, fmt.Errorf("join source must be list, got %T", srcVal)
		}
		fc := f
		opts.Joins = append(opts.Joins, Join{
			Items: joinList,
			Merge: func(left, right any) (any, error) {
				m := map[string]any{"__join__": true}
				if lm, ok := left.(map[string]any); ok && lm["__join__"] == true {
					for k, v := range lm {
						if k == "__join__" {
							continue
						}
						m[k] = v
					}
				} else {
					m[q.Var] = left
				}
				m[fc.Var] = right
				return m, nil
			},
		})
	}

	for _, j := range q.Joins {
		srcVal, err := eval(j.Src)
		if err != nil {
			return nil, err
		}
		var joinList []any
		switch vv := srcVal.(type) {
		case []any:
			joinList = vv
		case *Group:
			joinList = vv.Items
		default:
			return nil, fmt.Errorf("join source must be list, got %T", srcVal)
		}
		jc := j
		opts.Joins = append(opts.Joins, Join{
			Items: joinList,
			On: func(left, right any) (bool, error) {
				setEnv(left)
				child.SetValue(jc.Var, right, true)
				cond, err := eval(jc.On)
				if err != nil {
					return false, err
				}
				return truthy(cond), nil
			},
			Merge: func(left, right any) (any, error) {
				m := map[string]any{"__join__": true}
				if lm, ok := left.(map[string]any); ok && lm["__join__"] == true {
					for k, v := range lm {
						if k == "__join__" {
							continue
						}
						m[k] = v
					}
				} else {
					m[q.Var] = left
				}
				m[jc.Var] = right
				return m, nil
			},
			Left:  jc.Side != nil && (*jc.Side == "left" || *jc.Side == "outer"),
			Right: jc.Side != nil && (*jc.Side == "right" || *jc.Side == "outer"),
		})
	}

	if q.Where != nil {
		opts.Where = func(item any) (bool, error) {
			setEnv(item)
			cond, err := eval(q.Where)
			if err != nil {
				return false, err
			}
			return truthy(cond), nil
		}
	}

	if q.Group != nil {
		opts.GroupBy = func(item any) (any, error) {
			setEnv(item)
			return eval(q.Group.Expr)
		}
		opts.SelectGroup = func(g *Group) (any, error) {
			child.SetValue(q.Group.Name, g, true)
			return eval(q.Select)
		}
	} else {
		opts.Select = func(item any) (any, error) {
			setEnv(item)
			return eval(q.Select)
		}
	}

	if q.Sort != nil {
		opts.SortKey = func(item any) (any, error) {
			setEnv(item)
			return eval(q.Sort)
		}
	}

	if q.Skip != nil {
		v, err := eval(q.Skip)
		if err != nil {
			return nil, err
		}
		n, ok := v.(int)
		if !ok {
			return nil, fmt.Errorf("skip expects int, got %T", v)
		}
		opts.Skip = &n
	}

	if q.Take != nil {
		v, err := eval(q.Take)
		if err != nil {
			return nil, err
		}
		n, ok := v.(int)
		if !ok {
			return nil, fmt.Errorf("take expects int, got %T", v)
		}
		opts.Take = &n
	}

	return Query(list, opts)
}

func truthy(val any) bool {
	switch v := val.(type) {
	case nil:
		return false
	case bool:
		return v
	case int:
		return v != 0
	case int64:
		return v != 0
	case float64:
		return v != 0
	case string:
		return v != ""
	case []any:
		return len(v) > 0
	case map[string]any:
		return len(v) > 0
	case *Group:
		return len(v.Items) > 0
	default:
		return true
	}
}
