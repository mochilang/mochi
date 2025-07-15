package data

import (
	"fmt"
	"sort"

	"mochi/parser"
	"mochi/types"
)

// ExecPlan executes the logical plan built from a query expression.
// eval is used to evaluate embedded expressions within the plan nodes.
func ExecPlan(plan Plan, env *types.Env, eval func(*parser.Expr) (any, error)) ([]any, error) {
	setEnv := func(item any, alias string) {
		if m, ok := item.(map[string]any); ok && m["__join__"] == true {
			for k, v := range m {
				if k == "__join__" {
					continue
				}
				env.SetValue(k, v, true)
			}
		} else if alias != "" {
			env.SetValue(alias, item, true)
		}
	}

	var exec func(Plan, string) ([]any, string, error)
	exec = func(pl Plan, ctx string) ([]any, string, error) {
		switch p := pl.(type) {
		case *ScanPlan:
			val, err := eval(p.Src)
			if err != nil {
				return nil, "", err
			}
			var list []any
			switch v := val.(type) {
			case []any:
				list = v
			case *Group:
				list = v.Items
			default:
				if ctx == "join" {
					return nil, "", fmt.Errorf("join source must be list, got %T", val)
				}
				return nil, "", fmt.Errorf("query source must be list, got %T", val)
			}
			out := append([]any(nil), list...)
			return out, p.Alias, nil

		case *WherePlan:
			items, alias, err := exec(p.Input, ctx)
			if err != nil {
				return nil, "", err
			}
			filtered := make([]any, 0, len(items))
			for _, it := range items {
				setEnv(it, alias)
				keep, err := eval(p.Cond)
				if err != nil {
					return nil, "", err
				}
				if truthy(keep) {
					filtered = append(filtered, it)
				}
			}
			return filtered, alias, nil

		case *SelectPlan:
			items, alias, err := exec(p.Input, ctx)
			if err != nil {
				return nil, "", err
			}
			results := make([]any, 0, len(items))
			for _, it := range items {
				setEnv(it, alias)
				val, err := eval(p.Expr)
				if err != nil {
					return nil, "", err
				}
				results = append(results, val)
			}
			return results, "", nil

		case *JoinPlan:
			leftItems, lalias, err := exec(p.Left, ctx)
			if err != nil {
				return nil, "", err
			}
			rightItems, ralias, err := exec(p.Right, "join")
			if err != nil {
				return nil, "", err
			}
			joinLeft := p.JoinType == "left" || p.JoinType == "outer"
			joinRight := p.JoinType == "right" || p.JoinType == "outer"
			merged := make([]any, 0)

			merge := func(l, r any) map[string]any {
				m := map[string]any{"__join__": true}
				if lm, ok := l.(map[string]any); ok && lm["__join__"] == true {
					for k, v := range lm {
						if k == "__join__" {
							continue
						}
						m[k] = v
					}
				} else if lalias != "" {
					m[lalias] = l
				}
				if rm, ok := r.(map[string]any); ok && rm["__join__"] == true {
					for k, v := range rm {
						if k == "__join__" {
							continue
						}
						m[k] = v
					}
				} else if ralias != "" {
					m[ralias] = r
				}
				return m
			}

			if joinLeft && joinRight { // full outer join
				matchedRights := make([]bool, len(rightItems))
				for _, left := range leftItems {
					matched := false
					for ri, right := range rightItems {
						keep := true
						if p.On != nil {
							setEnv(left, lalias)
							setEnv(right, ralias)
							cond, err := eval(p.On)
							if err != nil {
								return nil, "", err
							}
							keep = truthy(cond)
						}
						if !keep {
							continue
						}
						matched = true
						matchedRights[ri] = true
						merged = append(merged, merge(left, right))
					}
					if !matched {
						merged = append(merged, merge(left, nil))
					}
				}
				for ri, right := range rightItems {
					if !matchedRights[ri] {
						merged = append(merged, merge(nil, right))
					}
				}
			} else if joinRight {
				for _, right := range rightItems {
					matched := false
					for _, left := range leftItems {
						keep := true
						if p.On != nil {
							setEnv(left, lalias)
							setEnv(right, ralias)
							cond, err := eval(p.On)
							if err != nil {
								return nil, "", err
							}
							keep = truthy(cond)
						}
						if !keep {
							continue
						}
						matched = true
						merged = append(merged, merge(left, right))
					}
					if !matched {
						merged = append(merged, merge(nil, right))
					}
				}
			} else {
				for _, left := range leftItems {
					matched := false
					for _, right := range rightItems {
						keep := true
						if p.On != nil {
							setEnv(left, lalias)
							setEnv(right, ralias)
							cond, err := eval(p.On)
							if err != nil {
								return nil, "", err
							}
							keep = truthy(cond)
						}
						if !keep {
							continue
						}
						matched = true
						merged = append(merged, merge(left, right))
					}
					if joinLeft && !matched {
						merged = append(merged, merge(left, nil))
					}
				}
			}

			return merged, "", nil

		case *GroupPlan:
			items, alias, err := exec(p.Input, ctx)
			if err != nil {
				return nil, "", err
			}
			groups := map[string]*Group{}
			order := []string{}
			for _, it := range items {
				setEnv(it, alias)
				var key any
				if len(p.By) == 1 {
					key, err = eval(p.By[0])
				} else {
					m := map[string]any{}
					for idx, e := range p.By {
						v, err2 := eval(e)
						if err2 != nil {
							return nil, "", err2
						}
						m[fmt.Sprintf("k%d", idx+1)] = v
					}
					key = m
				}
				if err != nil {
					return nil, "", err
				}
				ks := fmt.Sprint(key)
				g, ok := groups[ks]
				if !ok {
					g = &Group{Key: key}
					groups[ks] = g
					order = append(order, ks)
				}
				g.Items = append(g.Items, it)
			}
			results := make([]any, 0, len(groups))
			for _, ks := range order {
				g := groups[ks]
				results = append(results, g)
			}
			return results, p.Name, nil

		case *SortPlan:
			items, alias, err := exec(p.Input, ctx)
			if err != nil {
				return nil, "", err
			}
			type pair struct {
				item any
				key  any
			}
			pairs := make([]pair, len(items))
			for idx, it := range items {
				setEnv(it, alias)
				key, err := eval(p.Key)
				if err != nil {
					return nil, "", err
				}
				pairs[idx] = pair{it, key}
			}
			sort.Slice(pairs, func(i, j int) bool {
				a, b := pairs[i].key, pairs[j].key
				switch av := a.(type) {
				case int:
					switch bv := b.(type) {
					case int:
						return av < bv
					case float64:
						return float64(av) < bv
					}
				case float64:
					switch bv := b.(type) {
					case int:
						return av < float64(bv)
					case float64:
						return av < bv
					}
				case string:
					bs, _ := b.(string)
					return av < bs
				}
				return fmt.Sprint(a) < fmt.Sprint(b)
			})
			for idx, p := range pairs {
				items[idx] = p.item
			}
			return items, alias, nil

		case *LimitPlan:
			items, alias, err := exec(p.Input, ctx)
			if err != nil {
				return nil, "", err
			}
			if p.Skip != nil {
				v, err := eval(p.Skip)
				if err != nil {
					return nil, "", err
				}
				n, ok := v.(int)
				if !ok {
					return nil, "", fmt.Errorf("skip expects int, got %T", v)
				}
				if n < len(items) {
					items = items[n:]
				} else {
					items = []any{}
				}
			}
			if p.Take != nil {
				v, err := eval(p.Take)
				if err != nil {
					return nil, "", err
				}
				n, ok := v.(int)
				if !ok {
					return nil, "", fmt.Errorf("take expects int, got %T", v)
				}
				if n < len(items) {
					items = items[:n]
				}
			}
			return items, alias, nil
		default:
			return nil, "", fmt.Errorf("unknown plan node %T", p)
		}
	}

	items, _, err := exec(plan, "query")
	return items, err
}

// EvalQuery interprets a parsed QueryExpr by first building a logical plan
// and then executing it.
func EvalQuery(q *parser.QueryExpr, env *types.Env, eval func(*parser.Expr) (any, error)) ([]any, error) {
	plan, err := BuildPlan(q)
	if err != nil {
		return nil, err
	}
	return ExecPlan(plan, env, eval)
}
