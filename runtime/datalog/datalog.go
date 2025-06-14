package datalog

import (
	"mochi/parser"
)

type Term struct {
	Var string
	Val any
}

func (t Term) IsVar() bool { return t.Var != "" }

// Predicate call
type Pred struct {
	Name string
	Args []Term
}

type Atom struct {
	Call *Pred
	Cond *parser.Expr
}

type Rule struct {
	Name   string
	Params []string
	Body   []Atom
}

type Engine struct {
	Facts map[string][][]any
	Rules map[string][]Rule
}

func NewEngine() *Engine {
	return &Engine{Facts: map[string][][]any{}, Rules: map[string][]Rule{}}
}

func (e *Engine) AddFact(name string, args []any) {
	e.Facts[name] = append(e.Facts[name], args)
}

func (e *Engine) AddRule(r Rule) {
	e.Rules[r.Name] = append(e.Rules[r.Name], r)
}

type evalFunc func(map[string]any, *parser.Expr) (any, error)

func (e *Engine) Query(name string, args []Term, eval evalFunc) ([]map[string]any, error) {
	return e.solve(name, args, map[string]any{}, eval)
}

func (e *Engine) solve(name string, args []Term, env map[string]any, eval evalFunc) ([]map[string]any, error) {
	var results []map[string]any
	// facts
	if tuples, ok := e.Facts[name]; ok {
		for _, tup := range tuples {
			if env2, ok := unify(args, tup, env); ok {
				results = append(results, env2)
			}
		}
	}
	// rules
	for _, r := range e.Rules[name] {
		if len(r.Params) != len(args) {
			continue
		}
		env2 := copyEnv(env)
		for i, param := range r.Params {
			t := args[i]
			if t.IsVar() {
				if v, ok := env2[t.Var]; ok {
					env2[param] = v
				}
			} else {
				env2[param] = t.Val
			}
		}
		subres, err := e.solveAtoms(r.Body, env2, eval)
		if err != nil {
			return nil, err
		}
		for _, sr := range subres {
			envOut := copyEnv(env)
			ok := true
			for i, param := range r.Params {
				t := args[i]
				v := sr[param]
				if t.IsVar() {
					if vv, ok := envOut[t.Var]; ok {
						if vv != v {
							ok = false
							break
						}
					} else {
						envOut[t.Var] = v
					}
				} else if t.Val != v {
					ok = false
					break
				}
			}
			if ok {
				for k, v := range sr {
					if _, exists := envOut[k]; !exists {
						envOut[k] = v
					}
				}
				results = append(results, envOut)
			}
		}
	}
	return results, nil
}

func (e *Engine) solveAtoms(atoms []Atom, env map[string]any, eval evalFunc) ([]map[string]any, error) {
	if len(atoms) == 0 {
		return []map[string]any{env}, nil
	}
	first := atoms[0]
	rest := atoms[1:]
	envs, err := e.solveAtom(first, env, eval)
	if err != nil {
		return nil, err
	}
	var results []map[string]any
	for _, ev := range envs {
		sub, err := e.solveAtoms(rest, ev, eval)
		if err != nil {
			return nil, err
		}
		results = append(results, sub...)
	}
	return results, nil
}

func truthy(v any) bool {
	switch x := v.(type) {
	case bool:
		return x
	case int:
		return x != 0
	case int64:
		return x != 0
	case float64:
		return x != 0
	case string:
		return x != ""
	default:
		return v != nil
	}
}

func (e *Engine) solveAtom(a Atom, env map[string]any, eval evalFunc) ([]map[string]any, error) {
	if a.Call != nil {
		return e.solve(a.Call.Name, a.Call.Args, env, eval)
	}
	if a.Cond != nil {
		v, err := eval(env, a.Cond)
		if err != nil {
			return nil, err
		}
		if truthy(v) {
			return []map[string]any{env}, nil
		}
		return nil, nil
	}
	return nil, nil
}

func unify(terms []Term, tuple []any, env map[string]any) (map[string]any, bool) {
	if len(terms) != len(tuple) {
		return nil, false
	}
	env2 := copyEnv(env)
	for i, t := range terms {
		val := tuple[i]
		if t.IsVar() {
			if v, ok := env2[t.Var]; ok {
				if v != val {
					return nil, false
				}
			} else {
				env2[t.Var] = val
			}
		} else {
			if t.Val != val {
				return nil, false
			}
		}
	}
	return env2, true
}

func copyEnv(m map[string]any) map[string]any {
	out := map[string]any{}
	for k, v := range m {
		out[k] = v
	}
	return out
}
