package logic

// Package logic provides a tiny Datalog-style engine used by the interpreter.

// Term represents either a variable or constant value.
type Term struct {
	Var   string
	Val   any
	IsVar bool
}

type Predicate struct {
	Name string
	Args []Term
}

type Rule struct {
	Head Predicate
	Body []Condition
}

type Condition struct {
	Pred *Predicate
	Neq  *NotEqual
}

type NotEqual struct {
	A string
	B string
}

// Engine stores facts and rules.
type Engine struct {
	Facts map[string][][]any
	Rules []Rule
}

func NewEngine() *Engine {
	return &Engine{Facts: map[string][][]any{}, Rules: []Rule{}}
}

// AddFact adds a fact to the engine.
func (e *Engine) AddFact(p Predicate) {
	tuple := make([]any, len(p.Args))
	for i, a := range p.Args {
		if a.IsVar {
			// variables aren't allowed in facts
			return
		}
		tuple[i] = a.Val
	}
	e.Facts[p.Name] = append(e.Facts[p.Name], tuple)
}

// AddRule registers a rule.
func (e *Engine) AddRule(r Rule) { e.Rules = append(e.Rules, r) }

// Query derives all rules then returns matches for predicate.
func (e *Engine) Query(q Predicate) []map[string]any {
	e.derive()
	results := []map[string]any{}
	tuples := e.Facts[q.Name]
	for _, tup := range tuples {
		env, ok := unify(q, tup, map[string]any{})
		if !ok {
			continue
		}
		res := map[string]any{}
		for _, a := range q.Args {
			if a.IsVar {
				res[a.Var] = env[a.Var]
			}
		}
		results = append(results, res)
	}
	return results
}

func (e *Engine) derive() {
	changed := true
	for changed {
		changed = false
		for _, r := range e.Rules {
			tuples := applyRule(r, e.Facts)
			for _, t := range tuples {
				if !containsTuple(e.Facts[r.Head.Name], t) {
					e.Facts[r.Head.Name] = append(e.Facts[r.Head.Name], t)
					changed = true
				}
			}
		}
	}
}

func applyRule(r Rule, facts map[string][][]any) [][]any {
	var results [][]any
	var backtrack func(int, map[string]any)
	backtrack = func(i int, env map[string]any) {
		if i == len(r.Body) {
			tup := make([]any, len(r.Head.Args))
			for j, a := range r.Head.Args {
				if a.IsVar {
					tup[j] = env[a.Var]
				} else {
					tup[j] = a.Val
				}
			}
			results = append(results, tup)
			return
		}
		cond := r.Body[i]
		if cond.Neq != nil {
			if env[cond.Neq.A] != env[cond.Neq.B] {
				backtrack(i+1, env)
			}
			return
		}
		pred := cond.Pred
		for _, tup := range facts[pred.Name] {
			newEnv, ok := unify(*pred, tup, env)
			if !ok {
				continue
			}
			backtrack(i+1, newEnv)
		}
	}
	backtrack(0, map[string]any{})
	return results
}

func unify(pred Predicate, tuple []any, env map[string]any) (map[string]any, bool) {
	if len(tuple) < len(pred.Args) {
		return nil, false
	}
	newEnv := map[string]any{}
	for k, v := range env {
		newEnv[k] = v
	}
	for i, a := range pred.Args {
		val := tuple[i]
		if a.IsVar {
			if bound, ok := newEnv[a.Var]; ok {
				if bound != val {
					return nil, false
				}
			} else {
				newEnv[a.Var] = val
			}
		} else {
			if a.Val != val {
				return nil, false
			}
		}
	}
	return newEnv, true
}

func containsTuple(list [][]any, t []any) bool {
	for _, x := range list {
		if len(x) != len(t) {
			continue
		}
		match := true
		for i := range x {
			if x[i] != t[i] {
				match = false
				break
			}
		}
		if match {
			return true
		}
	}
	return false
}
