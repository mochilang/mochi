package logic

import (
	"mochi/parser"
)

// Term represents either a variable or constant value.
type Term struct {
	Var   string
	Value any
	IsVar bool
}

// Predicate in a rule body.
type Predicate struct {
	Name  string
	Terms []Term
}

// Rule defines head predicate and body predicates/expressions.
type Rule struct {
	HeadName  string
	HeadVars  []string
	BodyPreds []Predicate
	BodyExprs []*parser.Expr
}

// Engine stores facts and rules.
type Engine struct {
	Facts map[string][][]any
	Rules []Rule
}

// New creates a new logic engine.
func New() *Engine {
	return &Engine{Facts: map[string][][]any{}, Rules: []Rule{}}
}

// AddFact adds a fact.
func (e *Engine) AddFact(name string, tuple []any) {
	e.Facts[name] = append(e.Facts[name], tuple)
}

// AddRule adds a rule.
func (e *Engine) AddRule(r Rule) {
	e.Rules = append(e.Rules, r)
}

// hasFact checks if a fact already exists.
func (e *Engine) hasFact(name string, tuple []any) bool {
	for _, f := range e.Facts[name] {
		if len(f) != len(tuple) {
			continue
		}
		match := true
		for i, v := range f {
			if v != tuple[i] {
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

// materialize applies rules until no new facts are derived.
func (e *Engine) materialize(eval func(*parser.Expr, map[string]any) (any, error)) error {
	changed := true
	for changed {
		changed = false
		for _, r := range e.Rules {
			tuples, err := e.applyRule(r, eval)
			if err != nil {
				return err
			}
			for _, t := range tuples {
				if !e.hasFact(r.HeadName, t) {
					e.Facts[r.HeadName] = append(e.Facts[r.HeadName], t)
					changed = true
				}
			}
		}
	}
	return nil
}

func copyBindings(src map[string]any) map[string]any {
	dst := make(map[string]any, len(src))
	for k, v := range src {
		dst[k] = v
	}
	return dst
}

// unifyPredicate attempts to unify a predicate with a fact given existing bindings.
func unifyPredicate(pred Predicate, fact []any, bindings map[string]any) (map[string]any, bool) {
	if len(pred.Terms) != len(fact) {
		return nil, false
	}
	b := copyBindings(bindings)
	for i, term := range pred.Terms {
		val := fact[i]
		if term.IsVar {
			if cur, ok := b[term.Var]; ok {
				if cur != val {
					return nil, false
				}
			} else {
				b[term.Var] = val
			}
		} else {
			if term.Value != val {
				return nil, false
			}
		}
	}
	return b, true
}

func (e *Engine) applyRule(r Rule, eval func(*parser.Expr, map[string]any) (any, error)) ([][]any, error) {
	results := [][]any{}

	var backtrack func(int, map[string]any) error
	backtrack = func(idx int, bind map[string]any) error {
		if idx == len(r.BodyPreds) {
			// evaluate expressions
			for _, ex := range r.BodyExprs {
				val, err := eval(ex, bind)
				if err != nil {
					return err
				}
				keep, ok := val.(bool)
				if !ok || !keep {
					return nil
				}
			}
			tuple := make([]any, len(r.HeadVars))
			for i, v := range r.HeadVars {
				tuple[i] = bind[v]
			}
			results = append(results, tuple)
			return nil
		}
		pred := r.BodyPreds[idx]
		facts := e.Facts[pred.Name]
		for _, fact := range facts {
			nb, ok := unifyPredicate(pred, fact, bind)
			if !ok {
				continue
			}
			if err := backtrack(idx+1, nb); err != nil {
				return err
			}
		}
		return nil
	}

	if err := backtrack(0, map[string]any{}); err != nil {
		return nil, err
	}
	return results, nil
}

// Query evaluates a predicate and returns variable bindings.
func (e *Engine) Query(name string, vars []string, eval func(*parser.Expr, map[string]any) (any, error)) ([]map[string]any, error) {
	if err := e.materialize(eval); err != nil {
		return nil, err
	}
	var results []map[string]any
	for _, fact := range e.Facts[name] {
		if len(fact) != len(vars) {
			continue
		}
		m := map[string]any{}
		for i, v := range vars {
			m[v] = fact[i]
		}
		results = append(results, m)
	}
	return results, nil
}
