// Package lower - Phase 8: Datalog compile-time evaluation for Kotlin backend.
//
// The Datalog program (facts + rules) is fully captured in the aotir node at
// lower time. We run a semi-naive bottom-up fixpoint evaluator and emit the
// pre-computed result as a static Kotlin list literal. No runtime engine needed.
package lower

import (
	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/kotlin/ktree"
)

// lowerDatalogQueryExpr evaluates the Datalog program at compile time and emits
// a Kotlin mutableListOf("val1", "val2", ...) literal containing the results.
func (l *lowerer) lowerDatalogQueryExpr(e *aotir.DatalogQueryExpr) (ktree.Expr, error) {
	results := datalogEvalKt(e)
	elems := make([]ktree.Expr, len(results))
	for i, r := range results {
		elems[i] = &ktree.StringLitExpr{Value: r}
	}
	return &ktree.ListLitExpr{ElemType: "String", Elems: elems}, nil
}

// datalogEvalKt runs the semi-naive bottom-up Datalog evaluator and returns
// the flat list of free-variable values from all matching tuples.
func datalogEvalKt(e *aotir.DatalogQueryExpr) []string {
	if e.Prog == nil {
		return nil
	}

	// Relation name -> set of tuples (each tuple is []string).
	state := map[string][][]string{}

	// Seed with base facts.
	for _, f := range e.Prog.Facts {
		args := make([]string, len(f.Args))
		copy(args, f.Args)
		state[f.Name] = append(state[f.Name], args)
	}

	// Semi-naive fixpoint: iterate until no new tuples are derived.
	for {
		changed := false
		for _, rule := range e.Prog.Rules {
			newTuples := dlDeriveRule(rule, state)
			for _, t := range newTuples {
				if !dlTupleInRelation(state[rule.HeadName], t) {
					state[rule.HeadName] = append(state[rule.HeadName], t)
					changed = true
				}
			}
		}
		if !changed {
			break
		}
	}

	// Collect matching tuples for the query.
	rel := state[e.QueryName]
	var out []string
	for _, tuple := range rel {
		if len(tuple) != len(e.QueryArgs) {
			continue
		}
		match := true
		for i, qa := range e.QueryArgs {
			if qa != "" {
				expected := qa
				if len(expected) >= 2 && expected[0] == '"' && expected[len(expected)-1] == '"' {
					expected = expected[1 : len(expected)-1]
				}
				if tuple[i] != expected {
					match = false
					break
				}
			}
		}
		if match {
			for i, qa := range e.QueryArgs {
				if qa == "" {
					out = append(out, tuple[i])
				}
			}
		}
	}
	return out
}

func dlDeriveRule(rule aotir.DatalogRule, state map[string][][]string) [][]string {
	results := []map[string]string{{}}
	for _, lit := range rule.Body {
		if lit.IsNeq {
			var next []map[string]string
			for _, env := range results {
				a, aok := env[lit.NeqA]
				b, bok := env[lit.NeqB]
				if !aok || !bok || a != b {
					next = append(next, env)
				}
			}
			results = next
			continue
		}
		if lit.IsNot {
			var next []map[string]string
			for _, env := range results {
				matched := false
				for _, t := range state[lit.Name] {
					if len(t) != len(lit.Args) {
						continue
					}
					ok := true
					for i, arg := range lit.Args {
						val := dlResolveArg(arg, env)
						if val != t[i] {
							ok = false
							break
						}
					}
					if ok {
						matched = true
						break
					}
				}
				if !matched {
					next = append(next, env)
				}
			}
			results = next
			continue
		}
		var next []map[string]string
		for _, env := range results {
			for _, t := range state[lit.Name] {
				if len(t) != len(lit.Args) {
					continue
				}
				newEnv := dlCopyEnv(env)
				ok := true
				for i, arg := range lit.Args {
					if dlIsVar(arg) {
						if existing, found := newEnv[arg]; found {
							if existing != t[i] {
								ok = false
								break
							}
						} else {
							newEnv[arg] = t[i]
						}
					} else {
						val := dlResolveArg(arg, env)
						if val != t[i] {
							ok = false
							break
						}
					}
				}
				if ok {
					next = append(next, newEnv)
				}
			}
		}
		results = next
	}

	var heads [][]string
	for _, env := range results {
		head := make([]string, len(rule.HeadArgs))
		valid := true
		for i, arg := range rule.HeadArgs {
			if dlIsVar(arg) {
				v, ok := env[arg]
				if !ok {
					valid = false
					break
				}
				head[i] = v
			} else {
				head[i] = dlResolveArg(arg, env)
			}
		}
		if valid {
			heads = append(heads, head)
		}
	}
	return heads
}

func dlResolveArg(arg string, env map[string]string) string {
	if len(arg) >= 2 && arg[0] == '"' && arg[len(arg)-1] == '"' {
		return arg[1 : len(arg)-1]
	}
	if v, ok := env[arg]; ok {
		return v
	}
	return arg
}

func dlIsVar(arg string) bool {
	if len(arg) == 0 {
		return false
	}
	return arg[0] != '"'
}

func dlTupleInRelation(rel [][]string, t []string) bool {
	for _, r := range rel {
		if len(r) != len(t) {
			continue
		}
		match := true
		for i := range r {
			if r[i] != t[i] {
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

func dlCopyEnv(env map[string]string) map[string]string {
	cp := make(map[string]string, len(env))
	for k, v := range env {
		cp[k] = v
	}
	return cp
}
