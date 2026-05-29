package lower

import (
	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/go/gotree"
)

// lowerDatalogQueryExpr runs a compile-time semi-naive bottom-up Datalog
// evaluator and returns a static Go slice literal `[]string{...}` of the
// flat free-variable values (same layout as the BEAM and C backends).
//
// Datalog is fully evaluated at lowering time: there is no runtime
// engine call. This works because Mochi Datalog programs are closed
// (all facts and rules are known at compile time) and the evaluator
// uses semi-naive fixpoint over a finite Herbrand universe, which
// always terminates.
func (l *lowerer) lowerDatalogQueryExpr(e *aotir.DatalogQueryExpr) (gotree.Expr, error) {
	if e.Prog == nil {
		return emptyStringSliceLit(), nil
	}
	results := datalogEval(e)
	elems := make([]gotree.Expr, 0, len(results))
	for _, r := range results {
		// gotree.BasicLit{Kind: StringLit} re-quotes Value via
		// strconv.Quote, so pass the raw fact string (no manual
		// quoting) to avoid producing `"\"Alice\""` literals.
		elems = append(elems, &gotree.BasicLit{
			Kind:  gotree.StringLit,
			Value: r,
		})
	}
	return &gotree.CompositeLit{
		Type: &gotree.Ident{Name: "[]string"},
		Elts: elems,
	}, nil
}

// emptyStringSliceLit returns the gotree.Expr for `[]string{}`. Used
// when the program is empty so the caller still gets a typed slice
// expression instead of nil.
func emptyStringSliceLit() gotree.Expr {
	return &gotree.CompositeLit{Type: &gotree.Ident{Name: "[]string"}}
}

// datalogEval performs semi-naive bottom-up evaluation of e.Prog and
// returns the flat list of free-variable values from matching tuples.
// Mirrors the BEAM backend's evaluator so the per-target outputs stay
// byte-identical (modulo target syntax).
func datalogEval(e *aotir.DatalogQueryExpr) []string {
	state := map[string][][]string{}

	for _, f := range e.Prog.Facts {
		args := make([]string, len(f.Args))
		copy(args, f.Args)
		state[f.Name] = append(state[f.Name], args)
	}

	for {
		changed := false
		for _, rule := range e.Prog.Rules {
			derived := deriveDatalogRule(rule, state)
			for _, t := range derived {
				if !datalogTupleIn(state[rule.HeadName], t) {
					state[rule.HeadName] = append(state[rule.HeadName], t)
					changed = true
				}
			}
		}
		if !changed {
			break
		}
	}

	rel := state[e.QueryName]
	var out []string
	for _, tuple := range rel {
		if len(tuple) != len(e.QueryArgs) {
			continue
		}
		match := true
		for i, qa := range e.QueryArgs {
			if qa == "" {
				continue
			}
			if tuple[i] != datalogUnquote(qa) {
				match = false
				break
			}
		}
		if !match {
			continue
		}
		for i, qa := range e.QueryArgs {
			if qa == "" {
				out = append(out, tuple[i])
			}
		}
	}
	return out
}

// deriveDatalogRule joins one rule's body literals against the current
// state and returns every head tuple the rule contributes this round.
// The join is a nested-loop sequential walk: env binds variables left
// to right; positive literals join, negation prunes, and inequality
// filters.
func deriveDatalogRule(rule aotir.DatalogRule, state map[string][][]string) [][]string {
	envs := []map[string]string{{}}
	for _, lit := range rule.Body {
		if lit.IsNeq {
			var next []map[string]string
			for _, env := range envs {
				a, aok := env[lit.NeqA]
				b, bok := env[lit.NeqB]
				if !aok || !bok || a != b {
					next = append(next, env)
				}
			}
			envs = next
			continue
		}
		if lit.IsNot {
			var next []map[string]string
			for _, env := range envs {
				matched := false
				for _, t := range state[lit.Name] {
					if len(t) != len(lit.Args) {
						continue
					}
					ok := true
					for i, arg := range lit.Args {
						if datalogResolve(arg, env) != t[i] {
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
			envs = next
			continue
		}
		var next []map[string]string
		for _, env := range envs {
			for _, t := range state[lit.Name] {
				if len(t) != len(lit.Args) {
					continue
				}
				newEnv := datalogCopyEnv(env)
				ok := true
				for i, arg := range lit.Args {
					if datalogIsVar(arg) {
						if existing, bound := newEnv[arg]; bound {
							if existing != t[i] {
								ok = false
								break
							}
						} else {
							newEnv[arg] = t[i]
						}
					} else if t[i] != datalogUnquote(arg) {
						ok = false
						break
					}
				}
				if ok {
					next = append(next, newEnv)
				}
			}
		}
		envs = next
	}

	out := make([][]string, 0, len(envs))
	for _, env := range envs {
		head := make([]string, len(rule.HeadArgs))
		for i, ha := range rule.HeadArgs {
			if datalogIsVar(ha) {
				head[i] = env[ha]
			} else {
				head[i] = datalogUnquote(ha)
			}
		}
		out = append(out, head)
	}
	return out
}

func datalogTupleIn(rel [][]string, t []string) bool {
	for _, r := range rel {
		if len(r) != len(t) {
			continue
		}
		eq := true
		for i := range r {
			if r[i] != t[i] {
				eq = false
				break
			}
		}
		if eq {
			return true
		}
	}
	return false
}

func datalogResolve(arg string, env map[string]string) string {
	if datalogIsVar(arg) {
		return env[arg]
	}
	return datalogUnquote(arg)
}

// datalogIsVar reports whether s is a Datalog variable (as opposed to
// a string-quoted constant). The aotir Datalog encoding quotes
// constants with embedded double-quotes, so anything not starting with
// a quote is a variable. Empty strings (used for free query args) are
// not variables and are handled by the caller before reaching here.
func datalogIsVar(s string) bool {
	return len(s) > 0 && s[0] != '"'
}

// datalogUnquote strips the outer double-quotes from a quoted constant.
// Non-quoted input is returned verbatim (defensive: lets the caller
// resolve already-bound variable values without a separate branch).
func datalogUnquote(s string) string {
	if len(s) >= 2 && s[0] == '"' && s[len(s)-1] == '"' {
		return s[1 : len(s)-1]
	}
	return s
}

func datalogCopyEnv(env map[string]string) map[string]string {
	out := make(map[string]string, len(env))
	for k, v := range env {
		out[k] = v
	}
	return out
}
