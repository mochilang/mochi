package lower

import "mochi/transpiler3/c/aotir"

// datalogEval runs a semi-naive bottom-up Datalog evaluator over e.Prog
// and returns the flat list of free-variable values for tuples that match
// the query. The layout mirrors the BEAM backend's compile-time evaluator
// so the same fixture .out files validate both backends.
func datalogEval(e *aotir.DatalogQueryExpr) []string {
	if e == nil || e.Prog == nil {
		return nil
	}
	state := map[string][][]string{}

	for _, f := range e.Prog.Facts {
		args := make([]string, len(f.Args))
		copy(args, f.Args)
		state[f.Name] = append(state[f.Name], args)
	}

	for {
		changed := false
		for _, rule := range e.Prog.Rules {
			newTuples := dlDeriveRule(rule, state)
			for _, t := range newTuples {
				if !dlTupleIn(state[rule.HeadName], t) {
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
			if qa != "" {
				if tuple[i] != dlUnquote(qa) {
					match = false
					break
				}
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

func dlDeriveRule(rule aotir.DatalogRule, state map[string][][]string) [][]string {
	results := []map[string]string{{}}
	for _, lit := range rule.Body {
		switch {
		case lit.IsNeq:
			var next []map[string]string
			for _, env := range results {
				a, aok := env[lit.NeqA]
				b, bok := env[lit.NeqB]
				if !aok || !bok || a != b {
					next = append(next, env)
				}
			}
			results = next
		case lit.IsNot:
			var next []map[string]string
			for _, env := range results {
				matched := false
				for _, t := range state[lit.Name] {
					if len(t) != len(lit.Args) {
						continue
					}
					ok := true
					for i, arg := range lit.Args {
						if dlResolve(arg, env) != t[i] {
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
		default:
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
							if existing, bound := newEnv[arg]; bound {
								if existing != t[i] {
									ok = false
									break
								}
							} else {
								newEnv[arg] = t[i]
							}
						} else if dlUnquote(arg) != t[i] {
							ok = false
							break
						}
					}
					if ok {
						next = append(next, newEnv)
					}
				}
			}
			results = next
		}
	}

	out := make([][]string, 0, len(results))
	for _, env := range results {
		head := make([]string, len(rule.HeadArgs))
		for i, ha := range rule.HeadArgs {
			if dlIsVar(ha) {
				head[i] = env[ha]
			} else {
				head[i] = dlUnquote(ha)
			}
		}
		out = append(out, head)
	}
	return out
}

func dlTupleIn(rel [][]string, t []string) bool {
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

func dlResolve(arg string, env map[string]string) string {
	if dlIsVar(arg) {
		return env[arg]
	}
	return dlUnquote(arg)
}

func dlIsVar(s string) bool { return len(s) > 0 && s[0] != '"' }

func dlUnquote(s string) string {
	if len(s) >= 2 && s[0] == '"' && s[len(s)-1] == '"' {
		return s[1 : len(s)-1]
	}
	return s
}

func dlCopyEnv(env map[string]string) map[string]string {
	out := make(map[string]string, len(env))
	for k, v := range env {
		out[k] = v
	}
	return out
}
