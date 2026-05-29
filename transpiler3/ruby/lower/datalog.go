package lower

import (
	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/ruby/rtree"
)

// lowerDatalogQueryExpr evaluates the Datalog program at compile time
// (semi-naive bottom-up fixpoint) and emits a Ruby Array literal containing
// the pre-computed result strings. Mirrors the JVM and BEAM strategies:
// since the full DatalogProgram (facts + rules) is captured in the aotir
// node at lower time, no runtime engine is needed.
func lowerDatalogQueryExpr(e *aotir.DatalogQueryExpr) (rtree.Expr, error) {
	results := datalogEval(e)
	parts := make([]rtree.Expr, len(results))
	for i, r := range results {
		parts[i] = &rtree.StringLit{Value: r}
	}
	strs := make([]string, len(parts))
	for i, p := range parts {
		strs[i] = p.RubyExprString()
	}
	joined := ""
	for i, s := range strs {
		if i > 0 {
			joined += ", "
		}
		joined += s
	}
	return &rtree.RawExpr{Text: "[" + joined + "]"}, nil
}

// datalogEval performs semi-naive bottom-up evaluation of e.Prog and
// returns the flat list of free-variable values from matching tuples.
func datalogEval(e *aotir.DatalogQueryExpr) []string {
	if e.Prog == nil {
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
			newTuples := deriveRule(rule, state)
			for _, t := range newTuples {
				if !tupleInRelation(state[rule.HeadName], t) {
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

func deriveRule(rule aotir.DatalogRule, state map[string][][]string) [][]string {
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
						val := resolveArg(arg, env)
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
				newEnv := copyEnv(env)
				ok := true
				for i, arg := range lit.Args {
					if isVar(arg) {
						if existing, found := newEnv[arg]; found {
							if existing != t[i] {
								ok = false
								break
							}
						} else {
							newEnv[arg] = t[i]
						}
					} else {
						val := resolveArg(arg, env)
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
			if isVar(arg) {
				v, ok := env[arg]
				if !ok {
					valid = false
					break
				}
				head[i] = v
			} else {
				head[i] = resolveArg(arg, env)
			}
		}
		if valid {
			heads = append(heads, head)
		}
	}
	return heads
}

func resolveArg(arg string, env map[string]string) string {
	if len(arg) >= 2 && arg[0] == '"' && arg[len(arg)-1] == '"' {
		return arg[1 : len(arg)-1]
	}
	if v, ok := env[arg]; ok {
		return v
	}
	return arg
}

func isVar(arg string) bool {
	if len(arg) == 0 {
		return false
	}
	if arg[0] == '"' {
		return false
	}
	return true
}

func tupleInRelation(rel [][]string, t []string) bool {
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

func copyEnv(env map[string]string) map[string]string {
	cp := make(map[string]string, len(env))
	for k, v := range env {
		cp[k] = v
	}
	return cp
}
