package lower

import (
	"mochi/transpiler3/c/aotir"
)

// datalogEval performs a semi-naive bottom-up fixpoint over e.Prog and
// returns the flat list of free-variable values from matching tuples.
// The algorithm matches transpiler3/beam/lower/lower.go:datalogEval so
// Python emit is byte-equal with vm3 and the BEAM backend.
//
// We evaluate at compile time and emit a static Python list[str] literal
// because the runtime engine would add a multi-hundred-LOC dependency to
// the wheel for a feature whose program text is already known statically.
// Mochi facts/rules cannot be reloaded at runtime, so compile-time eval is
// lossless.
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
			newTuples := datalogDeriveRule(rule, state)
			for _, t := range newTuples {
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
			if qa != "" {
				expected := datalogUnquote(qa)
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

func datalogDeriveRule(rule aotir.DatalogRule, state map[string][][]string) [][]string {
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
						val := datalogResolveArg(arg, env)
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
				newEnv := datalogCopyEnv(env)
				ok := true
				for i, arg := range lit.Args {
					if datalogIsVariable(arg) {
						if existing, bound := newEnv[arg]; bound {
							if existing != t[i] {
								ok = false
								break
							}
						} else {
							newEnv[arg] = t[i]
						}
					} else {
						expected := datalogUnquote(arg)
						if t[i] != expected {
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

	var out [][]string
	for _, env := range results {
		head := make([]string, len(rule.HeadArgs))
		for i, ha := range rule.HeadArgs {
			if datalogIsVariable(ha) {
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

func datalogResolveArg(arg string, env map[string]string) string {
	if datalogIsVariable(arg) {
		return env[arg]
	}
	return datalogUnquote(arg)
}

func datalogIsVariable(s string) bool {
	return len(s) > 0 && s[0] != '"'
}

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
