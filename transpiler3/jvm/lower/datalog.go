package lower

import (
	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// lowerDatalogQueryExpr evaluates the Datalog program at compile time
// (semi-naive bottom-up fixpoint) and emits a static Java list literal
// containing the pre-computed result strings.
//
// This mirrors the BEAM backend's compile-time evaluation strategy:
// since the full DatalogProgram (facts + rules) is already captured in
// the aotir node at lower time, there is no need for a runtime engine.
// The emitted Java is simply:
//
//	new java.util.ArrayList<>(java.util.List.of("val1", "val2", ...))
//
// For an empty result, it emits new java.util.ArrayList<>().
func (l *lowerer) lowerDatalogQueryExpr(e *aotir.DatalogQueryExpr) (javasrc.Expr, error) {
	results := datalogEval(e)
	if len(results) == 0 {
		return &javasrc.NewExpr{
			Type: javasrc.TypeRef{Name: "java.util.ArrayList"},
			Args: nil,
		}, nil
	}
	elems := make([]javasrc.Expr, len(results))
	for i, r := range results {
		elems[i] = javasrc.StringLit(r)
	}
	listOf := &javasrc.StaticCallExpr{
		Class:  "java.util.List",
		Method: "of",
		Args:   elems,
	}
	return &javasrc.NewExpr{
		Type: javasrc.TypeRef{Name: "java.util.ArrayList"},
		Args: []javasrc.Expr{listOf},
	}, nil
}

// datalogEval performs semi-naive bottom-up evaluation of e.Prog and
// returns the flat list of free-variable values from matching tuples.
// Variable ordering in the output matches the order of free variables
// (empty-string entries) in e.QueryArgs, left-to-right.
func datalogEval(e *aotir.DatalogQueryExpr) []string {
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
				// Bound argument: qa is "\"value\"" -- strip quotes.
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

// deriveRule computes new head tuples by evaluating one rule body against state.
func deriveRule(rule aotir.DatalogRule, state map[string][][]string) [][]string {
	// Simple nested-loop join over body literals.
	// env maps variable names to bound values.
	results := []map[string]string{{}}
	for _, lit := range rule.Body {
		if lit.IsNeq {
			// Filter: env[NeqA] != env[NeqB].
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
			// Negation-as-failure: keep env only if no tuple in lit.Name matches.
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
		// Positive literal: join with relation tuples.
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
						// Variable: bind or check consistency.
						if existing, found := newEnv[arg]; found {
							if existing != t[i] {
								ok = false
								break
							}
						} else {
							newEnv[arg] = t[i]
						}
					} else {
						// Constant: must match exactly.
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

	// Build head tuples from the surviving bindings.
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

// resolveArg returns the value of arg: if arg is a quoted constant it strips
// the quotes; if it is a variable name it looks up env; otherwise returns arg.
func resolveArg(arg string, env map[string]string) string {
	if len(arg) >= 2 && arg[0] == '"' && arg[len(arg)-1] == '"' {
		return arg[1 : len(arg)-1]
	}
	if v, ok := env[arg]; ok {
		return v
	}
	return arg
}

// isVar reports whether arg is a Datalog variable (uppercase first letter
// or the wildcard "_"). Constants are quoted strings.
func isVar(arg string) bool {
	if len(arg) == 0 {
		return false
	}
	if arg[0] == '"' {
		return false
	}
	return true
}

// tupleInRelation reports whether t is already present in rel.
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

// copyEnv makes a shallow copy of a variable binding map.
func copyEnv(env map[string]string) map[string]string {
	cp := make(map[string]string, len(env))
	for k, v := range env {
		cp[k] = v
	}
	return cp
}
