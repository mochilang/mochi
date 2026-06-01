// Phase 7: Query DSL and Datalog lowering.
//
// QueryScopeStmt wraps a from/where/select pipeline. In the aotir IR it
// carries a pre-declared result variable (ResultVar) and a Body block
// containing the ForEachStmt / IfStmt / AppendExpr chain. The TS lowering
// re-uses the existing ForEachStmt + IfStmt + LetDecl lowering; the scope
// node only contributes the initial `const __qN: T[] = [];` declaration.
//
// DatalogQueryExpr is evaluated at compile time using a semi-naive
// bottom-up fixpoint. The result is a static `string[]` literal in the
// emitted TypeScript. No runtime Datalog engine is needed.
package lower

import (
	"fmt"
	"strconv"
	"strings"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

// lowerQueryScopeStmt translates a QueryScopeStmt to the TS pattern:
//
//	const __qN: T[] = [];
//	for (const x of xs) {
//	    if (cond) { __qN.push(proj); }
//	}
//
// The ResultVar is already declared by the aotir IR as a LetStmt before
// the QueryScopeStmt reaches the TS lowerer. The scope simply lowers the
// Body block, which contains the ForEachStmt and any sort/slice steps.
func (l *lowerer) lowerQueryScopeStmt(s *aotir.QueryScopeStmt) ([]tstree.Stmt, error) {
	if s.Body == nil {
		return nil, nil
	}
	return l.lowerBlock(s.Body.Statements)
}

// lowerDatalogQueryExpr runs the Datalog semi-naive fixpoint at compile
// time and returns a static string[] literal.
func (l *lowerer) lowerDatalogQueryExpr(e *aotir.DatalogQueryExpr) (tstree.Expr, error) {
	results := datalogTSEval(e)
	elems := make([]tstree.Expr, 0, len(results))
	for _, r := range results {
		elems = append(elems, &tstree.StringLit{Value: r})
	}
	return &tstree.RawExpr{Text: buildTSStringArrayLiteral(elems)}, nil
}

// buildTSStringArrayLiteral builds a `["a", "b", ...]` literal string.
func buildTSStringArrayLiteral(elems []tstree.Expr) string {
	if len(elems) == 0 {
		return "[]"
	}
	var sb strings.Builder
	sb.WriteByte('[')
	for i, e := range elems {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(e.TsString(0))
	}
	sb.WriteByte(']')
	return sb.String()
}

// datalogTSEval performs semi-naive bottom-up Datalog evaluation and
// returns the flat list of free-variable values from matching tuples.
// This is a copy of the Go transpiler's datalogEval function adapted
// to return []string for the TS string[] literal.
func datalogTSEval(e *aotir.DatalogQueryExpr) []string {
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
			derived := deriveDatalogTSRule(rule, state)
			for _, t := range derived {
				if !datalogTSTupleIn(state[rule.HeadName], t) {
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
			if tuple[i] != datalogTSUnquote(qa) {
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

func deriveDatalogTSRule(rule aotir.DatalogRule, state map[string][][]string) [][]string {
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
						if datalogTSResolve(arg, env) != t[i] {
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
		for _, t := range state[lit.Name] {
			if len(t) != len(lit.Args) {
				continue
			}
			for _, env := range envs {
				newEnv := copyEnvTS(env)
				ok := true
				for i, arg := range lit.Args {
					val := datalogTSUnquote(arg)
					if isVarTS(arg) {
						if bound, exists := newEnv[arg]; exists {
							if bound != t[i] {
								ok = false
								break
							}
						} else {
							newEnv[arg] = t[i]
						}
					} else {
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
		envs = next
	}
	var results [][]string
	for _, env := range envs {
		head := make([]string, len(rule.HeadArgs))
		for i, arg := range rule.HeadArgs {
			head[i] = datalogTSResolve(arg, env)
		}
		results = append(results, head)
	}
	return results
}

func datalogTSResolve(arg string, env map[string]string) string {
	if isVarTS(arg) {
		if v, ok := env[arg]; ok {
			return v
		}
		return arg
	}
	return datalogTSUnquote(arg)
}

func datalogTSUnquote(s string) string {
	if len(s) >= 2 && s[0] == '"' && s[len(s)-1] == '"' {
		unq, err := strconv.Unquote(s)
		if err == nil {
			return unq
		}
		return s[1 : len(s)-1]
	}
	return s
}

func isVarTS(s string) bool {
	if len(s) == 0 {
		return false
	}
	c := s[0]
	return (c >= 'A' && c <= 'Z') || c == '_'
}

func copyEnvTS(env map[string]string) map[string]string {
	out := make(map[string]string, len(env))
	for k, v := range env {
		out[k] = v
	}
	return out
}

func datalogTSTupleIn(tuples [][]string, t []string) bool {
	for _, ex := range tuples {
		if len(ex) != len(t) {
			continue
		}
		match := true
		for i := range ex {
			if ex[i] != t[i] {
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

// Ensure unused import fmt doesn't cause a compile error.
var _ = fmt.Sprintf
