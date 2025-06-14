package logic

// Simple Datalog-style logic programming runtime.

// Term represents either a variable or constant value.
type Term struct {
	Var   string
	Const any
}

// Predicate is a predicate with terms.
type Predicate struct {
	Name  string
	Terms []Term
}

// Rule represents a rule with a head predicate and body predicates.
type Rule struct {
	Head Predicate
	Body []Predicate
}

// DB stores facts and rules.
type DB struct {
	facts map[string][][]any
	rules map[string][]Rule
}

// New returns a new empty logic database.
func New() *DB {
	return &DB{facts: map[string][][]any{}, rules: map[string][]Rule{}}
}

// AddFact adds a fact with the given predicate name and arguments.
func (db *DB) AddFact(name string, args []any) {
	db.facts[name] = append(db.facts[name], args)
}

// AddRule adds a rule with head predicate name/vars and body predicates.
func (db *DB) AddRule(name string, vars []string, body []Predicate) {
	head := Predicate{Name: name, Terms: make([]Term, len(vars))}
	for i, v := range vars {
		head.Terms[i] = Term{Var: v}
	}
	db.rules[name] = append(db.rules[name], Rule{Head: head, Body: body})
}

// Query evaluates the predicate with variables and returns variable bindings.
func (db *DB) Query(name string, vars []string) []map[string]any {
	q := Predicate{Name: name, Terms: make([]Term, len(vars))}
	for i, v := range vars {
		q.Terms[i] = Term{Var: v}
	}
	envs := db.solve([]Predicate{q}, map[string]any{})
	res := make([]map[string]any, len(envs))
	for i, env := range envs {
		m := map[string]any{}
		for _, v := range vars {
			if val, ok := env[v]; ok {
				m[v] = val
			}
		}
		res[i] = m
	}
	return res
}

func copyEnv(env map[string]any) map[string]any {
	out := make(map[string]any, len(env))
	for k, v := range env {
		out[k] = v
	}
	return out
}

func getVal(t Term, env map[string]any) (any, bool) {
	if t.Var != "" {
		v, ok := env[t.Var]
		return v, ok
	}
	return t.Const, true
}

func unifyTerms(a, b []Term, env map[string]any) (map[string]any, bool) {
	if len(a) != len(b) {
		return nil, false
	}
	out := copyEnv(env)
	for i := range a {
		v1, ok1 := getVal(a[i], out)
		v2, ok2 := getVal(b[i], out)
		switch {
		case ok1 && ok2:
			if v1 != v2 {
				return nil, false
			}
			if a[i].Var != "" {
				out[a[i].Var] = v1
			}
			if b[i].Var != "" {
				out[b[i].Var] = v1
			}
		case ok1:
			if b[i].Var != "" {
				out[b[i].Var] = v1
			} else if v1 != v2 {
				return nil, false
			}
		case ok2:
			if a[i].Var != "" {
				out[a[i].Var] = v2
			} else if v1 != v2 {
				return nil, false
			}
		default:
			if a[i].Var != "" {
				if b[i].Var != "" {
					// both vars unbound - nothing to do
				} else {
					out[a[i].Var] = v2
				}
			} else if b[i].Var != "" {
				out[b[i].Var] = v1
			} else if v1 != v2 {
				return nil, false
			}
		}
	}
	return out, true
}

func (db *DB) solve(goals []Predicate, env map[string]any) []map[string]any {
	if len(goals) == 0 {
		return []map[string]any{env}
	}
	g := goals[0]
	rest := goals[1:]
	var res []map[string]any
	// facts
	if tuples, ok := db.facts[g.Name]; ok {
		for _, tup := range tuples {
			fpred := Predicate{Name: g.Name, Terms: make([]Term, len(tup))}
			for i, v := range tup {
				fpred.Terms[i] = Term{Const: v}
			}
			env2, ok := unifyTerms(g.Terms, fpred.Terms, env)
			if !ok {
				continue
			}
			res = append(res, db.solve(rest, env2)...)
		}
	}
	// rules
	if rs, ok := db.rules[g.Name]; ok {
		for _, r := range rs {
			env2, ok := unifyTerms(g.Terms, r.Head.Terms, env)
			if !ok {
				continue
			}
			newGoals := append([]Predicate{}, r.Body...)
			newGoals = append(newGoals, rest...)
			res = append(res, db.solve(newGoals, env2)...)
		}
	}
	return res
}
