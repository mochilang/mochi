package datalog

import (
	"fmt"
	"regexp"
	"strings"
)

type Atom struct {
	Name string
	Vars []string
}

type NotEqual struct {
	X string
	Y string
}

type Condition struct {
	Atom     *Atom
	NotEqual *NotEqual
}

type Rule struct {
	Head Atom
	Body []Condition
}

type DB struct {
	Facts map[string][][]any
	Rules []Rule
}

func NewDB() *DB {
	return &DB{Facts: map[string][][]any{}, Rules: []Rule{}}
}

func (db *DB) AddFact(name string, args []any) {
	db.Facts[name] = append(db.Facts[name], args)
}

func (db *DB) AddRule(r Rule) {
	db.Rules = append(db.Rules, r)
}

func (db *DB) factExists(name string, tup []any) bool {
	for _, f := range db.Facts[name] {
		if len(f) != len(tup) {
			continue
		}
		match := true
		for i := range f {
			if f[i] != tup[i] {
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

func copyAssign(m map[string]any) map[string]any {
	n := map[string]any{}
	for k, v := range m {
		n[k] = v
	}
	return n
}

func unify(assign map[string]any, vars []string, fact []any) map[string]any {
	if len(vars) != len(fact) {
		return nil
	}
	res := copyAssign(assign)
	for i, v := range vars {
		val := fact[i]
		if exist, ok := res[v]; ok {
			if exist != val {
				return nil
			}
		}
		res[v] = val
	}
	return res
}

func (db *DB) derive() {
	changed := true
	for changed {
		changed = false
		for _, r := range db.Rules {
			assigns := []map[string]any{{}}
			for _, cond := range r.Body {
				if cond.NotEqual != nil {
					newAssigns := []map[string]any{}
					for _, a := range assigns {
						x, xok := a[cond.NotEqual.X]
						y, yok := a[cond.NotEqual.Y]
						if xok && yok {
							if x != y {
								newAssigns = append(newAssigns, a)
							}
						} else {
							newAssigns = append(newAssigns, a)
						}
					}
					assigns = newAssigns
					continue
				}
				facts := db.Facts[cond.Atom.Name]
				newAssigns := []map[string]any{}
				for _, a := range assigns {
					for _, f := range facts {
						if ua := unify(a, cond.Atom.Vars, f); ua != nil {
							newAssigns = append(newAssigns, ua)
						}
					}
				}
				assigns = newAssigns
			}
			for _, a := range assigns {
				tup := make([]any, len(r.Head.Vars))
				for i, v := range r.Head.Vars {
					tup[i] = a[v]
				}
				if !db.factExists(r.Head.Name, tup) {
					db.Facts[r.Head.Name] = append(db.Facts[r.Head.Name], tup)
					changed = true
				}
			}
		}
	}
}

func (db *DB) Query(q Query) ([]map[string]any, error) {
	db.derive()
	results := []map[string]any{}
	facts := db.Facts[q.Atom.Name]
	for _, f := range facts {
		if len(f) != len(q.Atom.Vars) {
			continue
		}
		res := map[string]any{}
		for i, v := range q.Atom.Vars {
			res[v] = f[i]
		}
		results = append(results, res)
	}
	return results, nil
}

// --- Parsing ---

var atomRE = regexp.MustCompile(`^([a-zA-Z_][a-zA-Z0-9_]*)\(([^)]*)\)$`)

func parseAtom(s string) (Atom, error) {
	s = strings.TrimSpace(s)
	m := atomRE.FindStringSubmatch(s)
	if m == nil {
		return Atom{}, fmt.Errorf("invalid atom: %s", s)
	}
	name := m[1]
	argsStr := strings.TrimSpace(m[2])
	vars := []string{}
	if argsStr != "" {
		parts := strings.Split(argsStr, ",")
		for _, p := range parts {
			vars = append(vars, strings.TrimSpace(p))
		}
	}
	return Atom{Name: name, Vars: vars}, nil
}

type Query struct {
	Atom Atom
}

func ParseQuery(s string) (Query, error) {
	a, err := parseAtom(strings.TrimSpace(s))
	if err != nil {
		return Query{}, err
	}
	return Query{Atom: a}, nil
}

func ParseRule(s string) (Rule, error) {
	parts := strings.Split(s, ":-")
	if len(parts) != 2 {
		return Rule{}, fmt.Errorf("invalid rule: %s", s)
	}
	headStr := strings.TrimSpace(parts[0])
	bodyStr := strings.TrimSpace(parts[1])
	head, err := parseAtom(headStr)
	if err != nil {
		return Rule{}, err
	}
	bodyParts := splitClauses(bodyStr)
	body := []Condition{}
	for _, bp := range bodyParts {
		bp = strings.TrimSpace(bp)
		if strings.Contains(bp, "!=") {
			sub := strings.Split(bp, "!=")
			if len(sub) != 2 {
				return Rule{}, fmt.Errorf("invalid inequality: %s", bp)
			}
			body = append(body, Condition{NotEqual: &NotEqual{strings.TrimSpace(sub[0]), strings.TrimSpace(sub[1])}})
		} else {
			at, err := parseAtom(bp)
			if err != nil {
				return Rule{}, err
			}
			body = append(body, Condition{Atom: &at})
		}
	}
	return Rule{Head: head, Body: body}, nil
}

func splitClauses(s string) []string {
	parts := []string{}
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '(':
			depth++
		case ')':
			if depth > 0 {
				depth--
			}
		case ',':
			if depth == 0 {
				parts = append(parts, strings.TrimSpace(s[start:i]))
				start = i + 1
			}
		}
	}
	if start < len(s) {
		parts = append(parts, strings.TrimSpace(s[start:]))
	}
	return parts
}
