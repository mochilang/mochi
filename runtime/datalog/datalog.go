package datalog

import (
	"fmt"
	"strings"
)

type Term struct {
	Var   string
	Const string
}

type Atom struct {
	Pred  string
	Terms []Term
}

type Rule struct {
	Head Atom
	Body []Atom
}

type KB struct {
	Facts map[string][][]string
	Rules []Rule
}

var global = NewKB()

func NewKB() *KB {
	return &KB{Facts: map[string][][]string{}, Rules: []Rule{}}
}

func Fact(pred string, args ...string) {
	global.Fact(pred, args...)
}

func RuleStr(rule string) error {
	r, err := parseRule(rule)
	if err != nil {
		return err
	}
	global.Rules = append(global.Rules, r)
	return nil
}

func QueryStr(q string) ([]map[string]string, error) {
	a, err := parseAtom(strings.TrimSpace(q))
	if err != nil {
		return nil, err
	}
	global.saturate()
	return global.query(a), nil
}

func (kb *KB) Fact(pred string, args ...string) {
	kb.Facts[pred] = append(kb.Facts[pred], append([]string(nil), args...))
}

func (kb *KB) query(goal Atom) []map[string]string {
	var results []map[string]string
	for _, fact := range kb.Facts[goal.Pred] {
		env := map[string]string{}
		if unifyFact(goal, fact, env) {
			results = append(results, env)
		}
	}
	return results
}

func unifyFact(atom Atom, fact []string, env map[string]string) bool {
	if len(atom.Terms) != len(fact) {
		return false
	}
	for i, t := range atom.Terms {
		val := fact[i]
		if t.Const != "" {
			if t.Const != val {
				return false
			}
			continue
		}
		if cur, ok := env[t.Var]; ok {
			if cur != val {
				return false
			}
		} else {
			env[t.Var] = val
		}
	}
	return true
}

func (kb *KB) saturate() {
	changed := true
	for changed {
		changed = false
		for _, r := range kb.Rules {
			tuples := kb.applyRule(r)
			for _, tup := range tuples {
				if !kb.hasFact(r.Head.Pred, tup) {
					kb.Facts[r.Head.Pred] = append(kb.Facts[r.Head.Pred], tup)
					changed = true
				}
			}
		}
	}
}

func (kb *KB) hasFact(pred string, tuple []string) bool {
	for _, f := range kb.Facts[pred] {
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

func (kb *KB) applyRule(r Rule) [][]string {
	envs := []map[string]string{{}}
	for _, atom := range r.Body {
		envs = kb.extendEnv(atom, envs)
	}
	var results [][]string
	for _, env := range envs {
		tup := make([]string, len(r.Head.Terms))
		for i, t := range r.Head.Terms {
			if t.Const != "" {
				tup[i] = t.Const
			} else {
				tup[i] = env[t.Var]
			}
		}
		results = append(results, tup)
	}
	return results
}

func (kb *KB) extendEnv(atom Atom, envs []map[string]string) []map[string]string {
	if atom.Pred == "!=" {
		var out []map[string]string
		for _, env := range envs {
			a := evalTerm(atom.Terms[0], env)
			b := evalTerm(atom.Terms[1], env)
			if a != b {
				out = append(out, copyEnv(env))
			}
		}
		return out
	}
	var out []map[string]string
	for _, fact := range kb.Facts[atom.Pred] {
		for _, env := range envs {
			dup := copyEnv(env)
			if unifyFact(atom, fact, dup) {
				out = append(out, dup)
			}
		}
	}
	return out
}

func copyEnv(in map[string]string) map[string]string {
	dup := map[string]string{}
	for k, v := range in {
		dup[k] = v
	}
	return dup
}

func evalTerm(t Term, env map[string]string) string {
	if t.Const != "" {
		return t.Const
	}
	return env[t.Var]
}

func parseRule(s string) (Rule, error) {
	parts := strings.Split(s, ":-")
	if len(parts) != 2 {
		return Rule{}, fmt.Errorf("invalid rule: %s", s)
	}
	head, err := parseAtom(strings.TrimSpace(parts[0]))
	if err != nil {
		return Rule{}, err
	}
	bodyParts := strings.Split(parts[1], ",")
	body := make([]Atom, 0, len(bodyParts))
	for _, bp := range bodyParts {
		a, err := parseAtom(strings.TrimSpace(bp))
		if err != nil {
			return Rule{}, err
		}
		body = append(body, a)
	}
	return Rule{Head: head, Body: body}, nil
}

func parseAtom(s string) (Atom, error) {
	s = strings.TrimSpace(s)
	if strings.Contains(s, "!=") {
		parts := strings.Split(s, "!=")
		if len(parts) != 2 {
			return Atom{}, fmt.Errorf("invalid inequality: %s", s)
		}
		left := strings.TrimSpace(parts[0])
		right := strings.TrimSpace(parts[1])
		return Atom{Pred: "!=", Terms: []Term{parseTerm(left), parseTerm(right)}}, nil
	}
	idx := strings.Index(s, "(")
	if idx < 0 || !strings.HasSuffix(s, ")") {
		return Atom{}, fmt.Errorf("invalid atom: %s", s)
	}
	pred := strings.TrimSpace(s[:idx])
	argsStr := s[idx+1 : len(s)-1]
	argParts := []string{}
	if strings.TrimSpace(argsStr) != "" {
		argParts = strings.Split(argsStr, ",")
	}
	terms := make([]Term, 0, len(argParts))
	for _, ap := range argParts {
		terms = append(terms, parseTerm(strings.TrimSpace(ap)))
	}
	return Atom{Pred: pred, Terms: terms}, nil
}

func parseTerm(s string) Term {
	s = strings.TrimSpace(s)
	if strings.HasPrefix(s, "\"") && strings.HasSuffix(s, "\"") {
		return Term{Const: strings.Trim(s, "\"")}
	}
	return Term{Var: s}
}

func init() {
	ffiRegister()
}
