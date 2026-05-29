package semver

import (
	"fmt"
	"strings"
)

// Operator is a PEP 440 version comparison operator.
type Operator int

const (
	OpInvalid Operator = iota
	OpEq               // ==
	OpNeq              // !=
	OpLt               // <
	OpLte              // <=
	OpGt               // >
	OpGte              // >=
	OpCompat           // ~=
	OpArbitrary        // ===
)

// String renders the canonical operator token.
func (o Operator) String() string {
	switch o {
	case OpEq:
		return "=="
	case OpNeq:
		return "!="
	case OpLt:
		return "<"
	case OpLte:
		return "<="
	case OpGt:
		return ">"
	case OpGte:
		return ">="
	case OpCompat:
		return "~="
	case OpArbitrary:
		return "==="
	default:
		return "?"
	}
}

// Clause is a single PEP 440 version specifier (e.g. ">=1.0").
type Clause struct {
	Op       Operator
	Version  Version
	Original string // exact source text (post-trimming) for diagnostics
}

// Specifier is a comma-separated list of Clauses, the form accepted by
// `dependency >= a, < b` and by Mochi's `import python "<pkg>@<spec>"`.
type Specifier struct {
	Clauses []Clause
}

// ParseClause parses a single PEP 440 version clause.
func ParseClause(s string) (Clause, error) {
	src := strings.TrimSpace(s)
	if src == "" {
		return Clause{}, fmt.Errorf("semver: empty version clause")
	}
	// Order matters: try the three-char operators first so "===" and ">="
	// are not mis-parsed as "==" or ">".
	type opEntry struct {
		token string
		op    Operator
	}
	ops := []opEntry{
		{"===", OpArbitrary},
		{"==", OpEq},
		{"!=", OpNeq},
		{"<=", OpLte},
		{">=", OpGte},
		{"~=", OpCompat},
		{"<", OpLt},
		{">", OpGt},
	}
	for _, e := range ops {
		if strings.HasPrefix(src, e.token) {
			versionText := strings.TrimSpace(src[len(e.token):])
			if versionText == "" {
				return Clause{}, fmt.Errorf("semver: clause %q missing version", src)
			}
			v, err := Parse(versionText)
			if err != nil {
				return Clause{}, fmt.Errorf("semver: clause %q: %w", src, err)
			}
			return Clause{Op: e.op, Version: v, Original: src}, nil
		}
	}
	return Clause{}, fmt.Errorf("semver: clause %q has no operator", src)
}

// ParseSpecifier parses a comma-separated list of clauses.
func ParseSpecifier(s string) (Specifier, error) {
	src := strings.TrimSpace(s)
	if src == "" {
		return Specifier{}, nil
	}
	var spec Specifier
	for _, raw := range strings.Split(src, ",") {
		c, err := ParseClause(raw)
		if err != nil {
			return Specifier{}, err
		}
		spec.Clauses = append(spec.Clauses, c)
	}
	return spec, nil
}

// String renders the canonical specifier as a comma-joined list. Each clause
// is rendered as op + canonical version (no spaces between op and version,
// space after each comma).
func (s Specifier) String() string {
	parts := make([]string, len(s.Clauses))
	for i, c := range s.Clauses {
		parts[i] = c.Op.String() + c.Version.String()
	}
	return strings.Join(parts, ", ")
}

// Match reports whether v satisfies every clause of the specifier. An empty
// specifier matches every version.
func (s Specifier) Match(v Version) bool {
	for _, c := range s.Clauses {
		if !c.Match(v) {
			return false
		}
	}
	return true
}

// Match reports whether v satisfies the clause's operator + version.
func (c Clause) Match(v Version) bool {
	cmp := v.Compare(c.Version)
	switch c.Op {
	case OpEq:
		return cmp == 0
	case OpNeq:
		return cmp != 0
	case OpLt:
		return cmp < 0
	case OpLte:
		return cmp <= 0
	case OpGt:
		return cmp > 0
	case OpGte:
		return cmp >= 0
	case OpCompat:
		return matchCompatible(v, c.Version)
	case OpArbitrary:
		return v.String() == c.Version.String()
	default:
		return false
	}
}

// matchCompatible implements PEP 440's "compatible release" operator (~=).
// Given a clause version with n release components, the candidate must be
// >= the clause version AND have the same first n-1 release components.
// Example: ~= 2.2 accepts 2.2, 2.2.99, 2.3, 2.9 but rejects 3.0 or 2.1.
func matchCompatible(v, ref Version) bool {
	if v.Compare(ref) < 0 {
		return false
	}
	n := len(ref.Release) - 1
	if n <= 0 {
		return false
	}
	if len(v.Release) < n {
		return false
	}
	for i := 0; i < n; i++ {
		if v.Release[i] != ref.Release[i] {
			return false
		}
	}
	return true
}
