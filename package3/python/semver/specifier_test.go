package semver

import (
	"testing"
)

func TestParseClauseBasic(t *testing.T) {
	cases := []struct {
		in       string
		wantOp   Operator
		wantVer  string
	}{
		{"==1.0", OpEq, "1.0"},
		{"!=1.0", OpNeq, "1.0"},
		{">=1.0", OpGte, "1.0"},
		{"<=1.0", OpLte, "1.0"},
		{">1.0", OpGt, "1.0"},
		{"<1.0", OpLt, "1.0"},
		{"~=1.0", OpCompat, "1.0"},
		{"===1.0+local", OpArbitrary, "1.0+local"},
		{"  >= 1.0  ", OpGte, "1.0"}, // surrounding whitespace ok
	}
	for _, tc := range cases {
		c, err := ParseClause(tc.in)
		if err != nil {
			t.Errorf("ParseClause(%q): %v", tc.in, err)
			continue
		}
		if c.Op != tc.wantOp {
			t.Errorf("ParseClause(%q).Op = %v; want %v", tc.in, c.Op, tc.wantOp)
		}
		if got := c.Version.String(); got != tc.wantVer {
			t.Errorf("ParseClause(%q).Version = %q; want %q", tc.in, got, tc.wantVer)
		}
	}
}

func TestParseClauseRejects(t *testing.T) {
	bad := []string{
		"",
		"1.0",       // no operator
		">=",        // missing version
		"=1.0",      // single =
		"<<1.0",     // wrong op
		">=v1.0",    // bad version
	}
	for _, s := range bad {
		if _, err := ParseClause(s); err == nil {
			t.Errorf("ParseClause(%q) accepted; expected error", s)
		}
	}
}

func TestParseSpecifierMultiple(t *testing.T) {
	spec, err := ParseSpecifier(">=1.0, <2.0, !=1.5")
	if err != nil {
		t.Fatalf("ParseSpecifier: %v", err)
	}
	if len(spec.Clauses) != 3 {
		t.Fatalf("got %d clauses; want 3", len(spec.Clauses))
	}
	if spec.Clauses[0].Op != OpGte || spec.Clauses[0].Version.String() != "1.0" {
		t.Errorf("clause 0 = %+v; want >=1.0", spec.Clauses[0])
	}
	if spec.Clauses[1].Op != OpLt || spec.Clauses[1].Version.String() != "2.0" {
		t.Errorf("clause 1 = %+v; want <2.0", spec.Clauses[1])
	}
	if spec.Clauses[2].Op != OpNeq || spec.Clauses[2].Version.String() != "1.5" {
		t.Errorf("clause 2 = %+v; want !=1.5", spec.Clauses[2])
	}
}

func TestParseSpecifierEmpty(t *testing.T) {
	spec, err := ParseSpecifier("")
	if err != nil {
		t.Fatalf("ParseSpecifier(\"\"): %v", err)
	}
	if len(spec.Clauses) != 0 {
		t.Errorf("empty specifier produced %d clauses; want 0", len(spec.Clauses))
	}
	// An empty specifier matches every version.
	v, _ := Parse("1.0")
	if !spec.Match(v) {
		t.Errorf("empty specifier did not match %v", v)
	}
}

func TestSpecifierStringRoundTrip(t *testing.T) {
	cases := []string{
		">=1.0",
		">=1.0, <2.0",
		"==1.0, !=1.5",
		"~=2.2",
		"===1.0+local",
	}
	for _, s := range cases {
		spec, err := ParseSpecifier(s)
		if err != nil {
			t.Fatalf("ParseSpecifier(%q): %v", s, err)
		}
		if got := spec.String(); got != s {
			t.Errorf("ParseSpecifier(%q).String() = %q; want %q", s, got, s)
		}
	}
}

func TestMatchEqualOperators(t *testing.T) {
	cases := []struct {
		spec    string
		version string
		want    bool
	}{
		{"==1.0", "1.0", true},
		{"==1.0", "1.0.0", true},
		{"==1.0", "1.1", false},
		{"!=1.0", "1.0", false},
		{"!=1.0", "1.1", true},
		{">=1.0", "1.0", true},
		{">=1.0", "0.9", false},
		{">=1.0", "1.0.dev0", false},
		{"<2.0", "1.0", true},
		{"<2.0", "2.0", false},
		{"<2.0", "2.0a1", true},
		{"<=1.0", "1.0", true},
		{"<=1.0", "1.0.post0", false},
		{">1.0", "1.0", false},
		{">1.0", "1.1", true},
	}
	for _, tc := range cases {
		spec, err := ParseSpecifier(tc.spec)
		if err != nil {
			t.Fatalf("ParseSpecifier(%q): %v", tc.spec, err)
		}
		v, err := Parse(tc.version)
		if err != nil {
			t.Fatalf("Parse(%q): %v", tc.version, err)
		}
		if got := spec.Match(v); got != tc.want {
			t.Errorf("%q matches %q = %v; want %v", tc.spec, tc.version, got, tc.want)
		}
	}
}

func TestMatchCompatible(t *testing.T) {
	// ~=2.2 should match 2.2, 2.2.99, 2.99 but reject 2.1.9 and 3.0.
	cases := []struct {
		spec    string
		version string
		want    bool
	}{
		{"~=2.2", "2.2", true},
		{"~=2.2", "2.2.0", true},
		{"~=2.2", "2.2.99", true},
		{"~=2.2", "2.99", true},
		{"~=2.2", "2.1", false},
		{"~=2.2", "2.1.9", false},
		{"~=2.2", "3.0", false},
		{"~=2.2.0", "2.2.0", true},
		{"~=2.2.0", "2.2.99", true},
		{"~=2.2.0", "2.3", false},
		// ~=2.2.0 == ">=2.2.0, ==2.2.*". "2.2" equals "2.2.0" under
		// PEP 440 zero-padding, so it matches both clauses.
		{"~=2.2.0", "2.2", true},
	}
	for _, tc := range cases {
		spec, err := ParseSpecifier(tc.spec)
		if err != nil {
			t.Fatalf("ParseSpecifier(%q): %v", tc.spec, err)
		}
		v, err := Parse(tc.version)
		if err != nil {
			t.Fatalf("Parse(%q): %v", tc.version, err)
		}
		if got := spec.Match(v); got != tc.want {
			t.Errorf("%q matches %q = %v; want %v", tc.spec, tc.version, got, tc.want)
		}
	}
}

func TestMatchCompatibleRequiresTwoComponents(t *testing.T) {
	// ~=1 is invalid because there is no n-1 prefix to lock.
	spec, err := ParseSpecifier("~=1")
	if err != nil {
		t.Fatalf("ParseSpecifier: %v", err)
	}
	v, _ := Parse("1.0")
	if got := spec.Match(v); got != false {
		t.Errorf("~=1 vs 1.0 = %v; want false (no n-1 prefix)", got)
	}
}

func TestMatchArbitrary(t *testing.T) {
	// === is strict byte-equal against String().
	spec, err := ParseSpecifier("===1.0+local")
	if err != nil {
		t.Fatalf("ParseSpecifier: %v", err)
	}
	v1, _ := Parse("1.0+local")
	if !spec.Match(v1) {
		t.Errorf("=== 1.0+local should match itself")
	}
	v2, _ := Parse("1.0")
	if spec.Match(v2) {
		t.Errorf("=== 1.0+local should not match 1.0")
	}
}

func TestMatchMultipleClauses(t *testing.T) {
	spec, _ := ParseSpecifier(">=1.0, <2.0, !=1.5")
	cases := map[string]bool{
		"0.9":  false,
		"1.0":  true,
		"1.4":  true,
		"1.5":  false,
		"1.99": true,
		"2.0":  false,
		"2.5":  false,
	}
	for s, want := range cases {
		v, _ := Parse(s)
		if got := spec.Match(v); got != want {
			t.Errorf("%v matches %q = %v; want %v", spec, s, got, want)
		}
	}
}

func TestOperatorString(t *testing.T) {
	cases := map[Operator]string{
		OpEq:        "==",
		OpNeq:       "!=",
		OpLt:        "<",
		OpLte:       "<=",
		OpGt:        ">",
		OpGte:       ">=",
		OpCompat:    "~=",
		OpArbitrary: "===",
		OpInvalid:   "?",
	}
	for op, want := range cases {
		if got := op.String(); got != want {
			t.Errorf("%d.String() = %q; want %q", int(op), got, want)
		}
	}
}
