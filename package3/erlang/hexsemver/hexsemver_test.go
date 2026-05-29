package hexsemver

import (
	"testing"
)

func mustParseVersion(t *testing.T, s string) Version {
	t.Helper()
	v, err := ParseVersion(s)
	if err != nil {
		t.Fatalf("ParseVersion(%q): %v", s, err)
	}
	return v
}

func mustParseConstraint(t *testing.T, s string) Constraint {
	t.Helper()
	c, err := ParseConstraint(s)
	if err != nil {
		t.Fatalf("ParseConstraint(%q): %v", s, err)
	}
	return c
}

func TestParseVersion_Full(t *testing.T) {
	v := mustParseVersion(t, "2.12.0")
	if v.Major != 2 || v.Minor != 12 || v.Patch != 0 || v.Pre != "" {
		t.Errorf("unexpected version: %+v", v)
	}
}

func TestParseVersion_PreRelease(t *testing.T) {
	v := mustParseVersion(t, "3.0.0-rc.2")
	if v.Major != 3 || v.Minor != 0 || v.Patch != 0 || v.Pre != "rc.2" {
		t.Errorf("unexpected version: %+v", v)
	}
}

func TestParseVersion_TwoPart(t *testing.T) {
	v := mustParseVersion(t, "2.1")
	if v.Major != 2 || v.Minor != 1 || v.Patch != 0 {
		t.Errorf("two-part parse: %+v", v)
	}
}

func TestParseVersion_Invalid(t *testing.T) {
	cases := []string{"", "1", "a.b.c", "1.2.3.4"}
	for _, s := range cases {
		if _, err := ParseVersion(s); err == nil {
			t.Errorf("ParseVersion(%q) should fail", s)
		}
	}
}

func TestVersionString(t *testing.T) {
	cases := []struct {
		in   string
		want string
	}{
		{"2.12.0", "2.12.0"},
		{"3.0.0-rc.2", "3.0.0-rc.2"},
	}
	for _, c := range cases {
		v := mustParseVersion(t, c.in)
		if got := v.String(); got != c.want {
			t.Errorf("Version(%q).String() = %q, want %q", c.in, got, c.want)
		}
	}
}

func TestVersionCompare(t *testing.T) {
	cases := []struct {
		a, b string
		want int
	}{
		{"1.0.0", "2.0.0", -1},
		{"2.0.0", "1.0.0", 1},
		{"2.1.0", "2.1.0", 0},
		{"2.1.1", "2.1.0", 1},
		{"2.1.0-rc.1", "2.1.0", -1}, // pre < release
		{"2.1.0", "2.1.0-rc.1", 1},
		{"2.1.0-rc.1", "2.1.0-rc.2", -1},
	}
	for _, c := range cases {
		a := mustParseVersion(t, c.a)
		b := mustParseVersion(t, c.b)
		got := a.Compare(b)
		if got != c.want {
			t.Errorf("Compare(%q, %q) = %d, want %d", c.a, c.b, got, c.want)
		}
	}
}

func TestParseConstraint_AllOps(t *testing.T) {
	ops := []string{"~>", ">=", "<=", ">", "<", "=="}
	for _, op := range ops {
		s := op + " 2.1.0"
		c := mustParseConstraint(t, s)
		if c.op != op {
			t.Errorf("op mismatch: got %q, want %q", c.op, op)
		}
	}
}

func TestParseConstraint_BareVersion(t *testing.T) {
	c := mustParseConstraint(t, "2.1.0")
	if c.op != "==" {
		t.Errorf("bare version should be == , got %q", c.op)
	}
}

func TestConstraint_TildeGreater_WithPatch(t *testing.T) {
	// ~> 2.1.3 means >= 2.1.3 and < 2.2.0
	c := mustParseConstraint(t, "~> 2.1.3")
	cases := []struct {
		v    string
		want bool
	}{
		{"2.1.3", true},
		{"2.1.9", true},
		{"2.2.0", false},
		{"2.0.9", false},
		{"3.0.0", false},
		{"2.1.2", false},
	}
	for _, tc := range cases {
		v := mustParseVersion(t, tc.v)
		if got := c.Matches(v); got != tc.want {
			t.Errorf("~> 2.1.3 matches(%q) = %v, want %v", tc.v, got, tc.want)
		}
	}
}

func TestConstraint_TildeGreater_NoPatch(t *testing.T) {
	// ~> 2.1 means >= 2.1.0 and < 3.0.0
	c := mustParseConstraint(t, "~> 2.1")
	cases := []struct {
		v    string
		want bool
	}{
		{"2.1.0", true},
		{"2.9.9", true},
		{"3.0.0", false},
		{"2.0.9", false},
		{"1.9.9", false},
	}
	for _, tc := range cases {
		v := mustParseVersion(t, tc.v)
		if got := c.Matches(v); got != tc.want {
			t.Errorf("~> 2.1 matches(%q) = %v, want %v", tc.v, got, tc.want)
		}
	}
}

func TestConstraint_ExactMatch(t *testing.T) {
	c := mustParseConstraint(t, "== 2.1.0")
	if !c.Matches(mustParseVersion(t, "2.1.0")) {
		t.Error("== 2.1.0 should match 2.1.0")
	}
	if c.Matches(mustParseVersion(t, "2.1.1")) {
		t.Error("== 2.1.0 should not match 2.1.1")
	}
}

func TestConstraint_Range(t *testing.T) {
	ge := mustParseConstraint(t, ">= 2.0.0")
	lt := mustParseConstraint(t, "< 3.0.0")
	cases := []struct {
		v    string
		geWant bool
		ltWant bool
	}{
		{"1.9.9", false, true},
		{"2.0.0", true, true},
		{"2.5.3", true, true},
		{"3.0.0", true, false},
		{"3.0.1", true, false},
	}
	for _, tc := range cases {
		v := mustParseVersion(t, tc.v)
		if got := ge.Matches(v); got != tc.geWant {
			t.Errorf(">= 2.0.0 matches(%q) = %v, want %v", tc.v, got, tc.geWant)
		}
		if got := lt.Matches(v); got != tc.ltWant {
			t.Errorf("< 3.0.0 matches(%q) = %v, want %v", tc.v, got, tc.ltWant)
		}
	}
}

func TestConstraint_PreRelease_Filtering(t *testing.T) {
	// Hex.pm: range constraints only include pre-releases when the constraint
	// itself has a pre-release tag. Plain >= 2.0.0 hides all pre-releases.
	c := mustParseConstraint(t, ">= 2.0.0")
	for _, s := range []string{"2.0.0-rc.1", "2.1.0-rc.1", "3.0.0-alpha.1"} {
		v := mustParseVersion(t, s)
		if c.Matches(v) {
			t.Errorf(">= 2.0.0 should NOT match pre-release %q", s)
		}
	}
	// A constraint that itself has a pre-release does pass through.
	cPre := mustParseConstraint(t, ">= 2.0.0-rc.1")
	if !cPre.Matches(mustParseVersion(t, "2.0.0-rc.2")) {
		t.Error(">= 2.0.0-rc.1 should match 2.0.0-rc.2")
	}
	// Exact match is always allowed.
	cExact := mustParseConstraint(t, "== 2.0.0-rc.1")
	if !cExact.Matches(mustParseVersion(t, "2.0.0-rc.1")) {
		t.Error("== 2.0.0-rc.1 should match 2.0.0-rc.1")
	}
}

func TestRequirements_Satisfies(t *testing.T) {
	reqs, err := ParseRequirements(">= 2.0.0, < 3.0.0")
	if err != nil {
		t.Fatalf("ParseRequirements: %v", err)
	}
	if !reqs.Satisfies(mustParseVersion(t, "2.5.0")) {
		t.Error("2.5.0 should satisfy >= 2.0.0, < 3.0.0")
	}
	if reqs.Satisfies(mustParseVersion(t, "3.0.0")) {
		t.Error("3.0.0 should not satisfy >= 2.0.0, < 3.0.0")
	}
	if reqs.Satisfies(mustParseVersion(t, "1.9.9")) {
		t.Error("1.9.9 should not satisfy >= 2.0.0, < 3.0.0")
	}
}

func TestRequirements_BestMatch(t *testing.T) {
	reqs, _ := ParseRequirements("~> 2.1")
	candidates := []Version{
		mustParseVersion(t, "2.0.0"),
		mustParseVersion(t, "2.1.0"),
		mustParseVersion(t, "2.3.5"),
		mustParseVersion(t, "3.0.0"),
	}
	best, ok := reqs.BestMatch(candidates)
	if !ok {
		t.Fatal("BestMatch should find a match")
	}
	if best.String() != "2.3.5" {
		t.Errorf("BestMatch = %q, want 2.3.5", best.String())
	}
}

func TestRequirements_BestMatch_None(t *testing.T) {
	reqs, _ := ParseRequirements("~> 5.0")
	candidates := []Version{
		mustParseVersion(t, "2.0.0"),
		mustParseVersion(t, "3.0.0"),
	}
	_, ok := reqs.BestMatch(candidates)
	if ok {
		t.Error("BestMatch should return false when no candidates satisfy")
	}
}

func TestConstraintString(t *testing.T) {
	c := mustParseConstraint(t, "~> 2.1.3")
	if c.String() != "~> 2.1.3" {
		t.Errorf("Constraint.String() = %q, want %q", c.String(), "~> 2.1.3")
	}
}
