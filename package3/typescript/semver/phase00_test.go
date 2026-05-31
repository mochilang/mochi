package semver_test

import (
	"testing"

	"mochi/package3/typescript/semver"
)

// TestPhase0Semver is the MEP-72 Phase 0 gate for the semver package.
func TestPhase0Semver(t *testing.T) {
	// Parse versions.
	v, err := semver.Parse("1.2.3")
	if err != nil {
		t.Fatalf("Parse 1.2.3: %v", err)
	}
	if v.Major != 1 || v.Minor != 2 || v.Patch != 3 {
		t.Errorf("Parse 1.2.3: got %v", v)
	}

	// Caret range.
	r, err := semver.ParseRange("^1.2.3")
	if err != nil {
		t.Fatalf("ParseRange ^1.2.3: %v", err)
	}
	cases := []struct {
		ver  string
		want bool
	}{
		{"1.2.3", true},
		{"1.9.9", true},
		{"2.0.0", false},
		{"1.2.2", false},
	}
	for _, c := range cases {
		v, err := semver.Parse(c.ver)
		if err != nil {
			t.Fatalf("Parse %s: %v", c.ver, err)
		}
		if got := r.Satisfies(v); got != c.want {
			t.Errorf("^1.2.3 Satisfies %s = %v, want %v", c.ver, got, c.want)
		}
	}

	// Tilde range.
	rt, err := semver.ParseRange("~1.2.3")
	if err != nil {
		t.Fatalf("ParseRange ~1.2.3: %v", err)
	}
	if !rt.Satisfies(v) {
		t.Error("~1.2.3 should satisfy 1.2.3")
	}
	v200, _ := semver.Parse("1.3.0")
	if rt.Satisfies(v200) {
		t.Error("~1.2.3 should not satisfy 1.3.0")
	}

	// MaxSatisfying.
	versions := []string{"1.0.0", "1.2.3", "1.5.0", "2.0.0"}
	best, ok := r.MaxSatisfying(versions)
	if !ok || best != "1.5.0" {
		t.Errorf("MaxSatisfying ^1.2.3 = %q, want 1.5.0", best)
	}

	// OR range.
	ror, err := semver.ParseRange("^1.0.0 || ^2.0.0")
	if err != nil {
		t.Fatalf("ParseRange OR: %v", err)
	}
	v1, _ := semver.Parse("1.9.0")
	v2, _ := semver.Parse("2.5.0")
	v3, _ := semver.Parse("3.0.0")
	if !ror.Satisfies(v1) {
		t.Error("^1||^2 should satisfy 1.9.0")
	}
	if !ror.Satisfies(v2) {
		t.Error("^1||^2 should satisfy 2.5.0")
	}
	if ror.Satisfies(v3) {
		t.Error("^1||^2 should not satisfy 3.0.0")
	}
}
