package semver_test

import (
	"testing"

	"mochi/package3/dotnet/semver"
)

func TestParseReq_valid(t *testing.T) {
	cases := []struct {
		input string
	}{
		{"1.0.0"},
		{"[1.0.0]"},
		{"[1.0.0, 2.0.0)"},
		{"[1.0.0, 2.0.0]"},
		{"(1.0.0, 2.0.0)"},
		{"(1.0.0,)"},
		{"(, 2.0.0)"},
		{"*"},
		{"1.2.3-beta"},
		{"[1.0.0-alpha, 2.0.0)"},
	}
	for _, tc := range cases {
		if _, err := semver.ParseReq(tc.input); err != nil {
			t.Errorf("ParseReq(%q) unexpected error: %v", tc.input, err)
		}
	}
}

func TestParseReq_invalid(t *testing.T) {
	cases := []string{
		"",
		"[",
		"[1.0.0",
		"1.0.0]",
		"[a.b.c]",
	}
	for _, tc := range cases {
		if _, err := semver.ParseReq(tc); err == nil {
			t.Errorf("ParseReq(%q) expected error, got nil", tc)
		}
	}
}

func TestReq_Satisfies_bare(t *testing.T) {
	req := semver.MustParseReq("1.0.0")
	cases := []struct {
		ver  string
		want bool
	}{
		{"1.0.0", true},
		{"1.5.0", true},
		{"2.0.0", true},
		{"0.9.0", false},
		{"0.9.9", false},
	}
	for _, tc := range cases {
		v := semver.MustParse(tc.ver)
		if got := req.Satisfies(v); got != tc.want {
			t.Errorf("(%q).Satisfies(%q) = %v; want %v", req, tc.ver, got, tc.want)
		}
	}
}

func TestReq_Satisfies_exact(t *testing.T) {
	req := semver.MustParseReq("[1.2.3]")
	cases := []struct {
		ver  string
		want bool
	}{
		{"1.2.3", true},
		{"1.2.4", false},
		{"1.2.2", false},
		{"2.0.0", false},
	}
	for _, tc := range cases {
		v := semver.MustParse(tc.ver)
		if got := req.Satisfies(v); got != tc.want {
			t.Errorf("(%q).Satisfies(%q) = %v; want %v", req, tc.ver, got, tc.want)
		}
	}
}

func TestReq_Satisfies_minIncMaxExc(t *testing.T) {
	req := semver.MustParseReq("[1.0.0, 2.0.0)")
	cases := []struct {
		ver  string
		want bool
	}{
		{"1.0.0", true},
		{"1.5.0", true},
		{"1.9.9", true},
		{"2.0.0", false},
		{"0.9.0", false},
		{"2.0.1", false},
	}
	for _, tc := range cases {
		v := semver.MustParse(tc.ver)
		if got := req.Satisfies(v); got != tc.want {
			t.Errorf("(%q).Satisfies(%q) = %v; want %v", req, tc.ver, got, tc.want)
		}
	}
}

func TestReq_Satisfies_bothInclusive(t *testing.T) {
	req := semver.MustParseReq("[1.0.0, 2.0.0]")
	if !req.Satisfies(semver.MustParse("2.0.0")) {
		t.Error("[1.0.0, 2.0.0] should include 2.0.0")
	}
	if req.Satisfies(semver.MustParse("2.0.1")) {
		t.Error("[1.0.0, 2.0.0] should not include 2.0.1")
	}
}

func TestReq_Satisfies_exclusiveMin(t *testing.T) {
	req := semver.MustParseReq("(1.0.0,)")
	if req.Satisfies(semver.MustParse("1.0.0")) {
		t.Error("(1.0.0,) must not include 1.0.0")
	}
	if !req.Satisfies(semver.MustParse("1.0.1")) {
		t.Error("(1.0.0,) must include 1.0.1")
	}
}

func TestReq_Satisfies_noLowerBound(t *testing.T) {
	req := semver.MustParseReq("(, 2.0.0)")
	if !req.Satisfies(semver.MustParse("1.9.9")) {
		t.Error("(, 2.0.0) must include 1.9.9")
	}
	if req.Satisfies(semver.MustParse("2.0.0")) {
		t.Error("(, 2.0.0) must not include 2.0.0")
	}
}

func TestReq_Satisfies_any(t *testing.T) {
	req := semver.MustParseReq("*")
	for _, ver := range []string{"0.0.0", "1.0.0", "99.0.0", "1.0.0-alpha"} {
		if !req.Satisfies(semver.MustParse(ver)) {
			t.Errorf("* must satisfy %q", ver)
		}
	}
}

func TestReq_String(t *testing.T) {
	cases := []string{"1.0.0", "[1.0.0]", "[1.0.0, 2.0.0)", "*"}
	for _, tc := range cases {
		r := semver.MustParseReq(tc)
		if r.String() != tc {
			t.Errorf("Req.String() = %q; want %q", r.String(), tc)
		}
	}
}

func TestMaxSatisfying(t *testing.T) {
	versions := []semver.Version{
		semver.MustParse("1.0.0"),
		semver.MustParse("1.5.0"),
		semver.MustParse("2.0.0"),
		semver.MustParse("2.1.0"),
	}
	req := semver.MustParseReq("[1.0.0, 2.0.0)")
	got, ok := semver.MaxSatisfying(req, versions)
	if !ok {
		t.Fatal("MaxSatisfying returned false")
	}
	if got.String() != "1.5.0" {
		t.Errorf("MaxSatisfying = %q; want %q", got, "1.5.0")
	}
}

func TestMaxSatisfying_noneMatch(t *testing.T) {
	versions := []semver.Version{semver.MustParse("3.0.0")}
	req := semver.MustParseReq("[1.0.0, 2.0.0)")
	_, ok := semver.MaxSatisfying(req, versions)
	if ok {
		t.Error("MaxSatisfying should return false when no versions match")
	}
}
