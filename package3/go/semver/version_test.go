package semver

import (
	"testing"
)

func TestParseBasic(t *testing.T) {
	v, err := Parse("v1.2.3")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if v.Major != 1 || v.Minor != 2 || v.Patch != 3 {
		t.Errorf("Parse(v1.2.3) = %+v", v)
	}
	if v.IsPrerelease() {
		t.Errorf("v1.2.3 reported as prerelease")
	}
	if v.MajorString() != "v1" {
		t.Errorf("MajorString = %q; want v1", v.MajorString())
	}
}

func TestParsePrerelease(t *testing.T) {
	v, err := Parse("v1.2.3-rc.1")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if !v.IsPrerelease() {
		t.Errorf("v1.2.3-rc.1 not flagged as prerelease")
	}
	if len(v.Pre) != 2 || v.Pre[0] != "rc" || v.Pre[1] != "1" {
		t.Errorf("Pre = %v; want [rc 1]", v.Pre)
	}
}

func TestParseIncompatibleBuildMetadata(t *testing.T) {
	v, err := Parse("v2.0.0+incompatible")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if len(v.Build) != 1 || v.Build[0] != "incompatible" {
		t.Errorf("Build = %v; want [incompatible]", v.Build)
	}
}

func TestParsePseudoVersion(t *testing.T) {
	v, err := Parse("v0.0.0-20210101000000-abcdef123456")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if !v.IsPseudoVersion() {
		t.Errorf("pseudo-version not detected")
	}
}

func TestParseRejects(t *testing.T) {
	cases := []string{
		"",
		"1.2.3",            // missing v
		"v1",               // too few components
		"v1.2",             // too few components
		"v1.2.3.4",         // too many components
		"v01.2.3",          // leading zero
		"v1.02.3",          // leading zero in minor
		"v1.2.03",          // leading zero in patch
		"v1.2.-3",          // negative-looking patch
		"v1.2.3-",          // empty pre
		"v1.2.3+",          // empty build
		"v1.2.3-rc..1",     // empty pre id
		"v1.2.3-rc.01",     // numeric pre with leading zero
		"v1.2.3-bad!ident", // forbidden char
	}
	for _, tc := range cases {
		if _, err := Parse(tc); err == nil {
			t.Errorf("Parse(%q) succeeded; want error", tc)
		}
	}
}

func TestCompareOrdering(t *testing.T) {
	cases := []struct {
		a, b string
		want int
	}{
		{"v1.0.0", "v1.0.0", 0},
		{"v1.0.0", "v1.0.1", -1},
		{"v1.1.0", "v1.0.9", +1},
		{"v2.0.0", "v1.99.99", +1},
		{"v1.0.0-alpha", "v1.0.0", -1},
		{"v1.0.0-alpha", "v1.0.0-beta", -1},
		{"v1.0.0-alpha.1", "v1.0.0-alpha.2", -1},
		{"v1.0.0-alpha.2", "v1.0.0-alpha.10", -1}, // numeric id sort
		{"v1.0.0-alpha", "v1.0.0-alpha.1", -1},    // longer chain wins
		{"v1.0.0-rc.1", "v1.0.0-beta", +1},        // alpha order rc > beta
	}
	for _, tc := range cases {
		va, _ := Parse(tc.a)
		vb, _ := Parse(tc.b)
		got := Compare(va, vb)
		if got != tc.want {
			t.Errorf("Compare(%s, %s) = %d; want %d", tc.a, tc.b, got, tc.want)
		}
	}
}

func TestCompareIgnoresBuild(t *testing.T) {
	a, _ := Parse("v2.0.0+incompatible")
	b, _ := Parse("v2.0.0")
	if Compare(a, b) != 0 {
		t.Errorf("Compare ignored build metadata wrong: got %d", Compare(a, b))
	}
}

func TestSortStable(t *testing.T) {
	in := []string{"v1.2.3", "v0.9.0", "v1.0.0-rc.1", "v1.0.0", "v2.0.0+incompatible"}
	Sort(in)
	want := []string{"v0.9.0", "v1.0.0-rc.1", "v1.0.0", "v1.2.3", "v2.0.0+incompatible"}
	for i := range want {
		if in[i] != want[i] {
			t.Errorf("Sort[%d] = %q; want %q (full=%v)", i, in[i], want[i], in)
			break
		}
	}
}

func TestSortInvalidValuesGoLast(t *testing.T) {
	in := []string{"not-semver", "v1.0.0", "junk"}
	Sort(in)
	if in[0] != "v1.0.0" {
		t.Errorf("Sort placed invalid first: %v", in)
	}
}

func TestMax(t *testing.T) {
	if Max(nil) != "" {
		t.Errorf("Max(nil) = %q; want \"\"", Max(nil))
	}
	got := Max([]string{"v1.2.3", "v0.9.0", "v1.10.0", "v1.2.4-rc.1"})
	if got != "v1.10.0" {
		t.Errorf("Max = %q; want v1.10.0", got)
	}
}

func TestIsValid(t *testing.T) {
	if !IsValid("v1.0.0") {
		t.Errorf("IsValid(v1.0.0) = false")
	}
	if IsValid("not-a-version") {
		t.Errorf("IsValid(not-a-version) = true")
	}
}

func TestStringRoundTrip(t *testing.T) {
	cases := []string{
		"v1.2.3",
		"v1.2.3-rc.1",
		"v0.0.0-20210101000000-abcdef123456",
		"v2.0.0+incompatible",
		"v1.2.3-rc.1+meta.2",
	}
	for _, tc := range cases {
		v, err := Parse(tc)
		if err != nil {
			t.Fatalf("Parse(%q): %v", tc, err)
		}
		if v.String() != tc {
			t.Errorf("String round-trip on %q produced %q", tc, v.String())
		}
	}
}

func TestCompareStringsHandlesInvalid(t *testing.T) {
	if CompareStrings("v1.0.0", "junk") >= 0 {
		t.Errorf("CompareStrings(v1.0.0, junk) >= 0; want -1 (valid < invalid)")
	}
	if CompareStrings("junk", "v1.0.0") <= 0 {
		t.Errorf("CompareStrings(junk, v1.0.0) <= 0; want +1")
	}
	if CompareStrings("alpha", "beta") != -1 {
		t.Errorf("CompareStrings(alpha, beta) lex order wrong")
	}
}

func TestIsPseudoVersionNegative(t *testing.T) {
	cases := []string{
		"v1.0.0",
		"v0.0.0-rc.1",
		"v0.0.0-20210101000000",                   // missing sha
		"v0.0.0-20210101000000-abcdef",            // sha too short
		"v0.0.0-20210101000000-abcdef1234567",     // sha too long
		"v0.0.0-2021010100000-abcdef123456",       // ts too short
		"v0.0.0-202101010000000-abcdef123456",     // ts too long
		"v0.0.0-20210101000000-ABCDEF123456",      // sha not lowercase hex
	}
	for _, tc := range cases {
		v, err := Parse(tc)
		if err != nil {
			continue
		}
		if v.IsPseudoVersion() {
			t.Errorf("IsPseudoVersion(%q) = true; want false", tc)
		}
	}
}
