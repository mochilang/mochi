package semver_test

import (
	"testing"

	"mochi/package3/dotnet/semver"
)

func TestParse_valid(t *testing.T) {
	cases := []struct {
		input string
		want  semver.Version
	}{
		{"1.0.0", semver.Version{Major: 1}},
		{"1.2.3", semver.Version{Major: 1, Minor: 2, Patch: 3}},
		{"1.2.3.4", semver.Version{Major: 1, Minor: 2, Patch: 3, Revision: 4}},
		{"0.0.0", semver.Version{}},
		{"10.20.30", semver.Version{Major: 10, Minor: 20, Patch: 30}},
		{"1.2.3-alpha", semver.Version{Major: 1, Minor: 2, Patch: 3, PreRelease: "alpha"}},
		{"1.2.3-alpha.1", semver.Version{Major: 1, Minor: 2, Patch: 3, PreRelease: "alpha.1"}},
		{"1.2.3-beta.2", semver.Version{Major: 1, Minor: 2, Patch: 3, PreRelease: "beta.2"}},
		{"1.2.3-rc.1", semver.Version{Major: 1, Minor: 2, Patch: 3, PreRelease: "rc.1"}},
		{"2.0.0-preview.3", semver.Version{Major: 2, PreRelease: "preview.3"}},
		{"1.2.3.4-alpha", semver.Version{Major: 1, Minor: 2, Patch: 3, Revision: 4, PreRelease: "alpha"}},
		{"6.0.0", semver.Version{Major: 6}},
		{"3.14.159", semver.Version{Major: 3, Minor: 14, Patch: 159}},
	}
	for _, tc := range cases {
		got, err := semver.Parse(tc.input)
		if err != nil {
			t.Errorf("Parse(%q) error: %v", tc.input, err)
			continue
		}
		if got != tc.want {
			t.Errorf("Parse(%q) = %+v; want %+v", tc.input, got, tc.want)
		}
	}
}

func TestParse_invalid(t *testing.T) {
	cases := []string{
		"",
		"1",
		"1.2",
		"1.2.3.4.5",
		"a.b.c",
		"1.2.x",
		"-1.0.0",
	}
	for _, tc := range cases {
		if _, err := semver.Parse(tc); err == nil {
			t.Errorf("Parse(%q) expected error, got nil", tc)
		}
	}
}

func TestVersion_String(t *testing.T) {
	cases := []struct {
		v    semver.Version
		want string
	}{
		{semver.Version{Major: 1, Minor: 2, Patch: 3}, "1.2.3"},
		{semver.Version{Major: 1, Minor: 2, Patch: 3, Revision: 4}, "1.2.3.4"},
		{semver.Version{Major: 1, Minor: 2, Patch: 3, PreRelease: "alpha"}, "1.2.3-alpha"},
		{semver.Version{Major: 1, Minor: 2, Patch: 3, Revision: 4, PreRelease: "beta.1"}, "1.2.3.4-beta.1"},
		{semver.Version{}, "0.0.0"},
	}
	for _, tc := range cases {
		if got := tc.v.String(); got != tc.want {
			t.Errorf("Version.String() = %q; want %q", got, tc.want)
		}
	}
}

func TestVersion_Compare(t *testing.T) {
	cases := []struct {
		a, b string
		want int
	}{
		{"1.0.0", "1.0.0", 0},
		{"1.0.0", "2.0.0", -1},
		{"2.0.0", "1.0.0", 1},
		{"1.1.0", "1.2.0", -1},
		{"1.0.1", "1.0.2", -1},
		{"1.0.0.1", "1.0.0.2", -1},
		{"1.0.0.2", "1.0.0.1", 1},
		// Stable > pre-release
		{"1.0.0", "1.0.0-alpha", 1},
		{"1.0.0-alpha", "1.0.0", -1},
		// Pre-release ordering
		{"1.0.0-alpha", "1.0.0-beta", -1},
		{"1.0.0-beta", "1.0.0-alpha", 1},
		{"1.0.0-alpha.1", "1.0.0-alpha.2", -1},
		// Numeric vs string identifiers
		{"1.0.0-1", "1.0.0-alpha", -1},
	}
	for _, tc := range cases {
		a := semver.MustParse(tc.a)
		b := semver.MustParse(tc.b)
		got := a.Compare(b)
		if signum(got) != signum(tc.want) {
			t.Errorf("(%q).Compare(%q) = %d; want signum %d", tc.a, tc.b, got, tc.want)
		}
	}
}

func TestVersion_IsPreRelease(t *testing.T) {
	if semver.MustParse("1.0.0").IsPreRelease() {
		t.Error("1.0.0 should not be pre-release")
	}
	if !semver.MustParse("1.0.0-alpha").IsPreRelease() {
		t.Error("1.0.0-alpha should be pre-release")
	}
}

func TestMustParse_panic(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("MustParse of invalid string should panic")
		}
	}()
	semver.MustParse("not-a-version")
}

func signum(n int) int {
	switch {
	case n < 0:
		return -1
	case n > 0:
		return 1
	default:
		return 0
	}
}
