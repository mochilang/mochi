package semver

import "testing"

func TestParseValid(t *testing.T) {
	cases := []struct {
		in   string
		want Version
	}{
		{"0.0.0", Version{0, 0, 0, "", ""}},
		{"1.2.3", Version{1, 2, 3, "", ""}},
		{"10.20.30", Version{10, 20, 30, "", ""}},
		{"1.0.0-alpha", Version{1, 0, 0, "alpha", ""}},
		{"1.0.0-alpha.1", Version{1, 0, 0, "alpha.1", ""}},
		{"1.0.0-0.3.7", Version{1, 0, 0, "0.3.7", ""}},
		{"1.0.0-x.7.z.92", Version{1, 0, 0, "x.7.z.92", ""}},
		{"1.0.0+20130313144700", Version{1, 0, 0, "", "20130313144700"}},
		{"1.0.0-beta+exp.sha.5114f85", Version{1, 0, 0, "beta", "exp.sha.5114f85"}},
		{"1.0.0+21AF26D3----117B344092BD", Version{1, 0, 0, "", "21AF26D3----117B344092BD"}},
	}
	for _, tc := range cases {
		got, err := Parse(tc.in)
		if err != nil {
			t.Errorf("Parse(%q) err = %v; want nil", tc.in, err)
			continue
		}
		if got != tc.want {
			t.Errorf("Parse(%q) = %+v; want %+v", tc.in, got, tc.want)
		}
	}
}

func TestParseInvalid(t *testing.T) {
	cases := []string{
		"",
		"1",
		"1.2",
		"1.2.3.4",
		"01.0.0",
		"1.01.0",
		"1.0.01",
		"a.b.c",
		"1.0.0-",
		"1.0.0+",
		"1.0.0-+",
		"1.0.0-alpha..1",
		"1.0.0-alpha_1",
	}
	for _, s := range cases {
		if _, err := Parse(s); err == nil {
			t.Errorf("Parse(%q) err = nil; want error", s)
		}
	}
}

func TestParseRelaxed(t *testing.T) {
	cases := []struct {
		in   string
		want Version
	}{
		{"1", Version{1, 0, 0, "", ""}},
		{"1.2", Version{1, 2, 0, "", ""}},
		{"1.2.3", Version{1, 2, 3, "", ""}},
		{"0", Version{0, 0, 0, "", ""}},
	}
	for _, tc := range cases {
		got, err := ParseRelaxed(tc.in)
		if err != nil {
			t.Errorf("ParseRelaxed(%q) err = %v", tc.in, err)
			continue
		}
		if got != tc.want {
			t.Errorf("ParseRelaxed(%q) = %+v; want %+v", tc.in, got, tc.want)
		}
	}
}

func TestMustParse(t *testing.T) {
	v := MustParse("1.2.3")
	if v.String() != "1.2.3" {
		t.Errorf("MustParse(1.2.3).String() = %q; want 1.2.3", v.String())
	}
	defer func() {
		if r := recover(); r == nil {
			t.Errorf("MustParse(bad) did not panic")
		}
	}()
	_ = MustParse("not-a-version")
}

func TestVersionString(t *testing.T) {
	v := Version{1, 2, 3, "alpha.1", "build.5"}
	if got := v.String(); got != "1.2.3-alpha.1+build.5" {
		t.Errorf("Version.String() = %q; want 1.2.3-alpha.1+build.5", got)
	}
}

func TestVersionCompareMajorMinorPatch(t *testing.T) {
	cases := []struct {
		a, b string
		want int
	}{
		{"1.0.0", "2.0.0", -1},
		{"2.0.0", "2.1.0", -1},
		{"2.1.0", "2.1.1", -1},
		{"1.0.0", "1.0.0", 0},
		{"2.0.0", "1.0.0", 1},
	}
	for _, tc := range cases {
		got := MustParse(tc.a).Compare(MustParse(tc.b))
		if got != tc.want {
			t.Errorf("%s.Compare(%s) = %d; want %d", tc.a, tc.b, got, tc.want)
		}
	}
}

func TestVersionComparePrerelease(t *testing.T) {
	// semver.org §11
	cases := []struct {
		a, b string
		want int
	}{
		{"1.0.0-alpha", "1.0.0", -1},
		{"1.0.0", "1.0.0-alpha", 1},
		{"1.0.0-alpha", "1.0.0-alpha.1", -1},
		{"1.0.0-alpha.1", "1.0.0-alpha.beta", -1},
		{"1.0.0-alpha.beta", "1.0.0-beta", -1},
		{"1.0.0-beta", "1.0.0-beta.2", -1},
		{"1.0.0-beta.2", "1.0.0-beta.11", -1},
		{"1.0.0-beta.11", "1.0.0-rc.1", -1},
		{"1.0.0-rc.1", "1.0.0", -1},
	}
	for _, tc := range cases {
		got := MustParse(tc.a).Compare(MustParse(tc.b))
		if got != tc.want {
			t.Errorf("%s.Compare(%s) = %d; want %d", tc.a, tc.b, got, tc.want)
		}
	}
}

func TestVersionEqualIgnoresBuild(t *testing.T) {
	a := MustParse("1.2.3+build.1")
	b := MustParse("1.2.3+build.2")
	if !a.Equal(b) {
		t.Errorf("Equal should ignore build metadata: %v vs %v", a, b)
	}
}

func TestVersionEqualDistinguishesPrerelease(t *testing.T) {
	a := MustParse("1.2.3")
	b := MustParse("1.2.3-alpha")
	if a.Equal(b) {
		t.Errorf("Equal should distinguish pre-release: %v vs %v", a, b)
	}
}

func TestIsPrerelease(t *testing.T) {
	if MustParse("1.0.0").IsPrerelease() {
		t.Errorf("1.0.0 IsPrerelease should be false")
	}
	if !MustParse("1.0.0-alpha").IsPrerelease() {
		t.Errorf("1.0.0-alpha IsPrerelease should be true")
	}
}
