package semver

import "testing"

func TestParseReqWildcard(t *testing.T) {
	r, err := ParseReq("*")
	if err != nil {
		t.Fatalf("ParseReq(*): %v", err)
	}
	if !r.Matches(MustParse("1.0.0")) {
		t.Errorf("* should match 1.0.0")
	}
	if !r.Matches(MustParse("99.0.0")) {
		t.Errorf("* should match 99.0.0")
	}
	if r.Matches(MustParse("1.0.0-alpha")) {
		t.Errorf("* should not match prerelease")
	}
}

func TestParseReqEmpty(t *testing.T) {
	if _, err := ParseReq(""); err == nil {
		t.Errorf("ParseReq(\"\") should error")
	}
	if _, err := ParseReq("   "); err == nil {
		t.Errorf("ParseReq(spaces) should error")
	}
}

func TestParseReqInvalid(t *testing.T) {
	cases := []string{
		"~",
		"^",
		"=abc",
		">=z",
		"1.*.3", // wildcard not in last position
		"*.1",
	}
	for _, c := range cases {
		if _, err := ParseReq(c); err == nil {
			t.Errorf("ParseReq(%q) should error", c)
		}
	}
}

func TestReqStringFallback(t *testing.T) {
	// Zero Req String() should not panic.
	var r Req
	if got := r.String(); got != "*" {
		t.Errorf("zero Req.String() = %q; want *", got)
	}
}

func TestCaretMajorPositive(t *testing.T) {
	r := MustParseReq("^1.2.3")
	cases := []struct {
		v    string
		want bool
	}{
		{"1.2.3", true},
		{"1.2.4", true},
		{"1.3.0", true},
		{"1.99.99", true},
		{"2.0.0", false},
		{"1.2.2", false},
		{"0.99.0", false},
	}
	for _, tc := range cases {
		if got := r.Matches(MustParse(tc.v)); got != tc.want {
			t.Errorf("^1.2.3 Matches(%s) = %v; want %v", tc.v, got, tc.want)
		}
	}
}

func TestCaretZeroMinor(t *testing.T) {
	r := MustParseReq("^0.2.3")
	cases := []struct {
		v    string
		want bool
	}{
		{"0.2.3", true},
		{"0.2.99", true},
		{"0.3.0", false},
		{"0.2.2", false},
		{"1.0.0", false},
	}
	for _, tc := range cases {
		if got := r.Matches(MustParse(tc.v)); got != tc.want {
			t.Errorf("^0.2.3 Matches(%s) = %v; want %v", tc.v, got, tc.want)
		}
	}
}

func TestCaretZeroZero(t *testing.T) {
	r := MustParseReq("^0.0.3")
	cases := []struct {
		v    string
		want bool
	}{
		{"0.0.3", true},
		{"0.0.4", false},
		{"0.1.0", false},
	}
	for _, tc := range cases {
		if got := r.Matches(MustParse(tc.v)); got != tc.want {
			t.Errorf("^0.0.3 Matches(%s) = %v; want %v", tc.v, got, tc.want)
		}
	}
}

func TestCaretShortForms(t *testing.T) {
	cases := []struct {
		req  string
		v    string
		want bool
	}{
		{"^1.2", "1.2.0", true},
		{"^1.2", "1.99.0", true},
		{"^1.2", "2.0.0", false},
		{"^1", "1.99.99", true},
		{"^1", "2.0.0", false},
		{"^0.2", "0.2.99", true},
		{"^0.2", "0.3.0", false},
		{"^0", "0.99.99", true},
		{"^0", "1.0.0", false},
	}
	for _, tc := range cases {
		if got := MustParseReq(tc.req).Matches(MustParse(tc.v)); got != tc.want {
			t.Errorf("%s Matches(%s) = %v; want %v", tc.req, tc.v, got, tc.want)
		}
	}
}

func TestBareIsCaret(t *testing.T) {
	if !MustParseReq("1.2.3").Matches(MustParse("1.99.0")) {
		t.Errorf("bare 1.2.3 should treat as caret and match 1.99.0")
	}
	if MustParseReq("1.2.3").Matches(MustParse("2.0.0")) {
		t.Errorf("bare 1.2.3 should not match 2.0.0")
	}
}

func TestTilde(t *testing.T) {
	cases := []struct {
		req  string
		v    string
		want bool
	}{
		{"~1.2.3", "1.2.3", true},
		{"~1.2.3", "1.2.99", true},
		{"~1.2.3", "1.3.0", false},
		{"~1.2", "1.2.99", true},
		{"~1.2", "1.3.0", false},
		{"~1", "1.99.99", true},
		{"~1", "2.0.0", false},
	}
	for _, tc := range cases {
		if got := MustParseReq(tc.req).Matches(MustParse(tc.v)); got != tc.want {
			t.Errorf("%s Matches(%s) = %v; want %v", tc.req, tc.v, got, tc.want)
		}
	}
}

func TestExact(t *testing.T) {
	r := MustParseReq("=1.2.3")
	if !r.Matches(MustParse("1.2.3")) {
		t.Errorf("=1.2.3 should match 1.2.3")
	}
	if r.Matches(MustParse("1.2.4")) {
		t.Errorf("=1.2.3 should not match 1.2.4")
	}
}

func TestInequality(t *testing.T) {
	cases := []struct {
		req  string
		v    string
		want bool
	}{
		{">=1.2.3", "1.2.3", true},
		{">=1.2.3", "1.2.4", true},
		{">=1.2.3", "1.2.2", false},
		{">1.2.3", "1.2.3", false},
		{">1.2.3", "1.2.4", true},
		{"<=1.2.3", "1.2.3", true},
		{"<=1.2.3", "1.2.2", true},
		{"<=1.2.3", "1.2.4", false},
		{"<1.2.3", "1.2.3", false},
		{"<1.2.3", "1.2.2", true},
	}
	for _, tc := range cases {
		if got := MustParseReq(tc.req).Matches(MustParse(tc.v)); got != tc.want {
			t.Errorf("%s Matches(%s) = %v; want %v", tc.req, tc.v, got, tc.want)
		}
	}
}

func TestWildcard(t *testing.T) {
	cases := []struct {
		req  string
		v    string
		want bool
	}{
		{"1.2.*", "1.2.0", true},
		{"1.2.*", "1.2.99", true},
		{"1.2.*", "1.3.0", false},
		{"1.*", "1.99.99", true},
		{"1.*", "2.0.0", false},
	}
	for _, tc := range cases {
		if got := MustParseReq(tc.req).Matches(MustParse(tc.v)); got != tc.want {
			t.Errorf("%s Matches(%s) = %v; want %v", tc.req, tc.v, got, tc.want)
		}
	}
}

func TestIntersection(t *testing.T) {
	r := MustParseReq(">=1.2.3, <1.3.0")
	if !r.Matches(MustParse("1.2.3")) {
		t.Errorf(">=1.2.3, <1.3.0 should match 1.2.3")
	}
	if !r.Matches(MustParse("1.2.99")) {
		t.Errorf(">=1.2.3, <1.3.0 should match 1.2.99")
	}
	if r.Matches(MustParse("1.3.0")) {
		t.Errorf(">=1.2.3, <1.3.0 should not match 1.3.0")
	}
	if r.Matches(MustParse("1.2.2")) {
		t.Errorf(">=1.2.3, <1.3.0 should not match 1.2.2")
	}
}

func TestPrereleaseOptIn(t *testing.T) {
	// A pre-release version is matched only when the requirement explicitly
	// mentions the same MAJOR.MINOR.PATCH with a pre-release.
	if MustParseReq("^1.0.0").Matches(MustParse("1.0.0-alpha")) {
		t.Errorf("^1.0.0 should not match 1.0.0-alpha (pre-release opt-in)")
	}
	if !MustParseReq(">=1.0.0-alpha").Matches(MustParse("1.0.0-alpha")) {
		t.Errorf(">=1.0.0-alpha should match 1.0.0-alpha")
	}
	// Pre-release versions still satisfy non-prerelease requirements when
	// the upper bound includes them (cargo specifically excludes this).
	if MustParseReq("^1.0.0").Matches(MustParse("2.0.0-alpha")) {
		t.Errorf("^1.0.0 should not match 2.0.0-alpha")
	}
}

func TestMaxSatisfyingCaretMajor(t *testing.T) {
	versions := []Version{
		MustParse("1.0.0"),
		MustParse("1.2.0"),
		MustParse("1.2.5"),
		MustParse("1.2.99"),
		MustParse("1.3.0"),
		MustParse("2.0.0"),
	}
	best, ok := MaxSatisfying(MustParseReq("^1.2"), versions)
	if !ok {
		t.Fatalf("MaxSatisfying returned ok=false")
	}
	if got := best.String(); got != "1.3.0" {
		t.Errorf("MaxSatisfying(^1.2) = %q; want 1.3.0", got)
	}
}

func TestMaxSatisfyingNone(t *testing.T) {
	versions := []Version{MustParse("1.0.0")}
	if _, ok := MaxSatisfying(MustParseReq("^2"), versions); ok {
		t.Errorf("MaxSatisfying(^2) on [1.0.0] returned ok=true")
	}
}

func TestMaxSatisfyingEmpty(t *testing.T) {
	if _, ok := MaxSatisfying(MustParseReq("^1"), nil); ok {
		t.Errorf("MaxSatisfying on empty slice returned ok=true")
	}
}

func TestReqStringRoundTrip(t *testing.T) {
	cases := []string{"^1.2.3", "~1.2", ">=1.0, <2.0", "=1.2.3", "1.*"}
	for _, c := range cases {
		r := MustParseReq(c)
		if got := r.String(); got != c {
			t.Errorf("Req(%q).String() = %q; want %q", c, got, c)
		}
	}
}
