package semver

import (
	"testing"
)

func TestParseVersion(t *testing.T) {
	cases := []struct {
		input    string
		wantErr  bool
		segments int
	}{
		{"1.2.3", false, 3},
		{"1.0.0.pre", false, 4},
		{"2.0.0.rc1", false, 4},
		{"0.9.1", false, 3},
		{"1", false, 1},
		{"0", false, 1},
		{"1.0.0.alpha1", false, 4},
		{"3.1.4.1.5", false, 5},
		{"", true, 0},
		{"1..2", true, 0},
	}
	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			v, err := ParseVersion(tc.input)
			if tc.wantErr {
				if err == nil {
					t.Errorf("ParseVersion(%q) expected error, got nil", tc.input)
				}
				return
			}
			if err != nil {
				t.Fatalf("ParseVersion(%q) unexpected error: %v", tc.input, err)
			}
			if len(v.Segments) != tc.segments {
				t.Errorf("ParseVersion(%q) segments = %d, want %d", tc.input, len(v.Segments), tc.segments)
			}
			if v.Original != tc.input {
				t.Errorf("ParseVersion(%q) Original = %q, want %q", tc.input, v.Original, tc.input)
			}
		})
	}
}

func TestParseReq(t *testing.T) {
	cases := []struct {
		input   string
		wantOp  Op
		wantVer string
		wantErr bool
	}{
		{">= 1.0", OpGte, "1.0", false},
		{"~> 2.1", OpPessimistic, "2.1", false},
		{"= 1.2.3", OpEq, "1.2.3", false},
		{"!= 3.0", OpNeq, "3.0", false},
		{"> 0.9", OpGt, "0.9", false},
		{"< 2.0.0", OpLt, "2.0.0", false},
		{"<= 1.5.0", OpLte, "1.5.0", false},
		{"~> 1.2.3", OpPessimistic, "1.2.3", false},
		{"~> 0", OpPessimistic, "0", false},
		{"1.0.0", OpEq, "1.0.0", false}, // no operator defaults to =
	}
	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			r, err := ParseReq(tc.input)
			if tc.wantErr {
				if err == nil {
					t.Errorf("ParseReq(%q) expected error, got nil", tc.input)
				}
				return
			}
			if err != nil {
				t.Fatalf("ParseReq(%q) unexpected error: %v", tc.input, err)
			}
			if r.Op != tc.wantOp {
				t.Errorf("ParseReq(%q) op = %v, want %v", tc.input, r.Op, tc.wantOp)
			}
			if r.Version.Original != tc.wantVer {
				t.Errorf("ParseReq(%q) version = %q, want %q", tc.input, r.Version.Original, tc.wantVer)
			}
		})
	}
}

func TestSatisfies_Eq(t *testing.T) {
	cases := []struct {
		candidate string
		req       string
		want      bool
	}{
		{"1.2.3", "= 1.2.3", true},
		{"1.2.4", "= 1.2.3", false},
		{"1.2.3", "!= 1.2.3", false},
		{"1.2.4", "!= 1.2.3", true},
	}
	for _, tc := range cases {
		t.Run(tc.candidate+"_"+tc.req, func(t *testing.T) {
			c := MustParseVersion(tc.candidate)
			r := MustParseReq(tc.req)
			got := Satisfies(c, r)
			if got != tc.want {
				t.Errorf("Satisfies(%q, %q) = %v, want %v", tc.candidate, tc.req, got, tc.want)
			}
		})
	}
}

func TestSatisfies_Comparison(t *testing.T) {
	cases := []struct {
		candidate string
		req       string
		want      bool
	}{
		{"1.3.0", ">= 1.2.0", true},
		{"1.1.0", ">= 1.2.0", false},
		{"1.2.0", ">= 1.2.0", true},
		{"2.0.0", "> 1.9.9", true},
		{"1.9.9", "> 1.9.9", false},
		{"1.0.0", "< 2.0.0", true},
		{"2.0.0", "< 2.0.0", false},
		{"2.0.0", "<= 2.0.0", true},
		{"2.0.1", "<= 2.0.0", false},
	}
	for _, tc := range cases {
		t.Run(tc.candidate+"_"+tc.req, func(t *testing.T) {
			c := MustParseVersion(tc.candidate)
			r := MustParseReq(tc.req)
			got := Satisfies(c, r)
			if got != tc.want {
				t.Errorf("Satisfies(%q, %q) = %v, want %v", tc.candidate, tc.req, got, tc.want)
			}
		})
	}
}

func TestSatisfies_Pessimistic(t *testing.T) {
	cases := []struct {
		candidate string
		req       string
		want      bool
	}{
		// ~> 1.2 means >= 1.2, < 2.0
		{"1.2.0", "~> 1.2", true},
		{"1.9.9", "~> 1.2", true},
		{"2.0.0", "~> 1.2", false},
		{"1.1.9", "~> 1.2", false},
		// ~> 1.2.3 means >= 1.2.3, < 1.3.0
		{"1.2.3", "~> 1.2.3", true},
		{"1.2.9", "~> 1.2.3", true},
		{"1.3.0", "~> 1.2.3", false},
		{"1.2.2", "~> 1.2.3", false},
		// ~> 0 means >= 0, no upper bound
		{"0.0.1", "~> 0", true},
		{"99.0.0", "~> 0", true},
		// ~> 2.0.0 means >= 2.0.0, < 2.1.0
		{"2.0.5", "~> 2.0.0", true},
		{"2.1.0", "~> 2.0.0", false},
	}
	for _, tc := range cases {
		t.Run(tc.candidate+"_"+tc.req, func(t *testing.T) {
			c := MustParseVersion(tc.candidate)
			r := MustParseReq(tc.req)
			got := Satisfies(c, r)
			if got != tc.want {
				t.Errorf("Satisfies(%q, %q) = %v, want %v", tc.candidate, tc.req, got, tc.want)
			}
		})
	}
}

func TestPreReleaseSorting(t *testing.T) {
	// Pre-release versions sort BEFORE the corresponding release.
	cases := []struct {
		a    string
		b    string
		want int // -1, 0, 1
	}{
		{"1.0.0.pre", "1.0.0", -1},     // pre-release before release
		{"1.0.0", "1.0.0.pre", 1},      // release after pre-release
		{"1.0.0.rc1", "1.0.0", -1},     // rc before release
		{"1.0.0.alpha", "1.0.0.beta", -1}, // alpha before beta
		{"1.0.0.beta", "1.0.0.alpha", 1},  // beta after alpha
		{"1.0.0.rc1", "1.0.0.rc2", -1},   // rc1 before rc2 (string compare)
		{"1.0.0", "1.0.0", 0},             // equal
		{"2.0.0", "1.9.9", 1},             // higher release
	}
	for _, tc := range cases {
		t.Run(tc.a+"_vs_"+tc.b, func(t *testing.T) {
			a := MustParseVersion(tc.a)
			b := MustParseVersion(tc.b)
			got := compareVersions(a, b)
			// Normalize to -1/0/1.
			if got < 0 {
				got = -1
			} else if got > 0 {
				got = 1
			}
			if got != tc.want {
				t.Errorf("compareVersions(%q, %q) = %d, want %d", tc.a, tc.b, got, tc.want)
			}
		})
	}
}

func TestSortVersions(t *testing.T) {
	input := []string{"2.0.0", "1.0.0.pre", "1.0.0", "1.2.3", "0.9.0"}
	versions := make([]Version, len(input))
	for i, s := range input {
		versions[i] = MustParseVersion(s)
	}
	SortVersions(versions)
	want := []string{"0.9.0", "1.0.0.pre", "1.0.0", "1.2.3", "2.0.0"}
	for i, v := range versions {
		if v.Original != want[i] {
			t.Errorf("SortVersions[%d] = %q, want %q", i, v.Original, want[i])
		}
	}
}

func TestMaxSatisfying(t *testing.T) {
	allVersions := []Version{
		MustParseVersion("1.0.0"),
		MustParseVersion("1.1.0"),
		MustParseVersion("1.2.3"),
		MustParseVersion("1.9.9"),
		MustParseVersion("2.0.0"),
		MustParseVersion("2.1.0"),
		MustParseVersion("0.9.1"),
	}
	cases := []struct {
		req     string
		wantVer string
		wantErr bool
	}{
		{"~> 1.2", "1.9.9", false},
		{"~> 1.2.3", "1.2.3", false},
		{"~> 2.0", "2.1.0", false},
		{">= 1.0", "2.1.0", false},
		{"< 1.2.0", "1.1.0", false},
		{"= 1.1.0", "1.1.0", false},
		{"> 99.0.0", "", true},
		{"~> 3.0", "", true},
	}
	for _, tc := range cases {
		t.Run(tc.req, func(t *testing.T) {
			r := MustParseReq(tc.req)
			got, err := MaxSatisfying(allVersions, r)
			if tc.wantErr {
				if err == nil {
					t.Errorf("MaxSatisfying(%q) expected error, got %q", tc.req, got.Original)
				}
				return
			}
			if err != nil {
				t.Fatalf("MaxSatisfying(%q) unexpected error: %v", tc.req, err)
			}
			if got.Original != tc.wantVer {
				t.Errorf("MaxSatisfying(%q) = %q, want %q", tc.req, got.Original, tc.wantVer)
			}
		})
	}
}

func TestSatisfiesAll(t *testing.T) {
	reqs := []Req{
		MustParseReq(">= 1.0"),
		MustParseReq("< 2.0"),
	}
	cases := []struct {
		ver  string
		want bool
	}{
		{"1.0.0", true},
		{"1.5.0", true},
		{"2.0.0", false},
		{"0.9.9", false},
	}
	for _, tc := range cases {
		t.Run(tc.ver, func(t *testing.T) {
			v := MustParseVersion(tc.ver)
			got := SatisfiesAll(v, reqs)
			if got != tc.want {
				t.Errorf("SatisfiesAll(%q, [>=1.0, <2.0]) = %v, want %v", tc.ver, got, tc.want)
			}
		})
	}
}
