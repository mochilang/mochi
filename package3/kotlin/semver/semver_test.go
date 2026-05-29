package semver

import "testing"

func TestParse(t *testing.T) {
	cases := []struct {
		in    string
		major int
		minor int
		patch int
		qual  string
	}{
		{"1.7.3", 1, 7, 3, ""},
		{"1.9.23-SNAPSHOT", 1, 9, 23, "SNAPSHOT"},
		{"2.0", 2, 0, 0, ""},
		{"0.5.0", 0, 5, 0, ""},
	}
	for _, c := range cases {
		v, err := Parse(c.in)
		if err != nil {
			t.Errorf("Parse(%q) error: %v", c.in, err)
			continue
		}
		if v.Major != c.major || v.Minor != c.minor || v.Patch != c.patch || v.Qualifier != c.qual {
			t.Errorf("Parse(%q) = %v; want {%d,%d,%d,%q}", c.in, v, c.major, c.minor, c.patch, c.qual)
		}
	}
}

func TestParseRange(t *testing.T) {
	v173, _ := Parse("1.7.3")
	v200, _ := Parse("2.0.0")
	v180, _ := Parse("1.8.0")

	cases := []struct {
		in      string
		matches []Version
		rejects []Version
	}{
		{"1.7.3", []Version{v173}, []Version{v200}},
		{"[1.0,2.0)", []Version{v173, v180}, []Version{v200}},
		{"LATEST", []Version{v173, v200}, nil},
		{"1.7.+", []Version{v173}, []Version{v200}},
	}
	for _, c := range cases {
		r, err := ParseRange(c.in)
		if err != nil {
			t.Errorf("ParseRange(%q) error: %v", c.in, err)
			continue
		}
		for _, v := range c.matches {
			if !r.Matches(v) {
				t.Errorf("ParseRange(%q).Matches(%v) = false; want true", c.in, v)
			}
		}
		for _, v := range c.rejects {
			if r.Matches(v) {
				t.Errorf("ParseRange(%q).Matches(%v) = true; want false", c.in, v)
			}
		}
	}
}
