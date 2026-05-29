package semver

import (
	"sort"
	"testing"
)

func TestParseBasic(t *testing.T) {
	cases := []struct {
		in   string
		want Version
	}{
		{"1", Version{Release: []int{1}, Post: -1, Dev: -1}},
		{"1.2", Version{Release: []int{1, 2}, Post: -1, Dev: -1}},
		{"1.2.3", Version{Release: []int{1, 2, 3}, Post: -1, Dev: -1}},
		{"2!1.0", Version{Epoch: 2, Release: []int{1, 0}, Post: -1, Dev: -1}},
		{"1.0a1", Version{Release: []int{1, 0}, PreKind: "a", PreNum: 1, Post: -1, Dev: -1}},
		{"1.0b2", Version{Release: []int{1, 0}, PreKind: "b", PreNum: 2, Post: -1, Dev: -1}},
		{"1.0rc1", Version{Release: []int{1, 0}, PreKind: "rc", PreNum: 1, Post: -1, Dev: -1}},
		{"1.0.post5", Version{Release: []int{1, 0}, Post: 5, Dev: -1}},
		{"1.0.dev3", Version{Release: []int{1, 0}, Post: -1, Dev: 3}},
		{"1.0a1.post1.dev0", Version{Release: []int{1, 0}, PreKind: "a", PreNum: 1, Post: 1, Dev: 0}},
		{"1.0+local.1", Version{Release: []int{1, 0}, Post: -1, Dev: -1, Local: "local.1"}},
		{"1.0+local-1_2", Version{Release: []int{1, 0}, Post: -1, Dev: -1, Local: "local.1.2"}},
	}
	for _, tc := range cases {
		got, err := Parse(tc.in)
		if err != nil {
			t.Errorf("Parse(%q) error: %v", tc.in, err)
			continue
		}
		if !versionEqual(got, tc.want) {
			t.Errorf("Parse(%q) = %+v; want %+v", tc.in, got, tc.want)
		}
	}
}

func TestParseRejectsBadInput(t *testing.T) {
	bad := []string{
		"",
		"v1.0",
		"V1.0",
		" 1.0",
		"1.0 ",
		"1.",
		"..",
		"1!",
		"1.0+",
		"1.0+@bad",
		"1.0a",
		"1.0.dev",
		"abc",
		"-1.0",
	}
	for _, s := range bad {
		if _, err := Parse(s); err == nil {
			t.Errorf("Parse(%q) accepted; expected error", s)
		}
	}
}

func TestStringRoundTrip(t *testing.T) {
	canonical := []string{
		"1",
		"1.2.3",
		"2!1.0",
		"1.0a1",
		"1.0b2",
		"1.0rc1",
		"1.0.post5",
		"1.0.dev3",
		"1.0a1.post1.dev0",
		"1.0+local.1",
	}
	for _, s := range canonical {
		v, err := Parse(s)
		if err != nil {
			t.Fatalf("Parse(%q): %v", s, err)
		}
		if got := v.String(); got != s {
			t.Errorf("Parse(%q).String() = %q; want %q", s, got, s)
		}
	}
}

func TestCompareOrdering(t *testing.T) {
	// Ascending: each entry should be strictly less than the next.
	ordered := []string{
		"1.0.dev0",
		"1.0a1.dev0",
		"1.0a1",
		"1.0a2",
		"1.0b1",
		"1.0rc1",
		"1.0",
		"1.0.post0.dev0",
		"1.0.post0",
		"1.0.post1",
		"1.0.1",
		"1.1",
		"2.0",
		"2!0.5",
		"2!1.0",
	}
	versions := make([]Version, len(ordered))
	for i, s := range ordered {
		v, err := Parse(s)
		if err != nil {
			t.Fatalf("Parse(%q): %v", s, err)
		}
		versions[i] = v
	}
	for i := 0; i < len(versions)-1; i++ {
		if c := versions[i].Compare(versions[i+1]); c != -1 {
			t.Errorf("Compare(%s, %s) = %d; want -1", ordered[i], ordered[i+1], c)
		}
		if c := versions[i+1].Compare(versions[i]); c != 1 {
			t.Errorf("Compare(%s, %s) = %d; want 1", ordered[i+1], ordered[i], c)
		}
	}
}

func TestCompareEqual(t *testing.T) {
	a, _ := Parse("1.2.3")
	b, _ := Parse("1.2.3")
	if c := a.Compare(b); c != 0 {
		t.Errorf("Compare equal = %d; want 0", c)
	}
}

func TestCompareReleasePadsZeros(t *testing.T) {
	a, _ := Parse("1.0")
	b, _ := Parse("1.0.0")
	if c := a.Compare(b); c != 0 {
		t.Errorf("Compare 1.0 vs 1.0.0 = %d; want 0 (right-padded zeros)", c)
	}
}

func TestCompareLocalOrdering(t *testing.T) {
	a, _ := Parse("1.0")
	b, _ := Parse("1.0+local.1")
	c, _ := Parse("1.0+local.2")
	if r := a.Compare(b); r != -1 {
		t.Errorf("Compare 1.0 vs 1.0+local.1 = %d; want -1", r)
	}
	if r := b.Compare(c); r != -1 {
		t.Errorf("Compare 1.0+local.1 vs 1.0+local.2 = %d; want -1", r)
	}
}

func TestCompareLocalNumericVsAlpha(t *testing.T) {
	// PEP 440: numeric local segments sort higher than alphabetic ones.
	a, _ := Parse("1.0+alpha")
	b, _ := Parse("1.0+1")
	if r := a.Compare(b); r != -1 {
		t.Errorf("Compare 1.0+alpha vs 1.0+1 = %d; want -1 (numeric > alpha)", r)
	}
}

func TestIsPreRelease(t *testing.T) {
	cases := map[string]bool{
		"1.0":              false,
		"1.0a1":            true,
		"1.0b1":            true,
		"1.0rc1":           true,
		"1.0.dev0":         true,
		"1.0.post0":        false,
		"1.0.post0.dev0":   true,
		"1.0a1.post0":      true,
	}
	for s, want := range cases {
		v, err := Parse(s)
		if err != nil {
			t.Fatalf("Parse(%q): %v", s, err)
		}
		if got := v.IsPreRelease(); got != want {
			t.Errorf("Parse(%q).IsPreRelease() = %v; want %v", s, got, want)
		}
	}
}

func TestSortByCompare(t *testing.T) {
	shuffled := []string{
		"2.0",
		"1.0a1",
		"1.0.post0",
		"1.0",
		"1.0.dev0",
		"1.1",
	}
	want := []string{
		"1.0.dev0",
		"1.0a1",
		"1.0",
		"1.0.post0",
		"1.1",
		"2.0",
	}
	versions := make([]Version, len(shuffled))
	for i, s := range shuffled {
		v, err := Parse(s)
		if err != nil {
			t.Fatalf("Parse(%q): %v", s, err)
		}
		versions[i] = v
	}
	sort.Slice(versions, func(i, j int) bool {
		return versions[i].Compare(versions[j]) < 0
	})
	for i, v := range versions {
		if got := v.String(); got != want[i] {
			t.Errorf("sorted[%d] = %q; want %q", i, got, want[i])
		}
	}
}

func versionEqual(a, b Version) bool {
	if a.Epoch != b.Epoch || a.PreKind != b.PreKind || a.PreNum != b.PreNum {
		return false
	}
	if a.Post != b.Post || a.Dev != b.Dev || a.Local != b.Local {
		return false
	}
	if len(a.Release) != len(b.Release) {
		return false
	}
	for i := range a.Release {
		if a.Release[i] != b.Release[i] {
			return false
		}
	}
	return true
}
