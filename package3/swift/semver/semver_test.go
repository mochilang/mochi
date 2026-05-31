package semver_test

import (
	"testing"

	"mochi/package3/swift/semver"
)

func TestParse(t *testing.T) {
	cases := []struct {
		input string
		want  semver.Version
		isErr bool
	}{
		{"1.0.0", semver.Version{Major: 1, Minor: 0, Patch: 0}, false},
		{"v2.3.1", semver.Version{Major: 2, Minor: 3, Patch: 1}, false},
		{"0.9.15", semver.Version{Major: 0, Minor: 9, Patch: 15}, false},
		{"bad", semver.Version{}, true},
		{"1.0", semver.Version{}, true},
	}
	for _, tc := range cases {
		got, err := semver.Parse(tc.input)
		if tc.isErr {
			if err == nil {
				t.Errorf("Parse(%q) expected error, got %v", tc.input, got)
			}
			continue
		}
		if err != nil {
			t.Errorf("Parse(%q) unexpected error: %v", tc.input, err)
			continue
		}
		if got != tc.want {
			t.Errorf("Parse(%q) = %v, want %v", tc.input, got, tc.want)
		}
	}
}

func TestVersionString(t *testing.T) {
	v := semver.Version{Major: 1, Minor: 2, Patch: 3}
	if got := v.String(); got != "1.2.3" {
		t.Errorf("Version.String() = %q, want %q", got, "1.2.3")
	}
}

func TestVersionLess(t *testing.T) {
	a := semver.Version{1, 0, 0}
	b := semver.Version{1, 0, 1}
	c := semver.Version{2, 0, 0}
	if !a.Less(b) {
		t.Error("1.0.0 should be less than 1.0.1")
	}
	if !a.Less(c) {
		t.Error("1.0.0 should be less than 2.0.0")
	}
	if b.Less(a) {
		t.Error("1.0.1 should not be less than 1.0.0")
	}
	if a.Less(a) {
		t.Error("1.0.0 should not be less than itself")
	}
}

func TestParseReq(t *testing.T) {
	v100 := semver.Version{Major: 1, Minor: 0, Patch: 0}
	v110 := semver.Version{Major: 1, Minor: 1, Patch: 0}
	v200 := semver.Version{Major: 2, Minor: 0, Patch: 0}
	v231 := semver.Version{Major: 2, Minor: 3, Patch: 1}

	cases := []struct {
		input string
		kind  semver.ReqKind
		from  semver.Version
		to    semver.Version
		isErr bool
	}{
		{`from: "1.0.0"`, semver.ReqFrom, v100, semver.Version{}, false},
		{`exact: "2.3.1"`, semver.ReqExact, v231, semver.Version{}, false},
		{`upToNextMajor(from: "1.0.0")`, semver.ReqUpToNextMajor, v100, semver.Version{}, false},
		{`upToNextMinor(from: "1.1.0")`, semver.ReqUpToNextMinor, v110, semver.Version{}, false},
		{`"1.0.0"..<"2.0.0"`, semver.ReqRange, v100, v200, false},
		{`unknown`, semver.ReqFrom, semver.Version{}, semver.Version{}, true},
	}
	for _, tc := range cases {
		got, err := semver.ParseReq(tc.input)
		if tc.isErr {
			if err == nil {
				t.Errorf("ParseReq(%q) expected error", tc.input)
			}
			continue
		}
		if err != nil {
			t.Errorf("ParseReq(%q) unexpected error: %v", tc.input, err)
			continue
		}
		if got.Kind != tc.kind {
			t.Errorf("ParseReq(%q).Kind = %v, want %v", tc.input, got.Kind, tc.kind)
		}
		if got.From != tc.from {
			t.Errorf("ParseReq(%q).From = %v, want %v", tc.input, got.From, tc.from)
		}
		if tc.kind == semver.ReqRange && got.To != tc.to {
			t.Errorf("ParseReq(%q).To = %v, want %v", tc.input, got.To, tc.to)
		}
	}
}

func TestReqSatisfies(t *testing.T) {
	fromReq := semver.Req{Kind: semver.ReqFrom, From: semver.Version{1, 0, 0}}
	exactReq := semver.Req{Kind: semver.ReqExact, From: semver.Version{1, 2, 3}}
	majorReq := semver.Req{Kind: semver.ReqUpToNextMajor, From: semver.Version{1, 0, 0}}
	minorReq := semver.Req{Kind: semver.ReqUpToNextMinor, From: semver.Version{1, 2, 0}}
	rangeReq := semver.Req{Kind: semver.ReqRange, From: semver.Version{1, 0, 0}, To: semver.Version{2, 0, 0}}

	tests := []struct {
		req  semver.Req
		v    semver.Version
		want bool
	}{
		{fromReq, semver.Version{1, 5, 0}, true},
		{fromReq, semver.Version{2, 0, 0}, false},
		{fromReq, semver.Version{0, 9, 0}, false},
		{exactReq, semver.Version{1, 2, 3}, true},
		{exactReq, semver.Version{1, 2, 4}, false},
		{majorReq, semver.Version{1, 99, 0}, true},
		{majorReq, semver.Version{2, 0, 0}, false},
		{minorReq, semver.Version{1, 2, 5}, true},
		{minorReq, semver.Version{1, 3, 0}, false},
		{rangeReq, semver.Version{1, 5, 0}, true},
		{rangeReq, semver.Version{2, 0, 0}, false},
		{rangeReq, semver.Version{0, 9, 0}, false},
	}
	for _, tc := range tests {
		if got := tc.req.Satisfies(tc.v); got != tc.want {
			t.Errorf("Req{%v}.Satisfies(%v) = %v, want %v", tc.req.Kind, tc.v, got, tc.want)
		}
	}
}
