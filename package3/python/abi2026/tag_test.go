package abi2026

import (
	"strings"
	"testing"
)

func TestClassifyABITagHappyPaths(t *testing.T) {
	cases := []struct {
		raw  string
		want TagClass
	}{
		{"none", TagClassPure},
		{"abi3", TagClassLegacyABI3},
		{"abi2026", TagClassABI2026},
		{"cp312", TagClassLegacyCPython},
		{"cp313", TagClassLegacyCPython},
		{"cp313t", TagClassLegacyCPython},
		{"cp314", TagClassLegacyCPython},
		{"cp314t", TagClassLegacyCPython},
		{"cp310", TagClassLegacyCPython},
	}
	for _, tc := range cases {
		got, err := ClassifyABITag(tc.raw)
		if err != nil {
			t.Fatalf("ClassifyABITag(%q) error %v", tc.raw, err)
		}
		if got != tc.want {
			t.Errorf("ClassifyABITag(%q) = %v, want %v", tc.raw, got, tc.want)
		}
	}
}

func TestClassifyABITagErrors(t *testing.T) {
	cases := []struct {
		raw     string
		wantSub string
		class   TagClass
	}{
		{"", "empty ABI tag", TagClassUnknown},
		{"pp310", "unrecognised", TagClassUnrecognised},
		{"cp", "unrecognised", TagClassUnrecognised},
		{"cpXY", "unrecognised", TagClassUnrecognised},
		{"abi42", "unrecognised", TagClassUnrecognised},
		{"garbage", "unrecognised", TagClassUnrecognised},
	}
	for _, tc := range cases {
		got, err := ClassifyABITag(tc.raw)
		if err == nil {
			t.Errorf("ClassifyABITag(%q) expected error, got class %v", tc.raw, got)
			continue
		}
		if !strings.Contains(err.Error(), tc.wantSub) {
			t.Errorf("ClassifyABITag(%q) error %q does not contain %q", tc.raw, err.Error(), tc.wantSub)
		}
		if got != tc.class {
			t.Errorf("ClassifyABITag(%q) class = %v, want %v", tc.raw, got, tc.class)
		}
	}
}

func TestTagClassString(t *testing.T) {
	cases := []struct {
		c    TagClass
		want string
	}{
		{TagClassUnknown, "unknown"},
		{TagClassPure, "pure"},
		{TagClassLegacyCPython, "legacy-cpython"},
		{TagClassLegacyABI3, "legacy-abi3"},
		{TagClassABI2026, "abi2026"},
		{TagClassUnrecognised, "unrecognised"},
		{TagClass(99), "unknown"},
	}
	for _, tc := range cases {
		if got := tc.c.String(); got != tc.want {
			t.Errorf("TagClass(%d).String() = %q, want %q", tc.c, got, tc.want)
		}
	}
}

func TestIsCPythonABI(t *testing.T) {
	cases := []struct {
		raw  string
		want bool
	}{
		{"cp312", true},
		{"cp313t", true},
		{"cp3", false},
		{"cp", false},
		{"cp312x", false},
		{"cpXY", false},
	}
	for _, tc := range cases {
		if got := isCPythonABI(tc.raw); got != tc.want {
			t.Errorf("isCPythonABI(%q) = %v, want %v", tc.raw, got, tc.want)
		}
	}
}
