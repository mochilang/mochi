package abi2026

import (
	"strings"
	"testing"
)

func TestPolicyString(t *testing.T) {
	cases := []struct {
		p    Policy
		want string
	}{
		{PolicyUnknown, "unknown"},
		{PolicyLegacy, "legacy"},
		{PolicyAbi2026, "abi2026"},
		{PolicyBoth, "both"},
		{Policy(99), "unknown"},
	}
	for _, tc := range cases {
		if got := tc.p.String(); got != tc.want {
			t.Errorf("Policy(%d).String() = %q, want %q", tc.p, got, tc.want)
		}
	}
}

func TestParsePolicy(t *testing.T) {
	happy := []struct {
		s    string
		want Policy
	}{
		{"legacy", PolicyLegacy},
		{"abi2026", PolicyAbi2026},
		{"both", PolicyBoth},
	}
	for _, tc := range happy {
		got, err := ParsePolicy(tc.s)
		if err != nil {
			t.Fatalf("ParsePolicy(%q) error %v", tc.s, err)
		}
		if got != tc.want {
			t.Errorf("ParsePolicy(%q) = %v, want %v", tc.s, got, tc.want)
		}
	}

	bad := []struct {
		s       string
		wantSub string
	}{
		{"", "must be specified"},
		{"strict", "unknown policy"},
		{"LEGACY", "unknown policy"},
	}
	for _, tc := range bad {
		got, err := ParsePolicy(tc.s)
		if err == nil {
			t.Errorf("ParsePolicy(%q) expected error, got %v", tc.s, got)
			continue
		}
		if !strings.Contains(err.Error(), tc.wantSub) {
			t.Errorf("ParsePolicy(%q) error %q does not contain %q", tc.s, err.Error(), tc.wantSub)
		}
		if got != PolicyUnknown {
			t.Errorf("ParsePolicy(%q) on error returned %v, want PolicyUnknown", tc.s, got)
		}
	}
}

func TestPolicyAcceptsMatrix(t *testing.T) {
	type cell struct {
		policy Policy
		class  TagClass
		want   bool
	}
	cases := []cell{
		// Pure always accepted except under PolicyUnknown.
		{PolicyUnknown, TagClassPure, false},
		{PolicyLegacy, TagClassPure, true},
		{PolicyAbi2026, TagClassPure, true},
		{PolicyBoth, TagClassPure, true},

		// LegacyCPython.
		{PolicyUnknown, TagClassLegacyCPython, false},
		{PolicyLegacy, TagClassLegacyCPython, true},
		{PolicyAbi2026, TagClassLegacyCPython, false},
		{PolicyBoth, TagClassLegacyCPython, true},

		// LegacyABI3.
		{PolicyUnknown, TagClassLegacyABI3, false},
		{PolicyLegacy, TagClassLegacyABI3, true},
		{PolicyAbi2026, TagClassLegacyABI3, false},
		{PolicyBoth, TagClassLegacyABI3, true},

		// ABI2026.
		{PolicyUnknown, TagClassABI2026, false},
		{PolicyLegacy, TagClassABI2026, false},
		{PolicyAbi2026, TagClassABI2026, true},
		{PolicyBoth, TagClassABI2026, true},

		// Unrecognised never accepted.
		{PolicyLegacy, TagClassUnrecognised, false},
		{PolicyAbi2026, TagClassUnrecognised, false},
		{PolicyBoth, TagClassUnrecognised, false},
	}
	for _, c := range cases {
		got := c.policy.Accepts(c.class)
		if got != c.want {
			t.Errorf("Policy(%v).Accepts(%v) = %v, want %v", c.policy, c.class, got, c.want)
		}
	}
}

func TestPolicyRankOrdering(t *testing.T) {
	p := PolicyBoth
	abi2026 := p.Rank(TagClassABI2026)
	abi3 := p.Rank(TagClassLegacyABI3)
	cp := p.Rank(TagClassLegacyCPython)
	pure := p.Rank(TagClassPure)
	if !(abi2026 > abi3 && abi3 > cp && cp > pure && pure > 0) {
		t.Errorf("PolicyBoth.Rank ordering broken: abi2026=%d abi3=%d cp=%d pure=%d", abi2026, abi3, cp, pure)
	}

	// Under PolicyLegacy, ABI2026 is rejected -> Rank == 0.
	if r := PolicyLegacy.Rank(TagClassABI2026); r != 0 {
		t.Errorf("PolicyLegacy.Rank(ABI2026) = %d, want 0", r)
	}
	// Under PolicyAbi2026, legacy classes -> 0.
	if r := PolicyAbi2026.Rank(TagClassLegacyABI3); r != 0 {
		t.Errorf("PolicyAbi2026.Rank(LegacyABI3) = %d, want 0", r)
	}
	if r := PolicyAbi2026.Rank(TagClassLegacyCPython); r != 0 {
		t.Errorf("PolicyAbi2026.Rank(LegacyCPython) = %d, want 0", r)
	}

	// Unrecognised always 0.
	if r := PolicyBoth.Rank(TagClassUnrecognised); r != 0 {
		t.Errorf("Rank(Unrecognised) = %d, want 0", r)
	}
}
