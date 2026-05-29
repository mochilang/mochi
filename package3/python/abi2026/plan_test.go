package abi2026

import (
	"strings"
	"testing"
)

func TestSelectorRejectsPolicyUnknown(t *testing.T) {
	s := Selector{}
	_, err := s.Select(nil)
	if err == nil {
		t.Fatal("Selector.Select expected error for PolicyUnknown")
	}
	if !strings.Contains(err.Error(), "Policy must be set") {
		t.Errorf("Selector.Select error %q does not mention Policy", err.Error())
	}
}

func TestSelectorPolicyBothPrefersABI2026(t *testing.T) {
	s := Selector{Policy: PolicyBoth}
	cands := []WheelCandidate{
		{Filename: "demo-1.0-cp312-cp312-manylinux_2_28_x86_64.whl"},
		{Filename: "demo-1.0-cp32-abi3-manylinux_2_28_x86_64.whl"},
		{Filename: "demo-1.0-cp314-abi2026-manylinux_2_28_x86_64.whl"},
		{Filename: "demo-1.0-py3-none-any.whl"},
	}
	res, err := s.Select(cands)
	if err != nil {
		t.Fatalf("Select error %v", err)
	}
	if res.Chosen == nil {
		t.Fatal("Select returned nil Chosen")
	}
	if res.ChosenClass != TagClassABI2026 {
		t.Errorf("ChosenClass = %v, want TagClassABI2026", res.ChosenClass)
	}
	if !strings.Contains(res.Chosen.Filename, "abi2026") {
		t.Errorf("Chosen filename %q does not contain abi2026", res.Chosen.Filename)
	}
	if res.ChosenTag != "abi2026" {
		t.Errorf("ChosenTag = %q, want abi2026", res.ChosenTag)
	}
}

func TestSelectorPolicyLegacyRejectsABI2026WithReason(t *testing.T) {
	s := Selector{Policy: PolicyLegacy}
	cands := []WheelCandidate{
		{Filename: "demo-1.0-cp32-abi3-manylinux_2_28_x86_64.whl"},
		{Filename: "demo-1.0-cp314-abi2026-manylinux_2_28_x86_64.whl"},
	}
	res, err := s.Select(cands)
	if err != nil {
		t.Fatalf("Select error %v", err)
	}
	if res.ChosenClass != TagClassLegacyABI3 {
		t.Errorf("ChosenClass = %v, want TagClassLegacyABI3", res.ChosenClass)
	}
	reason, ok := res.Reasons["demo-1.0-cp314-abi2026-manylinux_2_28_x86_64.whl"]
	if !ok {
		t.Fatal("PolicyLegacy did not record a reason for the abi2026 wheel")
	}
	if !strings.Contains(reason, "rejected by policy") {
		t.Errorf("reason %q does not mention policy rejection", reason)
	}
}

func TestSelectorPolicyAbi2026RejectsLegacy(t *testing.T) {
	s := Selector{Policy: PolicyAbi2026}
	cands := []WheelCandidate{
		{Filename: "demo-1.0-cp312-cp312-manylinux_2_28_x86_64.whl"},
		{Filename: "demo-1.0-cp32-abi3-manylinux_2_28_x86_64.whl"},
	}
	res, err := s.Select(cands)
	if err != nil {
		t.Fatalf("Select error %v", err)
	}
	if res.Chosen != nil {
		t.Errorf("PolicyAbi2026 should reject all legacy wheels, got %q", res.Chosen.Filename)
	}
	if len(res.Reasons) != 2 {
		t.Errorf("want 2 rejection reasons, got %d", len(res.Reasons))
	}
}

func TestSelectorPureFallback(t *testing.T) {
	s := Selector{Policy: PolicyAbi2026}
	cands := []WheelCandidate{
		{Filename: "demo-1.0-py3-none-any.whl"},
	}
	res, err := s.Select(cands)
	if err != nil {
		t.Fatalf("Select error %v", err)
	}
	if res.ChosenClass != TagClassPure {
		t.Errorf("ChosenClass = %v, want TagClassPure", res.ChosenClass)
	}
}

func TestSelectorFilenameTieBreak(t *testing.T) {
	// Two abi2026 wheels at the same class rank -> earlier filename wins.
	s := Selector{Policy: PolicyBoth}
	cands := []WheelCandidate{
		{Filename: "demo-1.0-cp314-abi2026-musllinux_1_1_x86_64.whl"},
		{Filename: "demo-1.0-cp314-abi2026-manylinux_2_28_x86_64.whl"},
	}
	res, err := s.Select(cands)
	if err != nil {
		t.Fatalf("Select error %v", err)
	}
	if res.Chosen == nil {
		t.Fatal("nil Chosen")
	}
	if !strings.Contains(res.Chosen.Filename, "manylinux") {
		t.Errorf("tie-break failed: chose %q, want manylinux variant", res.Chosen.Filename)
	}
}

func TestSelectorMalformedFilenameInReasons(t *testing.T) {
	s := Selector{Policy: PolicyBoth}
	cands := []WheelCandidate{
		{Filename: "garbage.tar.gz"},
		{Filename: "demo-1.0-cp314-abi2026-manylinux_2_28_x86_64.whl"},
	}
	res, err := s.Select(cands)
	if err != nil {
		t.Fatalf("Select error %v", err)
	}
	if _, ok := res.Reasons["garbage.tar.gz"]; !ok {
		t.Error("malformed filename did not get a Reason entry")
	}
	if res.Chosen == nil || !strings.Contains(res.Chosen.Filename, "abi2026") {
		t.Errorf("expected the abi2026 wheel chosen, got %+v", res.Chosen)
	}
}

func TestSelectorEmptyCandidates(t *testing.T) {
	s := Selector{Policy: PolicyBoth}
	res, err := s.Select(nil)
	if err != nil {
		t.Fatalf("Select error %v", err)
	}
	if res.Chosen != nil {
		t.Error("empty candidates should return nil Chosen")
	}
}

func TestSelectorUnrecognisedABIInReasons(t *testing.T) {
	s := Selector{Policy: PolicyBoth}
	cands := []WheelCandidate{
		{Filename: "demo-1.0-pp310-pypy310-manylinux_2_28_x86_64.whl"},
	}
	res, err := s.Select(cands)
	if err != nil {
		t.Fatalf("Select error %v", err)
	}
	if res.Chosen != nil {
		t.Errorf("expected nil Chosen for unrecognised ABI, got %q", res.Chosen.Filename)
	}
	reason, ok := res.Reasons["demo-1.0-pp310-pypy310-manylinux_2_28_x86_64.whl"]
	if !ok {
		t.Fatal("unrecognised ABI did not record a reason")
	}
	if !strings.Contains(reason, "unrecognised") {
		t.Errorf("reason %q does not mention unrecognised", reason)
	}
}

func TestWheelABIExtraction(t *testing.T) {
	cases := []struct {
		name string
		want string
	}{
		{"demo-1.0-cp312-cp312-manylinux_2_28_x86_64.whl", "cp312"},
		{"demo-1.0-cp32-abi3-manylinux_2_28_x86_64.whl", "abi3"},
		{"demo-1.0-cp314-abi2026-manylinux_2_28_x86_64.whl", "abi2026"},
		{"demo-1.0-py3-none-any.whl", "none"},
	}
	for _, tc := range cases {
		got, err := wheelABI(tc.name)
		if err != nil {
			t.Errorf("wheelABI(%q) error %v", tc.name, err)
			continue
		}
		if got != tc.want {
			t.Errorf("wheelABI(%q) = %q, want %q", tc.name, got, tc.want)
		}
	}

	if _, err := wheelABI("foo.tar.gz"); err == nil {
		t.Error("wheelABI on non-.whl expected error")
	}
	if _, err := wheelABI("too-few.whl"); err == nil {
		t.Error("wheelABI on truncated name expected error")
	}
}
