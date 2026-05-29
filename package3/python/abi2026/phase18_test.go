package abi2026

import (
	"strings"
	"testing"
)

// TestPhase18Umbrella is the sentinel that proves Phase 18 ships an
// end-to-end ABI-tag transition: classify, gate, select, rename. If
// any of the four sub-cases below regress, the gate fails so the
// umbrella issue cannot be closed.
func TestPhase18Umbrella(t *testing.T) {
	t.Run("legacy policy rejects abi2026 wheels (migration-safety hatch)", func(t *testing.T) {
		s := Selector{Policy: PolicyLegacy}
		cands := []WheelCandidate{
			{Filename: "demo-1.0-cp314-abi2026-manylinux_2_28_x86_64.whl"},
			{Filename: "demo-1.0-cp32-abi3-manylinux_2_28_x86_64.whl"},
		}
		res, err := s.Select(cands)
		if err != nil {
			t.Fatalf("select error %v", err)
		}
		if res.ChosenClass != TagClassLegacyABI3 {
			t.Fatalf("PolicyLegacy must pin to abi3, chose class %v", res.ChosenClass)
		}
		if _, ok := res.Reasons["demo-1.0-cp314-abi2026-manylinux_2_28_x86_64.whl"]; !ok {
			t.Fatal("PolicyLegacy must record a rejection reason for the abi2026 wheel")
		}
	})

	t.Run("abi2026 policy rejects legacy wheels (post-migration end state)", func(t *testing.T) {
		s := Selector{Policy: PolicyAbi2026}
		cands := []WheelCandidate{
			{Filename: "demo-1.0-cp312-cp312-manylinux_2_28_x86_64.whl"},
			{Filename: "demo-1.0-cp32-abi3-manylinux_2_28_x86_64.whl"},
		}
		res, err := s.Select(cands)
		if err != nil {
			t.Fatalf("select error %v", err)
		}
		if res.Chosen != nil {
			t.Fatalf("PolicyAbi2026 must reject all legacy wheels, chose %q", res.Chosen.Filename)
		}
		for _, r := range res.Reasons {
			if !strings.Contains(r, "rejected by policy") {
				t.Errorf("PolicyAbi2026 rejection reason missing policy mention: %q", r)
			}
		}
	})

	t.Run("both policy prefers abi2026 over abi3 over cp3XY (migration window)", func(t *testing.T) {
		s := Selector{Policy: PolicyBoth}
		cands := []WheelCandidate{
			{Filename: "demo-1.0-cp312-cp312-manylinux_2_28_x86_64.whl"},
			{Filename: "demo-1.0-cp32-abi3-manylinux_2_28_x86_64.whl"},
			{Filename: "demo-1.0-cp314-abi2026-manylinux_2_28_x86_64.whl"},
		}
		res, err := s.Select(cands)
		if err != nil {
			t.Fatalf("select error %v", err)
		}
		if res.ChosenClass != TagClassABI2026 {
			t.Fatalf("PolicyBoth must prefer abi2026, chose %v", res.ChosenClass)
		}
	})

	t.Run("promote-downgrade round trip preserves the promoted shape", func(t *testing.T) {
		start := "demo-1.0-cp32-abi3-manylinux_2_28_x86_64.whl"
		promoted, err := PromoteToABI2026(start)
		if err != nil {
			t.Fatalf("promote: %v", err)
		}
		if !strings.Contains(promoted, "cp314-abi2026") {
			t.Fatalf("promoted shape %q missing cp314-abi2026", promoted)
		}
		// Round-trip back to abi3, then re-promote, must equal `promoted`.
		downgraded, err := DowngradeToABI3(promoted)
		if err != nil {
			t.Fatalf("downgrade: %v", err)
		}
		if downgraded != start {
			t.Fatalf("downgrade did not invert promote: got %q, want %q", downgraded, start)
		}
		again, err := PromoteToABI2026(downgraded)
		if err != nil {
			t.Fatalf("re-promote: %v", err)
		}
		if again != promoted {
			t.Fatalf("re-promote diverged: got %q, want %q", again, promoted)
		}
	})
}
