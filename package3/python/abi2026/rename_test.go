package abi2026

import (
	"strings"
	"testing"
)

func TestPromoteToABI2026Happy(t *testing.T) {
	in := "demo-1.0-cp32-abi3-manylinux_2_28_x86_64.whl"
	want := "demo-1.0-cp314-abi2026-manylinux_2_28_x86_64.whl"
	got, err := PromoteToABI2026(in)
	if err != nil {
		t.Fatalf("PromoteToABI2026(%q) error %v", in, err)
	}
	if got != want {
		t.Errorf("PromoteToABI2026(%q) = %q, want %q", in, got, want)
	}
}

func TestPromoteToABI2026Errors(t *testing.T) {
	cases := []struct {
		name    string
		wantSub string
	}{
		{"demo-1.0-cp312-cp312-manylinux_2_28_x86_64.whl", "cannot promote"},
		{"demo-1.0-py3-none-any.whl", "cannot promote"},
		{"foo.tar.gz", "does not end in .whl"},
		{"too-few.whl", "missing fields"},
	}
	for _, tc := range cases {
		_, err := PromoteToABI2026(tc.name)
		if err == nil {
			t.Errorf("PromoteToABI2026(%q) expected error", tc.name)
			continue
		}
		if !strings.Contains(err.Error(), tc.wantSub) {
			t.Errorf("PromoteToABI2026(%q) error %q does not contain %q", tc.name, err.Error(), tc.wantSub)
		}
	}
}

func TestDowngradeToABI3Happy(t *testing.T) {
	in := "demo-1.0-cp314-abi2026-manylinux_2_28_x86_64.whl"
	want := "demo-1.0-cp32-abi3-manylinux_2_28_x86_64.whl"
	got, err := DowngradeToABI3(in)
	if err != nil {
		t.Fatalf("DowngradeToABI3(%q) error %v", in, err)
	}
	if got != want {
		t.Errorf("DowngradeToABI3(%q) = %q, want %q", in, got, want)
	}
}

func TestDowngradeToABI3Errors(t *testing.T) {
	cases := []struct {
		name    string
		wantSub string
	}{
		{"demo-1.0-cp32-abi3-manylinux_2_28_x86_64.whl", "cannot downgrade"},
		{"demo-1.0-cp312-cp312-manylinux_2_28_x86_64.whl", "cannot downgrade"},
		{"foo.tar.gz", "does not end in .whl"},
	}
	for _, tc := range cases {
		_, err := DowngradeToABI3(tc.name)
		if err == nil {
			t.Errorf("DowngradeToABI3(%q) expected error", tc.name)
			continue
		}
		if !strings.Contains(err.Error(), tc.wantSub) {
			t.Errorf("DowngradeToABI3(%q) error %q does not contain %q", tc.name, err.Error(), tc.wantSub)
		}
	}
}

func TestPromoteDowngradeRoundTrip(t *testing.T) {
	start := "demo-1.0-cp32-abi3-manylinux_2_28_x86_64.whl"
	promoted, err := PromoteToABI2026(start)
	if err != nil {
		t.Fatalf("promote: %v", err)
	}
	roundTrip, err := DowngradeToABI3(promoted)
	if err != nil {
		t.Fatalf("downgrade: %v", err)
	}
	if roundTrip != start {
		t.Errorf("round trip: got %q, want %q", roundTrip, start)
	}

	// And the reverse: downgrade-then-promote should reach the abi2026 form.
	startNew := "demo-1.0-cp314-abi2026-manylinux_2_28_x86_64.whl"
	downgraded, err := DowngradeToABI3(startNew)
	if err != nil {
		t.Fatalf("downgrade: %v", err)
	}
	roundTripNew, err := PromoteToABI2026(downgraded)
	if err != nil {
		t.Fatalf("promote: %v", err)
	}
	if roundTripNew != startNew {
		t.Errorf("reverse round trip: got %q, want %q", roundTripNew, startNew)
	}
}

func TestSplitWheelFilename(t *testing.T) {
	prefix, tag, err := splitWheelFilename("demo-1.0-cp32-abi3-manylinux_2_28_x86_64.whl")
	if err != nil {
		t.Fatalf("splitWheelFilename error %v", err)
	}
	if prefix != "demo-1.0" {
		t.Errorf("prefix = %q, want %q", prefix, "demo-1.0")
	}
	if tag != "cp32-abi3-manylinux_2_28_x86_64" {
		t.Errorf("tag = %q, want %q", tag, "cp32-abi3-manylinux_2_28_x86_64")
	}

	if _, _, err := splitWheelFilename("foo"); err == nil {
		t.Error("expected error for no .whl suffix")
	}
	if _, _, err := splitWheelFilename("too-few.whl"); err == nil {
		t.Error("expected error for too-few fields")
	}
}
