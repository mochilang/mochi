package ffi_test

import (
	"testing"

	"mochi/runtime/mochi/ffi"
)

func TestSealRoundTripInt(t *testing.T) {
	h := int64(42)
	if got := ffi.Unseal(ffi.Seal(h)); got != h {
		t.Errorf("round-trip int64: got %d, want %d", got, h)
	}
}

func TestSealRoundTripList(t *testing.T) {
	src := []int64{1, 2, 3}
	got := ffi.Unseal(ffi.Seal(src))
	if len(got) != len(src) {
		t.Fatalf("len mismatch: %d vs %d", len(got), len(src))
	}
	for i := range src {
		if got[i] != src[i] {
			t.Errorf("idx %d: got %d, want %d", i, got[i], src[i])
		}
	}
}

func TestSealRoundTripMap(t *testing.T) {
	src := map[int64]int64{1: 10, 2: 20}
	got := ffi.Unseal(ffi.Seal(src))
	if len(got) != len(src) {
		t.Fatalf("len mismatch: %d vs %d", len(got), len(src))
	}
	for k, v := range src {
		if got[k] != v {
			t.Errorf("key %d: got %d, want %d", k, got[k], v)
		}
	}
}
