package main

import "testing"

func TestSample(t *testing.T) {
	intervals := [][2]int{{1, 8}, {5, 8}, {7, 10}, {8, 9}}
	min, max := solve(5, 10, intervals)
	if min != 1 || max != 4 {
		t.Fatalf("expected 1 4, got %d %d", min, max)
	}
}

func TestEmptyIntersection(t *testing.T) {
	intervals := [][2]int{{1, 2}, {3, 4}}
	min, max := solve(5, 6, intervals)
	if min != 0 || max != 0 {
		t.Fatalf("expected 0 0, got %d %d", min, max)
	}
}
