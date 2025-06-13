package leetcode

import (
	"strings"
	"testing"
)

// Simple smoke test to ensure Download returns content for a known problem.
func TestDownload(t *testing.T) {
	content, err := Download(1)
	if err != nil {
		t.Fatalf("download failed: %v", err)
	}
	if len(content) == 0 {
		t.Fatal("empty content")
	}
	if !contains(content, "array of integers") {
		t.Fatalf("unexpected content: %q", content[:50])
	}
}

func contains(s, substr string) bool {
	return strings.Contains(s, substr)
}
