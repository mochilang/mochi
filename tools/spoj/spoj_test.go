//go:build slow

package spoj

import (
	"strings"
	"testing"
)

func TestListAndDownload(t *testing.T) {
	probs, err := List(1)
	if err != nil {
		t.Fatalf("List error: %v", err)
	}
	if len(probs) == 0 {
		t.Fatal("no problems returned")
	}
	p := probs[0]
	if p.URL == "" {
		t.Fatal("empty problem URL")
	}
	md, err := Download(p.ID)
	if err != nil {
		t.Fatalf("Download error: %v", err)
	}
	if !strings.Contains(md, p.Name) {
		t.Fatalf("markdown does not contain problem name")
	}
}
