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

func TestListSection(t *testing.T) {
	for _, sec := range []Section{SectionTutorial, SectionChallenge} {
		probs, err := ListSection(sec, 0)
		if err != nil {
			t.Logf("section %s page 0 error (may be empty): %v", sec, err)
			continue
		}
		t.Logf("section %s: %d problems on page 0", sec, len(probs))
		for _, p := range probs {
			if p.Section != sec {
				t.Errorf("problem %d has section %q, want %q", p.ID, p.Section, sec)
			}
		}
	}
}
