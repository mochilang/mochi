package pas_test

import (
	"testing"

	"mochi/tools/a2mochi/x/pas"
)

func TestParse_NotImplemented(t *testing.T) {
	if _, err := pas.Parse("{}"); err == nil {
		t.Fatalf("expected error")
	}
}

func TestConvert_NotImplemented(t *testing.T) {
	if _, err := pas.Convert(&pas.Node{}); err == nil {
		t.Fatalf("expected error")
	}
}
