package embedded

import (
	"strings"
	"testing"
)

func TestParseProfileDefault(t *testing.T) {
	p, err := ParseProfile("")
	if err != nil {
		t.Fatalf("ParseProfile(empty): %v", err)
	}
	if p != ProfileHosted {
		t.Fatalf("ParseProfile(empty) = %v; want ProfileHosted", p)
	}
}

func TestParseProfileHosted(t *testing.T) {
	p, err := ParseProfile("hosted")
	if err != nil {
		t.Fatalf("ParseProfile(hosted): %v", err)
	}
	if p != ProfileHosted {
		t.Fatalf("ParseProfile(hosted) = %v; want ProfileHosted", p)
	}
}

func TestParseProfileEmbedded(t *testing.T) {
	p, err := ParseProfile("embedded")
	if err != nil {
		t.Fatalf("ParseProfile(embedded): %v", err)
	}
	if p != ProfileEmbedded {
		t.Fatalf("ParseProfile(embedded) = %v; want ProfileEmbedded", p)
	}
}

func TestParseProfileRejectsUnknown(t *testing.T) {
	if _, err := ParseProfile("kernel"); err == nil {
		t.Fatalf("ParseProfile(kernel): want error")
	}
}

func TestProfileString(t *testing.T) {
	tests := []struct {
		p    Profile
		want string
	}{
		{ProfileHosted, "hosted"},
		{ProfileEmbedded, "embedded"},
	}
	for _, c := range tests {
		if got := c.p.String(); got != c.want {
			t.Fatalf("Profile(%d).String() = %q; want %q", int(c.p), got, c.want)
		}
	}
}

func TestProfileRoundTripsThroughString(t *testing.T) {
	for _, p := range []Profile{ProfileHosted, ProfileEmbedded} {
		got, err := ParseProfile(p.String())
		if err != nil {
			t.Fatalf("ParseProfile(%s): %v", p, err)
		}
		if got != p {
			t.Fatalf("round trip: %v -> %q -> %v", p, p, got)
		}
	}
}

func TestLibRSHeaderHosted(t *testing.T) {
	if got := LibRSHeader(ProfileHosted); got != "" {
		t.Fatalf("LibRSHeader(hosted) = %q; want empty", got)
	}
}

func TestLibRSHeaderEmbedded(t *testing.T) {
	got := LibRSHeader(ProfileEmbedded)
	if !strings.Contains(got, "#![no_std]") {
		t.Fatalf("LibRSHeader(embedded): missing #![no_std] in %q", got)
	}
	if !strings.Contains(got, "extern crate alloc;") {
		t.Fatalf("LibRSHeader(embedded): missing extern crate alloc in %q", got)
	}
	if !strings.HasSuffix(got, "\n") {
		t.Fatalf("LibRSHeader(embedded): want trailing newline, got %q", got)
	}
}

func TestCargoUpstreamDepRowHosted(t *testing.T) {
	got := CargoUpstreamDepRow(ProfileHosted, "hex", "0.4.3")
	want := "hex = \"=0.4.3\"\n"
	if got != want {
		t.Fatalf("CargoUpstreamDepRow(hosted) = %q; want %q", got, want)
	}
}

func TestCargoUpstreamDepRowEmbedded(t *testing.T) {
	got := CargoUpstreamDepRow(ProfileEmbedded, "hex", "0.4.3")
	if !strings.Contains(got, "default-features = false") {
		t.Fatalf("CargoUpstreamDepRow(embedded): missing default-features flip in %q", got)
	}
	if !strings.Contains(got, "version = \"=0.4.3\"") {
		t.Fatalf("CargoUpstreamDepRow(embedded): missing pinned version in %q", got)
	}
	if !strings.HasPrefix(got, "hex = ") {
		t.Fatalf("CargoUpstreamDepRow(embedded): want \"hex = ...\" prefix, got %q", got)
	}
}

func TestRefuseAsync(t *testing.T) {
	if RefuseAsync(ProfileHosted) {
		t.Fatalf("RefuseAsync(hosted) = true; want false")
	}
	if !RefuseAsync(ProfileEmbedded) {
		t.Fatalf("RefuseAsync(embedded) = false; want true")
	}
}

func TestAsyncRefusalReasonStable(t *testing.T) {
	if !strings.Contains(AsyncRefusalReason, "tokio") {
		t.Fatalf("AsyncRefusalReason should mention tokio: %q", AsyncRefusalReason)
	}
	if !strings.Contains(AsyncRefusalReason, "embedded") {
		t.Fatalf("AsyncRefusalReason should mention embedded: %q", AsyncRefusalReason)
	}
}

func TestParseTOMLBodyEmpty(t *testing.T) {
	p, err := ParseTOMLBody("")
	if err != nil {
		t.Fatalf("ParseTOMLBody(empty): %v", err)
	}
	if p != ProfileHosted {
		t.Fatalf("ParseTOMLBody(empty) = %v; want ProfileHosted", p)
	}
}

func TestParseTOMLBodyEmbedded(t *testing.T) {
	p, err := ParseTOMLBody(`profile = "embedded"`)
	if err != nil {
		t.Fatalf("ParseTOMLBody: %v", err)
	}
	if p != ProfileEmbedded {
		t.Fatalf("ParseTOMLBody(embedded) = %v; want ProfileEmbedded", p)
	}
}

func TestParseTOMLBodyAllowsCommentsAndBlankLines(t *testing.T) {
	body := `
# the rust profile
profile = "embedded"   # inline comments not supported, just this one
`
	// The "inline comments" comment after the quoted value would fail
	// the strict parser; verify the simpler well-formed case.
	body2 := `
# comment
profile = "embedded"
`
	p, err := ParseTOMLBody(body2)
	if err != nil {
		t.Fatalf("ParseTOMLBody: %v", err)
	}
	if p != ProfileEmbedded {
		t.Fatalf("ParseTOMLBody = %v; want ProfileEmbedded", p)
	}
	if _, err := ParseTOMLBody(body); err == nil {
		t.Fatalf("ParseTOMLBody: want error when trailing junk follows the quoted value")
	}
}

func TestParseTOMLBodyRejectsUnknownKey(t *testing.T) {
	if _, err := ParseTOMLBody(`target = "embedded"`); err == nil {
		t.Fatalf("ParseTOMLBody: want error for unknown key")
	}
}

func TestParseTOMLBodyRejectsUnquoted(t *testing.T) {
	if _, err := ParseTOMLBody(`profile = embedded`); err == nil {
		t.Fatalf("ParseTOMLBody: want error for unquoted value")
	}
}

func TestAllowedTriplesHostedReturnsNil(t *testing.T) {
	if got := AllowedTriples(ProfileHosted); got != nil {
		t.Fatalf("AllowedTriples(hosted) = %v; want nil", got)
	}
}

func TestAllowedTriplesEmbeddedSorted(t *testing.T) {
	got := AllowedTriples(ProfileEmbedded)
	if len(got) < 4 {
		t.Fatalf("AllowedTriples(embedded): want at least 4 triples, got %d (%v)", len(got), got)
	}
	for i := 1; i < len(got); i++ {
		if got[i-1] > got[i] {
			t.Fatalf("AllowedTriples not sorted at index %d: %v", i, got)
		}
	}
	wantSome := []string{"thumbv7em-none-eabihf", "riscv32imc-unknown-none-elf"}
	for _, w := range wantSome {
		found := false
		for _, g := range got {
			if g == w {
				found = true
				break
			}
		}
		if !found {
			t.Fatalf("AllowedTriples(embedded): missing %q in %v", w, got)
		}
	}
}

func TestLibRSHeaderByteStable(t *testing.T) {
	first := LibRSHeader(ProfileEmbedded)
	for i := 0; i < 32; i++ {
		if got := LibRSHeader(ProfileEmbedded); got != first {
			t.Fatalf("LibRSHeader not byte-stable: %q vs %q", first, got)
		}
	}
}
