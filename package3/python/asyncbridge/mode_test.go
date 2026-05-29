package asyncbridge

import "testing"

func TestParseModeAccepted(t *testing.T) {
	cases := []struct {
		in   string
		want Mode
	}{
		{"", PerCall},
		{"per-call", PerCall},
		{"percall", PerCall},
		{"persistent", Persistent},
	}
	for _, c := range cases {
		got, err := ParseMode(c.in)
		if err != nil {
			t.Errorf("ParseMode(%q) error: %v", c.in, err)
			continue
		}
		if got != c.want {
			t.Errorf("ParseMode(%q) = %v, want %v", c.in, got, c.want)
		}
	}
}

func TestParseModeRejected(t *testing.T) {
	cases := []string{"PerCall", "Persistent", "asyncio.run", "loop", "foo"}
	for _, c := range cases {
		if _, err := ParseMode(c); err == nil {
			t.Errorf("ParseMode(%q) should error", c)
		}
	}
}

func TestModeString(t *testing.T) {
	cases := map[Mode]string{
		PerCall:    "per-call",
		Persistent: "persistent",
	}
	for m, want := range cases {
		if got := m.String(); got != want {
			t.Errorf("Mode(%d).String() = %q, want %q", m, got, want)
		}
	}
	other := Mode(99)
	if got := other.String(); got == "" {
		t.Errorf("Mode(99).String() returned empty; want fallback")
	}
}

func TestModeRoundTrip(t *testing.T) {
	for _, m := range []Mode{PerCall, Persistent} {
		got, err := ParseMode(m.String())
		if err != nil {
			t.Errorf("ParseMode(%q) error: %v", m.String(), err)
			continue
		}
		if got != m {
			t.Errorf("ParseMode(%q) round-trip = %v, want %v", m.String(), got, m)
		}
	}
}
