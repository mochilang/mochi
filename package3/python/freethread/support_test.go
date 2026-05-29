package freethread

import "testing"

func TestSupportLevelString(t *testing.T) {
	cases := map[SupportLevel]string{
		SupportUnknown:      "unknown",
		SupportFull:         "full",
		SupportUntested:     "untested",
		SupportIncompatible: "incompatible",
	}
	for level, want := range cases {
		if got := level.String(); got != want {
			t.Errorf("%d.String() = %q, want %q", level, got, want)
		}
	}
}

func TestWheelClassString(t *testing.T) {
	cases := map[WheelClass]string{
		WheelClassUnknown:      "unknown",
		WheelClassPure:         "pure",
		WheelClassFreeThreaded: "free-threaded",
		WheelClassLegacy:       "legacy",
		WheelClassDenied:       "denied",
	}
	for cls, want := range cases {
		if got := cls.String(); got != want {
			t.Errorf("%d.String() = %q, want %q", cls, got, want)
		}
	}
}

func TestWheelCompatPureAlwaysFull(t *testing.T) {
	c := WheelCompat{Target: ABITag{Interpreter: "cp", Major: 3, Minor: 13, FreeThreaded: true}}
	cls, lvl, err := c.Score(ResolvedWheel{Distribution: "click", Pure: true})
	if err != nil {
		t.Fatalf("Score: %v", err)
	}
	if cls != WheelClassPure || lvl != SupportFull {
		t.Errorf("class=%v level=%v, want pure/full", cls, lvl)
	}
}

func TestWheelCompatFreeThreadedExtension(t *testing.T) {
	c := WheelCompat{Target: ABITag{Interpreter: "cp", Major: 3, Minor: 13, FreeThreaded: true}}
	cls, lvl, err := c.Score(ResolvedWheel{Distribution: "numpy", ABI: "cp313t"})
	if err != nil {
		t.Fatalf("Score: %v", err)
	}
	if cls != WheelClassFreeThreaded || lvl != SupportFull {
		t.Errorf("class=%v level=%v, want free-threaded/full", cls, lvl)
	}
}

func TestWheelCompatLegacyOnFreeThreaded(t *testing.T) {
	c := WheelCompat{Target: ABITag{Interpreter: "cp", Major: 3, Minor: 13, FreeThreaded: true}}
	cls, lvl, err := c.Score(ResolvedWheel{Distribution: "old-pkg", ABI: "cp313"})
	if err != nil {
		t.Fatalf("Score: %v", err)
	}
	if cls != WheelClassLegacy || lvl != SupportUntested {
		t.Errorf("class=%v level=%v, want legacy/untested", cls, lvl)
	}
}

func TestWheelCompatLegacyOnLegacy(t *testing.T) {
	c := WheelCompat{Target: ABITag{Interpreter: "cp", Major: 3, Minor: 13, FreeThreaded: false}}
	cls, lvl, err := c.Score(ResolvedWheel{Distribution: "old-pkg", ABI: "cp313"})
	if err != nil {
		t.Fatalf("Score: %v", err)
	}
	if cls != WheelClassLegacy || lvl != SupportFull {
		t.Errorf("class=%v level=%v, want legacy/full", cls, lvl)
	}
}

func TestWheelCompatDeniedOverridesABITag(t *testing.T) {
	c := WheelCompat{
		Target:   ABITag{Interpreter: "cp", Major: 3, Minor: 13, FreeThreaded: true},
		DenyList: map[string]struct{}{"lxml": {}},
	}
	cls, lvl, err := c.Score(ResolvedWheel{Distribution: "lxml", ABI: "cp313t"})
	if err != nil {
		t.Fatalf("Score: %v", err)
	}
	if cls != WheelClassDenied || lvl != SupportIncompatible {
		t.Errorf("class=%v level=%v, want denied/incompatible", cls, lvl)
	}
}

func TestWheelCompatRejectsMinorMismatch(t *testing.T) {
	c := WheelCompat{Target: ABITag{Interpreter: "cp", Major: 3, Minor: 13, FreeThreaded: true}}
	_, lvl, err := c.Score(ResolvedWheel{Distribution: "pkg", ABI: "cp314t"})
	if err == nil {
		t.Fatal("expected minor mismatch error")
	}
	if lvl != SupportIncompatible {
		t.Errorf("level = %v, want incompatible", lvl)
	}
}

func TestWheelCompatRejectsBadABI(t *testing.T) {
	c := WheelCompat{Target: ABITag{Interpreter: "cp", Major: 3, Minor: 13, FreeThreaded: true}}
	_, lvl, err := c.Score(ResolvedWheel{Distribution: "pkg", ABI: "garbage"})
	if err == nil {
		t.Fatal("expected ABI parse error")
	}
	if lvl != SupportIncompatible {
		t.Errorf("level = %v, want incompatible", lvl)
	}
}
