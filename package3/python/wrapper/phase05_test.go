package wrapper

import (
	"strings"
	"testing"

	"mochi/package3/python/errors"
	"mochi/package3/python/stubs"
)

// TestPhase5WrapperSynthesiser is the umbrella sentinel for MEP-71 Phase 5.
// It walks a representative `.pyi` fixture all the way through the wrapper
// synthesiser and asserts the Python source + .pyi + skip set are well-formed.
func TestPhase5WrapperSynthesiser(t *testing.T) {
	t.Run("end_to_end_pyi", func(t *testing.T) {
		// A miniature stub modelled on the kind of surface a typed PyPI
		// package exposes: a plain function, an async function, a
		// TypedDict, a Protocol, a frozen dataclass, plus a refused
		// (complex-typed) parameter and a private leading-underscore name.
		src := `from typing import Protocol, TypedDict

VERSION: str

def add(a: int, b: int) -> int: ...

async def fetch(url: str) -> bytes: ...

def _private() -> int: ...

def broken(z: complex) -> int: ...

class Point(TypedDict):
    x: int
    y: int

class Greeter(Protocol):
    def hello(self, who: str) -> str: ...
`
		surface, err := stubs.ParsePYI(src)
		if err != nil {
			t.Fatalf("ParsePYI: %v", err)
		}
		w, err := Synthesise("demo", surface, Options{})
		if err != nil {
			t.Fatalf("Synthesise: %v", err)
		}
		// Items expected: VERSION, Greeter, Point, add, fetch_sync. Five total,
		// in sorted SourceName order.
		wantNames := []string{
			"demo.Greeter",
			"demo.Point",
			"demo.VERSION",
			"demo.add",
			"demo.fetch",
		}
		if len(w.Items) != len(wantNames) {
			t.Fatalf("Items count = %d, want %d (%+v)", len(w.Items), len(wantNames), w.Items)
		}
		for i, want := range wantNames {
			if w.Items[i].SourceName != want {
				t.Errorf("Items[%d].SourceName = %q, want %q", i, w.Items[i].SourceName, want)
			}
		}
		// Skipped: _private (private) + broken (complex parameter).
		if len(w.Skipped) != 2 {
			t.Fatalf("Skipped = %+v", w.Skipped)
		}
		reasons := map[errors.SkipReason]int{}
		for _, s := range w.Skipped {
			reasons[s.Reason]++
		}
		if reasons[errors.SkipPrivateName] != 1 || reasons[errors.SkipNoComplexType] != 1 {
			t.Errorf("Skip reasons = %+v", reasons)
		}
	})

	t.Run("py_source_round_trip", func(t *testing.T) {
		src := `def add(a: int, b: int) -> int: ...
async def fetch(url: str) -> bytes: ...
`
		surface, _ := stubs.ParsePYI(src)
		w, _ := Synthesise("demo", surface, Options{Loop: EventLoopPersistent})
		py := w.PySource
		for _, s := range []string{
			"import asyncio",
			"import demo as _src",
			"from ._mochi_wrap import _persistent_loop, _run_async, _to_mochi_dict",
			"def add(arg0, arg1):",
			"return _src.add(arg0, arg1)",
			"def fetch_sync(arg0):",
			"_MOCHI_LOOP.run_until_complete(_src.fetch(arg0))",
			"async def fetch_async(arg0):",
			"return await _src.fetch(arg0)",
			"_MOCHI_LOOP = _persistent_loop()",
		} {
			if !strings.Contains(py, s) {
				t.Errorf("PySource missing %q\n--- got ---\n%s", s, py)
			}
		}
	})

	t.Run("pyi_round_trip", func(t *testing.T) {
		src := `def add(a: int, b: int) -> int: ...
class Point(TypedDict):
    x: int
    y: int
`
		surface, _ := stubs.ParsePYI(src)
		w, _ := Synthesise("demo", surface, Options{})
		pyi := w.PYISource
		for _, s := range []string{
			"def add(arg0: int, arg1: int) -> int: ...",
			"class Point(TypedDict, total=False):",
			"    x: int",
			"    y: int",
		} {
			if !strings.Contains(pyi, s) {
				t.Errorf("PYISource missing %q\n--- got ---\n%s", s, pyi)
			}
		}
	})

	t.Run("skipped_text_round_trip", func(t *testing.T) {
		src := `def f(z: complex) -> int: ...`
		surface, _ := stubs.ParsePYI(src)
		w, _ := Synthesise("demo", surface, Options{})
		text := w.SkippedText()
		if !strings.Contains(text, "SKIPPED: demo.f") {
			t.Errorf("SkippedText missing entry: %s", text)
		}
		if !strings.Contains(text, "SkipNoComplexType") {
			t.Errorf("SkippedText missing reason: %s", text)
		}
	})

	t.Run("runtime_stable", func(t *testing.T) {
		if Runtime() != Runtime() || RuntimeStub() != RuntimeStub() {
			t.Error("runtime helpers not stable")
		}
	})
}
