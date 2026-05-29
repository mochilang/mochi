package emit

import (
	"strings"
	"testing"

	"mochi/package3/python/stubs"
	"mochi/package3/python/wrapper"
)

// TestPhase6ExternEmit is the umbrella sentinel for MEP-71 Phase 6. It walks
// a representative `.pyi` source all the way through the stub parser, the
// wrapper synthesiser, and the Mochi shim emitter, then asserts the shim
// contains the expected mix of `type ... = { ... }` aliases, opaque extern
// types, extern vars, and extern python fun declarations.
func TestPhase6ExternEmit(t *testing.T) {
	t.Run("end_to_end_pyi", func(t *testing.T) {
		src := `from typing import Protocol, TypedDict

VERSION: str

def add(a: int, b: int) -> int: ...

async def fetch(url: str) -> bytes: ...

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
		w, err := wrapper.Synthesise("demo", surface, wrapper.Options{})
		if err != nil {
			t.Fatalf("Synthesise: %v", err)
		}
		s, err := EmitShim(w, DefaultOptions())
		if err != nil {
			t.Fatalf("EmitShim: %v", err)
		}
		for _, want := range []string{
			"// MEP-71 Mochi shim for source package: demo",
			"type Point = {",
			"  x: int,",
			"  y: int",
			"}",
			"extern python type Greeter",
			"extern python var VERSION: string",
			"extern python fun add(int, int): int",
			"extern python fun fetch_sync(string): async bytes",
			"// SKIPPED items",
			"//   demo.broken (SkipNoComplexType)",
		} {
			if !strings.Contains(s.Source, want) {
				t.Errorf("missing %q in shim:\n%s", want, s.Source)
			}
		}
	})

	t.Run("deterministic_hash", func(t *testing.T) {
		src := `def f(x: int) -> int: ...`
		surface, _ := stubs.ParsePYI(src)
		w, _ := wrapper.Synthesise("p", surface, wrapper.Options{})
		s1, _ := EmitShim(w, DefaultOptions())
		s2, _ := EmitShim(w, DefaultOptions())
		if s1.SHA256 != s2.SHA256 || s1.Source != s2.Source {
			t.Errorf("non-deterministic: %s vs %s", s1.SHA256, s2.SHA256)
		}
	})

	t.Run("file_basename", func(t *testing.T) {
		src := `def f() -> int: ...`
		surface, _ := stubs.ParsePYI(src)
		w, _ := wrapper.Synthesise("requests", surface, wrapper.Options{})
		s, _ := EmitShim(w, DefaultOptions())
		if s.Module != "requests_shim" {
			t.Errorf("module = %q", s.Module)
		}
	})

	t.Run("persistent_loop_header_marker", func(t *testing.T) {
		src := `async def fetch(url: str) -> bytes: ...`
		surface, _ := stubs.ParsePYI(src)
		w, _ := wrapper.Synthesise("p", surface, wrapper.Options{Loop: wrapper.EventLoopPersistent})
		s, _ := EmitShim(w, DefaultOptions())
		if !strings.Contains(s.Source, "Loop: persistent") {
			t.Errorf("header should record loop mode: %s", s.Source)
		}
	})
}
