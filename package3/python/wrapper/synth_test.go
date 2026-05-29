package wrapper

import (
	"strings"
	"testing"

	"mochi/package3/python/errors"
	"mochi/package3/python/stubs"
)

func TestSynthesiseRequiresPackageName(t *testing.T) {
	_, err := Synthesise("", &stubs.ModuleSurface{}, Options{})
	if err == nil {
		t.Fatal("expected error for empty package name")
	}
}

func TestSynthesiseRequiresValidPyIdent(t *testing.T) {
	for _, bad := range []string{"1numpy", "-foo", "no-dash", "with space", "py.ki"} {
		if _, err := Synthesise(bad, &stubs.ModuleSurface{}, Options{}); err == nil {
			t.Errorf("expected error for invalid package name %q", bad)
		}
	}
	for _, good := range []string{"numpy", "PIL", "_underscore", "pkg2", "snake_case"} {
		if _, err := Synthesise(good, &stubs.ModuleSurface{}, Options{}); err != nil {
			t.Errorf("good name %q rejected: %v", good, err)
		}
	}
}

func TestSynthesiseRequiresSurface(t *testing.T) {
	_, err := Synthesise("foo", nil, Options{})
	if err == nil {
		t.Fatal("expected error for nil surface")
	}
}

func TestSynthesiseEmptyModule(t *testing.T) {
	w, err := Synthesise("emptypkg", &stubs.ModuleSurface{}, Options{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if w.Package != "emptypkg" || w.Module != "emptypkg_externs" {
		t.Errorf("metadata = %+v", w)
	}
	if !strings.Contains(w.PySource, "import emptypkg as _src") {
		t.Errorf("PySource missing source import: %q", w.PySource)
	}
	if !strings.Contains(w.PySource, "from ._mochi_wrap import _to_mochi_dict") {
		t.Errorf("PySource missing runtime helper import: %q", w.PySource)
	}
	if len(w.Items) != 0 || len(w.Skipped) != 0 {
		t.Errorf("expected empty items+skipped, got items=%d skipped=%d", len(w.Items), len(w.Skipped))
	}
}

func TestSynthesisePlainFunction(t *testing.T) {
	surface := &stubs.ModuleSurface{
		Functions: []stubs.FuncDecl{
			{
				Name: "add",
				Params: []stubs.ParamDecl{
					{Name: "a", Type: "int"},
					{Name: "b", Type: "int"},
				},
				ReturnType: "int",
			},
		},
	}
	w, err := Synthesise("math2", surface, Options{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(w.Items) != 1 {
		t.Fatalf("items = %+v", w.Items)
	}
	item := w.Items[0]
	if item.Name != "add" || item.SourceName != "math2.add" || item.Kind != ItemFunc {
		t.Errorf("item = %+v", item)
	}
	if item.IsAsync {
		t.Error("plain fn should not be async")
	}
	if !strings.Contains(w.PySource, "def add(arg0, arg1):") {
		t.Errorf("missing def: %s", w.PySource)
	}
	if !strings.Contains(w.PySource, "return _src.add(arg0, arg1)") {
		t.Errorf("missing forward call: %s", w.PySource)
	}
	if !strings.Contains(w.PYISource, "def add(arg0: int, arg1: int) -> int: ...") {
		t.Errorf("missing .pyi signature: %s", w.PYISource)
	}
}

func TestSynthesiseAsyncFunction(t *testing.T) {
	surface := &stubs.ModuleSurface{
		Functions: []stubs.FuncDecl{
			{
				Name:       "fetch",
				IsAsync:    true,
				Params:     []stubs.ParamDecl{{Name: "url", Type: "str"}},
				ReturnType: "bytes",
			},
		},
	}
	w, err := Synthesise("httpx_lite", surface, Options{Loop: EventLoopPerCall})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(w.Items) != 1 {
		t.Fatalf("items = %+v", w.Items)
	}
	item := w.Items[0]
	if item.Name != "fetch_sync" || !item.IsAsync || item.Loop != EventLoopPerCall {
		t.Errorf("item = %+v", item)
	}
	if !strings.Contains(w.PySource, "import asyncio") {
		t.Errorf("async wrapper should import asyncio: %s", w.PySource)
	}
	if !strings.Contains(w.PySource, "def fetch_sync(arg0):") {
		t.Errorf("missing sync entry: %s", w.PySource)
	}
	if !strings.Contains(w.PySource, "_run_async(_src.fetch(arg0))") {
		t.Errorf("missing _run_async: %s", w.PySource)
	}
	if !strings.Contains(w.PySource, "async def fetch_async(arg0):") {
		t.Errorf("missing async entry: %s", w.PySource)
	}
	if !strings.Contains(w.PySource, "return await _src.fetch(arg0)") {
		t.Errorf("missing await: %s", w.PySource)
	}
}

func TestSynthesisePersistentLoop(t *testing.T) {
	surface := &stubs.ModuleSurface{
		Functions: []stubs.FuncDecl{
			{
				Name:       "fetch",
				IsAsync:    true,
				Params:     []stubs.ParamDecl{{Name: "url", Type: "str"}},
				ReturnType: "bytes",
			},
		},
	}
	w, err := Synthesise("httpx_lite", surface, Options{Loop: EventLoopPersistent})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !strings.Contains(w.PySource, "_MOCHI_LOOP = _persistent_loop()") {
		t.Errorf("persistent loop init missing: %s", w.PySource)
	}
	if !strings.Contains(w.PySource, "_MOCHI_LOOP.run_until_complete(_src.fetch(arg0))") {
		t.Errorf("persistent loop dispatch missing: %s", w.PySource)
	}
	if !strings.Contains(w.PySource, "from ._mochi_wrap import _persistent_loop") {
		t.Errorf("persistent loop import missing: %s", w.PySource)
	}
}

func TestSynthesiseTypedDictRecord(t *testing.T) {
	surface := &stubs.ModuleSurface{
		Classes: []stubs.ClassDecl{
			{
				Name:        "Point",
				IsTypedDict: true,
				Fields: []stubs.FieldDecl{
					{Name: "x", Type: "int"},
					{Name: "y", Type: "int"},
				},
			},
		},
	}
	w, err := Synthesise("geom", surface, Options{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(w.Items) != 1 || w.Items[0].Kind != ItemRecord {
		t.Fatalf("items = %+v", w.Items)
	}
	if !strings.Contains(w.PySource, "Point = _src.Point") {
		t.Errorf("missing record re-export: %s", w.PySource)
	}
	if !strings.Contains(w.PySource, "def _Point_to_mochi_dict(value):") {
		t.Errorf("missing _to_mochi_dict companion: %s", w.PySource)
	}
	if !strings.Contains(w.PySource, `"x": _to_mochi_dict(getattr(value, "x"))`) {
		t.Errorf("missing field projection: %s", w.PySource)
	}
	if !strings.Contains(w.PYISource, "class Point(TypedDict, total=False):") {
		t.Errorf("missing TypedDict stub: %s", w.PYISource)
	}
	if !strings.Contains(w.PYISource, "    x: int") {
		t.Errorf("missing field stub: %s", w.PYISource)
	}
}

func TestSynthesiseProtocolInterface(t *testing.T) {
	surface := &stubs.ModuleSurface{
		Classes: []stubs.ClassDecl{
			{
				Name:       "Greeter",
				IsProtocol: true,
				Methods: []stubs.FuncDecl{
					{
						Name: "hello",
						Params: []stubs.ParamDecl{
							{Name: "self"},
							{Name: "who", Type: "str"},
						},
						ReturnType: "str",
					},
				},
			},
		},
	}
	w, err := Synthesise("pkg", surface, Options{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(w.Items) != 1 || w.Items[0].Kind != ItemInterface {
		t.Fatalf("items = %+v", w.Items)
	}
	if !strings.Contains(w.PySource, "Greeter = _src.Greeter") {
		t.Errorf("missing interface re-export: %s", w.PySource)
	}
	if !strings.Contains(w.PYISource, "class Greeter(Protocol):") {
		t.Errorf("missing Protocol stub: %s", w.PYISource)
	}
	if !strings.Contains(w.PYISource, "    def hello(self, arg0: str) -> str: ...") {
		t.Errorf("missing method stub: %s", w.PYISource)
	}
}

func TestSynthesiseMutableDataclassRefused(t *testing.T) {
	surface := &stubs.ModuleSurface{
		Classes: []stubs.ClassDecl{
			{
				Name:        "Mut",
				IsDataclass: true,
				Decorators:  []string{"dataclass"},
				Fields:      []stubs.FieldDecl{{Name: "v", Type: "int"}},
			},
		},
	}
	w, err := Synthesise("pkg", surface, Options{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(w.Items) != 0 {
		t.Errorf("expected refusal, got items = %+v", w.Items)
	}
	if len(w.Skipped) != 1 || w.Skipped[0].Reason != errors.SkipUnsupportedTypingConstruct {
		t.Errorf("skipped = %+v", w.Skipped)
	}
	if w.Skipped[0].ItemPath != "pkg.Mut" {
		t.Errorf("ItemPath should be qualified with package: %q", w.Skipped[0].ItemPath)
	}
	if w.Skipped[0].Override == "" {
		t.Error("expected override hint from class refusal")
	}
}

func TestSynthesiseFrozenDataclassRecord(t *testing.T) {
	surface := &stubs.ModuleSurface{
		Classes: []stubs.ClassDecl{
			{
				Name:        "Vec",
				IsDataclass: true,
				Decorators:  []string{"dataclass(frozen=True)"},
				Fields: []stubs.FieldDecl{
					{Name: "x", Type: "int"},
					{Name: "y", Type: "int"},
				},
			},
		},
	}
	w, err := Synthesise("pkg", surface, Options{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(w.Items) != 1 {
		t.Fatalf("items = %+v", w.Items)
	}
	item := w.Items[0]
	if item.Kind != ItemRecord || item.Name != "Vec" {
		t.Errorf("item = %+v", item)
	}
}

func TestSynthesisePrivateNameSkipped(t *testing.T) {
	surface := &stubs.ModuleSurface{
		Functions: []stubs.FuncDecl{
			{Name: "_internal", ReturnType: "int"},
			{Name: "public", ReturnType: "int"},
		},
		Classes: []stubs.ClassDecl{
			{Name: "_PrivCls", IsTypedDict: true},
		},
		Constants: []stubs.ConstantDecl{
			{Name: "_VERSION", Type: "str"},
		},
	}
	w, err := Synthesise("pkg", surface, Options{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(w.Items) != 1 || w.Items[0].SourceName != "pkg.public" {
		t.Errorf("items = %+v", w.Items)
	}
	if len(w.Skipped) != 3 {
		t.Fatalf("expected 3 private skips, got %+v", w.Skipped)
	}
	for _, s := range w.Skipped {
		if s.Reason != errors.SkipPrivateName {
			t.Errorf("skip reason = %v (want SkipPrivateName)", s.Reason)
		}
	}
}

func TestSynthesiseRefusedItemQualified(t *testing.T) {
	surface := &stubs.ModuleSurface{
		Functions: []stubs.FuncDecl{
			{
				Name: "blob",
				Params: []stubs.ParamDecl{
					{Name: "z", Type: "complex"},
				},
				ReturnType: "int",
			},
		},
	}
	w, err := Synthesise("pkg", surface, Options{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(w.Items) != 0 {
		t.Errorf("expected refusal, items = %+v", w.Items)
	}
	if len(w.Skipped) != 1 {
		t.Fatalf("skipped = %+v", w.Skipped)
	}
	if w.Skipped[0].ItemPath != "pkg.blob" {
		t.Errorf("ItemPath should be qualified: %q", w.Skipped[0].ItemPath)
	}
	if w.Skipped[0].Reason != errors.SkipNoComplexType {
		t.Errorf("reason = %v", w.Skipped[0].Reason)
	}
}

func TestSynthesiseConstantsAndUnannotated(t *testing.T) {
	surface := &stubs.ModuleSurface{
		Constants: []stubs.ConstantDecl{
			{Name: "VERSION", Type: "str", Default: `"1.0"`},
			{Name: "BAD", Type: ""},
		},
	}
	w, err := Synthesise("pkg", surface, Options{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(w.Items) != 1 || w.Items[0].Name != "VERSION" {
		t.Errorf("items = %+v", w.Items)
	}
	if !strings.Contains(w.PySource, "VERSION = _src.VERSION") {
		t.Errorf("missing constant re-export: %s", w.PySource)
	}
	if !strings.Contains(w.PYISource, "VERSION: str") {
		t.Errorf("missing constant stub: %s", w.PYISource)
	}
	if len(w.Skipped) != 1 || w.Skipped[0].Reason != errors.SkipUnsupportedTypingConstruct {
		t.Errorf("skipped = %+v", w.Skipped)
	}
}

func TestSynthesiseAllowPartialAny(t *testing.T) {
	surface := &stubs.ModuleSurface{
		Functions: []stubs.FuncDecl{
			{
				Name:       "echo",
				Params:     []stubs.ParamDecl{{Name: "x", Type: "Any"}},
				ReturnType: "Any",
			},
		},
	}
	// Default: Any refused.
	w, err := Synthesise("pkg", surface, Options{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(w.Items) != 0 {
		t.Errorf("expected refusal under default, got %+v", w.Items)
	}
	// AllowPartial: Any allowed.
	w, err = Synthesise("pkg", surface, Options{AllowPartial: true})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(w.Items) != 1 {
		t.Errorf("AllowPartial: expected 1 item, got %+v", w.Items)
	}
}

func TestSynthesiseStableOrder(t *testing.T) {
	surface := &stubs.ModuleSurface{
		Functions: []stubs.FuncDecl{
			{Name: "zeta", ReturnType: "int"},
			{Name: "alpha", ReturnType: "int"},
			{Name: "mu", ReturnType: "int"},
		},
	}
	w, err := Synthesise("pkg", surface, Options{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	names := []string{w.Items[0].SourceName, w.Items[1].SourceName, w.Items[2].SourceName}
	want := []string{"pkg.alpha", "pkg.mu", "pkg.zeta"}
	for i := range names {
		if names[i] != want[i] {
			t.Errorf("Items not sorted: got %v, want %v", names, want)
		}
	}
}

func TestSkippedText(t *testing.T) {
	w := &Wrapper{
		Package: "pkg",
		Skipped: []errors.SkipReport{
			{
				ItemPath: "pkg.bad",
				Reason:   errors.SkipNoComplexType,
				Detail:   "complex parameter z",
			},
		},
	}
	text := w.SkippedText()
	if !strings.Contains(text, "SKIPPED: pkg.bad") {
		t.Errorf("SkippedText missing entry: %s", text)
	}
	if !strings.Contains(text, "SkipNoComplexType") {
		t.Errorf("SkippedText missing reason: %s", text)
	}
}

func TestSkippedTextEmpty(t *testing.T) {
	w := &Wrapper{Package: "pkg"}
	if !strings.Contains(w.SkippedText(), "No items skipped") {
		t.Errorf("empty SkippedText shape: %s", w.SkippedText())
	}
}

func TestRuntimePresentAndStable(t *testing.T) {
	rt := Runtime()
	if !strings.Contains(rt, "def _run_async(awaitable") {
		t.Errorf("Runtime missing _run_async: %s", rt)
	}
	if !strings.Contains(rt, "def _persistent_loop()") {
		t.Errorf("Runtime missing _persistent_loop: %s", rt)
	}
	if !strings.Contains(rt, "def _materialise_iter(it") {
		t.Errorf("Runtime missing _materialise_iter: %s", rt)
	}
	if !strings.Contains(rt, "def _to_mochi_dict(value") {
		t.Errorf("Runtime missing _to_mochi_dict: %s", rt)
	}
	if Runtime() != Runtime() {
		t.Error("Runtime not deterministic")
	}
	if RuntimeStub() == "" {
		t.Error("RuntimeStub empty")
	}
}

func TestPYIIncludesTypingPrelude(t *testing.T) {
	w, _ := Synthesise("pkg", &stubs.ModuleSurface{}, Options{})
	for _, sym := range []string{"Awaitable", "AsyncIterator", "Optional", "Union", "Protocol", "TypedDict"} {
		if !strings.Contains(w.PYISource, sym) {
			t.Errorf("PYISource missing %s import: %s", sym, w.PYISource)
		}
	}
}
