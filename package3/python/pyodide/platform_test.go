package pyodide

import (
	"strings"
	"testing"
)

func TestParsePlatformTagPyodide(t *testing.T) {
	tag, err := ParsePlatformTag("pyodide_2024_0_wasm32")
	if err != nil {
		t.Fatalf("ParsePlatformTag: %v", err)
	}
	if tag.Kind != TagPyodide {
		t.Errorf("Kind = %v", tag.Kind)
	}
	if tag.Vintage != (Vintage{Year: 2024, Rev: 0}) {
		t.Errorf("Vintage = %+v", tag.Vintage)
	}
	if tag.Raw != "pyodide_2024_0_wasm32" {
		t.Errorf("Raw = %q", tag.Raw)
	}
}

func TestParsePlatformTagEmscripten(t *testing.T) {
	tag, err := ParsePlatformTag("emscripten_3_1_45_wasm32")
	if err != nil {
		t.Fatalf("ParsePlatformTag: %v", err)
	}
	if tag.Kind != TagEmscripten {
		t.Errorf("Kind = %v", tag.Kind)
	}
	if tag.Emscripten != (EmscriptenVersion{Major: 3, Minor: 1, Patch: 45}) {
		t.Errorf("Emscripten = %+v", tag.Emscripten)
	}
	if got := tag.Emscripten.String(); got != "3.1.45" {
		t.Errorf("String = %q", got)
	}
}

func TestParsePlatformTagWASI(t *testing.T) {
	tag, err := ParsePlatformTag("wasi_p2_wasm32")
	if err != nil {
		t.Fatalf("ParsePlatformTag: %v", err)
	}
	if tag.Kind != TagWASIPreview2 {
		t.Errorf("Kind = %v", tag.Kind)
	}

	tag, err = ParsePlatformTag("wasi_p2")
	if err != nil {
		t.Fatalf("ParsePlatformTag: %v", err)
	}
	if tag.Kind != TagWASIPreview2 {
		t.Errorf("Kind = %v", tag.Kind)
	}
}

func TestParsePlatformTagErrors(t *testing.T) {
	cases := []struct {
		raw  string
		want string
	}{
		{"manylinux_2_28_x86_64", "unrecognised wasm"},
		{"pyodide_2024_0", "missing _wasm32"},
		{"pyodide__wasm32", "must be pyodide"},
		{"pyodide_abc_0_wasm32", "bad year"},
		{"pyodide_2024_abc_wasm32", "bad rev"},
		{"pyodide_-1_0_wasm32", "bad year"},
		{"emscripten_3_1_45", "missing _wasm32"},
		{"emscripten_3_1_wasm32", "must be emscripten"},
		{"emscripten_abc_1_2_wasm32", "bad major"},
		{"emscripten_3_x_2_wasm32", "bad minor"},
		{"emscripten_3_1_x_wasm32", "bad patch"},
		{"", "unrecognised wasm"},
	}
	for _, tc := range cases {
		_, err := ParsePlatformTag(tc.raw)
		if err == nil {
			t.Errorf("ParsePlatformTag(%q) expected error", tc.raw)
			continue
		}
		if !strings.Contains(err.Error(), tc.want) {
			t.Errorf("ParsePlatformTag(%q) error %q does not contain %q", tc.raw, err.Error(), tc.want)
		}
	}
}

func TestTagSatisfiesRuntime(t *testing.T) {
	pyodide, _ := ParsePlatformTag("pyodide_2024_0_wasm32")
	emscripten, _ := ParsePlatformTag("emscripten_3_1_45_wasm32")
	wasi, _ := ParsePlatformTag("wasi_p2_wasm32")

	if !pyodide.SatisfiesRuntime(RuntimePyodide) {
		t.Error("pyodide should satisfy Pyodide")
	}
	if !emscripten.SatisfiesRuntime(RuntimePyodide) {
		t.Error("emscripten should satisfy Pyodide")
	}
	if pyodide.SatisfiesRuntime(RuntimeWASIPreview2) {
		t.Error("pyodide should NOT satisfy WASI")
	}
	if !wasi.SatisfiesRuntime(RuntimeWASIPreview2) {
		t.Error("wasi should satisfy WASI")
	}
	if wasi.SatisfiesRuntime(RuntimePyodide) {
		t.Error("wasi should NOT satisfy Pyodide")
	}
	if wasi.SatisfiesRuntime(RuntimeUnknown) {
		t.Error("nothing should satisfy Unknown")
	}
}

func TestTagKindString(t *testing.T) {
	cases := map[TagKind]string{
		TagUnknown:      "unknown",
		TagPyodide:      "pyodide",
		TagEmscripten:   "emscripten",
		TagWASIPreview2: "wasi-p2",
	}
	for k, want := range cases {
		if got := k.String(); got != want {
			t.Errorf("%d.String() = %q, want %q", k, got, want)
		}
	}
}
