package pyodide

import (
	"fmt"
	"strconv"
	"strings"
)

// TagKind identifies which wasm platform-tag family a string belongs
// to. The selector branches on it to apply family-specific rules
// (vintage gates for pyodide, ABI mode for wasi-p2).
type TagKind int

const (
	TagUnknown TagKind = iota

	// TagPyodide is the modern Pyodide distribution tag:
	// `pyodide_<year>_<rev>_wasm32` (e.g. pyodide_2024_0_wasm32).
	TagPyodide

	// TagEmscripten is the legacy CPython-on-Emscripten tag:
	// `emscripten_<major>_<minor>_<patch>_wasm32`
	// (e.g. emscripten_3_1_45_wasm32).
	TagEmscripten

	// TagWASIPreview2 is the experimental WASI Preview 2 tag:
	// `wasi_p2_wasm32` (no per-host vintage, the component model
	// keeps ABI compatibility across host versions).
	TagWASIPreview2
)

// String renders the tag kind for diagnostics.
func (k TagKind) String() string {
	switch k {
	case TagPyodide:
		return "pyodide"
	case TagEmscripten:
		return "emscripten"
	case TagWASIPreview2:
		return "wasi-p2"
	default:
		return "unknown"
	}
}

// Tag is the parsed form of a wasm platform tag. Vintage is populated
// only for TagPyodide; Emscripten is populated only for TagEmscripten.
type Tag struct {
	Raw        string
	Kind       TagKind
	Vintage    Vintage
	Emscripten EmscriptenVersion
}

// EmscriptenVersion is the (major, minor, patch) the legacy CPython-on-
// Emscripten wheels stamp in their platform tag.
type EmscriptenVersion struct {
	Major int
	Minor int
	Patch int
}

// String renders the Emscripten version in dotted form.
func (e EmscriptenVersion) String() string {
	return fmt.Sprintf("%d.%d.%d", e.Major, e.Minor, e.Patch)
}

// ParsePlatformTag lifts one of the three supported wasm tag families
// into a Tag. Anything else returns TagUnknown + an error: the wheel
// selector treats unknown tags as non-matches and the resolver
// surfaces the rejection through a SkipReason.
func ParsePlatformTag(raw string) (Tag, error) {
	t := Tag{Raw: raw}
	switch {
	case strings.HasPrefix(raw, "pyodide_"):
		if !strings.HasSuffix(raw, "_wasm32") {
			return t, fmt.Errorf("pyodide: tag %q missing _wasm32 suffix", raw)
		}
		rest := strings.TrimSuffix(strings.TrimPrefix(raw, "pyodide_"), "_wasm32")
		parts := strings.Split(rest, "_")
		if len(parts) != 2 {
			return t, fmt.Errorf("pyodide: tag %q must be pyodide_<year>_<rev>_wasm32", raw)
		}
		year, err := strconv.Atoi(parts[0])
		if err != nil || year <= 0 {
			return t, fmt.Errorf("pyodide: tag %q: bad year %q", raw, parts[0])
		}
		rev, err := strconv.Atoi(parts[1])
		if err != nil || rev < 0 {
			return t, fmt.Errorf("pyodide: tag %q: bad rev %q", raw, parts[1])
		}
		t.Kind = TagPyodide
		t.Vintage = Vintage{Year: year, Rev: rev}
		return t, nil

	case strings.HasPrefix(raw, "emscripten_"):
		if !strings.HasSuffix(raw, "_wasm32") {
			return t, fmt.Errorf("pyodide: tag %q missing _wasm32 suffix", raw)
		}
		rest := strings.TrimSuffix(strings.TrimPrefix(raw, "emscripten_"), "_wasm32")
		parts := strings.Split(rest, "_")
		if len(parts) != 3 {
			return t, fmt.Errorf("pyodide: tag %q must be emscripten_<major>_<minor>_<patch>_wasm32", raw)
		}
		maj, err := strconv.Atoi(parts[0])
		if err != nil || maj <= 0 {
			return t, fmt.Errorf("pyodide: tag %q: bad major %q", raw, parts[0])
		}
		min, err := strconv.Atoi(parts[1])
		if err != nil || min < 0 {
			return t, fmt.Errorf("pyodide: tag %q: bad minor %q", raw, parts[1])
		}
		pat, err := strconv.Atoi(parts[2])
		if err != nil || pat < 0 {
			return t, fmt.Errorf("pyodide: tag %q: bad patch %q", raw, parts[2])
		}
		t.Kind = TagEmscripten
		t.Emscripten = EmscriptenVersion{Major: maj, Minor: min, Patch: pat}
		return t, nil

	case raw == "wasi_p2_wasm32" || raw == "wasi_p2":
		t.Kind = TagWASIPreview2
		return t, nil

	default:
		return t, fmt.Errorf("pyodide: unrecognised wasm platform tag %q", raw)
	}
}

// SatisfiesRuntime reports whether t belongs to the family that
// Runtime selects. TagEmscripten is treated as RuntimePyodide-
// compatible: Pyodide is the only consumer of the legacy
// emscripten_X_Y_Z_wasm32 tag in the wild today.
func (t Tag) SatisfiesRuntime(r Runtime) bool {
	switch r {
	case RuntimePyodide:
		return t.Kind == TagPyodide || t.Kind == TagEmscripten
	case RuntimeWASIPreview2:
		return t.Kind == TagWASIPreview2
	default:
		return false
	}
}
