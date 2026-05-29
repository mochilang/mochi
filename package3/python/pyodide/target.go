package pyodide

import "fmt"

// Runtime identifies which wasm Python runtime the install is for.
// The two values cover today's PEP 425 + Pyodide distribution wheel
// tag families; sub-phase 18.x will introduce abi2026-aware variants
// once that transition lands.
type Runtime int

const (
	// RuntimeUnknown is the zero value; Target.Validate rejects it.
	RuntimeUnknown Runtime = iota

	// RuntimePyodide selects browser-side Pyodide, which publishes
	// `pyodide_<year>_<rev>_wasm32` (or legacy
	// `emscripten_<major>_<minor>_<patch>_wasm32`) wheels through the
	// pyodide.org distribution channel.
	RuntimePyodide

	// RuntimeWASIPreview2 selects server-side CPython compiled for
	// WASI Preview 2 (`wasi_p2_wasm32` family). Still experimental
	// in CPython 3.13; sub-phase 17 (free-threaded) will follow.
	RuntimeWASIPreview2
)

// String renders the runtime enum for human-facing diagnostics. The
// values are stable; tests + telemetry switch on them.
func (r Runtime) String() string {
	switch r {
	case RuntimePyodide:
		return "pyodide"
	case RuntimeWASIPreview2:
		return "wasi-p2"
	default:
		return "unknown"
	}
}

// ParseRuntime accepts "pyodide" / "wasi-p2" / "wasip2" (case-insensitive
// on the kebab variant) and rejects anything else. Empty input is an
// error: the install orchestrator must spell the target explicitly.
func ParseRuntime(s string) (Runtime, error) {
	switch s {
	case "pyodide":
		return RuntimePyodide, nil
	case "wasi-p2", "wasip2":
		return RuntimeWASIPreview2, nil
	case "":
		return RuntimeUnknown, fmt.Errorf("pyodide: runtime must be specified")
	default:
		return RuntimeUnknown, fmt.Errorf("pyodide: unknown runtime %q (want pyodide|wasi-p2)", s)
	}
}

// Target describes the install destination the wheel selector
// matches against.
//
//   - Runtime picks the wasm family.
//   - PythonABI is the CPython ABI tag the wheels were built against
//     (`cp312`, `cp313`, `cp313t`, ...). Empty matches anything.
//   - MinVintage gates Pyodide-vintage wheels: when set, the selector
//     rejects any wheel whose vintage compares older. Use
//     (year, rev) = (0, 0) to disable.
type Target struct {
	Runtime    Runtime
	PythonABI  string
	MinVintage Vintage
}

// Vintage is the (year, rev) pair the Pyodide distribution stamps on
// every wheel under the `pyodide_<year>_<rev>_wasm32` platform tag.
// Newer wheels are produced under a new vintage roughly every Pyodide
// release; the resolver must let users pin a floor when one fix
// requires the recompile.
type Vintage struct {
	Year int
	Rev  int
}

// Zero reports whether v is the all-zero floor (i.e. "no minimum").
func (v Vintage) Zero() bool { return v.Year == 0 && v.Rev == 0 }

// Less reports whether v < other. The comparison is lexicographic
// over (Year, Rev); (2024, 0) < (2024, 1) < (2025, 0).
func (v Vintage) Less(other Vintage) bool {
	if v.Year != other.Year {
		return v.Year < other.Year
	}
	return v.Rev < other.Rev
}

// String renders v as `<year>_<rev>`, matching the Pyodide platform
// tag fragment.
func (v Vintage) String() string {
	return fmt.Sprintf("%d_%d", v.Year, v.Rev)
}

// Validate checks Runtime is set. PythonABI is freeform on purpose so
// downstream wheels for future ABI tags work without code changes.
func (t Target) Validate() error {
	if t.Runtime == RuntimeUnknown {
		return fmt.Errorf("pyodide: target Runtime must be set")
	}
	return nil
}
