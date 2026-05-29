package freethread

import (
	"fmt"
	"sort"
	"strings"
)

// ModuleMarker captures the PEP 703 declaration a free-threaded-safe
// C extension emits at module init via the `Py_mod_gil` slot.
//
//   - Declared is true when the module declares the slot at all.
//   - GilDisabled is true when the slot's value is
//     `Py_MOD_GIL_NOT_USED` (the free-threaded opt-in). When the slot
//     declares `Py_MOD_GIL_USED` (or is absent) the extension is
//     considered not free-threaded-safe.
//   - Module is the dotted module name the extension exports
//     (`numpy._core._multiarray_umath`, ...). The auditor prints
//     this when emitting diagnostics.
type ModuleMarker struct {
	Module      string
	Declared    bool
	GilDisabled bool
}

// MarkerReader is the contract the live ELF / Mach-O / PE reader
// satisfies in sub-phase 17.1. Today the umbrella gate wires the
// StaticMarkerReader so the test stays offline.
type MarkerReader interface {
	Read(extensionPath string) (ModuleMarker, error)
}

// StaticMarkerReader returns Markers[path] verbatim. Missing paths
// return ModuleMarker{} (Declared = false) + nil error so the audit
// classifies the extension as legacy / unsafe under free-threaded.
type StaticMarkerReader struct {
	Markers map[string]ModuleMarker
}

// Read returns the configured marker for the path. A missing path is
// not an error; it represents "extension declares no free-threaded
// slot" which is the production default for the long tail of
// legacy wheels.
func (s StaticMarkerReader) Read(path string) (ModuleMarker, error) {
	m, ok := s.Markers[path]
	if !ok {
		return ModuleMarker{Module: filenameToModule(path)}, nil
	}
	if m.Module == "" {
		m.Module = filenameToModule(path)
	}
	return m, nil
}

// ExtensionAudit is the per-extension result the AuditWheel surface
// returns. It mirrors abi3.AuditExtension so the install CLI can
// merge the two reports into a single per-package matrix.
type ExtensionAudit struct {
	Path        string
	Module      string
	Marker      ModuleMarker
	FreeThreaded bool   // shorthand for Marker.Declared && Marker.GilDisabled
	Violation   string // empty when FreeThreaded is true
}

// AuditOptions wires a MarkerReader plus an optional acceptance gate.
type AuditOptions struct {
	Reader MarkerReader
}

// AuditReport is the umbrella audit result the CLI prints + the
// install gate switches on.
type AuditReport struct {
	Path       string
	Extensions []ExtensionAudit
	Violations []string
	OK         bool
}

// AuditWheel runs the marker reader against every extension path and
// reports whether the wheel is free-threaded-safe. OK is true iff
// every extension declares Py_MOD_GIL_NOT_USED.
func AuditWheel(extensions []string, opts AuditOptions) (*AuditReport, error) {
	if opts.Reader == nil {
		return nil, fmt.Errorf("freethread: AuditOptions.Reader must be set")
	}
	r := &AuditReport{}
	sorted := append([]string(nil), extensions...)
	sort.Strings(sorted)
	for _, path := range sorted {
		ext, err := AuditExtension(path, opts)
		if err != nil {
			return nil, err
		}
		r.Extensions = append(r.Extensions, ext)
		if ext.Violation != "" {
			r.Violations = append(r.Violations, ext.Violation)
		}
	}
	r.OK = len(r.Violations) == 0
	if r.OK && len(sorted) == 1 {
		r.Path = sorted[0]
	}
	return r, nil
}

// AuditExtension scores one .so / .dylib / .pyd path against the
// PEP 703 marker.
func AuditExtension(path string, opts AuditOptions) (ExtensionAudit, error) {
	if opts.Reader == nil {
		return ExtensionAudit{}, fmt.Errorf("freethread: AuditOptions.Reader must be set")
	}
	marker, err := opts.Reader.Read(path)
	if err != nil {
		return ExtensionAudit{}, fmt.Errorf("freethread: read %q: %w", path, err)
	}
	a := ExtensionAudit{
		Path:         path,
		Module:       marker.Module,
		Marker:       marker,
		FreeThreaded: marker.Declared && marker.GilDisabled,
	}
	if !a.FreeThreaded {
		switch {
		case !marker.Declared:
			a.Violation = fmt.Sprintf("%s: no Py_mod_gil slot declared", path)
		case !marker.GilDisabled:
			a.Violation = fmt.Sprintf("%s: Py_mod_gil declares Py_MOD_GIL_USED", path)
		}
	}
	return a, nil
}

// IsExtensionFilename reports whether name looks like a CPython
// extension. The check is case-insensitive on the suffix; phase 13's
// auditor uses the same predicate.
func IsExtensionFilename(name string) bool {
	lower := strings.ToLower(name)
	return strings.HasSuffix(lower, ".so") || strings.HasSuffix(lower, ".dylib") || strings.HasSuffix(lower, ".pyd")
}

func filenameToModule(path string) string {
	// Strip a directory + extension; the audit only uses this as a
	// human label in violations.
	idx := strings.LastIndex(path, "/")
	base := path
	if idx >= 0 {
		base = path[idx+1:]
	}
	for _, suf := range []string{".cpython-313t-x86_64-linux-gnu.so", ".cpython-313-x86_64-linux-gnu.so", ".so", ".dylib", ".pyd"} {
		base = strings.TrimSuffix(base, suf)
	}
	return base
}
