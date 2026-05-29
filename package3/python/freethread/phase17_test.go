package freethread

import (
	"strings"
	"testing"
)

// TestPhase17Sentinel is the umbrella gate for Phase 17. The
// user-facing goal is "install Python packages on a free-threaded
// CPython interpreter without lying about compatibility, and emit
// wrapper-side lock shims that pick the right primitive". The
// gate proves the resolver, the auditor, and the lock renderer
// all agree end-to-end.
func TestPhase17Sentinel(t *testing.T) {
	t.Run("free-threaded-numpy-passes-end-to-end", func(t *testing.T) {
		target := ABITag{Interpreter: "cp", Major: 3, Minor: 13, FreeThreaded: true}
		compat := WheelCompat{Target: target}
		cls, lvl, err := compat.Score(ResolvedWheel{Distribution: "numpy", ABI: "cp313t"})
		if err != nil {
			t.Fatalf("Score: %v", err)
		}
		if cls != WheelClassFreeThreaded || lvl != SupportFull {
			t.Errorf("class=%v level=%v, want free-threaded/full", cls, lvl)
		}

		// And the extension audit confirms the PEP 703 marker.
		reader := StaticMarkerReader{Markers: map[string]ModuleMarker{
			"/numpy/_core.cpython-313t.so": {Declared: true, GilDisabled: true},
		}}
		report, err := AuditWheel([]string{"/numpy/_core.cpython-313t.so"}, AuditOptions{Reader: reader})
		if err != nil {
			t.Fatalf("AuditWheel: %v", err)
		}
		if !report.OK {
			t.Errorf("expected OK, got violations %v", report.Violations)
		}
	})

	t.Run("legacy-extension-on-free-threaded-flagged", func(t *testing.T) {
		target := ABITag{Interpreter: "cp", Major: 3, Minor: 13, FreeThreaded: true}
		compat := WheelCompat{Target: target}
		cls, lvl, err := compat.Score(ResolvedWheel{Distribution: "old-pkg", ABI: "cp313"})
		if err != nil {
			t.Fatalf("Score: %v", err)
		}
		if cls != WheelClassLegacy || lvl != SupportUntested {
			t.Errorf("class=%v level=%v, want legacy/untested", cls, lvl)
		}

		// The audit also reports no PEP 703 marker present.
		reader := StaticMarkerReader{}
		report, err := AuditWheel([]string{"/old/ext.so"}, AuditOptions{Reader: reader})
		if err != nil {
			t.Fatalf("AuditWheel: %v", err)
		}
		if report.OK {
			t.Error("expected not OK")
		}
		if !strings.Contains(report.Violations[0], "no Py_mod_gil") {
			t.Errorf("violation = %q", report.Violations[0])
		}
	})

	t.Run("denylisted-package-incompatible", func(t *testing.T) {
		target := ABITag{Interpreter: "cp", Major: 3, Minor: 13, FreeThreaded: true}
		compat := WheelCompat{Target: target, DenyList: map[string]struct{}{"lxml": {}}}
		// Even when the wheel offers a cp313t-tagged variant the
		// deny-list wins.
		cls, lvl, err := compat.Score(ResolvedWheel{Distribution: "lxml", ABI: "cp313t"})
		if err != nil {
			t.Fatalf("Score: %v", err)
		}
		if cls != WheelClassDenied || lvl != SupportIncompatible {
			t.Errorf("class=%v level=%v, want denied/incompatible", cls, lvl)
		}
	})

	t.Run("lock-shim-renders-correct-primitive", func(t *testing.T) {
		shim := LockShim{Name: "demo_lock", CriticalSection: []QualifiedAttr{
			{Module: "demo", Attr: "cache"},
		}}

		gil, err := RenderLockShim(shim, LockGIL)
		if err != nil {
			t.Fatalf("LockGIL render: %v", err)
		}
		if !strings.Contains(gil, "threading.Lock") {
			t.Error("GIL shim missing threading.Lock")
		}
		if strings.Contains(gil, "_PyMutex") {
			t.Error("GIL shim should not reference _PyMutex")
		}

		py, err := RenderLockShim(shim, LockPyMutex)
		if err != nil {
			t.Fatalf("LockPyMutex render: %v", err)
		}
		if !strings.Contains(py, "_PyMutex") {
			t.Error("PyMutex shim missing _PyMutex")
		}
	})
}
