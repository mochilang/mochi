package freethread

import (
	"errors"
	"strings"
	"testing"
)

func TestAuditExtensionFreeThreaded(t *testing.T) {
	reader := StaticMarkerReader{Markers: map[string]ModuleMarker{
		"/path/numpy/_core.cpython-313t-x86_64-linux-gnu.so": {
			Module:      "numpy._core",
			Declared:    true,
			GilDisabled: true,
		},
	}}
	ext, err := AuditExtension("/path/numpy/_core.cpython-313t-x86_64-linux-gnu.so", AuditOptions{Reader: reader})
	if err != nil {
		t.Fatalf("AuditExtension: %v", err)
	}
	if !ext.FreeThreaded {
		t.Error("expected FreeThreaded = true")
	}
	if ext.Violation != "" {
		t.Errorf("Violation = %q, want empty", ext.Violation)
	}
}

func TestAuditExtensionDeclaresGilUsed(t *testing.T) {
	reader := StaticMarkerReader{Markers: map[string]ModuleMarker{
		"/path/legacy.so": {
			Module:      "legacy",
			Declared:    true,
			GilDisabled: false,
		},
	}}
	ext, err := AuditExtension("/path/legacy.so", AuditOptions{Reader: reader})
	if err != nil {
		t.Fatalf("AuditExtension: %v", err)
	}
	if ext.FreeThreaded {
		t.Error("expected FreeThreaded = false")
	}
	if !strings.Contains(ext.Violation, "Py_MOD_GIL_USED") {
		t.Errorf("Violation = %q, want Py_MOD_GIL_USED mention", ext.Violation)
	}
}

func TestAuditExtensionNoMarker(t *testing.T) {
	reader := StaticMarkerReader{}
	ext, err := AuditExtension("/path/silent.so", AuditOptions{Reader: reader})
	if err != nil {
		t.Fatalf("AuditExtension: %v", err)
	}
	if ext.FreeThreaded {
		t.Error("expected FreeThreaded = false for missing marker")
	}
	if !strings.Contains(ext.Violation, "no Py_mod_gil slot") {
		t.Errorf("Violation = %q, want missing-slot mention", ext.Violation)
	}
	if ext.Module != "silent" {
		t.Errorf("Module = %q, want %q (inferred from filename)", ext.Module, "silent")
	}
}

type explosiveReader struct{}

func (explosiveReader) Read(string) (ModuleMarker, error) {
	return ModuleMarker{}, errors.New("boom")
}

func TestAuditExtensionReaderError(t *testing.T) {
	_, err := AuditExtension("/x.so", AuditOptions{Reader: explosiveReader{}})
	if err == nil {
		t.Fatal("expected error")
	}
	if !strings.Contains(err.Error(), "boom") {
		t.Errorf("err = %q", err.Error())
	}
}

func TestAuditExtensionRejectsNilReader(t *testing.T) {
	_, err := AuditExtension("/x.so", AuditOptions{})
	if err == nil {
		t.Fatal("expected error")
	}
}

func TestAuditWheelHappy(t *testing.T) {
	reader := StaticMarkerReader{Markers: map[string]ModuleMarker{
		"/wheel/a.so": {Declared: true, GilDisabled: true},
		"/wheel/b.so": {Declared: true, GilDisabled: true},
	}}
	r, err := AuditWheel([]string{"/wheel/a.so", "/wheel/b.so"}, AuditOptions{Reader: reader})
	if err != nil {
		t.Fatalf("AuditWheel: %v", err)
	}
	if !r.OK {
		t.Errorf("expected OK, got violations %v", r.Violations)
	}
	if len(r.Extensions) != 2 {
		t.Errorf("Extensions = %d", len(r.Extensions))
	}
}

func TestAuditWheelReportsViolations(t *testing.T) {
	reader := StaticMarkerReader{Markers: map[string]ModuleMarker{
		"/wheel/a.so": {Declared: true, GilDisabled: true},
		"/wheel/b.so": {Declared: true, GilDisabled: false},
	}}
	r, err := AuditWheel([]string{"/wheel/a.so", "/wheel/b.so"}, AuditOptions{Reader: reader})
	if err != nil {
		t.Fatalf("AuditWheel: %v", err)
	}
	if r.OK {
		t.Error("expected not OK")
	}
	if len(r.Violations) != 1 {
		t.Errorf("Violations = %d", len(r.Violations))
	}
}

func TestAuditWheelRejectsNilReader(t *testing.T) {
	_, err := AuditWheel(nil, AuditOptions{})
	if err == nil {
		t.Fatal("expected error")
	}
}

func TestIsExtensionFilename(t *testing.T) {
	yes := []string{"x.so", "y.SO", "z.dylib", "w.DyLib", "v.pyd", "u.PYD"}
	no := []string{"x.txt", "y.so.bak", "z", "a.tar.gz"}
	for _, n := range yes {
		if !IsExtensionFilename(n) {
			t.Errorf("IsExtensionFilename(%q) = false, want true", n)
		}
	}
	for _, n := range no {
		if IsExtensionFilename(n) {
			t.Errorf("IsExtensionFilename(%q) = true, want false", n)
		}
	}
}

func TestStaticReaderPreservesExplicitModule(t *testing.T) {
	reader := StaticMarkerReader{Markers: map[string]ModuleMarker{
		"/p.so": {Module: "explicit", Declared: true, GilDisabled: true},
	}}
	m, err := reader.Read("/p.so")
	if err != nil {
		t.Fatalf("Read: %v", err)
	}
	if m.Module != "explicit" {
		t.Errorf("Module = %q, want explicit", m.Module)
	}
}

func TestFilenameToModuleStripsExtensions(t *testing.T) {
	cases := map[string]string{
		"/p/foo.cpython-313-x86_64-linux-gnu.so":  "foo",
		"/p/foo.cpython-313t-x86_64-linux-gnu.so": "foo",
		"/p/foo.so":                               "foo",
		"/p/foo.dylib":                            "foo",
		"/p/foo.pyd":                              "foo",
		"plain":                                   "plain",
	}
	for in, want := range cases {
		if got := filenameToModule(in); got != want {
			t.Errorf("filenameToModule(%q) = %q, want %q", in, got, want)
		}
	}
}
