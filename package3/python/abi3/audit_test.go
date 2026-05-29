package abi3

import (
	"errors"
	"strings"
	"testing"
)

type fakeReader struct {
	libs map[string][]LinkedLib
	err  error
}

func (f fakeReader) Read(path string) ([]LinkedLib, error) {
	if f.err != nil {
		return nil, f.err
	}
	return f.libs[path], nil
}

func TestAuditExtensionRequiresReader(t *testing.T) {
	if _, err := AuditExtension("/x.so", AuditOptions{Profile: KnownProfiles["manylinux_2_28_x86_64"]}); err == nil {
		t.Fatal("AuditExtension should reject nil reader")
	}
}

func TestAuditExtensionRequiresProfile(t *testing.T) {
	r := fakeReader{libs: map[string][]LinkedLib{}}
	if _, err := AuditExtension("/x.so", AuditOptions{Reader: r}); err == nil {
		t.Fatal("AuditExtension should reject zero profile")
	}
}

func TestAuditExtensionHappyPath(t *testing.T) {
	r := fakeReader{libs: map[string][]LinkedLib{
		"/wheel/foo.so": {
			{SoName: "libc.so.6", External: true},
			{SoName: "libm.so.6", External: true},
			{SoName: "libfoo.so.0", External: false}, // internal vendored
		},
	}}
	rep, err := AuditExtension("/wheel/foo.so", AuditOptions{
		Profile: KnownProfiles["manylinux_2_28_x86_64"],
		Reader:  r,
	})
	if err != nil {
		t.Fatalf("AuditExtension: %v", err)
	}
	if !rep.OK {
		t.Fatalf("expected OK report, got violations: %v", rep.Violations)
	}
	if len(rep.External) != 2 {
		t.Errorf("External count = %d", len(rep.External))
	}
	if len(rep.Disallowed) != 0 {
		t.Errorf("Disallowed should be empty: %v", rep.Disallowed)
	}
}

func TestAuditExtensionFlagsDisallowedExternal(t *testing.T) {
	r := fakeReader{libs: map[string][]LinkedLib{
		"/wheel/bar.so": {
			{SoName: "libc.so.6", External: true},
			{SoName: "libssl.so.3", External: true},
		},
	}}
	rep, err := AuditExtension("/wheel/bar.so", AuditOptions{
		Profile: KnownProfiles["manylinux_2_28_x86_64"],
		Reader:  r,
	})
	if err != nil {
		t.Fatalf("AuditExtension: %v", err)
	}
	if rep.OK {
		t.Fatal("expected report to be not-OK")
	}
	if len(rep.Disallowed) != 1 || rep.Disallowed[0].SoName != "libssl.so.3" {
		t.Errorf("Disallowed wrong: %+v", rep.Disallowed)
	}
	found := false
	for _, v := range rep.Violations {
		if strings.Contains(v, "libssl.so.3") && strings.Contains(v, "manylinux_2_28_x86_64") {
			found = true
		}
	}
	if !found {
		t.Errorf("violation message missing: %v", rep.Violations)
	}
}

func TestAuditExtensionPropagatesReaderError(t *testing.T) {
	r := fakeReader{err: errors.New("not an ELF")}
	_, err := AuditExtension("/wheel/x.so", AuditOptions{
		Profile: KnownProfiles["manylinux_2_28_x86_64"],
		Reader:  r,
	})
	if err == nil || !strings.Contains(err.Error(), "not an ELF") {
		t.Fatalf("expected reader error, got %v", err)
	}
}

func TestIsExtensionFilename(t *testing.T) {
	yes := []string{"foo.so", "bar.dylib", "baz.pyd", "FOO.SO"}
	no := []string{"foo.py", "bar.pyi", "metadata"}
	for _, n := range yes {
		if !IsExtensionFilename(n) {
			t.Errorf("%q should be an extension", n)
		}
	}
	for _, n := range no {
		if IsExtensionFilename(n) {
			t.Errorf("%q should NOT be an extension", n)
		}
	}
}

func TestAuditWheel(t *testing.T) {
	r := fakeReader{libs: map[string][]LinkedLib{
		"a.so": {{SoName: "libc.so.6", External: true}},
		"b.so": {{SoName: "libssl.so.3", External: true}},
	}}
	files := []string{"a.so", "metadata.txt", "b.so", "c.py"}
	reports, ok, err := AuditWheel(files, AuditOptions{
		Profile: KnownProfiles["manylinux_2_28_x86_64"],
		Reader:  r,
	})
	if err != nil {
		t.Fatalf("AuditWheel: %v", err)
	}
	if ok {
		t.Fatal("expected aggregate ok=false")
	}
	if len(reports) != 2 {
		t.Fatalf("expected 2 reports (skip non-.so), got %d", len(reports))
	}
}

func TestAuditWheelAllOK(t *testing.T) {
	r := fakeReader{libs: map[string][]LinkedLib{
		"a.so": {{SoName: "libc.so.6", External: true}},
	}}
	_, ok, err := AuditWheel([]string{"a.so"}, AuditOptions{
		Profile: KnownProfiles["manylinux_2_28_x86_64"],
		Reader:  r,
	})
	if err != nil {
		t.Fatalf("AuditWheel: %v", err)
	}
	if !ok {
		t.Fatal("expected aggregate ok=true")
	}
}
