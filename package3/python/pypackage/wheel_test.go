package pypackage

import (
	"strings"
	"testing"
)

func TestRenderWheelRejectsInvalidPackage(t *testing.T) {
	if _, err := RenderWheel(Package{}); err == nil {
		t.Fatal("expected validate error for empty Package")
	}
}

func TestRenderWheelLayout(t *testing.T) {
	l, err := RenderWheel(samplePackage())
	if err != nil {
		t.Fatalf("RenderWheel: %v", err)
	}
	want := []string{
		"mochi_sample/__init__.py",
		"mochi_sample/__init__.pyi",
		"mochi-sample-0.1.0.dist-info/METADATA",
		"mochi-sample-0.1.0.dist-info/WHEEL",
		"mochi-sample-0.1.0.dist-info/RECORD",
	}
	for _, p := range want {
		if _, ok := l.Files[p]; !ok {
			t.Fatalf("wheel missing %q (got %v)", p, l.Paths())
		}
	}
}

func TestRenderWheelRECORDFormat(t *testing.T) {
	l, err := RenderWheel(samplePackage())
	if err != nil {
		t.Fatalf("RenderWheel: %v", err)
	}
	record := l.Files["mochi-sample-0.1.0.dist-info/RECORD"]
	lines := strings.Split(strings.TrimRight(record, "\n"), "\n")
	if len(lines) != len(l.Files) {
		t.Fatalf("RECORD lines = %d, files = %d\n%s", len(lines), len(l.Files), record)
	}
	for _, line := range lines {
		parts := strings.Split(line, ",")
		if len(parts) != 3 {
			t.Fatalf("bad RECORD line: %q", line)
		}
		if strings.HasSuffix(parts[0], "/RECORD") {
			if parts[1] != "" || parts[2] != "" {
				t.Fatalf("RECORD self-line must be empty hash/size: %q", line)
			}
			continue
		}
		if !strings.HasPrefix(parts[1], "sha256=") {
			t.Fatalf("missing sha256 prefix in %q", line)
		}
		if parts[2] == "" {
			t.Fatalf("missing size in %q", line)
		}
	}
}

func TestRenderWheelRECORDDeterministic(t *testing.T) {
	a, err := RenderWheel(samplePackage())
	if err != nil {
		t.Fatalf("RenderWheel: %v", err)
	}
	b, err := RenderWheel(samplePackage())
	if err != nil {
		t.Fatalf("RenderWheel: %v", err)
	}
	if a.Files["mochi-sample-0.1.0.dist-info/RECORD"] != b.Files["mochi-sample-0.1.0.dist-info/RECORD"] {
		t.Fatalf("RECORD not deterministic")
	}
}
