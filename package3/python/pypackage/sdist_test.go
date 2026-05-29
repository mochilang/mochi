package pypackage

import (
	"strings"
	"testing"
)

func TestRenderSdistRejectsInvalidPackage(t *testing.T) {
	if _, err := RenderSdist(Package{}); err == nil {
		t.Fatal("expected validate error for empty Package")
	}
}

func TestRenderSdistLayout(t *testing.T) {
	l, err := RenderSdist(samplePackage())
	if err != nil {
		t.Fatalf("RenderSdist: %v", err)
	}
	want := []string{
		"mochi-sample-0.1.0/pyproject.toml",
		"mochi-sample-0.1.0/PKG-INFO",
		"mochi-sample-0.1.0/README.md",
		"mochi-sample-0.1.0/mochi_sample/__init__.py",
		"mochi-sample-0.1.0/mochi_sample/__init__.pyi",
		"mochi-sample-0.1.0/mochi_build/__init__.py",
	}
	for _, p := range want {
		if _, ok := l.Files[p]; !ok {
			t.Fatalf("sdist missing %q (got %v)", p, l.Paths())
		}
		if l.Files[p] == "" {
			t.Fatalf("sdist %q empty", p)
		}
	}
}

func TestRenderSdistREADMEContainsSummary(t *testing.T) {
	l, err := RenderSdist(samplePackage())
	if err != nil {
		t.Fatalf("RenderSdist: %v", err)
	}
	readme := l.Files["mochi-sample-0.1.0/README.md"]
	if !strings.Contains(readme, "# mochi-sample") || !strings.Contains(readme, "sample wrapper") {
		t.Fatalf("README missing pieces:\n%s", readme)
	}
}

func TestRenderSdistRespectsModuleOverride(t *testing.T) {
	p := samplePackage()
	p.Module = "custom_mod"
	l, err := RenderSdist(p)
	if err != nil {
		t.Fatalf("RenderSdist: %v", err)
	}
	if _, ok := l.Files["mochi-sample-0.1.0/custom_mod/__init__.py"]; !ok {
		t.Fatalf("module override not honoured: %v", l.Paths())
	}
}
