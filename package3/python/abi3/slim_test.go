package abi3

import (
	"strings"
	"testing"
)

func TestRenameWheelFilename(t *testing.T) {
	out, err := RenameWheelFilename("mochi-0.1.0-cp312-cp312-manylinux_2_28_x86_64.whl", "abi3")
	if err != nil {
		t.Fatalf("RenameWheelFilename: %v", err)
	}
	if out != "mochi-0.1.0-cp312-abi3-manylinux_2_28_x86_64.whl" {
		t.Fatalf("got %q", out)
	}
}

func TestRenameWheelFilenameRequiresABI(t *testing.T) {
	if _, err := RenameWheelFilename("a-b-cp312-cp312-any.whl", ""); err == nil {
		t.Fatal("RenameWheelFilename should reject empty abi")
	}
}

func TestPromoteWheelToABI3(t *testing.T) {
	out, err := PromoteWheelToABI3("mochi-0.1.0-cp312-cp312-manylinux_2_28_x86_64.whl")
	if err != nil {
		t.Fatalf("PromoteWheelToABI3: %v", err)
	}
	if out != "mochi-0.1.0-cp32-abi3-manylinux_2_28_x86_64.whl" {
		t.Fatalf("got %q", out)
	}
}

func TestPromoteWheelToABI3RejectsPyPy(t *testing.T) {
	if _, err := PromoteWheelToABI3("mochi-0.1.0-pp310-pypy310_pp73-manylinux_2_28_x86_64.whl"); err == nil {
		t.Fatal("PromoteWheelToABI3 should reject PyPy")
	}
}

func TestRenderWHEELMarker(t *testing.T) {
	out := RenderWHEELMarker("mochi-pkg/0.1", "manylinux_2_28_x86_64")
	wants := []string{
		"Wheel-Version: 1.0\n",
		"Generator: mochi-pkg/0.1\n",
		"Root-Is-Purelib: false\n",
		"Tag: cp32-abi3-manylinux_2_28_x86_64\n",
	}
	for _, w := range wants {
		if !strings.Contains(out, w) {
			t.Errorf("WHEEL missing %q\n%s", w, out)
		}
	}
}

func TestRenderWHEELMarkerDefaults(t *testing.T) {
	out := RenderWHEELMarker("", "")
	if !strings.Contains(out, "Generator: mochi-pkg\n") {
		t.Errorf("default generator missing:\n%s", out)
	}
	if !strings.Contains(out, "Tag: cp32-abi3-any\n") {
		t.Errorf("default platform missing:\n%s", out)
	}
}
