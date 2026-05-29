package abi3

import (
	"strings"
	"testing"
)

// TestPhase13ABI3 is the Phase 13 umbrella sentinel. It walks a
// synthetic wheel through the abi3 slimming + auditwheel pipeline and
// asserts that the renderer + auditor together produce a coherent
// result.
//
//  1. Parse a cp312 wheel filename, promote it to abi3, assert the
//     resulting filename + tag triple shape.
//  2. Render the WHEEL marker for the promoted wheel, assert it
//     embeds the abi3 tag.
//  3. Audit two extensions against the manylinux_2_28_x86_64 profile
//     via a mock SymbolReader: a clean extension passes, an extension
//     linking libssl violates, the aggregate report flags the wheel
//     as not-OK with the right violation message.
//  4. Confirm that a macOS wheel audits against the macosx_11_0_arm64
//     profile via the same code path (cross-platform coverage).
func TestPhase13ABI3(t *testing.T) {
	// Step 1: promote a cp312 filename to abi3.
	promoted, err := PromoteWheelToABI3("mochi-pkg-0.1.0-cp312-cp312-manylinux_2_28_x86_64.whl")
	if err != nil {
		t.Fatalf("PromoteWheelToABI3: %v", err)
	}
	if promoted != "mochi-pkg-0.1.0-cp32-abi3-manylinux_2_28_x86_64.whl" {
		t.Fatalf("promoted = %q", promoted)
	}

	_, tag, err := SplitWheelFilename(promoted)
	if err != nil {
		t.Fatalf("SplitWheelFilename: %v", err)
	}
	if !tag.IsABI3() {
		t.Fatalf("promoted tag must report IsABI3: %+v", tag)
	}

	// Step 2: WHEEL marker carries the promoted tag.
	wheelMarker := RenderWHEELMarker("mochi-pkg/1.0", "manylinux_2_28_x86_64")
	if !strings.Contains(wheelMarker, "Tag: cp32-abi3-manylinux_2_28_x86_64\n") {
		t.Fatalf("WHEEL marker missing abi3 tag:\n%s", wheelMarker)
	}

	// Step 3: audit two extensions on the manylinux_2_28 profile.
	r := fakeReader{libs: map[string][]LinkedLib{
		"mochi_pkg/_clean.so": {
			{SoName: "libc.so.6", External: true},
			{SoName: "libm.so.6", External: true},
			{SoName: "libpython3.so", External: false}, // vendored
		},
		"mochi_pkg/_dirty.so": {
			{SoName: "libc.so.6", External: true},
			{SoName: "libssl.so.3", External: true},
		},
	}}
	reports, ok, err := AuditWheel(
		[]string{"mochi_pkg/_clean.so", "mochi_pkg/__init__.py", "mochi_pkg/_dirty.so"},
		AuditOptions{Profile: KnownProfiles["manylinux_2_28_x86_64"], Reader: r},
	)
	if err != nil {
		t.Fatalf("AuditWheel: %v", err)
	}
	if ok {
		t.Fatal("aggregate must be not-OK because _dirty.so violates")
	}
	if len(reports) != 2 {
		t.Fatalf("expected 2 reports (skip __init__.py), got %d", len(reports))
	}
	var sawClean, sawDirty bool
	for _, r := range reports {
		switch r.Path {
		case "mochi_pkg/_clean.so":
			sawClean = true
			if !r.OK {
				t.Errorf("_clean.so should be OK: %+v", r)
			}
		case "mochi_pkg/_dirty.so":
			sawDirty = true
			if r.OK {
				t.Errorf("_dirty.so should NOT be OK")
			}
			if len(r.Disallowed) != 1 || r.Disallowed[0].SoName != "libssl.so.3" {
				t.Errorf("_dirty.so Disallowed wrong: %+v", r.Disallowed)
			}
			joined := strings.Join(r.Violations, "\n")
			if !strings.Contains(joined, "libssl.so.3") || !strings.Contains(joined, "manylinux_2_28_x86_64") {
				t.Errorf("_dirty.so violation missing details:\n%s", joined)
			}
		}
	}
	if !sawClean || !sawDirty {
		t.Fatalf("missing per-ext reports: clean=%v dirty=%v", sawClean, sawDirty)
	}

	// Step 4: macOS profile coverage via the same code path.
	macR := fakeReader{libs: map[string][]LinkedLib{
		"mochi_pkg/_mac.dylib": {
			{SoName: "/usr/lib/libSystem.B.dylib", External: true},
			{SoName: "/opt/homebrew/lib/libpng.dylib", External: true},
		},
	}}
	macRep, err := AuditExtension("mochi_pkg/_mac.dylib", AuditOptions{
		Profile: KnownProfiles["macosx_11_0_arm64"],
		Reader:  macR,
	})
	if err != nil {
		t.Fatalf("macOS AuditExtension: %v", err)
	}
	if macRep.OK {
		t.Fatal("macOS extension linking Homebrew lib must violate")
	}
	if len(macRep.Disallowed) != 1 || macRep.Disallowed[0].SoName != "/opt/homebrew/lib/libpng.dylib" {
		t.Fatalf("macOS Disallowed wrong: %+v", macRep.Disallowed)
	}
}
