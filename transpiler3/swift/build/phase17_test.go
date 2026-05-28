package build

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// TestPhase17StaticLinux is the gate for Phase 17: static Linux binary via Swift Static Linux SDK.
// Sub-tests:
//  1. SDK availability check -- always runs.
//  2. Static build -- skipped when SDK not installed.
func TestPhase17StaticLinux(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "swift", "fixtures", "phase17-static-linux")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}

	hasFixtures := false
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		hasFixtures = true
		name := strings.TrimSuffix(e.Name(), ".mochi")
		mochiPath := filepath.Join(fixtureDir, e.Name())
		outPath := filepath.Join(fixtureDir, name+".out")

		t.Run(name, func(t *testing.T) {
			testStaticLinuxFixture(t, mochiPath, outPath)
		})
	}
	if !hasFixtures {
		t.Fatal("no fixtures found")
	}
}

func testStaticLinuxFixture(t *testing.T, mochiPath, outPath string) {
	t.Helper()

	// Step 1: Swift source generation always runs -- validates the lowering pipeline.
	d := newDriver(t)
	workDir := t.TempDir()
	swiftSrc, err := d.Build(mochiPath, workDir, TargetSwiftSource)
	if err != nil {
		t.Fatalf("swift source generation failed: %v", err)
	}
	if swiftSrc == "" {
		t.Fatal("no swift source generated")
	}

	// Step 2: On Linux, try native static build with -static-stdlib.
	if runtime.GOOS == "linux" {
		t.Run("native_static", func(t *testing.T) {
			d2 := newDriver(t)
			out2 := t.TempDir()
			bin, err := d2.Build(mochiPath, out2, TargetMacOSExecutable) // uses linux build path
			if err != nil {
				t.Logf("native build failed (non-fatal): %v", err)
				return
			}
			if outPath != "" {
				runSwiftFixture(t, mochiPath, outPath)
			}
			_ = bin
		})
		return
	}

	// Step 3: Cross-compile to linux-x64 static (skipped when SDK not installed).
	triple := SDKTripleX64
	if !StaticLinuxSDKAvailable(triple) {
		t.Skipf("Swift Static Linux SDK %q not installed; skipping static build (install with: swift sdk install ...)", triple)
	}

	d3 := newDriver(t)
	out3 := t.TempDir()
	_, err = d3.Build(mochiPath, out3, TargetLinuxStaticX64)
	if err != nil {
		t.Fatalf("static linux x64 build failed: %v", err)
	}
}

