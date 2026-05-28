package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase16Repro is the Phase 16.3 gate: two independent builds of the same
// Mochi source produce byte-identical binaries when MOCHI_DETERMINISTIC=1.
func TestPhase16Repro(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping reproducibility test in short mode")
	}
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "swift", "fixtures", "phase16-repro")
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
		t.Run(name, func(t *testing.T) {
			testReproducibleBuild(t, mochiPath)
		})
	}
	if !hasFixtures {
		t.Fatal("no fixtures found")
	}
}

func testReproducibleBuild(t *testing.T, mochiPath string) {
	t.Helper()

	// Build 1
	d1 := newDriver(t)
	d1.Deterministic = true
	out1 := t.TempDir()
	bin1, err := d1.Build(mochiPath, out1, TargetMacOSExecutable)
	if err != nil {
		t.Fatalf("build 1 failed: %v", err)
	}

	// Build 2 (different temp dir, simulating different HOME)
	d2 := newDriver(t)
	d2.Deterministic = true
	out2 := t.TempDir()
	bin2, err := d2.Build(mochiPath, out2, TargetMacOSExecutable)
	if err != nil {
		t.Fatalf("build 2 failed: %v", err)
	}

	// Compare SHA-256
	h1, err := sha256File(bin1)
	if err != nil {
		t.Fatalf("sha256 bin1: %v", err)
	}
	h2, err := sha256File(bin2)
	if err != nil {
		t.Fatalf("sha256 bin2: %v", err)
	}
	if h1 != h2 {
		t.Errorf("non-reproducible build:\n  build1 SHA-256: %s\n  build2 SHA-256: %s", h1, h2)
	}
}
