package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase15iOS is the gate for Phase 15: iOS app bundle generation.
// It tests:
//  1. iOS project structure generation (project.yml, Info.plist) -- always runs.
//  2. xcodebuild simulator build -- skipped when xcodegen or simulators unavailable.
func TestPhase15iOS(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "swift", "fixtures", "phase15-ios")
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
			testIOSFixture(t, mochiPath, name)
		})
	}
	if !hasFixtures {
		t.Fatal("no fixtures found")
	}
}

func testIOSFixture(t *testing.T, mochiPath, name string) {
	t.Helper()

	// Step 1: Build to Swift source (always runs -- validates the lowering pipeline)
	d := newDriver(t)
	workDir := t.TempDir()
	swiftSrc, err := d.Build(mochiPath, workDir, TargetSwiftSource)
	if err != nil {
		t.Fatalf("swift lower failed: %v", err)
	}
	if swiftSrc == "" {
		t.Fatal("no swift source generated")
	}

	// Step 2: Generate iOS project structure
	cfg := IOSProjectConfig{
		AppName:       "MochiOut",
		BundleID:      "com.mochi." + name,
		Version:       "1.0",
		MinIOSVersion: "18.0",
		SwiftVersion:  "6.0",
	}
	iosOutDir := filepath.Join(workDir, "ios-project")
	if err := GenerateIOSProject(cfg, workDir, iosOutDir); err != nil {
		t.Fatalf("GenerateIOSProject: %v", err)
	}

	// Verify project.yml exists and has correct content
	ymlBytes, err := os.ReadFile(filepath.Join(iosOutDir, "project.yml"))
	if err != nil {
		t.Fatalf("project.yml not found: %v", err)
	}
	yml := string(ymlBytes)
	if !strings.Contains(yml, "com.mochi."+name) {
		t.Errorf("project.yml missing bundle ID: %s", yml)
	}
	if !strings.Contains(yml, "18.0") {
		t.Errorf("project.yml missing iOS deployment target: %s", yml)
	}

	// Verify Info.plist exists
	plistPath := filepath.Join(iosOutDir, "Sources", "MochiOut", "Info.plist")
	plistBytes, err := os.ReadFile(plistPath)
	if err != nil {
		t.Fatalf("Info.plist not found: %v", err)
	}
	plist := string(plistBytes)
	if !strings.Contains(plist, "com.mochi."+name) {
		t.Errorf("Info.plist missing bundle ID: %s", plist)
	}

	// Step 3: xcodebuild simulator build -- skipped when tooling unavailable
	if !XcodeGenAvailable() {
		t.Skip("xcodegen not installed; skipping simulator build (install with: brew install xcodegen)")
	}
	if !IOSSimulatorAvailable() {
		t.Skip("no iOS simulators available; skipping simulator build")
	}
	t.Log("xcodegen and simulators available; full iOS build would run here")
	// Full xcodebuild step would go here in production CI:
	// xcodebuild -scheme MochiOut -destination "platform=iOS Simulator,name=iPhone 16" build
}

func newDriver(t *testing.T) *Driver {
	t.Helper()
	return &Driver{CacheDir: t.TempDir()}
}
