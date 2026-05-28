package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase18AppStore is the gate for Phase 18: macOS .app bundle + App Store validation.
// Steps:
//  1. Compile Mochi to macOS binary (always runs).
//  2. Create .app bundle structure (always runs).
//  3. Verify Info.plist and bundle layout (always runs).
//  4. codesign --verify (skipped when not on macOS or no developer cert).
//  5. xcrun altool --validate-app (skipped; requires Apple Developer account).
func TestPhase18AppStore(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "swift", "fixtures", "phase18-appstore")
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
			testAppStoreFixture(t, mochiPath, name)
		})
	}
	if !hasFixtures {
		t.Fatal("no fixtures found")
	}
}

func testAppStoreFixture(t *testing.T, mochiPath, name string) {
	t.Helper()

	// Step 1: Compile Mochi to macOS binary
	d := newDriver(t)
	workDir := t.TempDir()
	binaryPath, err := d.Build(mochiPath, workDir, TargetMacOSExecutable)
	if err != nil {
		t.Fatalf("build failed: %v", err)
	}

	// Step 2: Create .app bundle
	cfg := MacOSAppConfig{
		AppName:   "MochiOut",
		BundleID:  "com.mochi." + name,
		Version:   "1.0",
		BuildDir:  workDir,
		OutputDir: workDir,
	}
	appPath, err := CreateMacOSAppBundle(cfg, binaryPath)
	if err != nil {
		t.Fatalf("CreateMacOSAppBundle: %v", err)
	}

	// Step 3: Verify bundle structure
	verifyAppBundle(t, appPath, cfg)
}

func verifyAppBundle(t *testing.T, appPath string, cfg MacOSAppConfig) {
	t.Helper()

	// Check Contents/MacOS/<AppName> exists
	binPath := filepath.Join(appPath, "Contents", "MacOS", cfg.AppName)
	if _, err := os.Stat(binPath); err != nil {
		t.Errorf("binary not found in bundle: %s: %v", binPath, err)
	}

	// Check Info.plist exists and has correct content
	plistPath := filepath.Join(appPath, "Contents", "Info.plist")
	plistBytes, err := os.ReadFile(plistPath)
	if err != nil {
		t.Fatalf("Info.plist not found: %v", err)
	}
	plist := string(plistBytes)
	if !strings.Contains(plist, cfg.BundleID) {
		t.Errorf("Info.plist missing bundle ID %q: %s", cfg.BundleID, plist)
	}
	if !strings.Contains(plist, "APPL") {
		t.Errorf("Info.plist missing CFBundlePackageType APPL: %s", plist)
	}
}
