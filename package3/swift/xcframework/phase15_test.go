package xcframework_test

import (
	"os"
	"path/filepath"
	"testing"

	"mochi/package3/swift/xcframework"
)

// TestPhase15XCFramework is the MEP-69 Phase 15 gate for XCFramework consumer.
func TestPhase15XCFramework(t *testing.T) {
	t.Run("ParseMissingInfoPlist", func(t *testing.T) {
		_, err := xcframework.Parse("/nonexistent/MyLib.xcframework")
		if err == nil {
			t.Error("expected error for missing Info.plist")
		}
	})

	t.Run("ParseAndSelectSlice", func(t *testing.T) {
		// Create a minimal .xcframework with Info.plist.
		dir := t.TempDir()
		xcfDir := filepath.Join(dir, "MyLib.xcframework")
		if err := os.MkdirAll(xcfDir, 0o755); err != nil {
			t.Fatalf("mkdir: %v", err)
		}

		// Minimal Apple Info.plist with two library slices.
		plist := `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
<key>AvailableLibraries</key>
<array>
<dict>
<key>LibraryIdentifier</key>
<string>macos-arm64_x86_64</string>
<key>LibraryPath</key>
<string>libMyLib.a</string>
<key>SupportedPlatform</key>
<string>macos</string>
</dict>
<dict>
<key>LibraryIdentifier</key>
<string>ios-arm64</string>
<key>LibraryPath</key>
<string>libMyLib.a</string>
<key>SupportedPlatform</key>
<string>ios</string>
</dict>
</array>
</dict>
</plist>`
		if err := os.WriteFile(filepath.Join(xcfDir, "Info.plist"), []byte(plist), 0o644); err != nil {
			t.Fatalf("write Info.plist: %v", err)
		}

		xcf, err := xcframework.Parse(xcfDir)
		if err != nil {
			t.Fatalf("Parse: %v", err)
		}
		if len(xcf.Libraries) == 0 {
			t.Fatal("expected at least one library slice")
		}

		// Select the macOS slice.
		lib, err := xcf.SelectSlice("macos")
		if err != nil {
			t.Fatalf("SelectSlice(macos): %v", err)
		}
		if lib.SupportedPlatform != "macos" {
			t.Errorf("SupportedPlatform: got %q, want macos", lib.SupportedPlatform)
		}

		// LibraryPath helper.
		libPath := xcf.LibraryPath(lib)
		if libPath == "" {
			t.Error("LibraryPath returned empty string")
		}
	})

	t.Run("SelectSliceNotFound", func(t *testing.T) {
		xcf := &xcframework.XCFramework{
			Libraries: []xcframework.Library{
				{LibraryIdentifier: "ios-arm64", SupportedPlatform: "ios", LibraryPath: "libMyLib.a"},
			},
		}
		_, err := xcf.SelectSlice("macos")
		if err == nil {
			t.Error("expected error when no macos slice is available")
		}
	})
}
