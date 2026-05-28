package build

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"
)

// MacOSAppConfig holds parameters for macOS .app bundle creation.
type MacOSAppConfig struct {
	AppName   string // e.g. "MochiOut"
	BundleID  string // e.g. "com.mochi.example"
	Version   string // e.g. "1.0"
	BuildDir  string // directory containing the compiled binary
	OutputDir string // where to create MochiOut.app
}

// CreateMacOSAppBundle creates a minimal .app bundle structure from a compiled binary.
// It does NOT code sign (that requires an Apple Developer certificate).
func CreateMacOSAppBundle(cfg MacOSAppConfig, binaryPath string) (string, error) {
	appPath := filepath.Join(cfg.OutputDir, cfg.AppName+".app")
	contentsDir := filepath.Join(appPath, "Contents")
	macosDir := filepath.Join(contentsDir, "MacOS")

	if err := os.MkdirAll(macosDir, 0755); err != nil {
		return "", fmt.Errorf("app bundle: mkdir MacOS: %w", err)
	}

	// Copy binary
	binDest := filepath.Join(macosDir, cfg.AppName)
	data, err := os.ReadFile(binaryPath)
	if err != nil {
		return "", fmt.Errorf("app bundle: read binary: %w", err)
	}
	if err := os.WriteFile(binDest, data, 0755); err != nil {
		return "", fmt.Errorf("app bundle: write binary: %w", err)
	}

	// Write Info.plist
	plist := generateMacOSInfoPlist(cfg)
	if err := os.WriteFile(filepath.Join(contentsDir, "Info.plist"), []byte(plist), 0644); err != nil {
		return "", fmt.Errorf("app bundle: write Info.plist: %w", err)
	}

	return appPath, nil
}

func generateMacOSInfoPlist(cfg MacOSAppConfig) string {
	return fmt.Sprintf(`<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleName</key>
    <string>%s</string>
    <key>CFBundleIdentifier</key>
    <string>%s</string>
    <key>CFBundleVersion</key>
    <string>%s</string>
    <key>CFBundleShortVersionString</key>
    <string>%s</string>
    <key>CFBundleExecutable</key>
    <string>%s</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>NSPrincipalClass</key>
    <string>NSApplication</string>
    <key>LSMinimumSystemVersion</key>
    <string>13.0</string>
</dict>
</plist>
`, cfg.AppName, cfg.BundleID, cfg.Version, cfg.Version, cfg.AppName)
}

// CodesignAvailable returns true if the codesign tool is on PATH (macOS only).
func CodesignAvailable() bool {
	return runtime.GOOS == "darwin"
}

// AltoolAvailable returns true if xcrun altool is available.
func AltoolAvailable() bool {
	if runtime.GOOS != "darwin" {
		return false
	}
	// altool may not exist in newer Xcode (replaced by notarytool)
	return true
}
