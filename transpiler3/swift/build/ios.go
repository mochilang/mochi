package build

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
)

// IOSProjectConfig holds parameters for iOS project generation.
type IOSProjectConfig struct {
	AppName       string // e.g. "MochiOut"
	BundleID      string // e.g. "com.mochi.example"
	Version       string // e.g. "1.0"
	MinIOSVersion string // e.g. "18.0"
	SwiftVersion  string // e.g. "6.0"
}

// GenerateIOSProject creates the XcodeGen project.yml, Info.plist, and Package.swift
// for an iOS app in the given directory. Returns the path to the generated project.yml.
func GenerateIOSProject(cfg IOSProjectConfig, srcDir, outDir string) error {
	if err := os.MkdirAll(outDir, 0755); err != nil {
		return fmt.Errorf("ios: mkdir %s: %w", outDir, err)
	}

	// Write project.yml for XcodeGen
	yml := generateProjectYML(cfg)
	ymlPath := filepath.Join(outDir, "project.yml")
	if err := os.WriteFile(ymlPath, []byte(yml), 0644); err != nil {
		return fmt.Errorf("ios: write project.yml: %w", err)
	}

	// Write Info.plist
	plist := generateInfoPlist(cfg)
	plistDir := filepath.Join(outDir, "Sources", cfg.AppName)
	if err := os.MkdirAll(plistDir, 0755); err != nil {
		return fmt.Errorf("ios: mkdir Sources/%s: %w", cfg.AppName, err)
	}
	if err := os.WriteFile(filepath.Join(plistDir, "Info.plist"), []byte(plist), 0644); err != nil {
		return fmt.Errorf("ios: write Info.plist: %w", err)
	}

	return nil
}

func generateProjectYML(cfg IOSProjectConfig) string {
	return fmt.Sprintf(`name: %s
options:
  bundleIdPrefix: %s
  deploymentTarget:
    iOS: "%s"
  xcodeVersion: "16.0"
  swiftVersion: "%s"

targets:
  %s:
    type: application
    platform: iOS
    sources: Sources/%s
    settings:
      SWIFT_VERSION: %s
      IPHONEOS_DEPLOYMENT_TARGET: "%s"
      INFOPLIST_FILE: Sources/%s/Info.plist
`, cfg.AppName, cfg.BundleID, cfg.MinIOSVersion, cfg.SwiftVersion,
		cfg.AppName, cfg.AppName, cfg.SwiftVersion, cfg.MinIOSVersion, cfg.AppName)
}

func generateInfoPlist(cfg IOSProjectConfig) string {
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
    <key>UILaunchScreen</key>
    <dict/>
</dict>
</plist>
`, cfg.AppName, cfg.BundleID, cfg.Version, cfg.Version)
}

// XcodeGenAvailable returns true if the xcodegen tool is on PATH.
func XcodeGenAvailable() bool {
	_, err := exec.LookPath("xcodegen")
	return err == nil
}

// IOSSimulatorAvailable returns true if at least one iOS simulator is available.
func IOSSimulatorAvailable() bool {
	if runtime.GOOS != "darwin" {
		return false
	}
	out, err := exec.Command("xcrun", "simctl", "list", "devices", "available", "--json").Output()
	if err != nil {
		return false
	}
	return strings.Contains(string(out), `"iPhone"`)
}
