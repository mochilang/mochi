// Package xcframework supports consuming .xcframework bundles.
// An XCFramework bundles multiple slices (iOS device, iOS simulator, macOS)
// in a single archive with an Info.plist describing the library variants.
package xcframework

import (
	"encoding/xml"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

// Library represents one slice inside an .xcframework.
type Library struct {
	// LibraryIdentifier is the platform+arch identifier (e.g. "macos-arm64_x86_64").
	LibraryIdentifier string
	// LibraryPath is the relative path to the .a or .framework inside the xcframework.
	LibraryPath string
	// SupportedPlatform is the platform ("macos", "ios", "iphonesimulator", etc.).
	SupportedPlatform string
	// SupportedArchitectures is the list of CPU architectures.
	SupportedArchitectures []string
}

// XCFramework describes a parsed .xcframework bundle.
type XCFramework struct {
	// Path is the root path of the .xcframework directory.
	Path string
	// Libraries is the list of available slices.
	Libraries []Library
}

// Parse reads the Info.plist from an .xcframework directory and returns the
// available library slices.
func Parse(xcframeworkPath string) (*XCFramework, error) {
	plistPath := filepath.Join(xcframeworkPath, "Info.plist")
	data, err := os.ReadFile(plistPath)
	if err != nil {
		return nil, fmt.Errorf("xcframework: read Info.plist: %w", err)
	}
	libs, err := parsePlist(data)
	if err != nil {
		return nil, fmt.Errorf("xcframework: parse Info.plist: %w", err)
	}
	return &XCFramework{Path: xcframeworkPath, Libraries: libs}, nil
}

// SelectSlice returns the library slice that best matches the current build platform.
// Returns an error if no suitable slice is found.
func (x *XCFramework) SelectSlice(platform string) (*Library, error) {
	if platform == "" {
		platform = hostPlatform()
	}
	for i := range x.Libraries {
		lib := &x.Libraries[i]
		if strings.HasPrefix(lib.SupportedPlatform, platform) ||
			strings.HasPrefix(lib.LibraryIdentifier, platform) {
			return lib, nil
		}
	}
	return nil, fmt.Errorf("xcframework: no slice for platform %q in %s", platform, x.Path)
}

// LibraryPath returns the full path to the static library for the selected slice.
func (x *XCFramework) LibraryPath(lib *Library) string {
	return filepath.Join(x.Path, lib.LibraryIdentifier, lib.LibraryPath)
}

// hostPlatform returns the current build platform name.
func hostPlatform() string {
	switch runtime.GOOS {
	case "darwin":
		return "macos"
	case "linux":
		return "linux"
	case "windows":
		return "windows"
	default:
		return runtime.GOOS
	}
}

// parsePlist is a minimal plist parser for XCFramework Info.plist files.
// Apple plist XML: <dict><key>AvailableLibraries</key><array><dict>...</dict></array></dict>
func parsePlist(data []byte) ([]Library, error) {
	dec := xml.NewDecoder(strings.NewReader(string(data)))

	var libs []Library
	inAvailableLibs := false
	inLib := false
	inKey := false
	inString := false
	var currentLib Library
	var pendingKey string

	for {
		tok, err := dec.Token()
		if err != nil {
			break
		}
		switch t := tok.(type) {
		case xml.StartElement:
			switch t.Name.Local {
			case "key":
				inKey = true
				pendingKey = ""
			case "string":
				inString = true
			case "array":
				if pendingKey == "AvailableLibraries" {
					inAvailableLibs = true
					pendingKey = ""
				}
			case "dict":
				if inAvailableLibs && !inLib {
					inLib = true
					currentLib = Library{}
				}
			}
		case xml.EndElement:
			switch t.Name.Local {
			case "key":
				inKey = false
			case "string":
				inString = false
			case "dict":
				if inLib {
					libs = append(libs, currentLib)
					currentLib = Library{}
					inLib = false
				}
			case "array":
				if inAvailableLibs {
					inAvailableLibs = false
				}
			}
		case xml.CharData:
			val := strings.TrimSpace(string(t))
			if val == "" {
				continue
			}
			if inKey {
				pendingKey = val
			} else if inString && inLib {
				switch pendingKey {
				case "LibraryIdentifier":
					currentLib.LibraryIdentifier = val
					pendingKey = ""
				case "LibraryPath":
					currentLib.LibraryPath = val
					pendingKey = ""
				case "SupportedPlatform":
					currentLib.SupportedPlatform = val
					pendingKey = ""
				}
			}
		}
	}
	return libs, nil
}
