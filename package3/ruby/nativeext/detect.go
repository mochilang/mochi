// Package nativeext implements native C extension gem detection and binary gem
// selection for the MEP-76 Ruby bridge.
//
// Phase 12 of MEP-76.
// See website/docs/research/0076/08-native-extensions.md for context.
package nativeext

import (
	"fmt"
	"os"
	"runtime"
	"strings"

	"mochi/package3/ruby/index"
)

// ExtStatus classifies a gem's native extension situation.
type ExtStatus int

const (
	// ExtNone means pure Ruby; no native code detected.
	ExtNone ExtStatus = iota
	// ExtBinaryAvailable means a pre-built binary gem exists for this platform.
	ExtBinaryAvailable
	// ExtSourceOnly means native code exists but no binary gem is available
	// for this platform; source compilation would be required.
	ExtSourceOnly
	// ExtPureRubyFallback means the gem has both native and pure-Ruby variants,
	// and the gem name is in PureRubyFallbackGems with a non-empty alternative.
	ExtPureRubyFallback
)

// String returns a human-readable name for the ExtStatus.
func (s ExtStatus) String() string {
	switch s {
	case ExtNone:
		return "ExtNone"
	case ExtBinaryAvailable:
		return "ExtBinaryAvailable"
	case ExtSourceOnly:
		return "ExtSourceOnly"
	case ExtPureRubyFallback:
		return "ExtPureRubyFallback"
	default:
		return fmt.Sprintf("ExtStatus(%d)", int(s))
	}
}

// DetectResult is returned by Detect.
type DetectResult struct {
	GemName         string
	Version         string
	Status          ExtStatus
	BinaryPlatforms []string // unique non-empty platforms from the version list
	Advice          string   // human-readable advice for the user
}

// PureRubyFallbackGems is the curated map of gems that have known pure-Ruby
// alternatives when the native extension is not available. An empty string
// value means there is no pure-Ruby alternative for that gem.
var PureRubyFallbackGems = map[string]string{
	"nokogiri": "nokogiri-pure-ruby", // hypothetical; nokogiri is native-only in practice
	"pg":       "",                   // no pure Ruby alternative
	"mysql2":   "",
	"bcrypt":   "",
	"json":     "json_pure",
	"psych":    "", // bundled with Ruby stdlib; no separate pure gem
	"stringio": "",
	"strscan":  "",
}

// Detect examines a gem's version list to determine its native extension status.
//
// versions is the list from the compact index (index.GemVersion). platform is
// the Ruby platform string to match (e.g. "x86_64-linux"). Pass "" to
// auto-detect using CurrentPlatform.
//
// The version field of the returned DetectResult is set to the version string
// of the first (newest) entry in versions, or empty if versions is empty.
func Detect(gemName string, versions []index.GemVersion, platform string) *DetectResult {
	if platform == "" {
		platform = CurrentPlatform()
	}

	// Collect all unique non-empty platforms across all versions.
	platformSet := map[string]struct{}{}
	for _, v := range versions {
		if v.Platform != "" {
			platformSet[v.Platform] = struct{}{}
		}
	}
	binaryPlatforms := make([]string, 0, len(platformSet))
	for p := range platformSet {
		binaryPlatforms = append(binaryPlatforms, p)
	}
	sortStrings(binaryPlatforms)

	// Determine the version string from the first entry (compact index is
	// newest-first after parseInfoBody reversal).
	version := ""
	if len(versions) > 0 {
		version = versions[0].Version
	}

	result := &DetectResult{
		GemName:         gemName,
		Version:         version,
		BinaryPlatforms: binaryPlatforms,
	}

	// Case 1: no binary platforms at all — pure Ruby gem.
	if len(platformSet) == 0 {
		result.Status = ExtNone
		result.Advice = fmt.Sprintf(
			"%s is a pure-Ruby gem with no native extensions. It will install on any platform.", gemName)
		return result
	}

	// Case 2: check if a binary gem exists for the current platform.
	if hasPlatform(platformSet, platform) {
		result.Status = ExtBinaryAvailable
		result.Advice = fmt.Sprintf(
			"A pre-built binary gem for %s is available for %s. "+
				"No C compiler or native library headers are required.", gemName, platform)
		return result
	}

	// Case 3: native code exists but no binary for this platform.
	// Check if a pure-Ruby fallback is available.
	if alt, ok := PureRubyFallbackGems[gemName]; ok && alt != "" {
		result.Status = ExtPureRubyFallback
		result.Advice = fmt.Sprintf(
			"No binary gem for %s on %s. "+
				"A pure-Ruby alternative is available: %s. "+
				"Note: the pure-Ruby variant may have known performance limitations; "+
				"the binary gem is preferred when available.", gemName, platform, alt)
		return result
	}

	// Case 4: native extension, no binary for this platform, no pure alternative.
	result.Status = ExtSourceOnly
	result.Advice = fmt.Sprintf(
		"No binary gem for %s on %s (available binary platforms: %s). "+
			"Source compilation is required. "+
			"To enable, add `allow_source_build = true` under [ruby.native] in mochi.toml. "+
			"Ensure the native library headers and a C compiler are installed.",
		gemName, platform, strings.Join(binaryPlatforms, ", "))
	return result
}

// CurrentPlatform returns the current Ruby platform string. It checks the
// RUBY_PLATFORM environment variable first. If that is unset, it synthesises
// the platform from runtime.GOOS and runtime.GOARCH using RubyGems conventions.
func CurrentPlatform() string {
	if p := os.Getenv("RUBY_PLATFORM"); p != "" {
		return p
	}
	return goToRubyPlatform(runtime.GOOS, runtime.GOARCH)
}

// goToRubyPlatform converts a Go (GOOS, GOARCH) pair to the RubyGems platform
// string convention. Returns "ruby" for unknown combinations.
func goToRubyPlatform(goos, goarch string) string {
	switch goos {
	case "linux":
		switch goarch {
		case "amd64":
			return "x86_64-linux"
		case "arm64":
			return "aarch64-linux"
		case "386":
			return "x86-linux"
		default:
			return goarch + "-linux"
		}
	case "darwin":
		switch goarch {
		case "arm64":
			return "arm64-darwin"
		case "amd64":
			return "x86_64-darwin"
		default:
			return goarch + "-darwin"
		}
	case "windows":
		switch goarch {
		case "amd64":
			return "x64-mingw32"
		case "arm64":
			return "x64-mingw32"
		default:
			return "x86-mingw32"
		}
	default:
		return "ruby"
	}
}

// hasPlatform reports whether platformSet contains a platform that is
// compatible with the requested one. A platform is compatible if it matches
// exactly, or if the version is a prefix match (e.g. "arm64-darwin" matches
// "arm64-darwin-23").
func hasPlatform(platformSet map[string]struct{}, want string) bool {
	if _, ok := platformSet[want]; ok {
		return true
	}
	// Also check prefix matches for versioned platform strings like
	// "arm64-darwin-23" when looking for "arm64-darwin".
	for p := range platformSet {
		if strings.HasPrefix(p, want) || strings.HasPrefix(want, p) {
			return true
		}
	}
	return false
}

// sortStrings sorts a string slice in place (simple insertion sort to avoid
// importing sort just for this).
func sortStrings(ss []string) {
	for i := 1; i < len(ss); i++ {
		for j := i; j > 0 && ss[j] < ss[j-1]; j-- {
			ss[j], ss[j-1] = ss[j-1], ss[j]
		}
	}
}
