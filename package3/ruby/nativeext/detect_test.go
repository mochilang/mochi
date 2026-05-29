package nativeext

import (
	"strings"
	"testing"

	"mochi/package3/ruby/index"
)

// helpers

func gv(version, platform string) index.GemVersion {
	return index.GemVersion{Version: version, Platform: platform}
}

func versions(vs ...index.GemVersion) []index.GemVersion { return vs }

// ---- CurrentPlatform / goToRubyPlatform tests ----

func TestGoToRubyPlatform_LinuxAmd64(t *testing.T) {
	p := goToRubyPlatform("linux", "amd64")
	if p != "x86_64-linux" {
		t.Errorf("expected x86_64-linux, got %q", p)
	}
}

func TestGoToRubyPlatform_LinuxArm64(t *testing.T) {
	p := goToRubyPlatform("linux", "arm64")
	if p != "aarch64-linux" {
		t.Errorf("expected aarch64-linux, got %q", p)
	}
}

func TestGoToRubyPlatform_DarwinAmd64(t *testing.T) {
	p := goToRubyPlatform("darwin", "amd64")
	if p != "x86_64-darwin" {
		t.Errorf("expected x86_64-darwin, got %q", p)
	}
}

func TestGoToRubyPlatform_DarwinArm64(t *testing.T) {
	p := goToRubyPlatform("darwin", "arm64")
	if p != "arm64-darwin" {
		t.Errorf("expected arm64-darwin, got %q", p)
	}
}

func TestGoToRubyPlatform_WindowsAmd64(t *testing.T) {
	p := goToRubyPlatform("windows", "amd64")
	if p != "x64-mingw32" {
		t.Errorf("expected x64-mingw32, got %q", p)
	}
}

func TestGoToRubyPlatform_Unknown(t *testing.T) {
	p := goToRubyPlatform("plan9", "mips")
	if p != "ruby" {
		t.Errorf("expected ruby for unknown platform, got %q", p)
	}
}

func TestCurrentPlatform_EnvOverride(t *testing.T) {
	t.Setenv("RUBY_PLATFORM", "x86_64-linux-musl")
	p := CurrentPlatform()
	if p != "x86_64-linux-musl" {
		t.Errorf("expected x86_64-linux-musl from env, got %q", p)
	}
}

func TestCurrentPlatform_FallsBackToRuntime(t *testing.T) {
	t.Setenv("RUBY_PLATFORM", "")
	p := CurrentPlatform()
	// Just verify it returns something non-empty.
	if p == "" {
		t.Error("expected non-empty platform string")
	}
}

// ---- Detect tests ----

func TestDetect_PureRubyGem(t *testing.T) {
	vs := versions(
		gv("1.0.0", ""),
		gv("0.9.0", ""),
	)
	r := Detect("rack", vs, "x86_64-linux")
	if r.Status != ExtNone {
		t.Errorf("expected ExtNone, got %v", r.Status)
	}
	if r.GemName != "rack" {
		t.Errorf("expected GemName=rack, got %q", r.GemName)
	}
	if len(r.BinaryPlatforms) != 0 {
		t.Errorf("expected empty BinaryPlatforms, got %v", r.BinaryPlatforms)
	}
	if !strings.Contains(r.Advice, "pure-Ruby") {
		t.Errorf("advice should mention pure-Ruby, got %q", r.Advice)
	}
}

func TestDetect_EmptyVersionList(t *testing.T) {
	r := Detect("unknown-gem", nil, "x86_64-linux")
	if r.Status != ExtNone {
		t.Errorf("expected ExtNone for empty versions, got %v", r.Status)
	}
	if r.Version != "" {
		t.Errorf("expected empty version, got %q", r.Version)
	}
}

func TestDetect_BinaryAvailable_ExactMatch(t *testing.T) {
	vs := versions(
		gv("1.16.2", "x86_64-linux"),
		gv("1.16.2", "arm64-darwin"),
		gv("1.16.2", ""),
	)
	r := Detect("nokogiri", vs, "x86_64-linux")
	if r.Status != ExtBinaryAvailable {
		t.Errorf("expected ExtBinaryAvailable, got %v", r.Status)
	}
	if !strings.Contains(r.Advice, "x86_64-linux") {
		t.Errorf("advice should mention platform, got %q", r.Advice)
	}
}

func TestDetect_BinaryAvailable_Arm64Darwin(t *testing.T) {
	vs := versions(
		gv("1.16.2", "x86_64-linux"),
		gv("1.16.2", "arm64-darwin"),
	)
	r := Detect("nokogiri", vs, "arm64-darwin")
	if r.Status != ExtBinaryAvailable {
		t.Errorf("expected ExtBinaryAvailable, got %v", r.Status)
	}
}

func TestDetect_BinaryPlatformsCollected(t *testing.T) {
	vs := versions(
		gv("1.0.0", "x86_64-linux"),
		gv("1.0.0", "arm64-darwin"),
		gv("1.0.0", "x64-mingw32"),
		gv("1.0.0", ""),
	)
	r := Detect("native-gem", vs, "x86_64-linux")
	if len(r.BinaryPlatforms) != 3 {
		t.Errorf("expected 3 binary platforms, got %v", r.BinaryPlatforms)
	}
}

func TestDetect_BinaryPlatformsSorted(t *testing.T) {
	vs := versions(
		gv("1.0.0", "x86_64-linux"),
		gv("1.0.0", "arm64-darwin"),
		gv("1.0.0", "aarch64-linux"),
	)
	r := Detect("gem", vs, "x86_64-linux")
	for i := 1; i < len(r.BinaryPlatforms); i++ {
		if r.BinaryPlatforms[i] < r.BinaryPlatforms[i-1] {
			t.Errorf("BinaryPlatforms not sorted: %v", r.BinaryPlatforms)
			break
		}
	}
}

func TestDetect_SourceOnly_NoBinaryForPlatform(t *testing.T) {
	vs := versions(
		gv("0.5.6", "x86_64-linux"),
		gv("0.5.6", "x86_64-darwin"),
	)
	r := Detect("mysql2", vs, "arm64-darwin")
	if r.Status != ExtSourceOnly {
		t.Errorf("expected ExtSourceOnly for mysql2 on arm64-darwin, got %v", r.Status)
	}
	if !strings.Contains(r.Advice, "allow_source_build") {
		t.Errorf("advice should mention allow_source_build, got %q", r.Advice)
	}
}

func TestDetect_PureRubyFallback_JsonGem(t *testing.T) {
	vs := versions(
		gv("2.7.1", "x86_64-linux"),
		gv("2.7.1", "x86_64-darwin"),
		gv("2.7.1", ""),
	)
	// arm64-darwin has no binary for json in this list.
	r := Detect("json", vs, "arm64-darwin")
	if r.Status != ExtPureRubyFallback {
		t.Errorf("expected ExtPureRubyFallback for json on arm64-darwin, got %v", r.Status)
	}
	if !strings.Contains(r.Advice, "json_pure") {
		t.Errorf("advice should mention json_pure, got %q", r.Advice)
	}
}

func TestDetect_PureRubyFallback_NokogiriHypothetical(t *testing.T) {
	vs := versions(
		gv("1.16.2", "x86_64-linux"),
	)
	// arm64-darwin not in the list, and nokogiri has a hypothetical pure-ruby alternative.
	r := Detect("nokogiri", vs, "arm64-darwin")
	if r.Status != ExtPureRubyFallback {
		t.Errorf("expected ExtPureRubyFallback for nokogiri with no arm64-darwin binary, got %v", r.Status)
	}
}

func TestDetect_PgGem_SourceOnly_NoFallback(t *testing.T) {
	vs := versions(
		gv("1.5.8", "x86_64-linux"),
		gv("1.5.8", "x86_64-darwin"),
	)
	r := Detect("pg", vs, "arm64-darwin")
	if r.Status != ExtSourceOnly {
		t.Errorf("expected ExtSourceOnly for pg on arm64-darwin (no pure alternative), got %v", r.Status)
	}
}

func TestDetect_VersionFromFirstEntry(t *testing.T) {
	vs := versions(
		gv("2.0.0", "x86_64-linux"),
		gv("1.0.0", "x86_64-linux"),
	)
	r := Detect("gem", vs, "x86_64-linux")
	if r.Version != "2.0.0" {
		t.Errorf("expected version 2.0.0 from first entry, got %q", r.Version)
	}
}

func TestDetect_PlatformPrefixMatch(t *testing.T) {
	// "arm64-darwin-23" should match a request for "arm64-darwin".
	vs := versions(
		gv("1.0.0", "arm64-darwin-23"),
	)
	r := Detect("native", vs, "arm64-darwin")
	if r.Status != ExtBinaryAvailable {
		t.Errorf("expected ExtBinaryAvailable for arm64-darwin-23 when asking for arm64-darwin, got %v", r.Status)
	}
}

func TestDetect_MultipleVersionsAllPureRuby(t *testing.T) {
	vs := versions(
		gv("3.0.0", ""),
		gv("2.5.0", ""),
		gv("1.0.0", ""),
	)
	r := Detect("sinatra", vs, "x86_64-linux")
	if r.Status != ExtNone {
		t.Errorf("expected ExtNone for sinatra (pure Ruby), got %v", r.Status)
	}
}

func TestDetect_GemNamePreserved(t *testing.T) {
	vs := versions(gv("1.0.0", ""))
	r := Detect("my-special-gem", vs, "x86_64-linux")
	if r.GemName != "my-special-gem" {
		t.Errorf("expected GemName=my-special-gem, got %q", r.GemName)
	}
}

func TestDetect_AutoDetectPlatform(t *testing.T) {
	t.Setenv("RUBY_PLATFORM", "x86_64-linux")
	vs := versions(
		gv("1.0.0", "x86_64-linux"),
		gv("1.0.0", "arm64-darwin"),
	)
	// Pass "" to trigger auto-detect; RUBY_PLATFORM=x86_64-linux should match.
	r := Detect("native", vs, "")
	if r.Status != ExtBinaryAvailable {
		t.Errorf("expected ExtBinaryAvailable with auto-detected x86_64-linux, got %v", r.Status)
	}
}

func TestDetect_UnknownGemNotInFallbackMap_SourceOnly(t *testing.T) {
	vs := versions(
		gv("0.1.0", "x86_64-linux"),
	)
	// "totally-unknown-native-gem" is not in PureRubyFallbackGems.
	r := Detect("totally-unknown-native-gem", vs, "arm64-darwin")
	if r.Status != ExtSourceOnly {
		t.Errorf("expected ExtSourceOnly for unknown gem, got %v", r.Status)
	}
}

func TestDetect_BcryptGem_SourceOnly(t *testing.T) {
	vs := versions(
		gv("3.1.7", "x86_64-linux"),
	)
	r := Detect("bcrypt", vs, "arm64-darwin")
	// bcrypt is in PureRubyFallbackGems but with empty value -> no pure fallback -> SourceOnly.
	if r.Status != ExtSourceOnly {
		t.Errorf("expected ExtSourceOnly for bcrypt (empty fallback entry), got %v", r.Status)
	}
}

func TestDetect_AdviceContainsGemName(t *testing.T) {
	vs := versions(gv("1.0.0", "x86_64-linux"))
	r := Detect("my-native-gem", vs, "arm64-darwin")
	if !strings.Contains(r.Advice, "my-native-gem") {
		t.Errorf("advice should contain gem name, got %q", r.Advice)
	}
}

func TestDetect_ExtNone_StatusString(t *testing.T) {
	if ExtNone.String() != "ExtNone" {
		t.Errorf("unexpected string for ExtNone: %q", ExtNone.String())
	}
}

func TestDetect_ExtBinaryAvailable_StatusString(t *testing.T) {
	if ExtBinaryAvailable.String() != "ExtBinaryAvailable" {
		t.Errorf("unexpected string for ExtBinaryAvailable: %q", ExtBinaryAvailable.String())
	}
}

func TestDetect_ExtSourceOnly_StatusString(t *testing.T) {
	if ExtSourceOnly.String() != "ExtSourceOnly" {
		t.Errorf("unexpected string for ExtSourceOnly: %q", ExtSourceOnly.String())
	}
}

func TestDetect_ExtPureRubyFallback_StatusString(t *testing.T) {
	if ExtPureRubyFallback.String() != "ExtPureRubyFallback" {
		t.Errorf("unexpected string for ExtPureRubyFallback: %q", ExtPureRubyFallback.String())
	}
}

func TestDetect_UnknownStatusString(t *testing.T) {
	s := ExtStatus(99)
	if !strings.Contains(s.String(), "99") {
		t.Errorf("unexpected string for unknown ExtStatus: %q", s.String())
	}
}

func TestDetect_NoDuplicateBinaryPlatforms(t *testing.T) {
	vs := versions(
		gv("1.0.0", "x86_64-linux"),
		gv("0.9.0", "x86_64-linux"), // same platform, different version
		gv("1.0.0", "arm64-darwin"),
	)
	r := Detect("dup-gem", vs, "x86_64-linux")
	if len(r.BinaryPlatforms) != 2 {
		t.Errorf("expected 2 unique binary platforms, got %v", r.BinaryPlatforms)
	}
}

func TestDetect_WindowsPlatform(t *testing.T) {
	vs := versions(
		gv("1.0.0", "x64-mingw32"),
		gv("1.0.0", "x86_64-linux"),
	)
	r := Detect("win-gem", vs, "x64-mingw32")
	if r.Status != ExtBinaryAvailable {
		t.Errorf("expected ExtBinaryAvailable for x64-mingw32, got %v", r.Status)
	}
}
