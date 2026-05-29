package parser

import "testing"

func TestErlangImportRef_Bare(t *testing.T) {
	pkg, version, ok := ErlangImportRef(`"cowboy"`)
	if !ok {
		t.Fatal("ErlangImportRef should accept bare package name")
	}
	if pkg != "cowboy" {
		t.Errorf("pkg = %q, want cowboy", pkg)
	}
	if version != "" {
		t.Errorf("version = %q, want empty", version)
	}
}

func TestErlangImportRef_WithVersion(t *testing.T) {
	pkg, version, ok := ErlangImportRef(`"cowboy@2.12.0"`)
	if !ok {
		t.Fatal("ErlangImportRef should accept package@version")
	}
	if pkg != "cowboy" {
		t.Errorf("pkg = %q, want cowboy", pkg)
	}
	if version != "2.12.0" {
		t.Errorf("version = %q, want 2.12.0", version)
	}
}

func TestErlangImportRef_TildeGreater(t *testing.T) {
	// Constraint syntax with ~> is valid as a version string.
	_, version, ok := ErlangImportRef(`"hackney@~> 1.20"`)
	if !ok {
		t.Fatal("ErlangImportRef should accept ~> constraint form")
	}
	if version != "~> 1.20" {
		t.Errorf("version = %q, want '~> 1.20'", version)
	}
}

func TestErlangImportRef_InvalidPkgName_Uppercase(t *testing.T) {
	_, _, ok := ErlangImportRef(`"Cowboy"`)
	if ok {
		t.Error("ErlangImportRef should reject uppercase package name")
	}
}

func TestErlangImportRef_InvalidPkgName_Hyphen(t *testing.T) {
	_, _, ok := ErlangImportRef(`"my-package"`)
	if ok {
		t.Error("ErlangImportRef should reject hyphen in package name")
	}
}

func TestErlangImportRef_InvalidPkgName_Empty(t *testing.T) {
	_, _, ok := ErlangImportRef(`""`)
	if ok {
		t.Error("ErlangImportRef should reject empty path")
	}
}

func TestErlangImportRef_InvalidPkgName_StartsWithDigit(t *testing.T) {
	_, _, ok := ErlangImportRef(`"2cowboy"`)
	if ok {
		t.Error("ErlangImportRef should reject package starting with digit")
	}
}

func TestErlangImportRef_EmptyVersion(t *testing.T) {
	_, _, ok := ErlangImportRef(`"cowboy@"`)
	if ok {
		t.Error("ErlangImportRef should reject empty version after @")
	}
}

func TestErlangImportRef_ValidUnderscored(t *testing.T) {
	pkg, _, ok := ErlangImportRef(`"erlware_commons"`)
	if !ok {
		t.Fatal("ErlangImportRef should accept underscored package names")
	}
	if pkg != "erlware_commons" {
		t.Errorf("pkg = %q", pkg)
	}
}

func TestHasErlangSemverPin_WithPin(t *testing.T) {
	if !HasErlangSemverPin(`"cowboy@2.12.0"`) {
		t.Error("should detect version pin")
	}
}

func TestHasErlangSemverPin_WithoutPin(t *testing.T) {
	if HasErlangSemverPin(`"cowboy"`) {
		t.Error("should not detect pin for bare package")
	}
}

func TestIsHexPmPackageName_Valid(t *testing.T) {
	for _, s := range []string{"cowboy", "ranch", "hackney", "jsx", "erlware_commons", "a", "a1"} {
		if !isHexPmPackageName(s) {
			t.Errorf("isHexPmPackageName(%q) should be true", s)
		}
	}
}

func TestIsHexPmPackageName_Invalid(t *testing.T) {
	for _, s := range []string{"", "2cowboy", "Cowboy", "my-pkg", "a b", "pkg!"} {
		if isHexPmPackageName(s) {
			t.Errorf("isHexPmPackageName(%q) should be false", s)
		}
	}
}

func TestParseNormalize_ErlangImportAccepted(t *testing.T) {
	src := `import erlang "cowboy" as cowboy`
	prog, err := ParseString(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if len(prog.Statements) == 0 || prog.Statements[0].Import == nil {
		t.Fatal("expected import statement")
	}
	im := prog.Statements[0].Import
	if im.Lang == nil || *im.Lang != "erlang" {
		t.Errorf("Lang = %v, want erlang", im.Lang)
	}
	if im.Path != "cowboy" {
		t.Errorf("Path = %q, want cowboy", im.Path)
	}
	if im.As != "cowboy" {
		t.Errorf("As = %q, want cowboy", im.As)
	}
}

func TestParseNormalize_ErlangImportWithVersion(t *testing.T) {
	src := `import erlang "cowboy@2.12.0" as cowboy`
	_, err := ParseString(src)
	if err != nil {
		t.Fatalf("parse error for versioned erlang import: %v", err)
	}
}

func TestParseNormalize_ErlangImportInvalidPath(t *testing.T) {
	src := `import erlang "MY-PACKAGE" as pkg`
	_, err := ParseString(src)
	if err == nil {
		t.Error("should reject uppercase/hyphenated erlang import path")
	}
}

func TestParseNormalize_ErlangLangRecognized(t *testing.T) {
	// `erlang` must be in knownImportLangs; unknown lang produces P064.
	src := `import erlang "cowboy"`
	_, err := ParseString(src)
	// May fail for other reasons (missing `as`) but should NOT be P064.
	if err != nil {
		msg := err.Error()
		if contains(msg, "P064") {
			t.Errorf("erlang should be a known import lang, got P064: %v", err)
		}
	}
}

func contains(s, sub string) bool {
	return len(s) >= len(sub) && (s == sub || len(s) > 0 && containsStr(s, sub))
}

func containsStr(s, sub string) bool {
	for i := 0; i <= len(s)-len(sub); i++ {
		if s[i:i+len(sub)] == sub {
			return true
		}
	}
	return false
}
