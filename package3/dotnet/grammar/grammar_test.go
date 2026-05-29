package grammar_test

import (
	"testing"

	"mochi/package3/dotnet/grammar"
)

func TestParseSpecBareID(t *testing.T) {
	spec, err := grammar.ParseSpec("Newtonsoft.Json")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if spec.ID != "Newtonsoft.Json" {
		t.Errorf("ID = %q; want Newtonsoft.Json", spec.ID)
	}
	if spec.VersionReq != "" {
		t.Errorf("VersionReq = %q; want empty", spec.VersionReq)
	}
	if spec.Source != "registry" {
		t.Errorf("Source = %q; want registry", spec.Source)
	}
}

func TestParseSpecExplicitVersion(t *testing.T) {
	spec, err := grammar.ParseSpec("Newtonsoft.Json@13.0.3")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if spec.ID != "Newtonsoft.Json" {
		t.Errorf("ID = %q; want Newtonsoft.Json", spec.ID)
	}
	if spec.VersionReq != "13.0.3" {
		t.Errorf("VersionReq = %q; want 13.0.3", spec.VersionReq)
	}
	if spec.Source != "registry" {
		t.Errorf("Source = %q; want registry", spec.Source)
	}
}

func TestParseSpecNuGetRange(t *testing.T) {
	spec, err := grammar.ParseSpec("Newtonsoft.Json@[13.0,14.0)")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if spec.ID != "Newtonsoft.Json" {
		t.Errorf("ID = %q; want Newtonsoft.Json", spec.ID)
	}
	if spec.VersionReq != "[13.0,14.0)" {
		t.Errorf("VersionReq = %q; want [13.0,14.0)", spec.VersionReq)
	}
	if spec.Source != "registry" {
		t.Errorf("Source = %q; want registry", spec.Source)
	}
}

func TestParseSpecGitWithRevision(t *testing.T) {
	spec, err := grammar.ParseSpec("MyPkg@git+https://github.com/foo/bar#abc123")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if spec.ID != "MyPkg" {
		t.Errorf("ID = %q; want MyPkg", spec.ID)
	}
	if spec.Source != "git" {
		t.Errorf("Source = %q; want git", spec.Source)
	}
	if spec.GitURL != "https://github.com/foo/bar" {
		t.Errorf("GitURL = %q; want https://github.com/foo/bar", spec.GitURL)
	}
	if spec.GitRev != "abc123" {
		t.Errorf("GitRev = %q; want abc123", spec.GitRev)
	}
}

func TestParseSpecGitNoRevision(t *testing.T) {
	spec, err := grammar.ParseSpec("MyPkg@git+https://github.com/foo/bar")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if spec.Source != "git" {
		t.Errorf("Source = %q; want git", spec.Source)
	}
	if spec.GitRev != "" {
		t.Errorf("GitRev = %q; want empty", spec.GitRev)
	}
	if spec.GitURL != "https://github.com/foo/bar" {
		t.Errorf("GitURL = %q", spec.GitURL)
	}
}

func TestParseSpecLocalPath(t *testing.T) {
	spec, err := grammar.ParseSpec("MyPkg@path+../my-pkg")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if spec.ID != "MyPkg" {
		t.Errorf("ID = %q; want MyPkg", spec.ID)
	}
	if spec.Source != "path" {
		t.Errorf("Source = %q; want path", spec.Source)
	}
	if spec.LocalPath != "../my-pkg" {
		t.Errorf("LocalPath = %q; want ../my-pkg", spec.LocalPath)
	}
}

func TestParseSpecLocalPathAbsolute(t *testing.T) {
	spec, err := grammar.ParseSpec("LocalPkg@path+/home/user/my-lib")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if spec.LocalPath != "/home/user/my-lib" {
		t.Errorf("LocalPath = %q; want /home/user/my-lib", spec.LocalPath)
	}
}

func TestParseSpecEmptyString(t *testing.T) {
	_, err := grammar.ParseSpec("")
	if err == nil {
		t.Error("expected error for empty spec; got nil")
	}
}

func TestParseSpecEmptyVersionAfterAt(t *testing.T) {
	_, err := grammar.ParseSpec("Pkg@")
	if err == nil {
		t.Error("expected error for empty version after @; got nil")
	}
}

func TestParseSpecWithHyphens(t *testing.T) {
	spec, err := grammar.ParseSpec("My-Package@1.2.3")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if spec.ID != "My-Package" {
		t.Errorf("ID = %q; want My-Package", spec.ID)
	}
	if spec.VersionReq != "1.2.3" {
		t.Errorf("VersionReq = %q; want 1.2.3", spec.VersionReq)
	}
}

func TestParseSpecWithUnderscores(t *testing.T) {
	spec, err := grammar.ParseSpec("My_Package@2.0.0")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if spec.ID != "My_Package" {
		t.Errorf("ID = %q; want My_Package", spec.ID)
	}
}

func TestParseSpecSingleWord(t *testing.T) {
	spec, err := grammar.ParseSpec("Serilog")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if spec.ID != "Serilog" {
		t.Errorf("ID = %q; want Serilog", spec.ID)
	}
	if spec.Source != "registry" {
		t.Errorf("Source = %q; want registry", spec.Source)
	}
}

func TestParseSpecInvalidIDChar(t *testing.T) {
	_, err := grammar.ParseSpec("Bad Pkg")
	if err == nil {
		t.Error("expected error for space in package id")
	}
}

func TestParseSpecGitEmptyURL(t *testing.T) {
	_, err := grammar.ParseSpec("Pkg@git+")
	if err == nil {
		t.Error("expected error for empty git URL")
	}
}

func TestParseSpecPathEmpty(t *testing.T) {
	_, err := grammar.ParseSpec("Pkg@path+")
	if err == nil {
		t.Error("expected error for empty local path")
	}
}

func TestEntryPointSimple(t *testing.T) {
	got := grammar.EntryPoint("Serilog")
	if got != "mochi_serilog" {
		t.Errorf("EntryPoint(Serilog) = %q; want mochi_serilog", got)
	}
}

func TestEntryPointDotted(t *testing.T) {
	got := grammar.EntryPoint("Newtonsoft.Json")
	if got != "mochi_newtonsoftjson" {
		t.Errorf("EntryPoint(Newtonsoft.Json) = %q; want mochi_newtonsoftjson", got)
	}
}

func TestEntryPointHyphenated(t *testing.T) {
	got := grammar.EntryPoint("My-Package")
	if got != "mochi_mypackage" {
		t.Errorf("EntryPoint(My-Package) = %q; want mochi_mypackage", got)
	}
}

func TestEntryPointMixedCase(t *testing.T) {
	got := grammar.EntryPoint("Microsoft.Extensions.Logging")
	if got != "mochi_microsoftextensionslogging" {
		t.Errorf("EntryPoint(Microsoft.Extensions.Logging) = %q; want mochi_microsoftextensionslogging", got)
	}
}

func TestLangToken(t *testing.T) {
	if grammar.LangToken != "dotnet" {
		t.Errorf("LangToken = %q; want dotnet", grammar.LangToken)
	}
}
