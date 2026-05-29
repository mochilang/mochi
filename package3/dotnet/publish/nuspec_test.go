package publish_test

import (
	"strings"
	"testing"

	"mochi/package3/dotnet/publish"
)

func sampleMeta() publish.PackageMeta {
	return publish.PackageMeta{
		ID:          "MyMochiLib",
		Version:     "1.0.0",
		Authors:     "tamnd",
		Description: "A Mochi-sourced library",
		ProjectURL:  "https://example.com",
		License:     "MIT",
		Tags:        "mochi dotnet",
	}
}

func TestEmitNuspec_xmlDeclaration(t *testing.T) {
	got := publish.EmitNuspec(sampleMeta(), "net8.0")
	if !strings.Contains(got, `<?xml version="1.0" encoding="utf-8"?>`) {
		t.Errorf("missing XML declaration: %s", got)
	}
}

func TestEmitNuspec_packageElement(t *testing.T) {
	got := publish.EmitNuspec(sampleMeta(), "net8.0")
	if !strings.Contains(got, "<package") {
		t.Errorf("missing <package> element: %s", got)
	}
}

func TestEmitNuspec_nuspecXmlns(t *testing.T) {
	got := publish.EmitNuspec(sampleMeta(), "net8.0")
	if !strings.Contains(got, "schemas.microsoft.com/packaging/2013/05/nuspec.xsd") {
		t.Errorf("missing nuspec xmlns: %s", got)
	}
}

func TestEmitNuspec_id(t *testing.T) {
	got := publish.EmitNuspec(sampleMeta(), "net8.0")
	if !strings.Contains(got, "<id>MyMochiLib</id>") {
		t.Errorf("missing id: %s", got)
	}
}

func TestEmitNuspec_version(t *testing.T) {
	got := publish.EmitNuspec(sampleMeta(), "net8.0")
	if !strings.Contains(got, "<version>1.0.0</version>") {
		t.Errorf("missing version: %s", got)
	}
}

func TestEmitNuspec_authors(t *testing.T) {
	got := publish.EmitNuspec(sampleMeta(), "net8.0")
	if !strings.Contains(got, "<authors>tamnd</authors>") {
		t.Errorf("missing authors: %s", got)
	}
}

func TestEmitNuspec_license(t *testing.T) {
	got := publish.EmitNuspec(sampleMeta(), "net8.0")
	if !strings.Contains(got, `<license type="expression">MIT</license>`) {
		t.Errorf("missing license: %s", got)
	}
}

func TestEmitNuspec_tags(t *testing.T) {
	got := publish.EmitNuspec(sampleMeta(), "net8.0")
	if !strings.Contains(got, "<tags>mochi dotnet</tags>") {
		t.Errorf("missing tags: %s", got)
	}
}

func TestEmitNuspec_dependencyGroup(t *testing.T) {
	got := publish.EmitNuspec(sampleMeta(), "net8.0")
	if !strings.Contains(got, "<dependencies>") {
		t.Errorf("missing dependencies: %s", got)
	}
	if !strings.Contains(got, `targetFramework="net8.0"`) {
		t.Errorf("missing targetFramework: %s", got)
	}
}

func TestEmitNuspec_projectUrl(t *testing.T) {
	got := publish.EmitNuspec(sampleMeta(), "net8.0")
	if !strings.Contains(got, "<projectUrl>https://example.com</projectUrl>") {
		t.Errorf("missing projectUrl: %s", got)
	}
}

func TestEmitNuspec_requireLicenseAcceptance(t *testing.T) {
	meta := sampleMeta()
	meta.RequireLicenseAcceptance = true
	got := publish.EmitNuspec(meta, "net8.0")
	if !strings.Contains(got, "<requireLicenseAcceptance>true</requireLicenseAcceptance>") {
		t.Errorf("missing requireLicenseAcceptance: %s", got)
	}
}

func TestEmitNuspec_noRequireLicenseAcceptance(t *testing.T) {
	got := publish.EmitNuspec(sampleMeta(), "net8.0")
	if strings.Contains(got, "requireLicenseAcceptance") {
		t.Errorf("unexpected requireLicenseAcceptance when false: %s", got)
	}
}

func TestEmitNuspec_readmeFile(t *testing.T) {
	meta := sampleMeta()
	meta.ReadmeFile = "README.md"
	got := publish.EmitNuspec(meta, "net8.0")
	if !strings.Contains(got, "<readme>README.md</readme>") {
		t.Errorf("missing readme: %s", got)
	}
}

func TestEmitNuspec_differentTFM(t *testing.T) {
	got := publish.EmitNuspec(sampleMeta(), "net6.0")
	if !strings.Contains(got, `targetFramework="net6.0"`) {
		t.Errorf("expected net6.0 targetFramework: %s", got)
	}
}

func TestEmitNuspec_xmlEscaping(t *testing.T) {
	meta := sampleMeta()
	meta.Description = "A & B < C > D"
	got := publish.EmitNuspec(meta, "net8.0")
	if strings.Contains(got, " & ") {
		t.Errorf("& should be escaped: %s", got)
	}
	if !strings.Contains(got, "&amp;") {
		t.Errorf("expected &amp;: %s", got)
	}
}
