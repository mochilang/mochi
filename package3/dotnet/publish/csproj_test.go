package publish_test

import (
	"strings"
	"testing"

	"mochi/package3/dotnet/publish"
)

func sampleLibraryCsproj() publish.LibraryCsproj {
	return publish.LibraryCsproj{
		TargetFramework: "net8.0",
		AssemblyName:    "MyMochiLib",
		Version:         "1.0.0",
		Authors:         "tamnd",
		Description:     "A Mochi library",
		PackageID:       "MyMochiLib",
		License:         "MIT",
		ProjectURL:      "https://example.com",
	}
}

func TestEmitLibraryCsproj_sdkAttribute(t *testing.T) {
	got := publish.EmitLibraryCsproj(sampleLibraryCsproj())
	if !strings.Contains(got, `Sdk="Microsoft.NET.Sdk"`) {
		t.Errorf("missing SDK attribute: %s", got)
	}
}

func TestEmitLibraryCsproj_targetFramework(t *testing.T) {
	got := publish.EmitLibraryCsproj(sampleLibraryCsproj())
	if !strings.Contains(got, "<TargetFramework>net8.0</TargetFramework>") {
		t.Errorf("missing TargetFramework: %s", got)
	}
}

func TestEmitLibraryCsproj_assemblyName(t *testing.T) {
	got := publish.EmitLibraryCsproj(sampleLibraryCsproj())
	if !strings.Contains(got, "<AssemblyName>MyMochiLib</AssemblyName>") {
		t.Errorf("missing AssemblyName: %s", got)
	}
}

func TestEmitLibraryCsproj_version(t *testing.T) {
	got := publish.EmitLibraryCsproj(sampleLibraryCsproj())
	if !strings.Contains(got, "<Version>1.0.0</Version>") {
		t.Errorf("missing Version: %s", got)
	}
}

func TestEmitLibraryCsproj_allowUnsafeBlocks(t *testing.T) {
	got := publish.EmitLibraryCsproj(sampleLibraryCsproj())
	if !strings.Contains(got, "<AllowUnsafeBlocks>true</AllowUnsafeBlocks>") {
		t.Errorf("missing AllowUnsafeBlocks: %s", got)
	}
}

func TestEmitLibraryCsproj_nullable(t *testing.T) {
	got := publish.EmitLibraryCsproj(sampleLibraryCsproj())
	if !strings.Contains(got, "<Nullable>enable</Nullable>") {
		t.Errorf("missing Nullable: %s", got)
	}
}

func TestEmitLibraryCsproj_implicitUsings(t *testing.T) {
	got := publish.EmitLibraryCsproj(sampleLibraryCsproj())
	if !strings.Contains(got, "<ImplicitUsings>disable</ImplicitUsings>") {
		t.Errorf("missing ImplicitUsings: %s", got)
	}
}

func TestEmitLibraryCsproj_noAOTByDefault(t *testing.T) {
	got := publish.EmitLibraryCsproj(sampleLibraryCsproj())
	if strings.Contains(got, "PublishAot") {
		t.Errorf("unexpected PublishAot in non-AOT csproj: %s", got)
	}
}

func TestEmitLibraryCsproj_aotEnabled(t *testing.T) {
	p := sampleLibraryCsproj()
	p.IsNativeAOT = true
	got := publish.EmitLibraryCsproj(p)
	if !strings.Contains(got, "<PublishAot>true</PublishAot>") {
		t.Errorf("missing PublishAot: %s", got)
	}
}

func TestEmitLibraryCsproj_runtimeIdentifier(t *testing.T) {
	p := sampleLibraryCsproj()
	p.RuntimeID = "linux-x64"
	got := publish.EmitLibraryCsproj(p)
	if !strings.Contains(got, "<RuntimeIdentifier>linux-x64</RuntimeIdentifier>") {
		t.Errorf("missing RuntimeIdentifier: %s", got)
	}
}

func TestEmitLibraryCsproj_noRuntimeIdentifierByDefault(t *testing.T) {
	got := publish.EmitLibraryCsproj(sampleLibraryCsproj())
	if strings.Contains(got, "RuntimeIdentifier") {
		t.Errorf("unexpected RuntimeIdentifier: %s", got)
	}
}

func TestEmitLibraryCsproj_license(t *testing.T) {
	got := publish.EmitLibraryCsproj(sampleLibraryCsproj())
	if !strings.Contains(got, "<PackageLicenseExpression>MIT</PackageLicenseExpression>") {
		t.Errorf("missing PackageLicenseExpression: %s", got)
	}
}
