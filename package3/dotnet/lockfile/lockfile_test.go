package lockfile_test

import (
	"strings"
	"testing"

	"mochi/package3/dotnet/lockfile"
)

func samplePackage() lockfile.DotNetPackage {
	return lockfile.DotNetPackage{
		ID:      "Newtonsoft.Json",
		Version: "13.0.3",
		Source: lockfile.Source{
			Kind:     lockfile.SourceRegistry,
			Registry: "https://api.nuget.org/v3/index.json",
		},
		NupkgSHA512:          "abc123",
		MetadataSHA256:       "def456",
		ShimSHA256:           "ghi789",
		CapabilitiesDeclared: []string{"net"},
		TargetFramework:      "net8.0",
		Dependencies:         []string{"Microsoft.CSharp@^4.7"},
	}
}

func TestEncode_containsHeader(t *testing.T) {
	got := lockfile.Encode([]lockfile.DotNetPackage{samplePackage()})
	if !strings.Contains(got, "[[dotnet-package]]") {
		t.Errorf("missing [[dotnet-package]] header: %s", got)
	}
}

func TestEncode_containsID(t *testing.T) {
	got := lockfile.Encode([]lockfile.DotNetPackage{samplePackage()})
	if !strings.Contains(got, `id = "Newtonsoft.Json"`) {
		t.Errorf("missing id: %s", got)
	}
}

func TestEncode_containsVersion(t *testing.T) {
	got := lockfile.Encode([]lockfile.DotNetPackage{samplePackage()})
	if !strings.Contains(got, `version = "13.0.3"`) {
		t.Errorf("missing version: %s", got)
	}
}

func TestEncode_containsSource(t *testing.T) {
	got := lockfile.Encode([]lockfile.DotNetPackage{samplePackage()})
	if !strings.Contains(got, `source = {`) {
		t.Errorf("missing source: %s", got)
	}
	if !strings.Contains(got, `kind = "registry"`) {
		t.Errorf("missing kind: %s", got)
	}
}

func TestEncode_containsNupkgSHA(t *testing.T) {
	got := lockfile.Encode([]lockfile.DotNetPackage{samplePackage()})
	if !strings.Contains(got, `nupkg-sha512 = "abc123"`) {
		t.Errorf("missing nupkg-sha512: %s", got)
	}
}

func TestEncode_containsMetadataSHA(t *testing.T) {
	got := lockfile.Encode([]lockfile.DotNetPackage{samplePackage()})
	if !strings.Contains(got, `metadata-sha256 = "def456"`) {
		t.Errorf("missing metadata-sha256: %s", got)
	}
}

func TestEncode_containsShimSHA(t *testing.T) {
	got := lockfile.Encode([]lockfile.DotNetPackage{samplePackage()})
	if !strings.Contains(got, `shim-sha256 = "ghi789"`) {
		t.Errorf("missing shim-sha256: %s", got)
	}
}

func TestEncode_containsCapabilities(t *testing.T) {
	got := lockfile.Encode([]lockfile.DotNetPackage{samplePackage()})
	if !strings.Contains(got, `capabilities-declared`) {
		t.Errorf("missing capabilities-declared: %s", got)
	}
}

func TestEncode_containsTargetFramework(t *testing.T) {
	got := lockfile.Encode([]lockfile.DotNetPackage{samplePackage()})
	if !strings.Contains(got, `target-framework = "net8.0"`) {
		t.Errorf("missing target-framework: %s", got)
	}
}

func TestEncode_containsDependencies(t *testing.T) {
	got := lockfile.Encode([]lockfile.DotNetPackage{samplePackage()})
	if !strings.Contains(got, `dependencies`) {
		t.Errorf("missing dependencies: %s", got)
	}
}

func TestEncode_sorted(t *testing.T) {
	pkgs := []lockfile.DotNetPackage{
		{ID: "ZZZ", Version: "1.0.0", Source: lockfile.Source{Kind: lockfile.SourceRegistry}},
		{ID: "AAA", Version: "1.0.0", Source: lockfile.Source{Kind: lockfile.SourceRegistry}},
	}
	got := lockfile.Encode(pkgs)
	aIdx := strings.Index(got, "AAA")
	zIdx := strings.Index(got, "ZZZ")
	if aIdx > zIdx {
		t.Errorf("packages not sorted alphabetically: A at %d, Z at %d", aIdx, zIdx)
	}
}

func TestEncode_empty(t *testing.T) {
	got := lockfile.Encode(nil)
	if got != "" {
		t.Errorf("expected empty string for nil input, got %q", got)
	}
}

func TestRoundTrip_basic(t *testing.T) {
	pkg := samplePackage()
	encoded := lockfile.Encode([]lockfile.DotNetPackage{pkg})
	decoded, err := lockfile.DecodeString(encoded)
	if err != nil {
		t.Fatalf("decode error: %v", err)
	}
	if len(decoded) != 1 {
		t.Fatalf("expected 1 entry, got %d", len(decoded))
	}
	got := decoded[0]
	if got.ID != pkg.ID {
		t.Errorf("ID: want %q, got %q", pkg.ID, got.ID)
	}
	if got.Version != pkg.Version {
		t.Errorf("Version: want %q, got %q", pkg.Version, got.Version)
	}
	if got.Source.Kind != pkg.Source.Kind {
		t.Errorf("Source.Kind: want %q, got %q", pkg.Source.Kind, got.Source.Kind)
	}
	if got.Source.Registry != pkg.Source.Registry {
		t.Errorf("Source.Registry: want %q, got %q", pkg.Source.Registry, got.Source.Registry)
	}
	if got.NupkgSHA512 != pkg.NupkgSHA512 {
		t.Errorf("NupkgSHA512: want %q, got %q", pkg.NupkgSHA512, got.NupkgSHA512)
	}
	if got.TargetFramework != pkg.TargetFramework {
		t.Errorf("TargetFramework: want %q, got %q", pkg.TargetFramework, got.TargetFramework)
	}
}

func TestRoundTrip_multiplePackages(t *testing.T) {
	pkgs := []lockfile.DotNetPackage{
		{ID: "Pkg.A", Version: "1.0.0", Source: lockfile.Source{Kind: lockfile.SourceRegistry, Registry: "https://nuget.org"}},
		{ID: "Pkg.B", Version: "2.0.0", Source: lockfile.Source{Kind: lockfile.SourceRegistry, Registry: "https://nuget.org"}},
	}
	encoded := lockfile.Encode(pkgs)
	decoded, err := lockfile.DecodeString(encoded)
	if err != nil {
		t.Fatalf("decode error: %v", err)
	}
	if len(decoded) != 2 {
		t.Fatalf("expected 2, got %d", len(decoded))
	}
}

func TestDecode_unknownKeysIgnored(t *testing.T) {
	input := `[[dotnet-package]]
id = "Test.Pkg"
version = "1.0.0"
source = { kind = "registry" }
unknown-future-key = "some value"
`
	decoded, err := lockfile.DecodeString(input)
	if err != nil {
		t.Fatalf("unexpected error with unknown keys: %v", err)
	}
	if len(decoded) != 1 {
		t.Fatalf("expected 1, got %d", len(decoded))
	}
	if decoded[0].ID != "Test.Pkg" {
		t.Errorf("ID: want Test.Pkg, got %s", decoded[0].ID)
	}
}

func TestDecode_gitSource(t *testing.T) {
	input := `[[dotnet-package]]
id = "MyPkg"
version = "0.1.0"
source = { kind = "git", url = "https://github.com/foo/bar", rev = "abc123" }
`
	decoded, err := lockfile.DecodeString(input)
	if err != nil {
		t.Fatalf("decode error: %v", err)
	}
	if decoded[0].Source.Kind != lockfile.SourceGit {
		t.Errorf("expected git source, got %s", decoded[0].Source.Kind)
	}
	if decoded[0].Source.URL != "https://github.com/foo/bar" {
		t.Errorf("URL: got %s", decoded[0].Source.URL)
	}
	if decoded[0].Source.Rev != "abc123" {
		t.Errorf("Rev: got %s", decoded[0].Source.Rev)
	}
}

func TestDecode_pathSource(t *testing.T) {
	input := `[[dotnet-package]]
id = "MyPkg"
version = "0.1.0"
source = { kind = "path", path = "../my-pkg" }
`
	decoded, err := lockfile.DecodeString(input)
	if err != nil {
		t.Fatalf("decode error: %v", err)
	}
	if decoded[0].Source.Kind != lockfile.SourcePath {
		t.Errorf("expected path source")
	}
	if decoded[0].Source.Path != "../my-pkg" {
		t.Errorf("Path: got %s", decoded[0].Source.Path)
	}
}

func TestDecode_capabilities(t *testing.T) {
	input := `[[dotnet-package]]
id = "Test"
version = "1.0.0"
source = { kind = "registry" }
capabilities-declared = ["net", "io"]
`
	decoded, err := lockfile.DecodeString(input)
	if err != nil {
		t.Fatalf("decode error: %v", err)
	}
	caps := decoded[0].CapabilitiesDeclared
	if len(caps) != 2 {
		t.Errorf("expected 2 caps, got %d: %v", len(caps), caps)
	}
}

func TestDecode_emptyInput(t *testing.T) {
	decoded, err := lockfile.DecodeString("")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(decoded) != 0 {
		t.Errorf("expected 0, got %d", len(decoded))
	}
}

func TestDecode_outsideBlocks(t *testing.T) {
	input := `# This is a comment
some-other-key = "some-value"

[[dotnet-package]]
id = "Test"
version = "1.0.0"
source = { kind = "registry" }
`
	decoded, err := lockfile.DecodeString(input)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(decoded) != 1 {
		t.Fatalf("expected 1, got %d", len(decoded))
	}
}

func TestEncode_gitSource(t *testing.T) {
	pkg := lockfile.DotNetPackage{
		ID:      "MyPkg",
		Version: "0.1.0",
		Source:  lockfile.Source{Kind: lockfile.SourceGit, URL: "https://github.com/foo/bar", Rev: "abc"},
	}
	got := lockfile.Encode([]lockfile.DotNetPackage{pkg})
	if !strings.Contains(got, `kind = "git"`) {
		t.Errorf("missing git kind: %s", got)
	}
	if !strings.Contains(got, `url = "https://github.com/foo/bar"`) {
		t.Errorf("missing url: %s", got)
	}
}

func TestEncode_pathSource(t *testing.T) {
	pkg := lockfile.DotNetPackage{
		ID:      "Local.Pkg",
		Version: "1.0.0",
		Source:  lockfile.Source{Kind: lockfile.SourcePath, Path: "../local-pkg"},
	}
	got := lockfile.Encode([]lockfile.DotNetPackage{pkg})
	if !strings.Contains(got, `kind = "path"`) {
		t.Errorf("missing path kind: %s", got)
	}
}

func TestEncode_separatorBetweenEntries(t *testing.T) {
	pkgs := []lockfile.DotNetPackage{
		{ID: "A", Version: "1.0.0", Source: lockfile.Source{Kind: lockfile.SourceRegistry}},
		{ID: "B", Version: "1.0.0", Source: lockfile.Source{Kind: lockfile.SourceRegistry}},
	}
	got := lockfile.Encode(pkgs)
	// Should have two [[dotnet-package]] headers.
	count := strings.Count(got, "[[dotnet-package]]")
	if count != 2 {
		t.Errorf("expected 2 headers, got %d: %s", count, got)
	}
}

func TestDecodeReader(t *testing.T) {
	input := `[[dotnet-package]]
id = "Test.Pkg"
version = "1.0.0"
source = { kind = "registry", registry = "https://nuget.org" }
`
	r := strings.NewReader(input)
	decoded, err := lockfile.Decode(r)
	if err != nil {
		t.Fatalf("decode error: %v", err)
	}
	if len(decoded) != 1 {
		t.Fatalf("expected 1, got %d", len(decoded))
	}
}

func TestEncode_omitsEmptyHashes(t *testing.T) {
	pkg := lockfile.DotNetPackage{
		ID:      "Minimal",
		Version: "1.0.0",
		Source:  lockfile.Source{Kind: lockfile.SourceRegistry},
	}
	got := lockfile.Encode([]lockfile.DotNetPackage{pkg})
	if strings.Contains(got, "nupkg-sha512") {
		t.Errorf("should omit empty nupkg-sha512: %s", got)
	}
	if strings.Contains(got, "shim-sha256") {
		t.Errorf("should omit empty shim-sha256: %s", got)
	}
}

func TestEncode_omitsEmptyDependencies(t *testing.T) {
	pkg := lockfile.DotNetPackage{
		ID:      "Minimal",
		Version: "1.0.0",
		Source:  lockfile.Source{Kind: lockfile.SourceRegistry},
	}
	got := lockfile.Encode([]lockfile.DotNetPackage{pkg})
	if strings.Contains(got, "dependencies") {
		t.Errorf("should omit empty dependencies: %s", got)
	}
}
