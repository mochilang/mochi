package lockfile

import (
	"reflect"
	"strings"
	"testing"
)

func sampleRegistryPkg() RustPackage {
	return RustPackage{
		Name:    "tokio",
		Version: "1.42.0",
		Source: Source{
			Kind:     SourceRegistry,
			Registry: "https://index.crates.io",
		},
		CrateBlake3:          "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef",
		CrateSHA256:          "fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210",
		RustdocTypesVersion:  "0.39.0",
		RustdocSHA256:        "aaaa",
		WrapperSHA256:        "bbbb",
		CapabilitiesDeclared: []string{"net", "fs"},
		Dependencies:         []string{"mio@^1.0", "bytes@^1.7", "pin-project-lite@^0.2"},
		Features:             []string{"rt", "macros"},
	}
}

func TestEncodeBasic(t *testing.T) {
	got := Encode([]RustPackage{sampleRegistryPkg()})
	wantBits := []string{
		"[[rust-package]]",
		`name = "tokio"`,
		`version = "1.42.0"`,
		`source = { kind = "registry", registry = "https://index.crates.io" }`,
		`crate-blake3 = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"`,
		`crate-sha256 = "fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210"`,
		`rustdoc-types-version = "0.39.0"`,
		`rustdoc-sha256 = "aaaa"`,
		`wrapper-sha256 = "bbbb"`,
		`capabilities-declared = ["net", "fs"]`,
		`dependencies = ["mio@^1.0", "bytes@^1.7", "pin-project-lite@^0.2"]`,
		`features = ["rt", "macros"]`,
	}
	for _, bit := range wantBits {
		if !strings.Contains(got, bit) {
			t.Errorf("Encode output missing %q\n--- output ---\n%s", bit, got)
		}
	}
}

func TestEncodeSortsByName(t *testing.T) {
	got := Encode([]RustPackage{
		{Name: "zlib", Version: "1.0", Source: Source{Kind: SourceRegistry}},
		{Name: "anyhow", Version: "1.0", Source: Source{Kind: SourceRegistry}},
		{Name: "hex", Version: "0.4.3", Source: Source{Kind: SourceRegistry}},
	})
	idxAnyhow := strings.Index(got, `name = "anyhow"`)
	idxHex := strings.Index(got, `name = "hex"`)
	idxZlib := strings.Index(got, `name = "zlib"`)
	if !(idxAnyhow >= 0 && idxAnyhow < idxHex && idxHex < idxZlib) {
		t.Errorf("Encode did not sort by name. anyhow=%d hex=%d zlib=%d\n%s",
			idxAnyhow, idxHex, idxZlib, got)
	}
}

func TestEncodeDeterministic(t *testing.T) {
	pkgs := []RustPackage{sampleRegistryPkg(), {
		Name: "hex", Version: "0.4.3",
		Source: Source{Kind: SourceRegistry, Registry: "https://index.crates.io"},
	}}
	first := Encode(pkgs)
	for i := 0; i < 10; i++ {
		if got := Encode(pkgs); got != first {
			t.Fatalf("Encode non-deterministic on iter %d\n--- first ---\n%s\n--- got ---\n%s", i, first, got)
		}
	}
}

func TestEncodeOmitsEmptyArrays(t *testing.T) {
	got := Encode([]RustPackage{{
		Name: "minimal", Version: "0.1.0",
		Source: Source{Kind: SourceRegistry},
	}})
	if strings.Contains(got, "capabilities-declared = []") {
		t.Errorf("Encode emitted empty capabilities-declared\n%s", got)
	}
	if strings.Contains(got, "dependencies = []") {
		t.Errorf("Encode emitted empty dependencies\n%s", got)
	}
	if strings.Contains(got, "features = []") {
		t.Errorf("Encode emitted empty features\n%s", got)
	}
}

func TestEncodeGitSource(t *testing.T) {
	got := Encode([]RustPackage{{
		Name: "ax", Version: "0.1",
		Source: Source{
			Kind: SourceGit,
			URL:  "https://github.com/ax/ax.git",
			Rev:  "abc123",
		},
	}})
	wantBit := `source = { kind = "git", url = "https://github.com/ax/ax.git", rev = "abc123" }`
	if !strings.Contains(got, wantBit) {
		t.Errorf("Encode missing git source line %q\n%s", wantBit, got)
	}
}

func TestEncodePathSource(t *testing.T) {
	got := Encode([]RustPackage{{
		Name: "local", Version: "0.0.0",
		Source: Source{
			Kind: SourcePath,
			Path: "./vendor/local",
		},
	}})
	wantBit := `source = { kind = "path", path = "./vendor/local" }`
	if !strings.Contains(got, wantBit) {
		t.Errorf("Encode missing path source line %q\n%s", wantBit, got)
	}
}

func TestDecodeBasic(t *testing.T) {
	in := Encode([]RustPackage{sampleRegistryPkg()})
	out, err := DecodeString(in)
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}
	if len(out) != 1 {
		t.Fatalf("Decode returned %d entries; want 1", len(out))
	}
	if !reflect.DeepEqual(out[0], sampleRegistryPkg()) {
		t.Errorf("Decode roundtrip mismatch\nwant %+v\nhave %+v", sampleRegistryPkg(), out[0])
	}
}

func TestDecodeMultiplePackages(t *testing.T) {
	in := Encode([]RustPackage{
		sampleRegistryPkg(),
		{Name: "hex", Version: "0.4.3", Source: Source{Kind: SourceRegistry, Registry: "https://index.crates.io"}},
	})
	out, err := DecodeString(in)
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}
	if len(out) != 2 {
		t.Fatalf("Decode returned %d entries; want 2", len(out))
	}
	if out[0].Name != "hex" {
		t.Errorf("Decode[0].Name = %q; want hex (sorted)", out[0].Name)
	}
	if out[1].Name != "tokio" {
		t.Errorf("Decode[1].Name = %q; want tokio", out[1].Name)
	}
}

func TestDecodeIgnoresCommentsAndBlankLines(t *testing.T) {
	in := `# header comment

[[rust-package]]
# inline comment
name = "hex"
version = "0.4.3"
source = { kind = "registry", registry = "https://index.crates.io" }
`
	out, err := DecodeString(in)
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}
	if len(out) != 1 || out[0].Name != "hex" {
		t.Errorf("Decode = %+v; want one hex entry", out)
	}
}

func TestDecodeIgnoresUnknownKey(t *testing.T) {
	in := `[[rust-package]]
name = "hex"
version = "0.4.3"
source = { kind = "registry" }
future-field = "x"
`
	out, err := DecodeString(in)
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}
	if len(out) != 1 || out[0].Name != "hex" {
		t.Errorf("unknown-key tolerance failed: %+v", out)
	}
}

func TestDecodeRejectsMalformedLine(t *testing.T) {
	in := `[[rust-package]]
name "hex"
`
	if _, err := DecodeString(in); err == nil {
		t.Errorf("Decode accepted line without '='")
	}
}

func TestDecodeRejectsUnquotedString(t *testing.T) {
	in := `[[rust-package]]
name = hex
version = "0.1"
source = { kind = "registry" }
`
	if _, err := DecodeString(in); err == nil {
		t.Errorf("Decode accepted unquoted string for name")
	}
}

func TestDecodeRejectsMissingSourceKind(t *testing.T) {
	in := `[[rust-package]]
name = "hex"
version = "0.1"
source = { registry = "x" }
`
	if _, err := DecodeString(in); err == nil {
		t.Errorf("Decode accepted source without kind")
	}
}

func TestDecodeGitSource(t *testing.T) {
	in := `[[rust-package]]
name = "ax"
version = "0.1"
source = { kind = "git", url = "https://github.com/ax/ax.git", rev = "abc" }
`
	out, err := DecodeString(in)
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}
	if out[0].Source.Kind != SourceGit || out[0].Source.URL != "https://github.com/ax/ax.git" || out[0].Source.Rev != "abc" {
		t.Errorf("git source decoded wrong: %+v", out[0].Source)
	}
}

func TestDecodePathSource(t *testing.T) {
	in := `[[rust-package]]
name = "local"
version = "0.0.0"
source = { kind = "path", path = "./vendor/local" }
`
	out, err := DecodeString(in)
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}
	if out[0].Source.Kind != SourcePath || out[0].Source.Path != "./vendor/local" {
		t.Errorf("path source decoded wrong: %+v", out[0].Source)
	}
}

func TestDecodeStringArrayWithCommasInside(t *testing.T) {
	in := `[[rust-package]]
name = "hex"
version = "0.1"
source = { kind = "registry" }
dependencies = ["a@^1.0, ^2.0", "b@^1"]
`
	out, err := DecodeString(in)
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}
	if len(out[0].Dependencies) != 2 || out[0].Dependencies[0] != "a@^1.0, ^2.0" {
		t.Errorf("string array with interior comma decoded wrong: %v", out[0].Dependencies)
	}
}

func TestDecodeEmptyArray(t *testing.T) {
	in := `[[rust-package]]
name = "hex"
version = "0.1"
source = { kind = "registry" }
dependencies = []
`
	out, err := DecodeString(in)
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}
	if len(out[0].Dependencies) != 0 {
		t.Errorf("empty array decoded as %v; want empty", out[0].Dependencies)
	}
}

func TestDecodeRoundTripStability(t *testing.T) {
	want := []RustPackage{
		sampleRegistryPkg(),
		{
			Name: "ax", Version: "0.1",
			Source:               Source{Kind: SourceGit, URL: "https://x", Rev: "ff"},
			CapabilitiesDeclared: []string{"net"},
		},
	}
	enc1 := Encode(want)
	dec, err := DecodeString(enc1)
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}
	enc2 := Encode(dec)
	if enc1 != enc2 {
		t.Errorf("encode-decode-encode not idempotent\n--- first ---\n%s\n--- second ---\n%s", enc1, enc2)
	}
}

func TestDecodeIgnoresPreambleLines(t *testing.T) {
	in := `# This is mochi.lock
version = 3
some-other-section = "x"

[[rust-package]]
name = "hex"
version = "0.4.3"
source = { kind = "registry" }
`
	out, err := DecodeString(in)
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}
	if len(out) != 1 || out[0].Name != "hex" {
		t.Errorf("Decode = %+v; want one hex entry (preamble must be ignored)", out)
	}
}

func TestDecodeReaderInput(t *testing.T) {
	in := strings.NewReader(`[[rust-package]]
name = "hex"
version = "0.4.3"
source = { kind = "registry" }
`)
	out, err := Decode(in)
	if err != nil {
		t.Fatalf("Decode reader: %v", err)
	}
	if len(out) != 1 || out[0].Name != "hex" {
		t.Errorf("Decode reader = %+v; want one hex entry", out)
	}
}

func TestSplitTopLevelHandlesNestedBraces(t *testing.T) {
	got := splitTopLevel(`kind = "x", source = { kind = "y", url = "u, v" }, more = "z"`, ',')
	want := []string{`kind = "x"`, `source = { kind = "y", url = "u, v" }`, `more = "z"`}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("splitTopLevel = %v; want %v", got, want)
	}
}

func TestSplitTopLevelHandlesNestedBrackets(t *testing.T) {
	got := splitTopLevel(`a = ["x, y", "z"], b = "c"`, ',')
	want := []string{`a = ["x, y", "z"]`, `b = "c"`}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("splitTopLevel = %v; want %v", got, want)
	}
}
