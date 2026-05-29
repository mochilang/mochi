package lock

import (
	"strings"
	"testing"
)

// ─── encode round-trip ────────────────────────────────────────────────────────

func TestEncodeDecode_Roundtrip(t *testing.T) {
	packages := []KotlinPackage{
		{
			Group:    "org.jetbrains.kotlinx",
			Artifact: "kotlinx-coroutines-core",
			Version:  "1.7.3",
			Source:   Source{Kind: SourceRegistry, Registry: "https://repo.maven.apache.org/maven2"},
			JarSHA256: "aabbccdd",
			JarBLAKE3: "11223344",
			MetadataSHA256: "meta1234",
			WrapperSHA256:  "wrap5678",
			CapabilitiesDeclared: []string{"net"},
			Dependencies: []string{
				"org.jetbrains.kotlin:kotlin-stdlib@1.9.23",
				"org.jetbrains.kotlinx:kotlinx-coroutines-bom@1.7.3",
			},
		},
		{
			Group:    "com.squareup.okhttp3",
			Artifact: "okhttp",
			Version:  "4.12.0",
			Source:   Source{Kind: SourceRegistry},
			JarSHA256: "deadbeef",
		},
	}

	encoded := Encode(packages)
	if !strings.Contains(encoded, "[[kotlin-package]]") {
		t.Errorf("encoded output missing [[kotlin-package]] header:\n%s", encoded)
	}

	decoded, err := DecodeString(encoded)
	if err != nil {
		t.Fatalf("DecodeString: %v\nInput:\n%s", err, encoded)
	}
	if len(decoded) != len(packages) {
		t.Fatalf("want %d packages, got %d", len(packages), len(decoded))
	}

	// Encoded output is sorted by group:artifact, so okhttp comes first.
	assertPackageEqual(t, decoded[0], packages[1]) // okhttp
	assertPackageEqual(t, decoded[1], packages[0]) // kotlinx-coroutines-core
}

func assertPackageEqual(t *testing.T, got, want KotlinPackage) {
	t.Helper()
	if got.Group != want.Group {
		t.Errorf("Group: got %q, want %q", got.Group, want.Group)
	}
	if got.Artifact != want.Artifact {
		t.Errorf("Artifact: got %q, want %q", got.Artifact, want.Artifact)
	}
	if got.Version != want.Version {
		t.Errorf("Version: got %q, want %q", got.Version, want.Version)
	}
	if got.JarSHA256 != want.JarSHA256 {
		t.Errorf("JarSHA256: got %q, want %q", got.JarSHA256, want.JarSHA256)
	}
	if got.JarBLAKE3 != want.JarBLAKE3 {
		t.Errorf("JarBLAKE3: got %q, want %q", got.JarBLAKE3, want.JarBLAKE3)
	}
	if got.MetadataSHA256 != want.MetadataSHA256 {
		t.Errorf("MetadataSHA256: got %q, want %q", got.MetadataSHA256, want.MetadataSHA256)
	}
	if got.WrapperSHA256 != want.WrapperSHA256 {
		t.Errorf("WrapperSHA256: got %q, want %q", got.WrapperSHA256, want.WrapperSHA256)
	}
}

// ─── determinism ──────────────────────────────────────────────────────────────

func TestEncode_Deterministic(t *testing.T) {
	packages := []KotlinPackage{
		{Group: "z.group", Artifact: "z-artifact", Version: "1.0", Source: Source{Kind: SourceRegistry}},
		{Group: "a.group", Artifact: "a-artifact", Version: "1.0", Source: Source{Kind: SourceRegistry}},
		{Group: "m.group", Artifact: "m-artifact", Version: "1.0", Source: Source{Kind: SourceRegistry}},
	}
	out1 := Encode(packages)
	out2 := Encode(packages)
	if out1 != out2 {
		t.Error("Encode is not deterministic")
	}
	// a.group should appear before m.group before z.group
	aIdx := strings.Index(out1, "a.group")
	mIdx := strings.Index(out1, "m.group")
	zIdx := strings.Index(out1, "z.group")
	if !(aIdx < mIdx && mIdx < zIdx) {
		t.Errorf("want alphabetical order; a@%d m@%d z@%d", aIdx, mIdx, zIdx)
	}
}

// ─── source kinds ─────────────────────────────────────────────────────────────

func TestEncodeDecode_GitSource(t *testing.T) {
	p := KotlinPackage{
		Group: "com.example", Artifact: "mylib", Version: "0.1.0",
		Source: Source{
			Kind: SourceGit,
			URL:  "https://github.com/example/mylib",
			Rev:  "abc123",
		},
	}
	encoded := Encode([]KotlinPackage{p})
	decoded, err := DecodeString(encoded)
	if err != nil {
		t.Fatalf("decode: %v", err)
	}
	if len(decoded) != 1 {
		t.Fatalf("want 1, got %d", len(decoded))
	}
	src := decoded[0].Source
	if src.Kind != SourceGit {
		t.Errorf("kind: %q", src.Kind)
	}
	if src.URL != "https://github.com/example/mylib" {
		t.Errorf("url: %q", src.URL)
	}
	if src.Rev != "abc123" {
		t.Errorf("rev: %q", src.Rev)
	}
}

func TestEncodeDecode_PathSource(t *testing.T) {
	p := KotlinPackage{
		Group: "com.example", Artifact: "local", Version: "dev",
		Source: Source{Kind: SourcePath, Path: "../local-lib"},
	}
	encoded := Encode([]KotlinPackage{p})
	decoded, err := DecodeString(encoded)
	if err != nil {
		t.Fatalf("decode: %v", err)
	}
	if decoded[0].Source.Path != "../local-lib" {
		t.Errorf("path: %q", decoded[0].Source.Path)
	}
}

// ─── forward-compat ───────────────────────────────────────────────────────────

func TestDecode_UnknownKeys(t *testing.T) {
	input := `[[kotlin-package]]
group = "com.example"
artifact = "lib"
version = "1.0"
source = { kind = "registry" }
unknown-future-key = "ignored"
another-unknown = ["a", "b"]
`
	pkgs, err := DecodeString(input)
	if err != nil {
		t.Fatalf("unexpected error on unknown keys: %v", err)
	}
	if len(pkgs) != 1 || pkgs[0].Artifact != "lib" {
		t.Errorf("unexpected result: %+v", pkgs)
	}
}

func TestDecode_OutsideBlockIgnored(t *testing.T) {
	input := `# mochi.lock
schema-version = "2"

[[go-module]]
path = "github.com/foo/bar"

[[kotlin-package]]
group = "com.example"
artifact = "lib"
version = "1.0"
source = { kind = "registry" }
`
	pkgs, err := DecodeString(input)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pkgs) != 1 {
		t.Fatalf("want 1 package, got %d", len(pkgs))
	}
}

// ─── empty inputs ─────────────────────────────────────────────────────────────

func TestEncode_Empty(t *testing.T) {
	if got := Encode(nil); got != "" {
		t.Errorf("Encode(nil) = %q; want empty", got)
	}
}

func TestDecode_Empty(t *testing.T) {
	pkgs, err := DecodeString("")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pkgs) != 0 {
		t.Errorf("want 0, got %d", len(pkgs))
	}
}

// ─── Check (lock drift detection) ────────────────────────────────────────────

func TestCheck_NoDrift(t *testing.T) {
	p := KotlinPackage{
		Group: "com.example", Artifact: "lib", Version: "1.0",
		Source:         Source{Kind: SourceRegistry},
		JarSHA256:      "aabb",
		JarBLAKE3:      "ccdd",
		MetadataSHA256: "eeff",
		WrapperSHA256:  "0011",
	}
	errs := Check([]KotlinPackage{p}, []KotlinPackage{p})
	if len(errs) != 0 {
		t.Errorf("want 0 drift errors, got %d: %v", len(errs), errs)
	}
}

func TestCheck_JarHashDrift(t *testing.T) {
	locked := KotlinPackage{Group: "com.example", Artifact: "lib", Version: "1.0",
		Source: Source{Kind: SourceRegistry}, JarSHA256: "old"}
	fresh := KotlinPackage{Group: "com.example", Artifact: "lib", Version: "1.0",
		Source: Source{Kind: SourceRegistry}, JarSHA256: "new"}
	errs := Check([]KotlinPackage{locked}, []KotlinPackage{fresh})
	if len(errs) != 1 {
		t.Fatalf("want 1 drift error, got %d", len(errs))
	}
	if errs[0].Field != "jar-sha256" {
		t.Errorf("field = %q; want jar-sha256", errs[0].Field)
	}
}

func TestCheck_WrapperDrift(t *testing.T) {
	locked := KotlinPackage{Group: "com.example", Artifact: "lib", Version: "1.0",
		Source: Source{Kind: SourceRegistry}, WrapperSHA256: "v1"}
	fresh := KotlinPackage{Group: "com.example", Artifact: "lib", Version: "1.0",
		Source: Source{Kind: SourceRegistry}, WrapperSHA256: "v2"}
	errs := Check([]KotlinPackage{locked}, []KotlinPackage{fresh})
	if len(errs) != 1 || errs[0].Field != "wrapper-sha256" {
		t.Errorf("want wrapper-sha256 drift, got %+v", errs)
	}
}

func TestCheck_MissingPackage(t *testing.T) {
	locked := KotlinPackage{Group: "com.example", Artifact: "old-lib", Version: "1.0",
		Source: Source{Kind: SourceRegistry}}
	fresh := KotlinPackage{Group: "com.example", Artifact: "new-lib", Version: "1.0",
		Source: Source{Kind: SourceRegistry}}
	errs := Check([]KotlinPackage{locked}, []KotlinPackage{fresh})
	if len(errs) != 1 || errs[0].Field != "package" {
		t.Errorf("want missing-package drift, got %+v", errs)
	}
}

func TestCheckErr_ReturnsNilOnClean(t *testing.T) {
	p := KotlinPackage{Group: "g", Artifact: "a", Version: "1.0",
		Source: Source{Kind: SourceRegistry}, JarSHA256: "x"}
	if err := CheckErr([]KotlinPackage{p}, []KotlinPackage{p}); err != nil {
		t.Errorf("unexpected error: %v", err)
	}
}

// ─── string array edge cases ──────────────────────────────────────────────────

func TestDecode_EmptyArray(t *testing.T) {
	input := `[[kotlin-package]]
group = "com.example"
artifact = "lib"
version = "1.0"
source = { kind = "registry" }
capabilities-declared = []
`
	pkgs, err := DecodeString(input)
	if err != nil {
		t.Fatalf("decode: %v", err)
	}
	if len(pkgs[0].CapabilitiesDeclared) != 0 {
		t.Errorf("expected empty slice, got %v", pkgs[0].CapabilitiesDeclared)
	}
}

func TestDecode_MultiDependencies(t *testing.T) {
	input := `[[kotlin-package]]
group = "io.ktor"
artifact = "ktor-client-core"
version = "2.3.9"
source = { kind = "registry" }
dependencies = ["org.jetbrains.kotlin:kotlin-stdlib@1.9.23", "io.ktor:ktor-io@2.3.9"]
`
	pkgs, err := DecodeString(input)
	if err != nil {
		t.Fatalf("decode: %v", err)
	}
	if len(pkgs[0].Dependencies) != 2 {
		t.Errorf("want 2 deps, got %d", len(pkgs[0].Dependencies))
	}
}
