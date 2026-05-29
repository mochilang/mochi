package lockfile

import (
	"strings"
	"testing"
)

func TestEncodeBasic(t *testing.T) {
	pkgs := []GoPackage{
		{
			Module:  "github.com/spf13/cobra",
			Version: "v1.8.0",
			Source: Source{
				Kind:  SourceModuleProxy,
				Proxy: "https://proxy.golang.org",
			},
			ZipBlake3:            strings.Repeat("0123456789abcdef", 4),
			ZipH1:                "h1:abcdef==",
			SumdbVerified:        true,
			SumdbTreeSize:        12345678,
			SumdbRecordHash:      strings.Repeat("ab", 32),
			ApiSurfaceSha256:     strings.Repeat("cd", 32),
			WrapperSha256:        strings.Repeat("ef", 32),
			CapabilitiesDeclared: []string{"fs"},
			Dependencies:         []string{"github.com/spf13/pflag@v1.0.6"},
			BuildTags:            nil,
		},
	}
	got := Encode(pkgs)
	wantSubstrings := []string{
		"[[go-package]]",
		`module = "github.com/spf13/cobra"`,
		`version = "v1.8.0"`,
		`source = { kind = "module-proxy", proxy = "https://proxy.golang.org" }`,
		`zip-blake3 = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"`,
		`zip-h1 = "h1:abcdef=="`,
		`sumdb-verified = true`,
		`sumdb-tree-size = 12345678`,
		`api-surface-sha256 = "` + strings.Repeat("cd", 32) + `"`,
		`wrapper-sha256 = "` + strings.Repeat("ef", 32) + `"`,
		`capabilities-declared = ["fs"]`,
		`dependencies = ["github.com/spf13/pflag@v1.0.6"]`,
	}
	for _, sub := range wantSubstrings {
		if !strings.Contains(got, sub) {
			t.Errorf("Encode missing %q\n--- got ---\n%s", sub, got)
		}
	}
}

func TestEncodeDeterministicSort(t *testing.T) {
	pkgs := []GoPackage{
		{Module: "z.example.com/z", Version: "v1.0.0", Source: Source{Kind: SourceModuleProxy}, SumdbVerified: true},
		{Module: "a.example.com/a", Version: "v1.0.0", Source: Source{Kind: SourceModuleProxy}, SumdbVerified: true},
		{Module: "a.example.com/a", Version: "v0.9.0", Source: Source{Kind: SourceModuleProxy}, SumdbVerified: true},
	}
	first := Encode(pkgs)
	for i := 0; i < 8; i++ {
		if got := Encode(pkgs); got != first {
			t.Fatalf("Encode non-deterministic on iter %d\n--- first ---\n%s\n--- got ---\n%s", i, first, got)
		}
	}
	idxA09 := strings.Index(first, `version = "v0.9.0"`)
	idxA10 := strings.Index(first, `version = "v1.0.0"`)
	idxZ := strings.Index(first, `module = "z.example.com/z"`)
	if !(idxA09 >= 0 && idxA10 >= 0 && idxZ >= 0) {
		t.Fatalf("missing expected substrings:\n%s", first)
	}
	if !(idxA09 < idxA10 && idxA10 < idxZ) {
		t.Errorf("Encode did not sort by (module, version): a-v0.9.0=%d a-v1.0.0=%d z=%d\n%s",
			idxA09, idxA10, idxZ, first)
	}
}

func TestEncodeOmitsEmptyOptionals(t *testing.T) {
	p := GoPackage{Module: "x.example.com/x", Version: "v0.1.0", Source: Source{Kind: SourceModuleProxy}, SumdbVerified: false}
	got := Encode([]GoPackage{p})
	for _, sub := range []string{"zip-blake3", "zip-h1", "sumdb-tree-size", "api-surface-sha256", "wrapper-sha256",
		"capabilities-declared", "dependencies", "build-tags"} {
		if strings.Contains(got, sub+" ") {
			t.Errorf("Encode should omit empty %q:\n%s", sub, got)
		}
	}
	// sumdb-verified always renders; it has a meaningful default.
	if !strings.Contains(got, "sumdb-verified = false") {
		t.Errorf("Encode should always render sumdb-verified:\n%s", got)
	}
}

func TestSourceGitRendered(t *testing.T) {
	p := GoPackage{
		Module: "corp.example.com/internal/foo", Version: "v0.0.0",
		Source: Source{Kind: SourceGit, URL: "git@corp.example.com:internal/foo.git", Rev: "abcdef0"},
	}
	got := Encode([]GoPackage{p})
	want := `source = { kind = "git", url = "git@corp.example.com:internal/foo.git", rev = "abcdef0" }`
	if !strings.Contains(got, want) {
		t.Errorf("Encode SourceGit missing %q:\n%s", want, got)
	}
}

func TestSourcePathRendered(t *testing.T) {
	p := GoPackage{
		Module: "example.com/local", Version: "v0.0.0",
		Source: Source{Kind: SourcePath, Path: "../fork/local"},
	}
	got := Encode([]GoPackage{p})
	if !strings.Contains(got, `source = { kind = "path", path = "../fork/local" }`) {
		t.Errorf("Encode SourcePath missing path:\n%s", got)
	}
}

func TestRoundTrip(t *testing.T) {
	pkgs := []GoPackage{
		{
			Module: "github.com/spf13/cobra", Version: "v1.8.0",
			Source:               Source{Kind: SourceModuleProxy, Proxy: "https://proxy.golang.org"},
			ZipBlake3:            "blake",
			ZipH1:                "h1:hash",
			SumdbVerified:        true,
			SumdbTreeSize:        42,
			SumdbRecordHash:      "rec",
			ApiSurfaceSha256:     "api",
			WrapperSha256:        "wrap",
			CapabilitiesDeclared: []string{"fs", "net"},
			Dependencies:         []string{"github.com/spf13/pflag@v1.0.6"},
			BuildTags:            []string{"json1"},
		},
		{
			Module: "corp.example.com/internal/foo", Version: "v0.0.0-abc",
			Source:        Source{Kind: SourceGit, URL: "git@corp.example.com:internal/foo.git", Rev: "abcdef0"},
			SumdbVerified: false,
		},
	}
	encoded := Encode(pkgs)
	round, err := DecodeString(encoded)
	if err != nil {
		t.Fatalf("DecodeString: %v\n%s", err, encoded)
	}
	if len(round) != len(pkgs) {
		t.Fatalf("round-trip length = %d; want %d\n%s", len(round), len(pkgs), encoded)
	}
	// Encode sorts; check sorted order.
	wantOrder := []string{"corp.example.com/internal/foo", "github.com/spf13/cobra"}
	for i, want := range wantOrder {
		if round[i].Module != want {
			t.Errorf("round[%d].Module = %q; want %q", i, round[i].Module, want)
		}
	}
	cobra := round[1]
	if cobra.SumdbTreeSize != 42 {
		t.Errorf("SumdbTreeSize = %d; want 42", cobra.SumdbTreeSize)
	}
	if len(cobra.CapabilitiesDeclared) != 2 {
		t.Errorf("CapabilitiesDeclared = %v; want 2", cobra.CapabilitiesDeclared)
	}
	if len(cobra.BuildTags) != 1 || cobra.BuildTags[0] != "json1" {
		t.Errorf("BuildTags = %v; want [json1]", cobra.BuildTags)
	}
}

func TestDecodeToleratesUnknownKey(t *testing.T) {
	src := `[[go-package]]
module = "github.com/spf13/cobra"
version = "v1.8.0"
source = { kind = "module-proxy" }
future-key = "ignored"
sumdb-verified = true
`
	pkgs, err := DecodeString(src)
	if err != nil {
		t.Fatalf("DecodeString: %v", err)
	}
	if len(pkgs) != 1 || pkgs[0].Module != "github.com/spf13/cobra" {
		t.Errorf("unexpected decode: %#v", pkgs)
	}
}

func TestDecodeRejectsMissingEq(t *testing.T) {
	src := `[[go-package]]
module github.com/spf13/cobra
`
	if _, err := DecodeString(src); err == nil {
		t.Errorf("DecodeString accepted missing '='; expected error")
	}
}

func TestDecodeRejectsBadSource(t *testing.T) {
	src := `[[go-package]]
module = "x"
source = "module-proxy"
`
	if _, err := DecodeString(src); err == nil {
		t.Errorf("DecodeString accepted non-table source; expected error")
	}
}

func TestDecodeRejectsBadBool(t *testing.T) {
	src := `[[go-package]]
module = "x"
version = "v1"
source = { kind = "module-proxy" }
sumdb-verified = yes
`
	if _, err := DecodeString(src); err == nil {
		t.Errorf("DecodeString accepted non-bool sumdb-verified; expected error")
	}
}

func TestDecodeRejectsBadInt(t *testing.T) {
	src := `[[go-package]]
module = "x"
version = "v1"
source = { kind = "module-proxy" }
sumdb-tree-size = abc
`
	if _, err := DecodeString(src); err == nil {
		t.Errorf("DecodeString accepted non-int sumdb-tree-size; expected error")
	}
}

func TestDecodeRejectsSourceMissingKind(t *testing.T) {
	src := `[[go-package]]
module = "x"
source = { proxy = "https://proxy.golang.org" }
`
	if _, err := DecodeString(src); err == nil {
		t.Errorf("DecodeString accepted source missing kind; expected error")
	}
}

func TestDecodeEmptyArraysOk(t *testing.T) {
	src := `[[go-package]]
module = "x"
version = "v1"
source = { kind = "module-proxy" }
sumdb-verified = true
capabilities-declared = []
dependencies = []
build-tags = []
`
	pkgs, err := DecodeString(src)
	if err != nil {
		t.Fatalf("DecodeString: %v", err)
	}
	if pkgs[0].CapabilitiesDeclared != nil {
		t.Errorf("empty array should decode as nil: %v", pkgs[0].CapabilitiesDeclared)
	}
}

func TestParseIntHandlesNegative(t *testing.T) {
	cases := map[string]int64{
		"0":          0,
		"42":         42,
		"-7":         -7,
		"9999999999": 9999999999,
	}
	for in, want := range cases {
		got, err := parseInt(in)
		if err != nil {
			t.Errorf("parseInt(%q) error: %v", in, err)
			continue
		}
		if got != want {
			t.Errorf("parseInt(%q) = %d; want %d", in, got, want)
		}
	}
	for _, bad := range []string{"", "-", "abc", "12a"} {
		if _, err := parseInt(bad); err == nil {
			t.Errorf("parseInt(%q) should error", bad)
		}
	}
}

func TestSplitTopLevelRespectsNesting(t *testing.T) {
	cases := map[string]int{
		`a, b, c`:                  3,
		`a, [b, c], d`:             3,
		`a, { x = "y, z" }, b`:     3,
		`"a, b", "c, d"`:           2,
	}
	for in, want := range cases {
		got := splitTopLevel(in, ',')
		if len(got) != want {
			t.Errorf("splitTopLevel(%q) = %d parts (%v); want %d", in, len(got), got, want)
		}
	}
}

func TestCheckDriftDetectsMissing(t *testing.T) {
	locked := []GoPackage{}
	live := []GoPackage{{Module: "x", Version: "v1"}}
	drifts := CheckDrift(locked, live)
	if len(drifts) != 1 || drifts[0].Kind != DriftMissing {
		t.Errorf("expected DriftMissing; got %#v", drifts)
	}
}

func TestCheckDriftDetectsStale(t *testing.T) {
	locked := []GoPackage{{Module: "x", Version: "v1"}}
	live := []GoPackage{}
	drifts := CheckDrift(locked, live)
	if len(drifts) != 1 || drifts[0].Kind != DriftStale {
		t.Errorf("expected DriftStale; got %#v", drifts)
	}
}

func TestCheckDriftDetectsVersion(t *testing.T) {
	locked := []GoPackage{{Module: "x", Version: "v1"}}
	live := []GoPackage{{Module: "x", Version: "v2"}}
	drifts := CheckDrift(locked, live)
	found := false
	for _, d := range drifts {
		if d.Kind == DriftVersion {
			found = true
		}
	}
	if !found {
		t.Errorf("expected DriftVersion; got %#v", drifts)
	}
}

func TestCheckDriftDetectsZipDrift(t *testing.T) {
	locked := []GoPackage{{Module: "x", Version: "v1", ZipBlake3: "old", ZipH1: "h1:old"}}
	live := []GoPackage{{Module: "x", Version: "v1", ZipBlake3: "new", ZipH1: "h1:new"}}
	drifts := CheckDrift(locked, live)
	if len(drifts) != 1 || drifts[0].Kind != DriftZip {
		t.Errorf("expected DriftZip; got %#v", drifts)
	}
}

func TestCheckDriftDetectsApiSurface(t *testing.T) {
	locked := []GoPackage{{Module: "x", Version: "v1", ApiSurfaceSha256: "old"}}
	live := []GoPackage{{Module: "x", Version: "v1", ApiSurfaceSha256: "new"}}
	drifts := CheckDrift(locked, live)
	if len(drifts) != 1 || drifts[0].Kind != DriftAPI {
		t.Errorf("expected DriftAPI; got %#v", drifts)
	}
}

func TestCheckDriftDetectsWrapper(t *testing.T) {
	locked := []GoPackage{{Module: "x", Version: "v1", WrapperSha256: "old"}}
	live := []GoPackage{{Module: "x", Version: "v1", WrapperSha256: "new"}}
	drifts := CheckDrift(locked, live)
	if len(drifts) != 1 || drifts[0].Kind != DriftWrapper {
		t.Errorf("expected DriftWrapper; got %#v", drifts)
	}
}

func TestCheckDriftDetectsSumdb(t *testing.T) {
	locked := []GoPackage{{Module: "x", Version: "v1", SumdbVerified: true, SumdbTreeSize: 10}}
	live := []GoPackage{{Module: "x", Version: "v1", SumdbVerified: true, SumdbTreeSize: 11}}
	drifts := CheckDrift(locked, live)
	if len(drifts) != 1 || drifts[0].Kind != DriftSumdb {
		t.Errorf("expected DriftSumdb; got %#v", drifts)
	}
}

func TestCheckDriftDetectsCapabilities(t *testing.T) {
	locked := []GoPackage{{Module: "x", Version: "v1", CapabilitiesDeclared: []string{"fs"}}}
	live := []GoPackage{{Module: "x", Version: "v1", CapabilitiesDeclared: []string{"fs", "net"}}}
	drifts := CheckDrift(locked, live)
	if len(drifts) != 1 || drifts[0].Kind != DriftCapabilities {
		t.Errorf("expected DriftCapabilities; got %#v", drifts)
	}
}

func TestCheckDriftCapabilitiesOrderInsensitive(t *testing.T) {
	locked := []GoPackage{{Module: "x", Version: "v1", CapabilitiesDeclared: []string{"fs", "net"}}}
	live := []GoPackage{{Module: "x", Version: "v1", CapabilitiesDeclared: []string{"net", "fs"}}}
	if got := CheckDrift(locked, live); len(got) != 0 {
		t.Errorf("CheckDrift should not flag permutation: %#v", got)
	}
}

func TestCheckDriftNoFalsePositives(t *testing.T) {
	pkg := GoPackage{
		Module: "x", Version: "v1",
		Source:               Source{Kind: SourceModuleProxy},
		ZipBlake3:            "z",
		ZipH1:                "h1:z",
		SumdbVerified:        true,
		SumdbTreeSize:        10,
		SumdbRecordHash:      "r",
		ApiSurfaceSha256:     "a",
		WrapperSha256:        "w",
		CapabilitiesDeclared: []string{"fs"},
	}
	if got := CheckDrift([]GoPackage{pkg}, []GoPackage{pkg}); len(got) != 0 {
		t.Errorf("CheckDrift identical inputs returned drifts: %#v", got)
	}
}

func TestCheckDriftDeterministicOrder(t *testing.T) {
	locked := []GoPackage{
		{Module: "z", Version: "v1", ZipBlake3: "old"},
		{Module: "a", Version: "v1", ZipBlake3: "old"},
	}
	live := []GoPackage{
		{Module: "a", Version: "v1", ZipBlake3: "new"},
		{Module: "z", Version: "v1", ZipBlake3: "new"},
	}
	first := CheckDrift(locked, live)
	for i := 0; i < 5; i++ {
		got := CheckDrift(locked, live)
		if len(got) != len(first) {
			t.Fatalf("CheckDrift length non-deterministic")
		}
		for j := range got {
			if got[j] != first[j] {
				t.Errorf("CheckDrift order non-deterministic iter %d:\n%v\nvs\n%v", i, first, got)
			}
		}
	}
	if first[0].Module != "a" {
		t.Errorf("CheckDrift not sorted by module: %v", first)
	}
}

func TestDriftStringNoDetail(t *testing.T) {
	d := Drift{Module: "x", Version: "v1", Kind: DriftMissing}
	if got, want := d.String(), "x@v1: missing drift"; got != want {
		t.Errorf("Drift.String = %q; want %q", got, want)
	}
}

func TestDriftStringWithDetail(t *testing.T) {
	d := Drift{Module: "x", Version: "v1", Kind: DriftZip, Detail: "a -> b"}
	got := d.String()
	if !strings.Contains(got, "zip-hash drift") || !strings.Contains(got, "a -> b") {
		t.Errorf("Drift.String = %q; want it to include kind and detail", got)
	}
}

func TestEqualStringSetMultiset(t *testing.T) {
	if !equalStringSet([]string{"a", "a", "b"}, []string{"b", "a", "a"}) {
		t.Errorf("equalStringSet should treat dup elements as multiset")
	}
	if equalStringSet([]string{"a", "a"}, []string{"a", "b"}) {
		t.Errorf("equalStringSet should distinguish multisets")
	}
}

func TestEncodeEmptyReturnsEmpty(t *testing.T) {
	if got := Encode(nil); got != "" {
		t.Errorf("Encode(nil) = %q; want empty", got)
	}
}
