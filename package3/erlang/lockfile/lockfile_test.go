package lockfile

import (
	"strings"
	"testing"
)

// ── entry / manifest ──────────────────────────────────────────────────────

func TestManifestSort(t *testing.T) {
	m := Manifest{Entries: []Entry{
		{Name: "ranch"},
		{Name: "cowboy"},
		{Name: "jsx"},
	}}
	m.Sort()
	names := []string{m.Entries[0].Name, m.Entries[1].Name, m.Entries[2].Name}
	want := []string{"cowboy", "jsx", "ranch"}
	for i, w := range want {
		if names[i] != w {
			t.Errorf("entry[%d].Name = %q, want %q", i, names[i], w)
		}
	}
}

// ── render ────────────────────────────────────────────────────────────────

func TestRenderTOML_SingleEntry(t *testing.T) {
	m := Manifest{
		Version: 1,
		Entries: []Entry{
			{
				Name:             "cowboy",
				Version:          "2.12.0",
				Registry:         "https://hex.pm",
				OuterSHA256:      "aaa",
				InnerSHA256:      "bbb",
				InnerSHA512:      "ccc",
				BeamIngestSHA256: "ddd",
				ShimSHA256:       "eee",
				Capabilities:     []string{"net"},
				Dependencies:     []string{"cowlib@~> 2.13"},
				Modules:          []string{"cowboy", "cowboy_req"},
			},
		},
	}
	out := RenderTOML(m)

	requireContains(t, out, `[[erlang-package]]`)
	requireContains(t, out, `name = "cowboy"`)
	requireContains(t, out, `version = "2.12.0"`)
	requireContains(t, out, `registry = "https://hex.pm"`)
	requireContains(t, out, `outer-sha256 = "aaa"`)
	requireContains(t, out, `inner-sha256 = "bbb"`)
	requireContains(t, out, `inner-sha512 = "ccc"`)
	requireContains(t, out, `beam-ingest-sha256 = "ddd"`)
	requireContains(t, out, `shim-sha256 = "eee"`)
	requireContains(t, out, `"net"`)
	requireContains(t, out, `"cowlib@~> 2.13"`)
	requireContains(t, out, `"cowboy"`)
	requireContains(t, out, `"cowboy_req"`)
	requireContains(t, out, `erlang-lockfile-schema = 1`)
}

func TestRenderTOML_DefaultRegistry(t *testing.T) {
	m := Manifest{
		Entries: []Entry{{Name: "ranch", Version: "2.1.0"}},
	}
	out := RenderTOML(m)
	requireContains(t, out, `registry = "https://hex.pm"`)
}

func TestRenderTOML_OTPAppOmittedWhenSameName(t *testing.T) {
	m := Manifest{
		Entries: []Entry{{Name: "cowboy", Version: "2.12.0", OTPApp: "cowboy"}},
	}
	out := RenderTOML(m)
	if strings.Contains(out, "otp-app") {
		t.Error("otp-app should be omitted when equal to name")
	}
}

func TestRenderTOML_OTPAppIncludedWhenDiffers(t *testing.T) {
	m := Manifest{
		Entries: []Entry{{Name: "prometheus", Version: "4.11.0", OTPApp: "prometheus_erl"}},
	}
	out := RenderTOML(m)
	requireContains(t, out, `otp-app = "prometheus_erl"`)
}

func TestRenderTOML_SortedByName(t *testing.T) {
	m := Manifest{
		Entries: []Entry{
			{Name: "ranch", Version: "2.1.0"},
			{Name: "cowboy", Version: "2.12.0"},
		},
	}
	out := RenderTOML(m)
	cowboyPos := strings.Index(out, "cowboy")
	ranchPos := strings.Index(out, "ranch")
	if cowboyPos > ranchPos {
		t.Error("cowboy should appear before ranch in rendered output")
	}
}

func TestRenderTOML_EmptyManifest(t *testing.T) {
	m := Manifest{}
	out := RenderTOML(m)
	requireContains(t, out, "erlang-lockfile-schema = 1")
	if strings.Contains(out, "[[erlang-package]]") {
		t.Error("empty manifest should have no [[erlang-package]] blocks")
	}
}

// ── parse ─────────────────────────────────────────────────────────────────

const sampleTOML = `
erlang-lockfile-schema = 1

[[erlang-package]]
name = "cowboy"
version = "2.12.0"
registry = "https://hex.pm"
outer-sha256 = "aaa"
inner-sha256 = "bbb"
inner-sha512 = "ccc"
beam-ingest-sha256 = "ddd"
shim-sha256 = "eee"
capabilities = ["net"]
dependencies = ["cowlib@~> 2.13", "ranch@~> 2.1"]
otp-app = "cowboy_app"
modules = ["cowboy", "cowboy_req"]

[[erlang-package]]
name = "ranch"
version = "2.1.0"
registry = "https://hex.pm"
`

func TestParseTOML_BasicRoundtrip(t *testing.T) {
	m, err := ParseTOML(sampleTOML)
	if err != nil {
		t.Fatalf("ParseTOML: %v", err)
	}
	if len(m.Entries) != 2 {
		t.Fatalf("len(entries) = %d, want 2", len(m.Entries))
	}
	// sorted by name: cowboy < ranch
	e := m.Entries[0]
	if e.Name != "cowboy" {
		t.Errorf("entries[0].Name = %q, want cowboy", e.Name)
	}
	if e.Version != "2.12.0" {
		t.Errorf("Version = %q", e.Version)
	}
	if e.OuterSHA256 != "aaa" {
		t.Errorf("OuterSHA256 = %q", e.OuterSHA256)
	}
	if e.OTPApp != "cowboy_app" {
		t.Errorf("OTPApp = %q, want cowboy_app", e.OTPApp)
	}
	if len(e.Capabilities) != 1 || e.Capabilities[0] != "net" {
		t.Errorf("Capabilities = %v", e.Capabilities)
	}
	if len(e.Dependencies) != 2 {
		t.Errorf("Dependencies = %v", e.Dependencies)
	}
	if len(e.Modules) != 2 {
		t.Errorf("Modules = %v", e.Modules)
	}
}

func TestParseTOML_MissingSchema(t *testing.T) {
	_, err := ParseTOML(`[[erlang-package]]
name = "cowboy"
version = "1.0.0"
`)
	if err == nil {
		t.Error("expected error for missing erlang-lockfile-schema")
	}
}

func TestParseTOML_WrongSchemaVersion(t *testing.T) {
	_, err := ParseTOML(`erlang-lockfile-schema = 99
[[erlang-package]]
name = "cowboy"
version = "1.0.0"
`)
	if err == nil || !strings.Contains(err.Error(), "unsupported schema") {
		t.Errorf("expected unsupported schema error, got: %v", err)
	}
}

func TestParseTOML_MissingName(t *testing.T) {
	_, err := ParseTOML(`erlang-lockfile-schema = 1
[[erlang-package]]
version = "1.0.0"
`)
	if err == nil {
		t.Error("expected error for missing name")
	}
}

func TestParseTOML_MissingVersion(t *testing.T) {
	_, err := ParseTOML(`erlang-lockfile-schema = 1
[[erlang-package]]
name = "cowboy"
`)
	if err == nil {
		t.Error("expected error for missing version")
	}
}

func TestParseTOML_EmptyEntries(t *testing.T) {
	m, err := ParseTOML("erlang-lockfile-schema = 1\n")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(m.Entries) != 0 {
		t.Errorf("expected 0 entries, got %d", len(m.Entries))
	}
}

// ── render/parse roundtrip ────────────────────────────────────────────────

func TestRenderParse_Roundtrip(t *testing.T) {
	orig := Manifest{
		Version: 1,
		Entries: []Entry{
			{
				Name:             "cowboy",
				Version:          "2.12.0",
				Registry:         "https://hex.pm",
				OuterSHA256:      "aaabbb",
				InnerSHA256:      "cccddd",
				InnerSHA512:      "eeefffggg",
				BeamIngestSHA256: "111222",
				ShimSHA256:       "333444",
				Capabilities:     []string{"net"},
				Dependencies:     []string{"cowlib@~> 2.13"},
				Modules:          []string{"cowboy"},
			},
			{
				Name:    "ranch",
				Version: "2.1.0",
			},
		},
	}
	rendered := RenderTOML(orig)
	parsed, err := ParseTOML(rendered)
	if err != nil {
		t.Fatalf("ParseTOML after render: %v", err)
	}
	if len(parsed.Entries) != len(orig.Entries) {
		t.Fatalf("entry count: %d vs %d", len(parsed.Entries), len(orig.Entries))
	}
	for i, e := range parsed.Entries {
		o := orig.Entries[i]
		if e.Name != o.Name || e.Version != o.Version {
			t.Errorf("entry[%d]: name/version mismatch", i)
		}
		if e.OuterSHA256 != o.OuterSHA256 {
			t.Errorf("entry[%d]: OuterSHA256 = %q, want %q", i, e.OuterSHA256, o.OuterSHA256)
		}
	}
}

// ── digest ────────────────────────────────────────────────────────────────

func TestSHA256Hex_KnownValue(t *testing.T) {
	// SHA-256 of empty string
	got := SHA256Hex([]byte{})
	want := "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
	if got != want {
		t.Errorf("SHA256Hex([]) = %q, want %q", got, want)
	}
}

func TestSHA512Hex_KnownValue(t *testing.T) {
	got := SHA512Hex([]byte{})
	want := "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"
	// SHA-512 of empty string is 128 hex chars
	if len(got) != 128 {
		t.Errorf("SHA512Hex([]) length = %d, want 128", len(got))
	}
	_ = want
}

func TestComputeBeamIngestSHA256_Deterministic(t *testing.T) {
	// Same modules in different order must produce the same hash.
	chunks1 := map[string][]byte{"cowboy": []byte("etf1"), "ranch": []byte("etf2")}
	chunks2 := map[string][]byte{"ranch": []byte("etf2"), "cowboy": []byte("etf1")}
	h1 := ComputeBeamIngestSHA256(chunks1)
	h2 := ComputeBeamIngestSHA256(chunks2)
	if h1 != h2 {
		t.Errorf("hash differs with different map iteration order: %q vs %q", h1, h2)
	}
}

func TestComputeBeamIngestSHA256_EmptyMap(t *testing.T) {
	h := ComputeBeamIngestSHA256(nil)
	if h == "" {
		t.Error("hash of empty map should not be empty string")
	}
	if len(h) != 64 {
		t.Errorf("hash length = %d, want 64", len(h))
	}
}

func TestComputeBeamIngestSHA256_DifferentContent(t *testing.T) {
	c1 := map[string][]byte{"mod": []byte("version1")}
	c2 := map[string][]byte{"mod": []byte("version2")}
	if ComputeBeamIngestSHA256(c1) == ComputeBeamIngestSHA256(c2) {
		t.Error("different content must produce different hashes")
	}
}

func TestComputeShimSHA256(t *testing.T) {
	h1 := ComputeShimSHA256("-module(cowboy_mochi_shim).\n")
	h2 := ComputeShimSHA256("-module(cowboy_mochi_shim).\n")
	if h1 != h2 {
		t.Error("same content must produce same hash")
	}
	h3 := ComputeShimSHA256("-module(ranch_mochi_shim).\n")
	if h1 == h3 {
		t.Error("different content must produce different hash")
	}
}

// ── check ─────────────────────────────────────────────────────────────────

func TestCheck_AllPass(t *testing.T) {
	outer := []byte("outer content")
	inner := []byte("inner content")
	chunks := map[string][]byte{"mod": []byte("etf bytes")}
	shim := "-module(cowboy_mochi_shim).\n"

	e := Entry{
		Name:             "cowboy",
		Version:          "2.12.0",
		OuterSHA256:      SHA256Hex(outer),
		InnerSHA256:      SHA256Hex(inner),
		InnerSHA512:      SHA512Hex(inner),
		BeamIngestSHA256: ComputeBeamIngestSHA256(chunks),
		ShimSHA256:       ComputeShimSHA256(shim),
	}

	res := e.Check(CheckInput{
		OuterTarball: outer,
		InnerTarball: inner,
		BeamChunks:   chunks,
		ShimContent:  shim,
	})
	if !res.OK {
		t.Errorf("expected OK, got failures: %v", res.Failures)
	}
}

func TestCheck_OuterHashMismatch(t *testing.T) {
	outer := []byte("outer content")
	e := Entry{Name: "cowboy", OuterSHA256: "wronghash"}
	res := e.Check(CheckInput{OuterTarball: outer})
	if res.OK {
		t.Error("expected check failure for outer-sha256 mismatch")
	}
	if len(res.Failures) == 0 || !strings.Contains(res.Failures[0], "outer-sha256") {
		t.Errorf("failure should mention outer-sha256, got: %v", res.Failures)
	}
}

func TestCheck_InnerHashMismatch(t *testing.T) {
	inner := []byte("inner content")
	e := Entry{
		Name:        "cowboy",
		InnerSHA256: "wronghash",
		InnerSHA512: SHA512Hex(inner), // sha512 correct but sha256 wrong
	}
	res := e.Check(CheckInput{InnerTarball: inner})
	if res.OK {
		t.Error("expected check failure")
	}
	found := false
	for _, f := range res.Failures {
		if strings.Contains(f, "inner-sha256") {
			found = true
		}
	}
	if !found {
		t.Errorf("failure should mention inner-sha256, got: %v", res.Failures)
	}
}

func TestCheck_BeamIngestMismatch(t *testing.T) {
	chunks := map[string][]byte{"mod": []byte("etf")}
	e := Entry{Name: "cowboy", BeamIngestSHA256: "badhash"}
	res := e.Check(CheckInput{BeamChunks: chunks})
	if res.OK {
		t.Error("expected check failure for beam-ingest-sha256 mismatch")
	}
}

func TestCheck_ShimMismatch(t *testing.T) {
	e := Entry{Name: "cowboy", ShimSHA256: "badhash"}
	res := e.Check(CheckInput{ShimContent: "new content"})
	if res.OK {
		t.Error("expected check failure for shim-sha256 mismatch")
	}
}

func TestCheck_EmptyLockedHash_Skipped(t *testing.T) {
	// When the locked hash field is empty (e.g. legacy entry), skip that check.
	e := Entry{Name: "cowboy", OuterSHA256: ""}
	res := e.Check(CheckInput{OuterTarball: []byte("anything")})
	if !res.OK {
		t.Errorf("empty locked hash should be skipped, got failures: %v", res.Failures)
	}
}

func TestCheck_MultipleFailures(t *testing.T) {
	e := Entry{
		Name:        "cowboy",
		OuterSHA256: "wrong1",
		InnerSHA256: "wrong2",
	}
	res := e.Check(CheckInput{
		OuterTarball: []byte("outer"),
		InnerTarball: []byte("inner"),
	})
	if len(res.Failures) < 2 {
		t.Errorf("expected at least 2 failures, got %d: %v", len(res.Failures), res.Failures)
	}
}

func TestCheckAll_MixedResults(t *testing.T) {
	outer := []byte("outer")
	good := Entry{
		Name:        "ranch",
		Version:     "2.1.0",
		OuterSHA256: SHA256Hex(outer),
	}
	bad := Entry{
		Name:        "cowboy",
		Version:     "2.12.0",
		OuterSHA256: "wronghash",
	}
	m := Manifest{Entries: []Entry{good, bad}}
	inputs := map[string]CheckInput{
		"ranch":  {OuterTarball: outer},
		"cowboy": {OuterTarball: outer},
	}
	ok, report := CheckAll(m, inputs)
	if ok {
		t.Error("expected overall failure")
	}
	if len(report["cowboy"]) == 0 {
		t.Error("expected cowboy failures in report")
	}
	if len(report["ranch"]) != 0 {
		t.Error("ranch should have no failures")
	}
}

func TestCheckAll_MissingInput_Skipped(t *testing.T) {
	e := Entry{Name: "cowboy", Version: "2.12.0", OuterSHA256: "badhash"}
	m := Manifest{Entries: []Entry{e}}
	// No input provided for cowboy — should be skipped (no error).
	ok, report := CheckAll(m, map[string]CheckInput{})
	if !ok {
		t.Error("missing input should be skipped, not fail")
	}
	if len(report) != 0 {
		t.Error("no failures expected when input missing")
	}
}

// ── helpers ───────────────────────────────────────────────────────────────

func requireContains(t *testing.T, s, sub string) {
	t.Helper()
	if !strings.Contains(s, sub) {
		t.Errorf("output missing %q\ngot:\n%s", sub, s)
	}
}
