package lockfile

import (
	"strings"
	"testing"
)

// TestPhase08LockfileIntegration is the MEP-73 Phase 8 sentinel. It
// pins the end-to-end contract that a `[[rust-package]]` table makes
// with mochi.lock: schema, encoding/decoding, drift detection.
func TestPhase08LockfileIntegration(t *testing.T) {
	t.Run("schema_roundtrip", func(t *testing.T) {
		pkgs := []RustPackage{
			{
				Name:    "anyhow",
				Version: "1.0.86",
				Source: Source{
					Kind:     SourceRegistry,
					Registry: "https://github.com/rust-lang/crates.io-index",
				},
				CrateBlake3:          "b3-anyhow",
				CrateSHA256:          "sha-anyhow",
				RustdocTypesVersion:  "32",
				RustdocSHA256:        "rd-anyhow",
				WrapperSHA256:        "wr-anyhow",
				CapabilitiesDeclared: []string{"pure"},
				Features:             []string{"std"},
				Dependencies:         []string{},
			},
			{
				Name:    "hex",
				Version: "0.4.3",
				Source: Source{
					Kind:     SourceRegistry,
					Registry: "https://github.com/rust-lang/crates.io-index",
				},
				CrateBlake3:          "b3-hex",
				CrateSHA256:          "sha-hex",
				RustdocTypesVersion:  "32",
				RustdocSHA256:        "rd-hex",
				WrapperSHA256:        "wr-hex",
				CapabilitiesDeclared: []string{"pure"},
				Features:             []string{"alloc"},
				Dependencies:         []string{},
			},
		}
		body1 := Encode(pkgs)
		decoded, err := DecodeString(body1)
		if err != nil {
			t.Fatalf("decode: %v", err)
		}
		if len(decoded) != len(pkgs) {
			t.Fatalf("expected %d pkgs, got %d", len(pkgs), len(decoded))
		}
		body2 := Encode(decoded)
		if body1 != body2 {
			t.Errorf("roundtrip not byte-stable\n---first---\n%s\n---second---\n%s", body1, body2)
		}
	})

	t.Run("wrapper_sha_pins_drift", func(t *testing.T) {
		w := basePkg()
		h := basePkg()
		h.WrapperSHA256 = WrapperSHA256("pub fn add(a: i64, b: i64) -> i64 { a - b }\n")
		w.WrapperSHA256 = WrapperSHA256("pub fn add(a: i64, b: i64) -> i64 { a + b }\n")
		drifts := Check([]RustPackage{w}, []RustPackage{h})
		if len(drifts) != 1 || drifts[0].Kind != DriftWrapperSHA256 {
			t.Fatalf("expected wrapper-sha256 drift, got %v", drifts)
		}
		if drifts[0].Want == drifts[0].Have {
			t.Errorf("want/have should differ: %+v", drifts[0])
		}
	})

	t.Run("check_clean_pair_returns_nil", func(t *testing.T) {
		w := basePkg()
		h := basePkg()
		if drifts := Check([]RustPackage{w}, []RustPackage{h}); drifts != nil {
			t.Fatalf("expected nil drift on identical input, got %v", drifts)
		}
	})

	t.Run("check_detects_capability_addition", func(t *testing.T) {
		w := basePkg()
		h := basePkg()
		h.CapabilitiesDeclared = []string{"pure", "net"}
		drifts := Check([]RustPackage{w}, []RustPackage{h})
		if len(drifts) != 1 || drifts[0].Kind != DriftCapabilityAdded {
			t.Fatalf("expected capability-added drift, got %v", drifts)
		}
	})

	t.Run("schema_matches_mep73_section_3", func(t *testing.T) {
		// Hand-written exemplar based on MEP-73 §3 — encoded shape
		// the spec promises a user-edited mochi.lock will look like.
		body := `# preamble line from surrounding mochi.lock

[[rust-package]]
name = "hex"
version = "0.4.3"
source = { kind = "registry", registry = "https://github.com/rust-lang/crates.io-index" }
crate-blake3 = "b3-hex"
crate-sha256 = "sha-hex"
rustdoc-types-version = "32"
rustdoc-sha256 = "rd-hex"
wrapper-sha256 = "wr-hex"
capabilities-declared = ["pure"]
features = ["alloc"]
`
		pkgs, err := DecodeString(body)
		if err != nil {
			t.Fatalf("decode: %v", err)
		}
		if len(pkgs) != 1 {
			t.Fatalf("expected 1 package, got %d", len(pkgs))
		}
		p := pkgs[0]
		if p.Name != "hex" || p.Version != "0.4.3" {
			t.Errorf("bad header: %+v", p)
		}
		if p.Source.Kind != SourceRegistry || !strings.Contains(p.Source.Registry, "crates.io-index") {
			t.Errorf("bad source: %+v", p.Source)
		}
		if p.CrateBlake3 != "b3-hex" || p.CrateSHA256 != "sha-hex" {
			t.Errorf("bad crate hashes: %+v", p)
		}
		if p.RustdocTypesVersion != "32" || p.RustdocSHA256 != "rd-hex" {
			t.Errorf("bad rustdoc fields: %+v", p)
		}
		if p.WrapperSHA256 != "wr-hex" {
			t.Errorf("bad wrapper sha: %+v", p)
		}
		if len(p.CapabilitiesDeclared) != 1 || p.CapabilitiesDeclared[0] != "pure" {
			t.Errorf("bad capabilities: %+v", p.CapabilitiesDeclared)
		}
		if len(p.Features) != 1 || p.Features[0] != "alloc" {
			t.Errorf("bad features: %+v", p.Features)
		}
	})

	t.Run("section_3_roundtrips_under_check", func(t *testing.T) {
		// Encode → Decode → Check(want, have) == nil
		want := []RustPackage{
			{
				Name:    "hex",
				Version: "0.4.3",
				Source: Source{
					Kind:     SourceRegistry,
					Registry: "https://github.com/rust-lang/crates.io-index",
				},
				CrateBlake3:          "b3-hex",
				CrateSHA256:          "sha-hex",
				RustdocTypesVersion:  "32",
				RustdocSHA256:        "rd-hex",
				WrapperSHA256:        "wr-hex",
				CapabilitiesDeclared: []string{"pure"},
				Features:             []string{"alloc"},
			},
		}
		body := Encode(want)
		decoded, err := DecodeString(body)
		if err != nil {
			t.Fatalf("decode: %v", err)
		}
		if drifts := Check(want, decoded); drifts != nil {
			t.Fatalf("expected clean check after roundtrip, got %v", drifts)
		}
	})
}
