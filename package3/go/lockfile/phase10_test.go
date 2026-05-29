package lockfile

import (
	"strings"
	"testing"
)

// TestPhase10LockfileSentinel is the MEP-74 phase 10 end-to-end
// sentinel. It exercises the full encode -> decode -> drift-check
// cycle against the cobra fixture (the canonical MEP-74 example) and
// proves:
//
//   - the rendered TOML contains every field MEP-74 spec §3
//     requires,
//   - the rendered TOML is deterministic across multiple runs,
//   - DecodeString reads back the same structure (round-trip),
//   - CheckDrift surfaces every kind of mismatch on a mutated copy
//     and returns no drifts for the unchanged copy.
//
// The fixture is intentionally inline so the sentinel does not
// depend on a separate goldens directory. The goldens for the
// `mochi pkg lock` CLI will land in phase 10.1 once the CLI wiring
// from MEP-57 is extended to read `[go-dependencies]`.
func TestPhase10LockfileSentinel(t *testing.T) {
	cobra := GoPackage{
		Module:  "github.com/spf13/cobra",
		Version: "v1.8.0",
		Source: Source{
			Kind:  SourceModuleProxy,
			Proxy: "https://proxy.golang.org",
		},
		ZipBlake3:       "1111111111111111111111111111111111111111111111111111111111111111",
		ZipH1:           "h1:cobra-v1.8.0",
		SumdbVerified:   true,
		SumdbTreeSize:   23456789,
		SumdbRecordHash: "abcdef0123456789",
		ApiSurfaceSha256: "2222222222222222222222222222222222222222222222222222222222222222",
		WrapperSha256:   "3333333333333333333333333333333333333333333333333333333333333333",
		CapabilitiesDeclared: []string{"fs"},
		Dependencies:    []string{"github.com/inconshreveable/mousetrap@v1.1.0", "github.com/spf13/pflag@v1.0.6"},
		BuildTags:       nil,
	}
	pflag := GoPackage{
		Module:               "github.com/spf13/pflag",
		Version:              "v1.0.6",
		Source:               Source{Kind: SourceModuleProxy, Proxy: "https://proxy.golang.org"},
		ZipBlake3:            "4444444444444444444444444444444444444444444444444444444444444444",
		ZipH1:                "h1:pflag-v1.0.6",
		SumdbVerified:        true,
		SumdbTreeSize:        23456789,
		SumdbRecordHash:      "fedcba9876543210",
		ApiSurfaceSha256:     "5555555555555555555555555555555555555555555555555555555555555555",
		WrapperSha256:        "6666666666666666666666666666666666666666666666666666666666666666",
		CapabilitiesDeclared: []string{},
	}
	pkgs := []GoPackage{cobra, pflag}

	t.Run("encode covers every spec field", func(t *testing.T) {
		encoded := Encode(pkgs)
		mandatoryFields := []string{
			"[[go-package]]",
			`module = "github.com/spf13/cobra"`,
			`version = "v1.8.0"`,
			`source = { kind = "module-proxy", proxy = "https://proxy.golang.org" }`,
			"zip-blake3 = ",
			"zip-h1 = ",
			"sumdb-verified = true",
			"sumdb-tree-size = 23456789",
			"sumdb-record-hash = ",
			"api-surface-sha256 = ",
			"wrapper-sha256 = ",
			`capabilities-declared = ["fs"]`,
			"dependencies = [",
		}
		for _, want := range mandatoryFields {
			if !strings.Contains(encoded, want) {
				t.Errorf("encoded body missing %q\n--- body ---\n%s", want, encoded)
			}
		}
	})

	t.Run("encode is byte-deterministic", func(t *testing.T) {
		first := Encode(pkgs)
		for i := 0; i < 16; i++ {
			if got := Encode(pkgs); got != first {
				t.Fatalf("Encode non-deterministic on iter %d", i)
			}
		}
		// Permute the slice; output should stay identical.
		permuted := []GoPackage{pflag, cobra}
		if got := Encode(permuted); got != first {
			t.Errorf("Encode not stable under permutation:\n--- first ---\n%s\n--- permuted ---\n%s", first, got)
		}
	})

	t.Run("round-trip preserves fields", func(t *testing.T) {
		encoded := Encode(pkgs)
		decoded, err := DecodeString(encoded)
		if err != nil {
			t.Fatalf("DecodeString: %v\n--- body ---\n%s", err, encoded)
		}
		if len(decoded) != 2 {
			t.Fatalf("decoded %d entries; want 2", len(decoded))
		}
		gotCobra := decoded[0]
		if gotCobra.Module != "github.com/spf13/cobra" {
			t.Errorf("first entry module = %q; want cobra", gotCobra.Module)
		}
		if gotCobra.SumdbTreeSize != cobra.SumdbTreeSize {
			t.Errorf("SumdbTreeSize = %d; want %d", gotCobra.SumdbTreeSize, cobra.SumdbTreeSize)
		}
		if gotCobra.ZipH1 != cobra.ZipH1 {
			t.Errorf("ZipH1 = %q; want %q", gotCobra.ZipH1, cobra.ZipH1)
		}
		if len(gotCobra.Dependencies) != 2 {
			t.Errorf("Dependencies = %v; want 2 entries", gotCobra.Dependencies)
		}
	})

	t.Run("drift-check on identical state is empty", func(t *testing.T) {
		if got := CheckDrift(pkgs, pkgs); len(got) != 0 {
			t.Errorf("CheckDrift on identical state returned drifts: %#v", got)
		}
	})

	t.Run("drift-check reports every mutated field", func(t *testing.T) {
		mutated := []GoPackage{
			{
				Module: cobra.Module, Version: cobra.Version, Source: cobra.Source,
				ZipBlake3:            "DRIFTED-ZIP-BLAKE3",
				ZipH1:                cobra.ZipH1,
				SumdbVerified:        cobra.SumdbVerified,
				SumdbTreeSize:        cobra.SumdbTreeSize + 1,
				SumdbRecordHash:      cobra.SumdbRecordHash,
				ApiSurfaceSha256:     "DRIFTED-API",
				WrapperSha256:        "DRIFTED-WRAPPER",
				CapabilitiesDeclared: []string{"fs", "net"},
				Dependencies:         cobra.Dependencies,
			},
			pflag,
		}
		drifts := CheckDrift(pkgs, mutated)
		kinds := map[DriftKind]int{}
		for _, d := range drifts {
			if d.Module != "github.com/spf13/cobra" {
				t.Errorf("drift on wrong module: %v", d)
			}
			kinds[d.Kind]++
		}
		for _, want := range []DriftKind{DriftZip, DriftSumdb, DriftAPI, DriftWrapper, DriftCapabilities} {
			if kinds[want] == 0 {
				t.Errorf("CheckDrift missing %s drift; got kinds %v", want, kinds)
			}
		}
	})

	t.Run("drift-check surfaces missing and stale", func(t *testing.T) {
		liveOnlyPflag := []GoPackage{pflag}
		drifts := CheckDrift(pkgs, liveOnlyPflag)
		// cobra in locked but not in live -> stale.
		stale := false
		for _, d := range drifts {
			if d.Kind == DriftStale && d.Module == "github.com/spf13/cobra" {
				stale = true
			}
		}
		if !stale {
			t.Errorf("expected stale drift for cobra; got %v", drifts)
		}

		lockedOnlyPflag := []GoPackage{pflag}
		drifts2 := CheckDrift(lockedOnlyPflag, pkgs)
		missing := false
		for _, d := range drifts2 {
			if d.Kind == DriftMissing && d.Module == "github.com/spf13/cobra" {
				missing = true
			}
		}
		if !missing {
			t.Errorf("expected missing drift for cobra; got %v", drifts2)
		}
	})
}
