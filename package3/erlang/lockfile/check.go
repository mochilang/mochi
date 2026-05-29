package lockfile

import "fmt"

// CheckInput contains the runtime-computed values to compare against the
// locked checksums in an Entry. All byte slices are the raw content (not
// hex strings). ShimContent is the text of the synthesised shim.erl.
type CheckInput struct {
	// OuterTarball is the content of the outer .tar.gz file downloaded
	// from Hex.pm. Used to recompute outer-sha256.
	OuterTarball []byte
	// InnerTarball is the content of the inner contents.tar.gz embedded
	// inside the outer archive. Used to recompute inner-sha256 and inner-sha512.
	InnerTarball []byte
	// BeamChunks maps module name → raw ETF payload bytes from the Dbgi/Abst
	// chunk. Used to recompute beam-ingest-sha256.
	BeamChunks map[string][]byte
	// ShimContent is the current synthesised shim.erl source text.
	// Used to recompute shim-sha256.
	ShimContent string
}

// CheckResult records the outcome of a per-entry checksum verification.
type CheckResult struct {
	// OK is true only when every individual check passed.
	OK bool
	// Failures lists each failed check as a human-readable description of
	// the mismatch (field name, want, got).
	Failures []string
}

// Check verifies that the five checksums recorded in e match the values
// recomputed from input. It returns a CheckResult whose OK field is true
// only when all five hashes agree.
func (e Entry) Check(input CheckInput) CheckResult {
	var failures []string

	check := func(field, want, got string) {
		if want != "" && want != got {
			failures = append(failures, fmt.Sprintf(
				"%s: want %s, got %s", field, truncate(want, 16), truncate(got, 16),
			))
		}
	}

	if len(input.OuterTarball) > 0 {
		check("outer-sha256", e.OuterSHA256, SHA256Hex(input.OuterTarball))
	}
	if len(input.InnerTarball) > 0 {
		check("inner-sha256", e.InnerSHA256, SHA256Hex(input.InnerTarball))
		check("inner-sha512", e.InnerSHA512, SHA512Hex(input.InnerTarball))
	}
	if len(input.BeamChunks) > 0 {
		check("beam-ingest-sha256", e.BeamIngestSHA256, ComputeBeamIngestSHA256(input.BeamChunks))
	}
	if input.ShimContent != "" {
		check("shim-sha256", e.ShimSHA256, ComputeShimSHA256(input.ShimContent))
	}

	return CheckResult{OK: len(failures) == 0, Failures: failures}
}

// CheckAll runs Check on every entry in m and collects all failures.
// Returns (allOK, map[packageName][]failureStrings).
func CheckAll(m Manifest, inputs map[string]CheckInput) (bool, map[string][]string) {
	allOK := true
	report := make(map[string][]string)
	for _, e := range m.Entries {
		inp, ok := inputs[e.Name]
		if !ok {
			continue
		}
		result := e.Check(inp)
		if !result.OK {
			allOK = false
			report[e.Name] = result.Failures
		}
	}
	return allOK, report
}

func truncate(s string, n int) string {
	if len(s) <= n {
		return s
	}
	return s[:n] + "..."
}
