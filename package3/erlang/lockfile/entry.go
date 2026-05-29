// Package lockfile defines the [[erlang-package]] repeated table that the
// MEP-66 bridge appends to mochi.lock, plus Parse/Render helpers and the
// --check-mode checksum verifier.
//
// On-disk shape (TOML):
//
//	[[erlang-package]]
//	name             = "cowboy"
//	version          = "2.12.0"
//	registry         = "https://hex.pm"
//	outer-sha256     = "<hex64>"
//	inner-sha256     = "<hex64>"
//	inner-sha512     = "<hex128>"
//	beam-ingest-sha256 = "<hex64>"
//	shim-sha256      = "<hex64>"
//	capabilities     = ["net"]
//	dependencies     = ["cowlib@~> 2.13", "ranch@~> 2.1"]
//	otp-app          = "cowboy"
//	modules          = ["cowboy", "cowboy_req"]
package lockfile

import "sort"

// SchemaVersion is the currently understood [[erlang-package]] schema.
const SchemaVersion = 1

// Entry represents one [[erlang-package]] block in mochi.lock.
type Entry struct {
	// Name is the Hex.pm package name (lowercase, e.g. "cowboy").
	Name string
	// Version is the exact resolved version ("2.12.0").
	Version string
	// Registry is the Hex.pm registry URL (default "https://hex.pm").
	Registry string
	// OuterSHA256 is the SHA-256 hex string of the outer .tar.gz file.
	OuterSHA256 string
	// InnerSHA256 is the SHA-256 hex string of the inner contents.tar.gz.
	InnerSHA256 string
	// InnerSHA512 is the SHA-512 hex string of the inner contents.tar.gz.
	InnerSHA512 string
	// BeamIngestSHA256 is the SHA-256 of the sorted, concatenated ETF bytes
	// read from the Dbgi/Abst chunks of every .beam file in the package.
	BeamIngestSHA256 string
	// ShimSHA256 is the SHA-256 of the synthesised shim.erl content.
	ShimSHA256 string
	// Capabilities is the sorted list of declared capability tokens.
	Capabilities []string
	// Dependencies is the list of dependency constraints ("cowlib@~> 2.13").
	Dependencies []string
	// OTPApp is the OTP application name (atom in the .app file). May differ
	// from Name for some packages.
	OTPApp string
	// Modules is the sorted list of Erlang module names exposed by the bridge.
	Modules []string
}

// Manifest is the collection of [[erlang-package]] entries for a workspace.
type Manifest struct {
	// Version is the schema version written to / read from the file.
	Version int
	// Entries is sorted by Name for deterministic output.
	Entries []Entry
}

// Sort sorts entries by Name ascending.
func (m *Manifest) Sort() {
	sort.SliceStable(m.Entries, func(i, j int) bool {
		return m.Entries[i].Name < m.Entries[j].Name
	})
}
