package sparse

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"sort"

	"mochi/package3/rust/semver"
)

// CrateEntry is one record from a sparse-index file. Each line of a
// sparse-index file is exactly one CrateEntry encoded as JSON. The shape
// follows cargo's RegistryPackage struct in
// cargo/src/cargo/sources/registry/index.rs.
//
// Fields are documented inline. The `V` field is the schema version of
// the entry: 1 (default, used when missing) lacks features2/links;
// 2 introduced features2 (so that complex feature graphs can avoid
// "default-features = false" diamond bugs); higher numbers are reserved.
type CrateEntry struct {
	// Name is the published crate name. Preserves publish-time casing
	// and separator. Index lookup must use CrateNameEqual for matching.
	Name string `json:"name"`

	// Vers is the semver string for this release. Empty Pre means a
	// non-prerelease version. cargo accepts the relaxed form but
	// publishes only full N.N.N strings.
	Vers string `json:"vers"`

	// Deps is the list of dependencies declared in this version's
	// Cargo.toml, normalised against the registry's view (i.e. patched
	// to point at the same registry where applicable).
	Deps []CrateDep `json:"deps"`

	// Cksum is the lowercase hex SHA-256 of the .crate tarball. The
	// digest is mandatory for sparse 1.68+; older mirrors may emit it
	// as the empty string when republishing legacy entries.
	Cksum string `json:"cksum"`

	// Features is the feature map exactly as it appears in Cargo.toml.
	Features map[string][]string `json:"features,omitempty"`

	// Features2 is the schema-v2 supplement to Features. When merging
	// for resolution, Features and Features2 are combined into one map
	// with Features2 winning on key collision.
	Features2 map[string][]string `json:"features2,omitempty"`

	// Yanked is true if cargo yank pulled this version from the index.
	// A yanked version is still resolvable if it appears in a lockfile
	// but is excluded from fresh resolution.
	Yanked bool `json:"yanked"`

	// Links is the `links =` Cargo.toml key. When non-empty, two
	// resolved versions of the same `links` value collide and the
	// resolver must fail. Used for native-library shims (libc, openssl
	// -sys, ...).
	Links string `json:"links,omitempty"`

	// V is the schema-version of the entry; 1 if absent.
	V int `json:"v,omitempty"`

	// RustVersion is the minimum supported rustc; "1.78" or similar.
	// When empty there is no MSRV constraint.
	RustVersion string `json:"rust_version,omitempty"`
}

// CrateDep is a single dependency record inside a CrateEntry. cargo
// records normalises this to a stable shape regardless of the dependency
// table the user wrote in their Cargo.toml.
type CrateDep struct {
	// Name is how the dependency is referenced by its consumer (may
	// differ from Package via dep-renaming).
	Name string `json:"name"`

	// Req is the semver requirement string (e.g. "^1.0", "~1.2",
	// ">=0.5, <0.7").
	Req string `json:"req"`

	// Features is the list of features requested in addition to default.
	Features []string `json:"features"`

	// Optional marks the dep as opt-in via a feature flag.
	Optional bool `json:"optional"`

	// DefaultFeatures defaults to true; false omits the "default" feature.
	DefaultFeatures bool `json:"default_features"`

	// Target gates the dep behind a cfg expression, e.g. "cfg(unix)"
	// or "x86_64-unknown-linux-gnu". Empty means unconditional.
	Target string `json:"target,omitempty"`

	// Kind is one of "normal", "dev", "build".
	Kind string `json:"kind,omitempty"`

	// Registry is the index URL when the dep comes from a non-default
	// registry, otherwise empty.
	Registry string `json:"registry,omitempty"`

	// Package is the original crate name when the dep is renamed
	// (e.g. `foo = { package = "real-foo", ... }`).
	Package string `json:"package,omitempty"`
}

// ParseIndex reads an NDJSON sparse-index stream and returns the entries
// in publish order (the order on disk). Blank lines are skipped. Each
// non-blank line must be a complete JSON object; partial reads are not
// supported because cargo's index files are typically a few KB.
func ParseIndex(r io.Reader) ([]CrateEntry, error) {
	out := []CrateEntry{}
	scanner := bufio.NewScanner(r)
	scanner.Buffer(make([]byte, 0, 64*1024), 2*1024*1024)
	lineNo := 0
	for scanner.Scan() {
		lineNo++
		line := scanner.Bytes()
		if len(line) == 0 {
			continue
		}
		var entry CrateEntry
		if err := json.Unmarshal(line, &entry); err != nil {
			return nil, fmt.Errorf("sparse: line %d: %w", lineNo, err)
		}
		out = append(out, entry)
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("sparse: scanning index: %w", err)
	}
	return out, nil
}

// Version returns the parsed semver Version of the entry. Returns an
// error if Vers does not parse, which indicates a corrupted index.
func (e CrateEntry) Version() (semver.Version, error) {
	v, err := semver.Parse(e.Vers)
	if err != nil {
		return semver.Version{}, fmt.Errorf("sparse: %q vers: %w", e.Name, err)
	}
	return v, nil
}

// MergedFeatures returns Features ∪ Features2 with Features2 winning on
// collision. Returns a freshly allocated map so the caller may mutate.
func (e CrateEntry) MergedFeatures() map[string][]string {
	out := make(map[string][]string, len(e.Features)+len(e.Features2))
	for k, v := range e.Features {
		copyV := make([]string, len(v))
		copy(copyV, v)
		out[k] = copyV
	}
	for k, v := range e.Features2 {
		copyV := make([]string, len(v))
		copy(copyV, v)
		out[k] = copyV
	}
	return out
}

// SortEntriesByVersion sorts entries ascending by their semver Version.
// Yanked entries sort after non-yanked entries at the same version.
// Entries with unparsable Vers sort last in input order. Mutates in place.
func SortEntriesByVersion(entries []CrateEntry) {
	sort.SliceStable(entries, func(i, j int) bool {
		vi, errI := entries[i].Version()
		vj, errJ := entries[j].Version()
		switch {
		case errI != nil && errJ != nil:
			return false
		case errI != nil:
			return false
		case errJ != nil:
			return true
		}
		if c := vi.Compare(vj); c != 0 {
			return c < 0
		}
		// Same version: non-yanked first.
		if entries[i].Yanked != entries[j].Yanked {
			return !entries[i].Yanked
		}
		return false
	})
}

// LatestMatching scans entries for the highest semver Version that
// satisfies req and is not yanked. Returns the matching entry and true,
// or a zero entry and false if no candidate qualifies.
func LatestMatching(entries []CrateEntry, req semver.Req) (CrateEntry, bool) {
	var best CrateEntry
	var bestV semver.Version
	found := false
	for _, e := range entries {
		if e.Yanked {
			continue
		}
		v, err := e.Version()
		if err != nil {
			continue
		}
		if !req.Matches(v) {
			continue
		}
		if !found || bestV.Compare(v) < 0 {
			best = e
			bestV = v
			found = true
		}
	}
	return best, found
}
