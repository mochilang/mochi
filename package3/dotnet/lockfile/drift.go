package lockfile

import (
	"crypto/sha256"
	"crypto/sha512"
	"encoding/hex"
	"fmt"
	"io"
	"os"
	"sort"
)

// DriftReport records one detected difference between the current and
// expected state of a [[dotnet-package]] entry.
type DriftReport struct {
	// ID is the NuGet package id the drift applies to.
	ID string
	// Field is the field name that drifted (e.g. "nupkg-sha512").
	Field string
	// Stored is the value recorded in mochi.lock.
	Stored string
	// Current is the freshly computed value.
	Current string
}

// String renders a DriftReport for diagnostic output.
func (d *DriftReport) String() string {
	return fmt.Sprintf("%s: %s drift: stored %s current %s", d.ID, d.Field, d.Stored, d.Current)
}

// CheckNupkgSHA512 verifies the .nupkg tarball at path against the stored hash.
// Returns a DriftReport if the hashes differ, nil if they match.
func CheckNupkgSHA512(path, stored string) (*DriftReport, error) {
	current, err := hashFileSHA512(path)
	if err != nil {
		return nil, fmt.Errorf("lockfile: check nupkg-sha512: %w", err)
	}
	if current == stored {
		return nil, nil
	}
	id := idFromPath(path)
	return &DriftReport{ID: id, Field: "nupkg-sha512", Stored: stored, Current: current}, nil
}

// CheckMetadataSHA256 verifies the metadata JSON at path against the stored hash.
// Returns a DriftReport if the hashes differ, nil if they match.
func CheckMetadataSHA256(path, stored string) (*DriftReport, error) {
	current, err := hashFileSHA256(path)
	if err != nil {
		return nil, fmt.Errorf("lockfile: check metadata-sha256: %w", err)
	}
	if current == stored {
		return nil, nil
	}
	id := idFromPath(path)
	return &DriftReport{ID: id, Field: "metadata-sha256", Stored: stored, Current: current}, nil
}

// CheckShimSHA256 verifies the Bridge.cs file at path against the stored hash.
// Returns a DriftReport if the hashes differ, nil if they match.
func CheckShimSHA256(path, stored string) (*DriftReport, error) {
	current, err := hashFileSHA256(path)
	if err != nil {
		return nil, fmt.Errorf("lockfile: check shim-sha256: %w", err)
	}
	if current == stored {
		return nil, nil
	}
	id := idFromPath(path)
	return &DriftReport{ID: id, Field: "shim-sha256", Stored: stored, Current: current}, nil
}

// CheckCapabilities verifies that the new capability set is a subset of stored.
// Returns a DriftReport if new capabilities appear (monotonicity rule from MEP-57 §1.6).
// Capability removal is not reported as drift.
func CheckCapabilities(id string, stored, current []string) *DriftReport {
	added := stringSetDiff(current, stored)
	if len(added) == 0 {
		return nil
	}
	sort.Strings(added)
	return &DriftReport{
		ID:      id,
		Field:   "capabilities-declared",
		Stored:  joinSortedSlice(stored),
		Current: fmt.Sprintf("new: %v", added),
	}
}

// hashFileSHA512 computes the lowercase hex SHA-512 of a file.
func hashFileSHA512(path string) (string, error) {
	f, err := os.Open(path)
	if err != nil {
		return "", err
	}
	defer f.Close()
	h := sha512.New()
	if _, err := io.Copy(h, f); err != nil {
		return "", err
	}
	return hex.EncodeToString(h.Sum(nil)), nil
}

// hashFileSHA256 computes the lowercase hex SHA-256 of a file.
func hashFileSHA256(path string) (string, error) {
	f, err := os.Open(path)
	if err != nil {
		return "", err
	}
	defer f.Close()
	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		return "", err
	}
	return hex.EncodeToString(h.Sum(nil)), nil
}

// stringSetDiff returns elements in a that are not in b.
func stringSetDiff(a, b []string) []string {
	set := map[string]struct{}{}
	for _, s := range b {
		set[s] = struct{}{}
	}
	var out []string
	for _, s := range a {
		if _, ok := set[s]; !ok {
			out = append(out, s)
		}
	}
	sort.Strings(out)
	return out
}

// joinSortedSlice returns a sorted comma-joined string of slice items.
func joinSortedSlice(vs []string) string {
	cp := append([]string{}, vs...)
	sort.Strings(cp)
	return fmt.Sprintf("%v", cp)
}

// idFromPath extracts a simple base name from a file path for use in DriftReport.ID.
func idFromPath(path string) string {
	// Use the file name as a best-effort id.
	for i := len(path) - 1; i >= 0; i-- {
		if path[i] == '/' || path[i] == '\\' {
			return path[i+1:]
		}
	}
	return path
}
