// Package lock handles the [[php-package]] table in mochi.lock.
//
// Each [[php-package]] entry records the resolved Composer package name,
// version, dist-sha256 (content-addressed cache key), reflection-sha256,
// PSR-4 autoload map, PHP version constraint, and direct dependencies.
//
// The package also implements --check mode: given a set of PhpPackage entries
// and the current reflection surface, it detects drift (changed dist-sha256 or
// reflection-sha256) and reports which packages are stale.
//
// Usage:
//
//	entries, err := lock.Parse(tomlBytes)
//	ok, drifted := lock.Check(entries, onDiskHashes)
package lock

import (
	"fmt"
	"strings"
)

// PhpPackage is one [[php-package]] entry in mochi.lock.
type PhpPackage struct {
	// Name is the Composer package name, e.g. "guzzlehttp/guzzle".
	Name string
	// Version is the resolved version string, e.g. "7.8.1".
	Version string
	// VersionNormalized is the normalized version, e.g. "7.8.1.0".
	VersionNormalized string
	// DistURL is the download URL for the dist zip.
	DistURL string
	// DistSHA256 is the SHA-256 hex of the downloaded dist zip.
	DistSHA256 string
	// ReflectionSHA256 is the SHA-256 hex of the reflection JSON output.
	ReflectionSHA256 string
	// PHPConstraint is the php requirement from composer.json, e.g. ">=8.1".
	PHPConstraint string
	// PSR4 maps namespace prefix -> source directory relative to the package root.
	PSR4 map[string]string
	// Classmap is a list of source files to scan for class definitions.
	Classmap []string
	// Files is the list of always-include files.
	Files []string
	// Require is the direct runtime dependencies (name -> constraint).
	Require map[string]string
}

// DriftEntry records a package whose lock entry no longer matches on-disk state.
type DriftEntry struct {
	// Name is the package name.
	Name string
	// Field is which field drifted: "dist-sha256" or "reflection-sha256".
	Field string
	// Locked is the SHA-256 recorded in the lockfile.
	Locked string
	// OnDisk is the SHA-256 observed on disk.
	OnDisk string
}

// String renders a DriftEntry as a human-readable line.
func (d DriftEntry) String() string {
	return fmt.Sprintf("%s: %s changed (locked=%s onDisk=%s)",
		d.Name, d.Field, short(d.Locked), short(d.OnDisk))
}

// OnDiskHashes holds the current SHA-256 values for a package.
type OnDiskHashes struct {
	// DistSHA256 is the hash of the currently-cached dist zip.
	DistSHA256 string
	// ReflectionSHA256 is the hash of the currently-cached reflection JSON.
	ReflectionSHA256 string
}

// Check compares the locked SHA-256 values against on-disk hashes.
// Returns (true, nil) if all packages match; (false, []DriftEntry) if any
// package has drifted.
func Check(entries []PhpPackage, hashes map[string]OnDiskHashes) (bool, []DriftEntry) {
	var drifted []DriftEntry
	for _, e := range entries {
		h, ok := hashes[e.Name]
		if !ok {
			drifted = append(drifted, DriftEntry{
				Name:   e.Name,
				Field:  "dist-sha256",
				Locked: e.DistSHA256,
				OnDisk: "(not cached)",
			})
			continue
		}
		if e.DistSHA256 != "" && h.DistSHA256 != e.DistSHA256 {
			drifted = append(drifted, DriftEntry{
				Name:   e.Name,
				Field:  "dist-sha256",
				Locked: e.DistSHA256,
				OnDisk: h.DistSHA256,
			})
		}
		if e.ReflectionSHA256 != "" && h.ReflectionSHA256 != e.ReflectionSHA256 {
			drifted = append(drifted, DriftEntry{
				Name:   e.Name,
				Field:  "reflection-sha256",
				Locked: e.ReflectionSHA256,
				OnDisk: h.ReflectionSHA256,
			})
		}
	}
	return len(drifted) == 0, drifted
}

// Format renders a slice of PhpPackage entries as TOML [[php-package]] blocks.
// The output is suitable for appending to a mochi.lock file.
func Format(entries []PhpPackage) string {
	var sb strings.Builder
	for _, e := range entries {
		fmt.Fprintf(&sb, "[[php-package]]\n")
		fmt.Fprintf(&sb, "name = %q\n", e.Name)
		fmt.Fprintf(&sb, "version = %q\n", e.Version)
		if e.VersionNormalized != "" {
			fmt.Fprintf(&sb, "version-normalized = %q\n", e.VersionNormalized)
		}
		if e.DistURL != "" {
			fmt.Fprintf(&sb, "dist-url = %q\n", e.DistURL)
		}
		if e.DistSHA256 != "" {
			fmt.Fprintf(&sb, "dist-sha256 = %q\n", e.DistSHA256)
		}
		if e.ReflectionSHA256 != "" {
			fmt.Fprintf(&sb, "reflection-sha256 = %q\n", e.ReflectionSHA256)
		}
		if e.PHPConstraint != "" {
			fmt.Fprintf(&sb, "php-constraint = %q\n", e.PHPConstraint)
		}
		for prefix, dir := range e.PSR4 {
			fmt.Fprintf(&sb, "psr4 = [[%q, %q]]\n", prefix, dir)
		}
		for pkg, constraint := range e.Require {
			fmt.Fprintf(&sb, "require = [[%q, %q]]\n", pkg, constraint)
		}
		fmt.Fprintf(&sb, "\n")
	}
	return sb.String()
}

// Vendor returns the vendor part of a Composer package name.
// "guzzlehttp/guzzle" -> "guzzlehttp"
func Vendor(name string) string {
	parts := strings.SplitN(name, "/", 2)
	if len(parts) == 2 {
		return parts[0]
	}
	return name
}

// Pkg returns the package part of a Composer package name.
// "guzzlehttp/guzzle" -> "guzzle"
func Pkg(name string) string {
	parts := strings.SplitN(name, "/", 2)
	if len(parts) == 2 {
		return parts[1]
	}
	return name
}

// short returns the first 12 hex chars of a SHA-256 hash, for display.
func short(sha256 string) string {
	if len(sha256) >= 12 {
		return sha256[:12] + "..."
	}
	return sha256
}
